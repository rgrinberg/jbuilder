open Import
open Fiber.O

type step = run_id:Run_id.t -> (unit, [ `Already_reported ]) Result.t Fiber.t

let next_run_id = ref 1

let build_finish (build_result : Build_outcome.t) =
  let message =
    match build_result with
    | Success -> Pp.tag User_message.Style.Success (Pp.verbatim "Success")
    | Failure ->
      let failure_message =
        match
          Build_system_error.(
            Id.Map.cardinal (Set.current (Fiber.Svar.read Build_system.errors)))
        with
        | 1 -> Pp.textf "Had 1 error"
        | n -> Pp.textf "Had %d errors" n
      in
      Pp.tag User_message.Style.Error failure_message
  in
  Console.Status_line.set
    (Constant (Pp.seq message (Pp.verbatim ", waiting for filesystem changes...")))
;;

let invalidation_of_file_events events =
  List.fold_left events ~init:Memo.Invalidation.empty ~f:(fun acc event ->
    let invalidation =
      match (event : Event.File_watcher_event.t) with
      | Fs_memo_event event -> Fs_memo.handle_fs_event event
      | Queue_overflow -> Memo.Invalidation.clear_caches ~reason:Event_queue_overflow
      | Watcher_terminated ->
        Log.info "Shutdown" [ "reason", Dyn.string "Filesystem watcher terminated" ];
        raise Dune_util.Report_error.Already_reported
    in
    Memo.Invalidation.combine acc invalidation)
;;

let rec handle_file_events t file_watcher =
  File_watcher.read file_watcher
  >>= function
  | None -> Fiber.return ()
  | Some events ->
    let* () =
      Scheduler.Build_loop.request_restart t (invalidation_of_file_events events)
    in
    handle_file_events t file_watcher
;;

let run_with_file_watcher t f =
  match Scheduler.Build_loop.file_watcher t with
  | None ->
    ignore (Fs_memo.init ~dune_file_watcher:None : Memo.Invalidation.t);
    f ()
  | Some file_watcher ->
    let initial_invalidation = Fs_memo.init ~dune_file_watcher:(Some file_watcher) in
    Memo.reset initial_invalidation;
    Fiber.fork_and_join_unit
      (fun () -> handle_file_events t file_watcher)
      (fun () ->
         Fiber.finalize f ~finally:(fun () ->
           File_watcher.close file_watcher;
           Fiber.return ()))
;;

let rec poll_iter t step =
  let run_id =
    let run_id = Run_id.Watch !next_run_id in
    incr next_run_id;
    run_id
  in
  let invalidation = Scheduler.Build_loop.pending_invalidation t in
  if Memo.Invalidation.is_empty invalidation
  then Memo.Metrics.reset ()
  else (
    Dune_trace.emit Build (fun () ->
      let reasons = Memo.Invalidation.details_hum ~max_elements:max_int invalidation in
      Dune_trace.Event.watch_build_restart
        ~run_id:(Run_id.to_int run_id)
        ~reasons
        ~at:(Time.now ()));
    let details_hum = Memo.Invalidation.details_hum invalidation in
    Console.maybe_clear_screen ~details_hum;
    Memo.reset invalidation);
  Scheduler.Build_loop.start_iteration t;
  let* res = step ~run_id in
  match Scheduler.Build_loop.finish_iteration t with
  | `Restart -> poll_iter t step
  | `Done ->
    let res : Build_outcome.t =
      match res with
      | Error `Already_reported -> Failure
      | Ok () -> Success
    in
    build_finish res;
    Fiber.return res
;;

(* Work we're allowed to do between successive polling iterations. this work
   should be fast and never fail (within reason) *)
let run_when_idle () : unit =
  Dune_trace.emit ~buffered:true Scheduler Dune_trace.Event.scheduler_idle;
  Dune_trace.flush ()
;;

let poll step =
  let* t = Scheduler.Build_loop.init () in
  let rec loop () =
    let* (_ : Build_outcome.t) = poll_iter t step in
    run_when_idle ();
    let* () = Scheduler.Build_loop.wait_for_build_input_change t in
    loop ()
  in
  run_with_file_watcher t loop
;;

let poll_passive ~get_build_request =
  let* t = Scheduler.Build_loop.init () in
  let rec loop () =
    let* step, response_ivar = get_build_request in
    (* Flush before to make the build reproducible. The passive watch mode is
       designed for tests and We want to observe all the change made by the
       test before starting the build. *)
    let* () =
      match Scheduler.Build_loop.file_watcher t with
      | None -> Fiber.return ()
      | Some file_watcher -> File_watcher.flush file_watcher
    in
    let* res = poll_iter t step in
    let* () = Fiber.Ivar.fill response_ivar res in
    loop ()
  in
  run_with_file_watcher t loop
;;
