open Stdune

module Request = struct
  type t = Ping

  let to_payload : t -> Dune_rpc.Payload.t = function
    | Ping -> { method_ = "ping"; params = List [] }

  let of_request _ = Some Ping

  let payload_of_response req resp : Sexp.t =
    match (req, resp) with
    | Ping, () -> List []

  let response_of_payload req resp =
    match (req, resp) with
    | Ping, _ -> ()
end

let on_request lock _ req : Sexp.t Fiber.t =
  match Request.of_request req with
  | Some r ->
    let open Fiber.O in
    let+ resp =
      Fiber.Mutex.with_lock lock (fun () ->
          match r with
          | Ping -> Fiber.return ())
    in
    Request.payload_of_response r resp
  | None ->
    (* send some standard error *)
    assert false

let handler lock : Dune_rpc.Handler.t =
  Dune_rpc.Handler.create ~on_request:(on_request lock)
    ~on_notification:(fun _ _ -> Fiber.return ())
    ~on_init:(fun _ -> Fiber.return ())

let config () =
  let mutex = Fiber.Mutex.create () in
  let handler = handler mutex in
  Dune_engine.Config.Rpc.Server { handler; backlog = 10; mutex }
