open Stdune

module Conv = struct
  (* Mini clone of Dune_lang.Decoder. Main advantage is that it forbids all the
     crazy stuff and is automatically bi-directional *)

  (* TODO error handling is complete crap for now.

     This should be unified with [Dune_lang.Decoder] eventually. *)

  exception Of_sexp

  module Fields = struct
    type t = Unparsed of Sexp.t String.Map.t

    let check_empty (Unparsed s) = if String.Map.is_empty s then raise Of_sexp

    let empty = Unparsed String.Map.empty

    let merge (Unparsed a) (Unparsed b) =
      Unparsed
        (String.Map.union a b ~f:(fun _ _ _ ->
             (* field names are guaranteed to be different at construction time
                in [Both] *)
             assert false))

    let of_field name sexp = Unparsed (String.Map.singleton name sexp)

    let of_sexp (x : Sexp.t) =
      match x with
      | Atom _ -> raise Of_sexp
      | List x -> (
        match
          String.Map.of_list_map x ~f:(function
            | List [ Atom s; v ] -> (s, v)
            | _ -> raise Of_sexp)
        with
        | Error _ -> raise Of_sexp
        | Ok s -> Unparsed s )

    let optional (Unparsed t) name =
      match String.Map.find t name with
      | None -> (None, Unparsed t)
      | Some v -> (Some v, Unparsed (String.Map.remove t name))

    let required t name =
      let r, t = optional t name in
      match r with
      | None -> raise Of_sexp
      | Some s -> (s, t)

    let to_sexp (Unparsed t) : Sexp.t =
      List
        ( String.Map.to_list t
        |> List.map ~f:(fun (k, v) -> Sexp.List [ Atom k; v ]) )
  end

  type values = Sexp.t

  type fields = Fields.t

  type ('a, 'kind) t =
    | Iso : ('a, 'kind) t * ('a -> 'b) * ('b -> 'a) -> ('b, 'kind) t
    | Both :
        (* Invariant: field names must be different *)
        ('a, fields) t
        * ('b, fields) t
        -> ('a * 'b, fields) t
    | Sexp : (Sexp.t, values) t
    | List : ('a, values) t -> ('a list, values) t
    | Field : string * 'a field -> ('a, fields) t
    | Either :
        (* Invariant: field names must be different *)
        ('a, fields) t
        * ('b, fields) t
        -> (('a, 'b) Either.t, fields) t
    | Record : ('a, fields) t -> ('a, values) t

  and 'a field =
    | Required : ('a, values) t -> 'a field
    | Optional : ('a, values) t -> 'a option field

  and 'k ret =
    | Values : values ret
    | Fields : Fields.t -> fields ret

  include struct
    [@@@ocaml.warning "-32"]

    let list x = List x
  end

  let discard_values ((a, x) : _ * values ret) =
    match (x : values ret) with
    | Values -> a

  let string =
    Iso
      ( Sexp
      , (function
        | Atom s -> s
        | List _ -> raise Of_sexp)
      , fun s -> Atom s )

  let to_sexp : 'a. ('a, values) t -> 'a -> Sexp.t =
   fun t a ->
    let rec loop : type a k. (a, k) t -> a -> k =
     fun t a ->
      match t with
      | Sexp -> a
      | List t -> List (List.map a ~f:(loop t))
      | Record r ->
        let fields = loop r a in
        Fields.to_sexp fields
      | Field (name, spec) -> (
        match spec with
        | Required t -> Fields.of_field name (loop t a)
        | Optional t -> (
          match a with
          | None -> Fields.empty
          | Some a -> Fields.of_field name (loop t a) ) )
      | Iso (t, _, from) -> loop t (from a)
      | Both (x, y) ->
        let x = loop x (fst a) in
        let y = loop y (snd a) in
        Fields.merge x y
      | Either (x, y) -> (
        match a with
        | Left a -> loop x a
        | Right a -> loop y a )
    in
    loop t a

  let of_sexp : 'a. ('a, values) t -> Sexp.t -> 'a =
   fun t sexp ->
    let rec loop : type a k. (a, k) t -> k -> a * k ret =
      fun (type a k) (t : (a, k) t) (ctx : k) : (a * k ret) ->
       match t with
       | Sexp -> (ctx, Values)
       | List t -> (
         match ctx with
         | List xs ->
           (List.map xs ~f:(fun x -> discard_values (loop t x)), Values)
         | Atom _ -> raise Of_sexp )
       | Record (r : (a, fields) t) ->
         let (fields : Fields.t) = Fields.of_sexp ctx in
         let a, Fields f = loop r fields in
         Fields.check_empty f;
         (a, Values)
       | Field (name, spec) -> (
         match spec with
         | Required v ->
           let field, rest = Fields.required ctx name in
           let t, Values = loop v field in
           (t, Fields rest)
         | Optional v ->
           let field, rest = Fields.optional ctx name in
           let t =
             match field with
             | None -> None
             | Some f ->
               let a, Values = loop v f in
               Some a
           in
           (t, Fields rest) )
       | Either (x, y) -> (
         try
           (* TODO share computation somehow *)
           let a, x = loop x ctx in
           (Left a, x)
         with Of_sexp ->
           let a, y = loop y ctx in
           (Right a, y) )
       | Iso (t, f, _) ->
         let a, k = loop t ctx in
         (f a, k)
       | Both (x, y) ->
         let a, Fields k = loop x ctx in
         let b, k = loop y k in
         ((a, b), k)
    in
    discard_values (loop t sexp)

  let record r = Record r

  let either x y = Either (x, y)

  let iso a t f = Iso (a, t, f)

  let field name spec = Field (name, spec)
end

module Id = struct
  type t = string

  let sexp = Conv.string
end

module Payload = struct
  type t =
    { method_ : string
    ; params : Sexp.t
    }

  let fields =
    let open Conv in
    let to_ (method_, params) = { method_; params } in
    let from { method_; params } = (method_, params) in
    iso
      (Both (field "method" (Required string), field "params" (Required Sexp)))
      to_ from
end

module Request = struct
  type t = Id.t * Payload.t
end

module Response = struct
  type t = Id.t * Sexp.t

  let fields =
    let open Conv in
    let id = field "id" (Required Id.sexp) in
    let payload = field "result" (Required Sexp) in
    Both (id, payload)
end

module Message = struct
  type t =
    | Request of Request.t
    | Notification of Payload.t

  let sexp =
    let open Conv in
    let to_ (id, payload) =
      match id with
      | None -> Notification payload
      | Some id -> Request (id, payload)
    in
    let from = function
      | Request (id, payload) -> (Some id, payload)
      | Notification payload -> (None, payload)
    in
    record
      (iso
         (let id = field "id" (Optional Id.sexp) in
          Both (id, Payload.fields))
         to_ from)
end

module Session = struct
  module Packet = struct
    type t =
      | Response of Response.t
      | Notification of Payload.t

    let sexp =
      let f = function
        | Left x -> Response x
        | Right y -> Notification y
      in
      let t = function
        | Response r -> Left r
        | Notification r -> Right r
      in
      let open Conv in
      record (iso (either Response.fields Payload.fields) f t)
  end

  type t =
    { messages : Message.t Fiber.Sequence.t
    ; send : Packet.t option -> unit Fiber.t
    }

  let notification t n = t.send (Some (Notification n))

  let request _ = assert false
end

module Handler = struct
  type t =
    { on_request : Session.t -> Request.t -> Sexp.t Fiber.t
    ; on_notification : Session.t -> Payload.t -> unit Fiber.t
    ; on_init : Session.t -> unit Fiber.t
    }

  let create ~on_request ~on_notification ~on_init =
    { on_request; on_notification; on_init }

  let handle t session =
    let open Fiber.O in
    let* () = t.on_init session in
    Fiber.Sequence.parallel_iter session.messages
      ~f:(fun (message : Message.t) ->
        match message with
        | Notification n -> t.on_notification session n
        | Request (id, r) ->
          let* response = t.on_request session (id, r) in
          session.send (Some (Response (id, response))))
end

let rpc_dir = lazy Path.Build.(relative root "rpc")

let fname = "conn"

let default_socket =
  let s = lazy (Path.build (Path.Build.relative (Lazy.force rpc_dir) fname)) in
  fun () -> Lazy.force s

let where () : Path.t option =
  match Sys.getenv_opt "DUNE_RPC" with
  | Some d -> Some (Path.external_ (Path.External.of_string d))
  | None -> (
    match Path.readdir_unsorted (Path.build (Lazy.force rpc_dir)) with
    | Error _ -> None
    | Ok set ->
      if List.mem ~set fname then
        Some (default_socket ())
      else
        None )

module Make (S : sig
  type t

  val write : t -> Sexp.t -> unit Fiber.t

  val read : t -> Sexp.t option Fiber.t

  val close : t -> unit Fiber.t
end) =
struct
  open Fiber.O

  let run sessions handler =
    Fiber.Sequence.parallel_iter sessions ~f:(fun session ->
        let session =
          let send packet =
            match packet with
            | None -> S.close session
            | Some packet ->
              let sexp = Conv.to_sexp Session.Packet.sexp packet in
              S.write session sexp
          in
          let rec messages : Message.t Fiber.Sequence.t Lazy.t =
            lazy
              (let+ read = S.read session in
               match read with
               | None -> Fiber.Sequence.Nil
               | Some sexp ->
                 let message = Conv.of_sexp Message.sexp sexp in
                 Fiber.Sequence.Cons (message, Lazy.force messages))
          in
          { Session.messages = Lazy.force messages; send }
        in
        Handler.handle handler session)
end
