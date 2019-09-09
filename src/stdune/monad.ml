module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Id = struct
  type 'a t = 'a

  let return x = x

  let ( >>= ) x f = f x
end

module Make (M : S) = struct
  open M

  let ( >>| ) x f = x >>= fun x -> return (f x)

  let ( let* ) = ( >>= )

  let ( let+ ) = ( >>| )
end
