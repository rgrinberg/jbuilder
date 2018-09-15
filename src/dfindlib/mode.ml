open! Stdune

type t = Byte | Native

let choose byte native = function
  | Byte   -> byte
  | Native -> native

let variant = choose Variant.byte Variant.native

module Dict = struct
  type 'a t =
    { byte   : 'a
    ; native : 'a
    }

  let of_func f =
    { byte   = f ~mode:Byte
    ; native = f ~mode:Native
    }

  let map2 a b ~f =
    { byte   = f a.byte   b.byte
    ; native = f a.native b.native
    }
end
