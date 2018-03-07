open Import

type for_ =
  | Program of string
  | Library of string

type t =
  { for_       : for_
  ; dir        : Path.t
  }

let path t socpe =
  Path.relative t.dir (
    sprintf ".%s.%s.%s"
      (match t.for_ with
       | Program s
       | Library s -> s)
      (match scope with
       | Module_scope.Private -> "private"
       | Public -> "public")
      (match t.for_ with
       | Program _ -> "eobjs"
       | Library _ -> "objs")
  )
