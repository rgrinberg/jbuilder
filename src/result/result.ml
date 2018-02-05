[@@@warning "-33"]
open Result_compat
open Pervasives
[@@@warning "+33"]

type ('a, 'error) t = ('a, 'error) result =
  | Ok    of 'a
  | Error of 'error

let map x ~f =
  match x with
  | Ok x -> Ok (f x)
  | Error _ as x -> x

type ('a, 'error) result = ('a, 'error) t
