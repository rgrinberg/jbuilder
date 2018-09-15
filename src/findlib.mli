include module type of Dfindlib.Findlib

(** A dummy package. This is used to implement [external-lib-deps] *)
val dummy_package : t -> name:Lib_name.t -> Package.t
