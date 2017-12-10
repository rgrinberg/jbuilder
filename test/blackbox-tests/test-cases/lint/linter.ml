let () =
  Format.eprintf "Linting %s successfully failed.@.%!" Sys.argv.(1);
  exit 1
