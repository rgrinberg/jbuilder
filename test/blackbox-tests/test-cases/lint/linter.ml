let () =
  let basename = Filename.basename Sys.argv.(1) in
  if basename = "pass.ml" then (
    exit 0
  ) else (
    Format.eprintf "Linting %s successfully failed.@.%!" Sys.argv.(1);
    exit 1
  )
