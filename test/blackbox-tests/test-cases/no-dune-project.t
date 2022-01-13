When no dune-project file is present, a warning is printed.

  $ touch dune
  $ dune build
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.

In release mode, this is fatal.

  $ dune build --release
  Error: No dune-project file has been found. A default one is assumed but the
  project might break when dune is upgraded. Please create a dune-project file.
  [1]

This corresponds to a flag:

  $ dune build --require-dune-project-file
  Error: No dune-project file has been found. A default one is assumed but the
  project might break when dune is upgraded. Please create a dune-project file.
  [1]
