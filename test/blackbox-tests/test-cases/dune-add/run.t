Adding a library
----------------

Can add a public library

  $ dune add lib test_lib ./_test_lib_dir --public
  Success: addition of library component named test_lib complete

Can build the public library

  $ cd _test_lib_dir && touch test_lib.opam && dune build
  Info: creating file dune-project with this contents:
  | (lang dune 1.8)
  | (name test_lib)
  
  $ cat ./_test_lib_dir/dune
  (library
   (public_name test_lib)
   (name test_lib))

Clean up the library tests

  $ rm -rf ./_test_lib_dir

Can add library with a specified public name

  $ dune add lib test_lib ./_test_lib_dir --public test_lib_public_name
  Success: addition of library component named test_lib complete

  $ cat ./_test_lib_dir/dune
  (library
   (public_name test_lib_public_name)
   (name test_lib))

Clean up library with specified public name

  $ rm -rf ./_test_lib_dir

Can add a library with inline tests

  $ dune add lib test_lib ./_inline_tests_lib --inline-tests --ppx ppx_inline_tests
  Success: addition of library component named test_lib complete
  $ cat ./_inline_tests_lib/dune
  (library
   (inline_tests)
   (name test_lib)
   (preprocess
    (pps ppx_inline_tests)))

Clean up library with inlines tests

  $ rm -rf ./_inline_tests_lib

Adding an executable
--------------------

Can add a public executable

  $ dune add exe test_bin ./_test_bin_dir --public
  Success: addition of executable component named test_bin complete

Can build an executable

  $ cd _test_bin_dir && touch test_bin.opam && dune build
  Info: creating file dune-project with this contents:
  | (lang dune 1.8)
  | (name test_bin)
  

Can run the created executable

  $ cd _test_bin_dir && dune exec test_bin
  Hello, World!

Clean up the executable tests

  $ rm -rf ./_test_bin_dir

Adding tests
------------

Can add tests

  $ dune add test test_tests ./_test_tests_dir --libs foo,bar
  Success: addition of test component named test_tests complete
  $ ls ./_test_tests_dir
  dune
  test_tests.ml
  $ cat ./_test_tests_dir/dune
  (test
   (name test_tests)
   (libraries foo bar))

Clean up the test tests

  $ rm -rf ./_test_tests_dir

Adding componets to default and non-standard places
---------------------------------------------------

Add a library in the current working directory

  $ dune add lib test_lib
  Success: addition of library component named test_lib complete
  $ cat dune
  (library
   (name test_lib))

Clean the library creation

  $ rm ./dune

Add a library to a dune file in a specified directory

  $ dune add lib test_lib ./_test_dir
  Success: addition of library component named test_lib complete
  $ ls ./_test_dir/dune
  ./_test_dir/dune

Clean up from the dune file created in ./test_dir

  $ rm -rf ./test_dir

Adding a library and an executable dependent on that library
------------------------------------------------------------

Can add a library and dependent executable in a combo project

  $ dune add lib test_lib ./_test_lib_exe_dir/src
  Success: addition of library component named test_lib complete
  $ dune add exe test_bin ./_test_lib_exe_dir/bin --libs test_lib --public
  Success: addition of executable component named test_bin complete

Can build the combo project

  $ cd _test_lib_exe_dir && touch test_bin.opam && dune build
  Info: creating file dune-project with this contents:
  | (lang dune 1.8)
  | (name test_bin)
  

Can run the combo project

  $ cd _test_lib_exe_dir && dune exec test_bin
  Hello, World!

Clean up the combo project

  $ rm -rf ./_test_lib_exe_dir

Adding libraries in a single directory
--------------------------------------

Can add multiple libraries in the same directory

  $ dune add lib test_lib1 ./_test_lib --public
  Success: addition of library component named test_lib1 complete
  $ dune add lib test_lib2 ./_test_lib --libs test_lib1
  Success: addition of library component named test_lib2 complete
  $ cat ./_test_lib/dune
  (library
   (public_name test_lib1)
   (name test_lib1))
  
  (library
   (name test_lib2)
   (libraries test_lib1))

Can build the multiple library project

  $ cd _test_lib && touch test_lib1.opam && dune build
  Info: creating file dune-project with this contents:
  | (lang dune 1.8)
  | (name test_lib1)
  

Clan up the multiple library project

  $ rm -rf ./_test_lib

Multiple ppxs and library dependencies
--------------------------------------

Can add multiple library dependencies in one command

  $ dune add lib test_lib ./_test_lib --libs foo,bar --ppx ppx_foo,ppx_bar
  Success: addition of library component named test_lib complete
  $ cat _test_lib/dune
  (library
   (name test_lib)
   (libraries foo bar)
   (preprocess
    (pps ppx_foo ppx_bar)))

Clean up the multiple dependencies project

  $ rm -rf ./_test_lib

Safety and Validation
---------------------

Will not overwrite existing files

  $ dune add exe test_bin ./existing_project/bin
  Warning: file existing_project/bin/main.ml was not created because it already exists
  Success: addition of executable component named test_bin complete
  $ cat ./existing_project/bin/main.ml
  () = print_endline "Goodbye"

Comments in dune files are preserved

  $ dune add lib test_lib2 ./existing_project/src
  Success: addition of library component named test_lib2 complete
  $ cat ./existing_project/src/dune
  ; A comment
  
  (library
   ; Another comment
   (name test_lib))
  
  (library
   (name test_lib2))

Will not create components with invalid names

  $ dune add lib invalid-component-name ./_test_lib
  A component named 'invalid-component-name' cannot be created because it is an invalid library name.
  Hint: library names must be non-empty and composed only of the following characters: 'A'..'Z',  'a'..'z', '_'  or '0'..'9'
  [1]
  $ ls ./_test_lib
  ls: ./_test_lib: No such file or directory
  [1]

Will fail and inform user when invalid component command is given

  $ dune add foo blah
  dune: COMPONENT_KIND argument: invalid value `foo', expected one of `exe',
        `lib' or `test'
  Usage: dune add [OPTION]... COMPONENT_KIND NAME [PATH]
  Try `dune add --help' or `dune --help' for more information.
  [1]

Will fail and inform user when an invalid option is given to a component

  $ dune add test test_foo --public
  The test component does not support the public option
  [1]
  $ dune add exe test_exe --inline-tests
  The executable component does not support the inline-tests option
  [1]

Adding fields to existing stanzas
---------------------------------

# TODO(shonfeder)
Adding fields to existing stanzas is currently not supported

  $ dune add exe test_bin ./_test_bin --libs test_lib1 --public
  Success: addition of executable component named test_bin complete
  $ dune add exe test_bin ./_test_bin --libs test_lib2
  Updating existing stanzas is not yet supported.
  A preexisting dune stanza conflicts with a generated stanza:
  
  Generated stanza:
  (executable (name main) (libraries test_lib2))
  
  Pre-existing stanza:
  (executable (public_name test_bin) (name main) (libraries test_lib1))
  [1]
  $ cat ./_test_bin/dune
  (executable
   (public_name test_bin)
   (name main)
   (libraries test_lib1))
