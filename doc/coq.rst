.. _coq-main:

******
Coq
******

Dune is also able to build Coq developments. A Coq project is a mix of
Coq ``.v`` files and (optionally) OCaml libraries linking to the Coq
API (in which case we say the project is a *Coq plugin*). To enable
Coq support in a dune project, the language version should be selected
in the ``dune-project`` file. As of today, the only version supported
is ``0.1`` and is experimental:

.. code:: scheme

    (using coq 0.1)

This will enable support for the ``coq.theory`` stanza in the current project. If the
language version is absent, dune will automatically add this line with the
latest Coq version to the project file once a ``(coq.theory ...)`` stanza is used anywhere.


Basic Usage
===========

The basic form for defining Coq libraries is very similar to the OCaml form:

.. code:: scheme

    (coq.theory
     (name <module_prefix>)
     (public_name <package.lib_name>)
     (synopsis <text>)
     (modules <ordered_set_lang>)
     (libraries <ocaml_libraries>)
     (theories <coq_libraries>)
     (flags <coq_flags>))

The stanza will build all ``.v`` files on the given directory. The semantics of fields is:

- ``<module_prefix>`` is a dot-separated list of valid Coq module
  names and determines the module scope under which the theory is
  compiled [``-R`` option]. For example, if ``<module_prefix>`` is
  ``foo.Bar``, the theory modules will be named as
  ``foo.Bar.module1``, ``foo.Bar.module2``, etc... Note that modules
  in the same theory don't see the ``foo.Bar`` prefix, in the same
  way that OCaml ``wrapped`` libraries do. For compatibility reasons,
  the 1.0 version of the Coq language installs a theory named
  ``foo.Bar`` under ``foo/Bar``. Also note that Coq supports composing
  a module path from different theories, thus you can name a theory
  ``foo.Bar`` and a second one ``foo.Baz`` and things will work
  properly,
- the ``modules`` field allows to constraint the set of modules
  included in the theory, similarly to its OCaml counterpart. Modules
  are specified in Coq notation, that is to say ``A/b.v`` is written
  ``A.b`` in this field,
- ``public_name`` is of the form ``pkg_name.identifier``; if present,
  Dune will generate install rules for the ``.vo`` files on the
  theory. ``pkg_name`` must be a valid package name; ``identifier``
  is not used in the 1.0 version of the language as in the legacy
  install setup, all packages share a common root namespace. Indeed,
  files will be installed in ``lib/coq/user-contrib/<module_prefix>``,
  as customary in the make-based Coq package eco-system. For
  compatibility, we also install under the ``user-contrib`` prefix the
  ``.cmxs`` files appearing in ``<ocaml_libraries>``,
- ``<coq_flags>`` will be passed to ``coqc`` as command-line options,
- the path to installed locations of ``<ocaml_libraries>`` will be passed to
  ``coqdep`` and ``coqc`` using Coq's ``-I`` flag; this allows for a Coq
  theory to depend on a ML plugin,
- your Coq theory can depend on other theories by specifying them in
  the ``<coq_libraries>`` field; if the theory is in-tree, you can use
  either the public or a ``<module_prefix>``; Dune will then pass to
  Coq the corresponding flags to make everything compile correctly
  [``-Q``]. For the 1.0 version of the Coq language, if the library is
  installed globally, you must use a ``<module_prefix>``, and Dune
  will just check that ``lib/coq/user-contrib/<module_prefix>`` does
  exist, as Coq will put the library in scope automatically.

Recursive Qualification of Modules
==================================

If you add:

.. code:: scheme

    (include_subdirs qualified)

to a ``dune`` file, Dune will to consider that all the modules in
their directory and sub-directories, adding a prefix to the module
name in the usual Coq style for sub-directories. For example, file ``A/b/C.v`` will be module ``A.b.C``.

Preprocessing with ``coqpp``
============================

Coq plugin writers usually need to write ``.mlg`` files to extend Coq
grammar. Such files are pre-processed with ``coqpp``; to help plugin
writers avoid boilerplate we provide a ``(coqpp ...)`` stanza:

.. code:: scheme

    (coq.pp (modules <mlg_list>))

which for each ``g_mod`` in ``<mlg_list>`` is equivalent to:

.. code:: scheme

    (rule
     (targets g_mod.ml)
     (deps (:mlg-file g_mod.mlg))
     (action (run coqpp %{mlg-file})))

More About Coq Module Prefixes
==============================

TODO

Limitations
===========

- .v always depend on the native version of a plugin,
- a ``foo.mlpack`` file must the present for locally defined plugins to work, this is a limitation of coqdep,
