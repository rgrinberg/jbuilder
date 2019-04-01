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

The stanza will build all `.v` files on the given directory. The semantics of fields is:

- ``<module_prefix>`` will be used as the default Coq library prefix ``-R``,
- the ``modules`` field does allow to constraint the set of modules
  included in the library, similarly to its OCaml counterpart,
- ``public_name`` will make Dune generate install rules for the `.vo`
  files; files will be installed in
  ``lib/coq/user-contrib/<module_prefix>``, as customary in the
  make-based Coq package eco-system. For compatibility, we also installs the `.cmxs`
  files appearing in `<ocaml-librarie>` under the `user-contrib` prefix.
- ``<coq_flags>`` will be passed to ``coqc``,
- the path to installed locations of ``<ocaml_libraries>`` will be passed to
  ``coqdep`` and ``coqc`` using Coq's ``-I`` flag; this allows for a Coq
  library to depend on a ML plugin,
- you can depend on in-tree ``<coq_libraries>``, in this case, Dune
  will pass to Coq the the corresponding `-Q` flag for the libraries
  and their dependencies.

Preprocessing with ``coqpp``
============================

Coq plugin writers usually need to write ``.mlg`` files to extend Coq
grammar. Such files are pre-processed with `coqpp`; to help plugin
writers avoid boilerplate we provide a `(coqpp ...)` stanza:

.. code:: scheme

    (coq.pp (modules <mlg_list>))

which for each ``g_mod`` in ``<mlg_list>```is equivalent to:

.. code:: scheme

    (rule
     (targets g_mod.ml)
     (deps (:mlg-file g_mod.mlg))
     (action (run coqpp %{mlg-file})))

Recursive Qualification of Modules
==================================

If you add:

.. code:: scheme

    (include_subdirs qualified)

to a ``dune`` file, Dune will to consider that all the modules in
their directory and sub-directories, adding a prefix to the module
name in the usual Coq style for sub-directories. For example, file ``A/b/C.v`` will be module ``A.b.C``.

Composition of Coq libraries
============================

For local Coq libraries, composition is possible using the
``theories`` field. Libraries are wrapped, however for their own
compilation the scope is opened so qualification is not necessary.

TODO: "rules about prefix shadowing"

Limitations
===========

- .v always depend on the native version of a plugin,
- a ``foo.mlpack`` file must the present for locally defined plugins to work, this is a limitation of coqdep,
