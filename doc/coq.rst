.. _coq-main:

******
Coq
******

Dune is also able to build Coq developments. A Coq project is a mix of
Coq ``.v`` files and OCaml libraries, known as *Coq plugins*. To enable
Coq support in a dune project, the language version should be selected
in the ``dune-project`` file. For example:

.. code:: scheme

    (using coq 0.2)

This will enable support for the ``coqlib`` stanza in the current project. If the
language version is absent, dune will automatically add this line with the
latest Coq version to the project file once a ``(coqlib ...)`` stanza is used anywhere.


Basic Usage
===========

The basic form for defining Coq libraries is very similar to the OCaml form:

.. code:: scheme

    (coqlib
     (name <module_prefix>)
     (public_name <package.module_prefix>)
     (synopsis <text>)
     (modules <ordered_set_expr>)
     (flags <coq_flags>))

The stanza will build all `.v` files on the given directory, *à la*
``coq_makefile``. The semantics of fields is:
- ``<module_prefix>`` will be used as the default Coq library prefix (``-R .``),
- ``public_name`` will make Dune generate install rules for the `.vo`
  files; files will be installed in ``lib/coq/<module_prefix>``.
- the ``modules`` field does allow to constraint the set of modules
  included in the compilation, similarly to its OCaml counterpart;
  ``:standard`` is bound to all the ``.v`` files in the directory,
- ``<coq_flags>`` will be passed to ``coqc``.

Library Composition and Handling
===================

The ``coqlib`` stanza does not yet support composition of Coq
libraries. In the current version, libraries are located using Coq's
built-in library management, thus Coq will always resort to the
installed version of a particular library.

This will be fixed in the future.
