***
FAQ
***

Why do many Jbuilder projects contain a Makefile?
=================================================

Many Jbuilder project contain a toplevel `Makefile`. It is often only there for
convenience, for the following reasons:

1. there are many different build systems out there, all with a different CLI.
   If you have been hacking for a long time, the one true invocation you know is
   `make && make install`, possibly preceded by `./configure`

2. you often have a few common operations that are not part of the build and
   `make <blah>` is a good way to provide them

3. `make` is shorter to type than `jbuilder build @install`

How to add a configure step to a jbuilder project?
==================================================

The with-configure-step_ example shows one way to do it which
preserves composability; i.e. it doesn't require manually running `./configure`
script when working on multiple projects at the same time.

.. _with-configure-step: https://github.com/ocaml/dune/tree/master/example/sample-projects/with-configure-step

Can I use topkg with jbuilder?
==============================

Yes, have a look at the topkg-jbuilder_ project for more details.

.. _topkg-jbuilder: https://github.com/samoht/topkg-jbuilder

here can I find some examples of projects using Jbuilder?
=========================================================

The dune-universe_ repository contains a snapshot of the latest versions of all
opam packages depending on jbuilder. It is therefore a useful reference to
search through to find different approaches to constructing build rules.

.. _dune-universe: https://github.com/dune-universe/dune-universe
