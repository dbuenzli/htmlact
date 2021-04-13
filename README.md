Hc — Connected hypertext for interactive web pages
==================================================
%%VERSION%%

Hc declares interactive web pages by using HTML data attributes to
request and seamlessly integrate HTML fragments from the server.

Using hypertext as the interaction substrate keeps logic on the server
and, in the simplest cases, reduces front-end client code to a generic
JavaScript driver and custom CSS animations.

Hc is distributed under the ISC license. Compilation of the JavaScript
driver depends on [brr][brr].

Homepage: https://erratique.ch/software/hc

[brr]: https://erratique.ch/software/brr

# Installation

hc can be installed with `opam`:

    opam install hc

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation can be consulted [online][doc] or via `odig doc hc`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/hc/doc
[ocaml-forum]: https://discuss.ocaml.org/

# Examples

The [`hc-examples`](examples/hc_examples.ml) executable serves
examples to show case hypertext interaction strategies. It is installed
if `webs` is. Install `hc` with tests to ensure it gets installed: 

    opam install hc --with-test

# Acknowledgments

Hc was inspired by [htmx](https://htmx.org).

