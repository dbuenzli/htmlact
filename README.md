Htmlact — Active HTML elements for dynamic webpages
===================================================
%%VERSION%%

Htmlact declares dynamic webpages by using HTML data attributes to
seamlessly update the page with HTML fragments requested from the
server.

Using HTML as the interaction substrate keeps logic on the server and,
in the simplest cases, reduces front-end client code to custom CSS
animations and inclusion of a generic JavaScript driver.

Htmlact is distributed under the ISC license. It has no dependencies 
but compilation of the JavaScript driver depends on [brr]. 

Homepage: https://erratique.ch/software/htmlact

[brr]: https://erratique.ch/software/brr

# Installation

htmlact can be installed with `opam`:

    opam install htmlact

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation can be consulted [online] or via `odig doc htmlact`.

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: https://erratique.ch/software/htmlact/doc
[OCaml forum]: https://discuss.ocaml.org/

# Examples

The [`htmlact-examples`](examples/htmlact_examples.ml) executable
serves examples to show case HTML interaction strategies. It is
installed if `webs` and `htmlit` are. Install `htmlact` with tests to
ensure it gets installed:

    opam install -t htmlact 

# Acknowledgments

Htmlact was inspired by [htmx](https://htmx.org).

