# Strictly Annotated

Strictly Annotated is a pretty-printer derived from [Strictly Pretty](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200) (Lindig 2000).
See below for a small introduction, or [this blog post](https://ayazhafiz.com/articles/21/strictly-annotated) for a detailed design.
Or just read the source, it's quite short.

The contribution of this package is pretty-printing in the presence of
annotations, which can be thought of comments in source code or some kind of
supplemental information that is in some sense less important than other terms
in a document. Terms of the document are pretty-printed in the usual way, and
annotations are formatted so as to be highly visible without interfering with
the document's terms.

For example, given the source

```
if a == b then a << 2 else a + b
```

with annotations attached on the terms `if`, `then`, and `else`, Strictly
Annotated may format this document as

```
if a == b    # new conditional
then a << 2  # true branch
else a + b   # false branch
```

or at a narrower width,

```
if  # new conditional
  a == b
then  # true branch
  a << 2
else  # false branch
  a + b
```

## History and Citations

As mentioned, this printer is derived from Lindig's [Strictly
Pretty](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200).
Strictly Pretty is derived from [A prettier printer](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf) (Walder, 1999),
which in turn was influenced by [The Design of a Pretty-printing Library](http://belle.sourceforge.net/doc/hughes95design.pdf) (Hughes, 1995).

Both Hughes and Walder embraced the formalization of an algebra for
pretty-printing, proving equational laws of pretty-printing combinators in
their implementations. An algebra of pretty-printing is both easy to express and
yields an efficient implementation in a lazy language like Haskell. Laziness is
important, because pretty-printing inevitably involves expanding some layout
choices to determine which is "the best" - having to eagerly expand all layout
choices would be a non-starter. Lindig was aware of this issue, and in his note
_Strictly Prettier_ attempted to translate the main ideas involved in Walder's
pretty-printing system for use in an eager execution.

Variants of these systems are used in the Haskell, OCaml, and Elixir standard
libraries, among others.
