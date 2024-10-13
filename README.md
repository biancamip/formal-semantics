## Formal Semantics 22/1

OCaml interpreter for L1 lang (based on [UFRGS comp sci](https://www.inf.ufrgs.br/site/en)'s formal semantics course) with the following extensions:
  - Adding memory;
  - Variable allocations & derefs;
  - While & Seq operations.

You can find the big step evaluator function (bse) + tests on file trab.ml

For a docker setup with ocaml (because I was using windows for some reason):

```
docker build -t ocaml-build .
docker run -v C:\git\formal-semantics\src:/ocaml-src --name ocaml -it --rm ocaml-build
```
