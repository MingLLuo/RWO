(lang dune 3.7)
(using menhir 2.1)
(name rwo_coding)

(generate_opam_files true)

(package
 (name rwo_coding)
 (description "code from Real World OCaml")
 (depends ocaml dune))