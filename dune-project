(lang dune 3.11)

(name parsekit)

(generate_opam_files true)

(source
 (github bogdan2412/parsekit))

(authors "Bogdan Tataroiu <b.tataroiu@gmail.com>")

(maintainers "Bogdan Tataroiu <b.tataroiu@gmail.com>")

(license GPL-3.0-or-later)

(package
 (name parsekit)
 (synopsis "Parser combinator library for OCaml")
 (depends
  ocaml
  dune
  base
  ppx_jane
  (expect_test_helpers_core :with-test)
  (stdio :with-test)))

(package
 (name json-parsekit)
 (synopsis "JSON parsing library built using the Parsekit library")
 (depends
  ocaml
  dune
  base
  parsekit
  ppx_jane
  (stdio :with-test)))
