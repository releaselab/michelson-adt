(lang dune 2.7)

(name michelson)

(generate_opam_files true)

(source
 (uri "git+https://gitlab.com/releaselab/fresco/michelson.git"))

(bug_reports "https://gitlab.com/releaselab/fresco/michelson/issues")

(homepage "https://gitlab.com/releaselab/fresco/michelson")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name michelson)
 (version 0.1)
 (synopsis
  "An OCaml library for parsing Michelson programs into an OCaml-usable AST.")
 (depends
  (dune :build)
  core
  (core_unix :with-test)
  bignum
  (tezos-micheline
   (>= 7.0))
  ppx_deriving
  (bisect_ppx
   (and
    :with-test
    (>= 2.5.0)))
  (alcotest :with-test)))
