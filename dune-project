(lang dune 2.7)

(name michelson-adt)

(generate_opam_files true)

(source
 (uri "git+https://github.com/releaselab/michelson-adt.git"))

(bug_reports "https://github.com/releaselab/michelson-adt/issues")

(homepage "https://github.com/releaselab/michelson-adt")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name michelson-adt)
 (version 0.1)
 (synopsis
  "An OCaml library with and ADT representation of Michelson smart contracts syntax")
 (depends
  (dune :build)
  (core
   (>= v0.15))
  (core_unix
   (and
    :with-test
    (>= v0.15)))
  bignum
  (tezos-micheline
   (>= 7.0))
  ppx_deriving
  (pp_loc
   (>= 2.0))
  (alcotest :with-test)))
