(lang dune 3.2)

(name tornado)

(version 0.1)

(maintainers "fraifelipe@gmail.com")

(generate_opam_files true)

(package
 (name tornado)
 (synopsis "tornado")
 (description "Tornado")
 (depends
  (alcotest :with-test)
  (dune
   (and
    :build
    (>= 3.1.1)))
  bencode
  eio_main
  uri
  stdint
  ppx_deriving
  ppx_let
  ppx_sexp_conv
  ))

