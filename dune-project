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
  piaf
  progress
  eio
  eio_main
  bencode
  stdint
  uri
  ppx_deriving))
