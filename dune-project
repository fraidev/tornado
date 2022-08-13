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
  (alcotest-lwt :with-test)
  (dune
   (and
    :build
    (>= 3.1.1)))
  bencode
  mirage-crypto
  conduit-lwt-unix
  uri
  piaf
  stdint
  ppx_deriving
  ppx_let
  ))

(package
 (name dev)
 (synopsis "A package to install dev dependencies")
 (description "THIS PACKAGE IS FOR DEVELOPMENT PURPOSE")
 (allow_empty)
 (depends
  (ocamlformat
   (>= 0.20))
  (ocamlformat-rpc
   (>= 0.19))
  (ocaml-lsp-server
   (>= 1.10.3))))
