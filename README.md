# Tornado

Tornado is a BitTorrent client written entirely in OCaml.

## Setup an opam env

First, create a switch like so

```bash
opam switch create . 4.14.0 --no-install
or
opam switch create . 5.0.0~alpha1+options --no-install
```

Then you can run

```
opam pin stdint https://github.com/andrenth/ocaml-stdint.git#322a8a4a8c69e4a0b75763460b915200356e3af3
opam install . --deps-only --with-test
```

and build the codebase with

```
dune build
```

Run test:

```
dune build @runtest --force --no-buffer
```

## Downloading a file

```
dune exec bin/cli.exe debian-11.4.0-arm64-netinst.iso.torrent
```

