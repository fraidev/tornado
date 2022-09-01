# Tornado

Tornado is a BitTorrent client written entirely in OCaml.

## Setup

### Nix

Just run:
```
nix develop -c $SHELL
```

### Opam

First, create a switch like so:

```bash
opam switch create . 5.0.0~alpha1+options --no-install
```

Then you can run:

```
opam pin stdint https://github.com/andrenth/ocaml-stdint.git#322a8a4a8c69e4a0b75763460b915200356e3af3
dune install ocamlformat
dune install ocaml-lsp-server
opam install . --deps-only --with-test
```

## Downloading a file and Test

Build the codebase with:

```
dune build
```

Run test:

```
dune build @runtest --force --no-buffer
```

Execute it for download a torrent file:

```
dune exec bin/cli.exe debian-11.4.0-arm64-netinst.iso.torrent
```

