# Latte compiler

Latte language compiler implemented in Haskell.

## Build
To build the `latc` binary from source simply run `make`.

## Library
`lib` directory contains a runtime library with all builtin functions.

## Dependencies
All required dependencies are described in `package.yaml` which was used to generate cabal config file using `hpack`.
Most notably the compiler makes use of `mtl` and `lens` packages.
