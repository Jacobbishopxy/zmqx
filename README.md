# Zmqx

Copied from:

- <https://github.com/awkward-squad/libzmq>
- <https://github.com/mitchellwrosen/zmq>

Support `base ^>=4.20.0.0`

## Usage

```cabal
# cabal.project

source-repository-package
  type: git
  location: git@github.com:Jacobbishopxy/zmqx.git
  tag: 2b649f32d1c5cfb947bd85c2aff504058612cb2e
```

## Test & Build

```sh
# test
cabal test test-simple-dealer --flag debug

# build
cabal build --flag debug
```
