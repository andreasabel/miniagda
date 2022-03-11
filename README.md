# MiniAgda

[![Cabal build][ci-badge]][ci-action]
[![Hackage][hackage-badge]][hackage]

 [ci-action]:     <https://github.com/andreasabel/MiniAgda/actions>
 [ci-badge]:      <https://github.com/andreasabel/MiniAgda/workflows/Haskell-CI/badge.svg>
 [hackage]:       <https://hackage.haskell.org/package/MiniAgda>
 [hackage-badge]: <https://img.shields.io/hackage/v/MiniAgda.svg?color=informational>

A prototypical dependently typed languages with sized types and variances.

## Installation

Requires `ghc` and `cabal`, for instance via the Haskell Platform or via `ghcup`.
In a shell, type
```
  cabal update
  cabal install MiniAgda
```

To build MiniAgda from source, replace the last command with

```
 make
```

## Examples

See directories ``test/succeed/`` and ``examples/``.

Some examples are commented on the (dormant) [MiniAgda blog](http://www.cse.chalmers.se/~abela/miniagda/index.html).
