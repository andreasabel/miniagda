# MiniAgda

[![Build Status](https://travis-ci.com/andreasabel/miniagda.svg?branch=master)](https://travis-ci.com/andreasabel/miniagda)
[![Hackage][hackage-badge]][hackage]

 [hackage]:       <https://hackage.haskell.org/package/MiniAgda>
 [hackage-badge]: <https://img.shields.io/hackage/v/MiniAgda.svg>

A prototypical dependently typed languages with sized types and variances.

## Installation

Requires GHC and cabal, for instance via the Haskell Platform.
In a shell, type
```
  cabal v1-update
  cabal v1-install alex
  cabal v1-install happy
  cabal v1-install MiniAgda
```

To build MiniAgda from source, replace the last command with

```
 make
```

## Examples

See directories ``test/succeed/`` and ``examples/``.

Some examples are commented on the (dormant) [MiniAgda blog](http://www.cse.chalmers.se/~abela/miniagda/index.html).
