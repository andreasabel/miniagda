# MiniAgda

[![Cabal build][ci-badge]][ci-action]
[![Hackage][hackage-badge]][hackage]

 [ci-action]:     <https://github.com/andreasabel/MiniAgda/actions>
 [ci-badge]:      <https://github.com/andreasabel/MiniAgda/workflows/Haskell-CI/badge.svg>
 [hackage]:       <https://hackage.haskell.org/package/MiniAgda>
 [hackage-badge]: <https://img.shields.io/hackage/v/MiniAgda.svg?color=informational>

A prototypical dependently typed languages with sized types and variances.

## Installation from Hackage

Requires `ghc` and `cabal`, for instance via the Haskell Platform or via `ghcup`.
In a shell, type
```
  cabal update
  cabal install MiniAgda
```

## Installation from Stackage

MiniAgda is not on Stackage because it depends on a specific version of `haskell-src-exts` rather than the latest version.
You can install with `stack` from source though, see next section.

## Installation from source

1. Obtain the sources.
   ```
   git clone https://github.com/andreasabel/miniagda
   cd miniagda
   ```

2. Checkout the desired version (optional):
   If you want to build a released version or a branch rather than the latest `master`, switch to this version/branch.
   ```
   git checkout $REF
   ```
   E.g. `REF = v0.2022.3.11` for version `0.2022.3.11` or `REF = unfold` for branch `unfold`.

3. Building and running the tests (optional).

   - With `cabal` (requires `ghc` in your PATH):
     ```
     cabal build --enable-tests
     cabal test
     ```
     After building, you can run MiniAgda locally, e.g.:
     ```
     cabal run miniagda -- examples/Gcd/gcd.ma
     ```

   - With `stack`:
     ```
     stack build --stack-yaml=stack-$GHCVER.yaml
     stack test  --stack-yaml=stack-$GHCVER.yaml
     ```
     At the time of writing (2025-07-23), `GHCVER` can be any of `9.12`, `9.10`, `9.8`, `9.6`, and some older ones.

     After building, you can run MiniAgda locally, e.g.:
     ```
     stack run --stack-yaml=stack-9.12.yaml -- examples/Gcd/gcd.ma
     ```
     You can copy `stack-$GHCVER.yaml` for your choice of `GHCVER` into `stack.yaml` and drop the `--stack-yaml` argument from `stack` invocation.


4. Install.
   - With `cabal` (requires `ghc` in your PATH):
     ```
     cabal install
     ```
   - With `stack`:
     ```
     stack install --stack-yaml=stack-$GHCVER.yaml
     ```

   Note that the respective installation directory should be on your PATH.

## Examples

See directories ``test/succeed/`` and ``examples/``.

Some examples are commented on the (dormant) [MiniAgda blog](http://www.cse.chalmers.se/~abela/miniagda/index.html).
