# WIP: givegif
[![Build Status](https://travis-ci.org/passy/givegif.svg?branch=master)](https://travis-ci.org/passy/givegif)

Work in progress. A CLI thing for GIFs via Giphy.

## Building

```bash
$ stack setup
$ stack test
$ stack install
```

## API Examples

```haskell
λ: import Data.Text
λ: search (Key "dc6zaTOxFJmzC") (Query "cute puppies")
```
