# giphy-api
[![Build Status](https://travis-ci.org/passy/givegif.svg?branch=master)](https://travis-ci.org/passy/givegif)

Work in progress.

## Building

```bash
$ stack setup
$ stack test
$ stack install
```

## API Examples

```haskell
λ: import qualified Web.Giphy as Giphy
λ: let config = Giphy.GiphyConfig $ Giphy.Key "dc6zaTOxFJmzC"
λ: Giphy.runGiphy (Giphy.search (Giphy.Query "cute puppies")) config
```
