# giphy-api
[![Build Status](https://travis-ci.org/passy/givegif.svg?branch=master)](https://travis-ci.org/passy/givegif)

Work in progress: Giphy HTTP API Wrapper.

## Building

```bash
$ stack setup
$ stack test
$ stack install
```

## Sample CLI Tool Usage

```bash
$ stack exec giphy-search -- "puppies"
Just https://media2.giphy.com/media/PjQFtJnmdOlwI/giphy.gif
$ stack exec giphy-search -- --translate "superman"
Just https://media3.giphy.com/media/eOewytQL4tOOA/giphy.gif
```

## API Examples

```haskell
λ: import qualified Web.Giphy as Giphy
λ: let config = Giphy.GiphyConfig $ Giphy.Key "dc6zaTOxFJmzC"
λ: Giphy.runGiphy (Giphy.search (Giphy.Query "cute puppies")) config
```
