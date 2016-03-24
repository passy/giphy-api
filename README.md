# giphy-api
[![Build Status](https://travis-ci.org/passy/giphy-api.svg?branch=master)](https://travis-ci.org/passy/giphy-api)
[![Hackage](http://img.shields.io/hackage/v/giphy-api.svg)](https://hackage.haskell.org/package/giphy-api)

A Haskell wrapper for the [Giphy HTTP API](https://github.com/Giphy/GiphyAPI)
using [servant-client](https://hackage.haskell.org/package/servant-client).

## Usage

The module provides a `Giphy` monad which can be run with `runGiphy` to lift it
into IO. Here's a simple usage example:

```hs
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Web.Giphy as Giphy

apiKey :: Giphy.Key
apiKey = Giphy.Key "dc6zaTOxFJmzC"

sample :: IO ()
sample = do
  let config = Giphy.GiphyConfig apiKey
  resp <- Giphy.runGiphy (app "puppies") config
  print resp

  where
    app :: T.Text -> Giphy.Giphy [Giphy.Gif]
    app q = do
      resp <- Giphy.search $ Giphy.Query q
      return $ Giphy._searchItems resp
```

For a slightly more complex example, check out the [sample app](app/Main.src),
which also features the use of lenses.

## Building

Use [stack](http://haskellstack.org) to build this library.

```bash
$ stack setup
$ stack test
$ stack build
# To install the sample tool
$ stack install
```

## Sample CLI Tool Usage

```bash
$ giphy-search --help
giphy-search

Usage: giphy-search ([-s|--search ARG] | [-t|--translate ARG] | [RANDOM_TAG])
  Find GIFs on the command line.

Available options:
  -h,--help                Show this help text
  -s,--search ARG          Use search to find a matching GIF.
  -t,--translate ARG       Use translate to find a matching GIF.
  -V,--version             Show version information
$ giphy-search puppies
Just https://media2.giphy.com/media/PjQFtJnmdOlwI/giphy.gif
$ giphy-search --translate superman
Just https://media3.giphy.com/media/eOewytQL4tOOA/giphy.gif
$ giphy-search "1C4D539A-B787-497F-B1DC-8FCF8D2C026D"
Nothing
```

## Missing features

- No network configuration. No proxies, no TLS certificate pinning, no custom
  networking stack. I'm still learning, let me know how to do this better!

- There is no compiler flag at the moment to disable lenses if those aren't
  needed. The library, however, uses `microlens` so the overhead should be
  minimal.

- The "Stickers" API endpoints are currently not covered. If you need them, let
  me know. PRs obviously very welcome.

- Some fields are currently not exposed. Again, if you need them, open an issue.
  For example, I have never seen a type value other than "gif" so I decided
  to skip it.
