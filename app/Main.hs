{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text              as T
import qualified Options.Applicative    as Opt
import qualified Web.Giphy              as Giphy

import           Control.Applicative    ((<**>))
import           Data.Version           (Version (), showVersion)
import           Paths_givegif          (version)

import           Data.Monoid            ((<>))

import           Control.Lens.Operators
import           Control.Lens.Prism     (_Right)

data Options = Options
  { query :: T.Text
  }

apiKey :: Giphy.Key
apiKey = Giphy.Key "dc6zaTOxFJmzC"

options :: Opt.Parser Options
options = Options . T.pack <$> Opt.argument Opt.str
                             ( Opt.metavar "QUERY"
                            <> Opt.help "Search query for your GIFs" )

cliParser :: Version -> Opt.ParserInfo Options
cliParser ver =
  Opt.info ( Opt.helper <*> options <**> versionInfo )
    ( Opt.fullDesc
   <> Opt.progDesc "Find GIFs on the command line."
   <> Opt.header "givegif" )
  where
    versionInfo = Opt.infoOption ( "givegif " <> showVersion ver )
      ( Opt.short 'V'
     <> Opt.long "version"
     <> Opt.hidden
     <> Opt.help "Show version information" )

main :: IO ()
main = Opt.execParser (cliParser version) >>= run
  where
    run :: Options -> IO ()
    run opts = do
      let q = Giphy.Query $ query opts
      resp <- Giphy.search apiKey q
      print $ resp ^? _Right . Giphy.searchItems
