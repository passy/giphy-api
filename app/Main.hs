{-# LANGUAGE OverloadedStrings          #-}

module Main where

import qualified Data.Text as T
import qualified Options.Applicative as Opt

import           Data.Version             (Version (), showVersion)
import Control.Applicative ((<**>))
import Paths_givegif (version)

import Data.Monoid ((<>))
import qualified Web.Giphy.Search as Giphy

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
      -- TODO: Lenses. For everything!
      resp <- Giphy.search apiKey (Giphy.Query $ query opts)
      print resp