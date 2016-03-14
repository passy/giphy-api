{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text              as T
import qualified Options.Applicative    as Opt
import qualified Web.Giphy              as Giphy
import qualified Network.URI            as URI

import           Control.Applicative    ((<**>))
import           Data.Version           (Version (), showVersion)
import           Paths_givegif          (version)

import           Data.Monoid            ((<>))

import           Control.Lens.Operators
import           Control.Lens.Prism     (_Right)
import           Control.Lens.Cons      (_head)
import           Control.Lens.At        (at)
import Control.Monad (join)

data Options = Options
  { query :: T.Text
  , useTranslate :: Bool
  }

apiKey :: Giphy.Key
apiKey = Giphy.Key "dc6zaTOxFJmzC"

options :: Opt.Parser Options
options = Options . T.pack <$> Opt.argument Opt.str
                             ( Opt.metavar "QUERY"
                            <> Opt.help "Search query for your GIFs" )
                           <*> Opt.switch
                             ( Opt.long "translate"
                            <> Opt.short 't'
                            <> Opt.help "Use translate instead of search" )

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
      let config = Giphy.GiphyConfig apiKey
      let q = Giphy.Query $ query opts
      resp <- Giphy.runGiphy (Giphy.search q) config
      let fstUrl = resp ^? _Right
                         . Giphy.searchItems
                         . _head
                         . Giphy.gifImages
                         . at "original"
                         . traverse
                         . Giphy.imageUrl
                         . traverse
      print fstUrl

translateApp :: T.Text -> Giphy.Giphy (Maybe URI.URI)
translateApp q = do
  let phrase = Giphy.Phrase q
  resp <- Giphy.translate phrase

  return $ resp ^? _Right
                 . Giphy.translateItems
                 . _head
                 . Giphy.gifImages
                 . at "original"
                 . traverse
                 . Giphy.imageUrl
                 . traverse

searchApp :: T.Text -> Giphy.Giphy (Maybe URI.URI)
searchApp q = do
  let phrase = Giphy.Phrase q
  resp <- Giphy.translate phrase

  return $ resp ^? _Right
                 . Giphy.searchItems
                 . _head
                 . Giphy.gifImages
                 . at "original"
                 . traverse
                 . Giphy.imageUrl
                 . traverse
