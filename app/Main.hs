{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text              as T
import qualified Options.Applicative    as Opt
import qualified Web.Giphy              as Giphy

import           Control.Applicative    ((<**>))
import           Control.Lens.At        (at)
import           Control.Lens.Cons      (_head)
import           Control.Lens.Operators
import           Control.Lens.Prism     (_Right)
import           Data.Monoid            ((<>))
import           Data.Version           (Version (), showVersion)
import           Paths_giphy_api        (version)
import           System.Environment     (getProgName)

data Options = Options
  { query        :: T.Text
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

cliParser :: String -> Version -> Opt.ParserInfo Options
cliParser progName ver =
  Opt.info ( Opt.helper <*> options <**> versionInfo )
    ( Opt.fullDesc
   <> Opt.progDesc "Find GIFs on the command line."
   <> Opt.header progName )
  where
    versionInfo = Opt.infoOption ( unwords [progName, showVersion ver] )
      ( Opt.short 'V'
     <> Opt.long "version"
     <> Opt.hidden
     <> Opt.help "Show version information" )

main :: IO ()
main = do
  progName <- getProgName
  Opt.execParser (cliParser progName version) >>= run
  where
    run :: Options -> IO ()
    run opts = do
      let config = Giphy.GiphyConfig apiKey
      let app = if useTranslate opts then translateApp else searchApp
      resp <- Giphy.runGiphy (app $ query opts) config
      let fstUrl = resp ^? _Right
                         . _head
                         . Giphy.gifImages
                         . at "original"
                         . traverse
                         . Giphy.imageUrl
                         . traverse
      print fstUrl

translateApp :: T.Text -> Giphy.Giphy [Giphy.Gif]
translateApp q = do
  resp <- Giphy.translate $ Giphy.Phrase q
  return . pure $ resp ^. Giphy.translateItem

searchApp :: T.Text -> Giphy.Giphy [Giphy.Gif]
searchApp q = do
  resp <- Giphy.search $ Giphy.Query q
  return $ resp ^. Giphy.searchItems
