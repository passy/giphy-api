{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

import           BasicPrelude
import           Data.Maybe           (fromJust)
import           System.Directory     (getCurrentDirectory)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.IO         as TIO
import           Network.URI          (parseURI)
import           Test.Hspec

import qualified Web.Giphy.Search     as Search

readFixture :: forall s b. FilePath -> IO BS.ByteString
readFixture path = do
    dir <- getCurrentDirectory
    BS.fromStrict . TE.encodeUtf8 <$> TIO.readFile (dir </> "test" </> "fixtures" </> path)

main :: IO ()
main = hspec $ do
  describe "Search" $ do
    describe "JSON Parsing" $ do

      it "parses a search response" $ do
        resp <- readFixture "search_response.json"
        let item = case Aeson.eitherDecode resp of
                      Left e -> error e
                      Right search -> head $ Search.searchItems search

        Search.gifId item `shouldBe` "QgcQLZa6glP2w"
        Search.gifSlug item `shouldBe` "cat-funny-QgcQLZa6glP2w"
        Search.gifUrl item `shouldBe` (fromJust $ parseURI "https://giphy.com/gifs/cat-funny-QgcQLZa6glP2w")

        let (Search.ImageMap m) = Search.gifImages item
        Map.keys m `shouldBe` [ "downsized"
                              , "downsized_large"
                              , "downsized_medium"
                              , "downsized_still"
                              , "fixed_height"
                              , "fixed_height_downsampled"
                              , "fixed_height_small"
                              , "fixed_height_small_still"
                              , "fixed_height_still"
                              , "fixed_width"
                              , "fixed_width_downsampled"
                              , "fixed_width_small"
                              , "fixed_width_small_still"
                              , "fixed_width_still"
                              , "looping"
                              , "original"
                              , "original_still"
                              ]
