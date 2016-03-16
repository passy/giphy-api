{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

import           BasicPrelude
import           Data.Maybe             (fromJust)
import           System.Directory       (getCurrentDirectory)

import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Map.Strict        as Map
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           Network.URI            (parseURI)
import           Control.Lens.Cons      (_head)
import           Control.Lens.At        (at)

import           Control.Lens.Operators
import           Test.Hspec

import qualified Web.Giphy              as Giphy

readFixture :: FilePath -> IO BS.ByteString
readFixture path = do
    dir <- getCurrentDirectory
    BS.fromStrict . TE.encodeUtf8 <$> TIO.readFile (dir </> "test" </> "fixtures" </> path)

main :: IO ()
main = hspec $ do
  describe "Giphy" $ do
    describe "JSON Parsing" $ do

      it "parses a search response" $ do
        resp <- readFixture "search_response.json"
        let res = either error id (Aeson.eitherDecode resp)
        let item = res ^?! Giphy.searchItems . _head

        item ^. Giphy.gifId `shouldBe` "QgcQLZa6glP2w"
        item ^. Giphy.gifSlug `shouldBe` "cat-funny-QgcQLZa6glP2w"
        item ^. Giphy.gifUrl `shouldBe` fromJust (parseURI "https://giphy.com/gifs/cat-funny-QgcQLZa6glP2w")
        res ^. Giphy.searchPagination . Giphy.paginationCount `shouldBe` 25
        res ^. Giphy.searchPagination . Giphy.paginationTotalCount `shouldBe` 2161

        let images = item ^. Giphy.gifImages

        Map.keys images `shouldBe` [
            "downsized"
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

        images ^? at "fixed_height" . traverse . Giphy.imageUrl . traverse `shouldBe`
          parseURI "https://media0.giphy.com/media/QgcQLZa6glP2w/200.gif"

      it "parses a translate response" $ do
        resp <- readFixture "translate_response.json"
        let item = case Aeson.eitherDecode resp of
                    Left err -> error err
                    Right i -> i ^?! Giphy.translateItem

        item ^. Giphy.gifId `shouldBe` "Zlkbqhurvg4Zq"
        item ^. Giphy.gifSlug `shouldBe` "superman-Zlkbqhurvg4Zq"
        item ^. Giphy.gifUrl `shouldBe` fromJust (parseURI "https://giphy.com/gifs/superman-Zlkbqhurvg4Zq")

        let images = item ^. Giphy.gifImages

        Map.keys images `shouldBe` [
            "downsized"
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

        images ^? at "fixed_height" . traverse . Giphy.imageUrl . traverse `shouldBe`
          parseURI "https://media0.giphy.com/media/Zlkbqhurvg4Zq/200.gif"
