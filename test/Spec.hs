{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

import BasicPrelude
import           System.Directory            (getCurrentDirectory)

import           Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS

import qualified Web.Giphy.Search as Search

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
        Search.gifUrl item `shouldBe` "https://giphy.com/gifs/cat-funny-QgcQLZa6glP2w"
