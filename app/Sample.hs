{-# LANGUAGE OverloadedStrings #-}

module Sample where

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
