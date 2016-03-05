{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}

module Web.Giphy.Search where

import qualified Data.Text as T
import qualified Servant.API as Servant
import qualified Servant.Client as Servant
import qualified Data.Aeson.Types as Aeson
import qualified Data.Proxy as Proxy

import GHC.Generics (Generic())
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Aeson ((.:))
import Servant.API ((:>))

-- | The API Key. See https://github.com/Giphy/GiphyAPI
newtype Key = Key T.Text
  deriving (Servant.ToText, Servant.FromText, Show, Eq)

-- | A search query.
newtype Query = Query T.Text
  deriving (Servant.ToText, Servant.FromText, Show, Eq)

newtype SearchResponse = SearchResponse {
  searchItems :: [ Gif ]
} deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON SearchResponse

-- | A search response item.
data Gif = Gif {
    gifId :: T.Text
  , gifSlug :: T.Text
  , gifUrl :: T.Text -- TODO: URL type
} deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON Gif where
  parseJSON (Aeson.Object o) = do
    d <- o .: "data"
    id_ <- d .: "id"
    slug <- d .: "slug"
    url <- d .: "url"
    return Gif {
        gifId = id_
      , gifSlug = slug
      , gifUrl = url
    }

-- | The Giphy API
type GiphyAPI = "v1"
  :> "gifs"
  :> "search"
  :> Servant.QueryParam "api_key" Key
  :> Servant.QueryParam "q" Query
  :> Servant.Get '[Servant.JSON] SearchResponse

api :: Proxy.Proxy GiphyAPI
api = Proxy.Proxy

search'
  :: Maybe Key
  -> Maybe Query
  -> EitherT Servant.ServantError IO SearchResponse
search' = Servant.client api host
  where host = Servant.BaseUrl Servant.Https "api.giphy.com" 443

search
  :: Key
  -> Query
  -> IO (Either Servant.ServantError SearchResponse)
search key query = runEitherT $ search' (pure key) (pure query)
