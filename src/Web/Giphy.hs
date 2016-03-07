{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Giphy
  ( Key(..)
  , Query(..)
  , ImageMap(..)
  , Gif(..)
  , Image(..)
  , SearchResponse(..)
  -- Lenses
  , gifId
  , gifSlug
  , gifUrl
  , gifImages
  , imageUrl
  , imageMp4Url
  , imageWidth
  , imageHeight
  , imageMap
  , searchItems
  -- Actions
  , search
  ) where

import           Control.Monad              (MonadPlus (), mzero)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Aeson                 ((.:), (.:?))
import qualified Data.Aeson.Types           as Aeson
import qualified Data.Map.Strict            as Map
import qualified Data.Proxy                 as Proxy
import qualified Data.Text                  as T
import           GHC.Generics               (Generic ())
import qualified Lens.Micro.TH              as Lens
import qualified Network.URI                as URI
import           Servant.API                ((:>))
import qualified Servant.API                as Servant
import qualified Servant.Client             as Servant
import qualified Text.Read                  as Read

maybeParse :: (Monad m, MonadPlus m) => (a -> Maybe b) -> m a -> m b
maybeParse f = (maybe mzero return . f =<<)

fromURI :: (Monad m, MonadPlus m) => m String -> m Servant.URI
fromURI = maybeParse URI.parseURI

fromInt :: (Monad m, MonadPlus m) => m String -> m Int
fromInt = maybeParse Read.readMaybe

-- | The API Key. See https://github.com/Giphy/GiphyAPI
newtype Key = Key T.Text
  deriving (Servant.ToText, Servant.FromText, Show, Eq)

-- | A search query.
newtype Query = Query T.Text
  deriving (Servant.ToText, Servant.FromText, Show, Eq)

data Image = Image {
    _imageUrl    :: Maybe URI.URI
  , _imageMp4Url :: Maybe URI.URI
  , _imageWidth  :: Maybe Int
  , _imageHeight :: Maybe Int
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''Image

instance Aeson.FromJSON Image where
  parseJSON (Aeson.Object o) =
    Image <$> (fromURI <$> (o .:? "url"))
          <*> (fromURI <$> (o .:? "mp4"))
          <*> (fromInt <$> (o .:? "width"))
          <*> (fromInt <$> (o .:? "height"))

newtype ImageMap = ImageMap { _imageMap :: Map.Map T.Text Image }
  deriving (Show, Eq, Ord, Generic)

instance Aeson.FromJSON ImageMap

Lens.makeLenses ''ImageMap

-- | A search response item.
data Gif = Gif {
    _gifId     :: T.Text
  , _gifSlug   :: T.Text
  , _gifUrl    :: URI.URI
  , _gifImages :: ImageMap
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''Gif

instance Aeson.FromJSON Gif where
  parseJSON (Aeson.Object o) =
    Gif <$> o .: "id"
        <*> o .: "slug"
        <*> fromURI (o .: "url")
        <*> o .: "images"

newtype SearchResponse = SearchResponse {
  _searchItems :: [Gif]
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''SearchResponse

instance Aeson.FromJSON SearchResponse where
  parseJSON (Aeson.Object o) =
    SearchResponse <$> o .: "data"

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

-- | Issue a search request for the given query.
search
  :: Key
  -> Query
  -> IO (Either Servant.ServantError SearchResponse)
search key query = runEitherT $ search' (pure key) (pure query)
