{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Giphy
  ( Key(..)
  , Query(..)
  , ImageMap()
  , Gif(..)
  , Image(..)
  , SearchResponse(..)
  , SingleGifResponse(..)
  , GiphyConfig(..)
  , Giphy()
  -- Lenses
  , gifId
  , gifSlug
  , gifUrl
  , gifImages
  , singleGifItem
  , imageUrl
  , imageMp4Url
  , imageWidth
  , imageHeight
  , searchItems
  -- Actions
  , search
  , gif
  -- Monad runners
  , runGiphy
  ) where

import           Control.Monad              (MonadPlus (), mzero)
import qualified Control.Monad.Reader       as Reader
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Data.Aeson                 ((.:), (.:?))
import qualified Data.Aeson.Types           as Aeson
import qualified Data.Map.Strict            as Map
import qualified Data.Proxy                 as Proxy
import qualified Data.Text                  as T
import           GHC.Generics               (Generic ())
import qualified Lens.Micro.TH              as Lens
import qualified Network.URI                as URI
import           Servant.API                ((:<|>) (..), (:>))
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

-- | Contains the key to access the API.
data GiphyConfig = GiphyConfig { configApiKey :: Key }
  deriving (Show, Eq)

-- | The Giphy monad contains the execution context.
type Giphy = Reader.ReaderT GiphyConfig (EitherT Servant.ServantError IO)

-- | A search query.
newtype Query = Query T.Text
  deriving (Servant.ToText, Servant.FromText, Show, Eq)

-- | A unique gif identifier.
newtype GifId = GifId T.Text
  deriving (Servant.ToText, Servant.FromText, Show, Eq)

-- | An image contained in a Giphy response.
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
  parseJSON _ = error "Invalid image response."

type ImageMap = Map.Map T.Text Image

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
  parseJSON _ = error "Invalid GIF response."

newtype SearchResponse = SearchResponse {
  _searchItems :: [Gif]
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''SearchResponse

instance Aeson.FromJSON SearchResponse where
  parseJSON (Aeson.Object o) =
    SearchResponse <$> o .: "data"
  parseJSON _ = error "Invalid search response."

newtype SingleGifResponse = SingleGifResponse {
  _singleGifItem :: Gif
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''SingleGifResponse

instance Aeson.FromJSON SingleGifResponse where
  parseJSON (Aeson.Object o) =
    SingleGifResponse <$> o .: "data"
  parseJSON _ = error "Invalid GIF response."

-- | The Giphy API
type GiphyAPI = "v1"
    :> "gifs"
    :> "search"
    :> Servant.QueryParam "api_key" Key
    :> Servant.QueryParam "q" Query
    :> Servant.Get '[Servant.JSON] SearchResponse
  :<|> "v1"
    :> "gifs"
    :> Servant.Capture "gif_id" GifId
    :> Servant.QueryParam "api_key" Key
    :> Servant.Get '[Servant.JSON] SingleGifResponse

api :: Proxy.Proxy GiphyAPI
api = Proxy.Proxy

search'
  :: Maybe Key
  -> Maybe Query
  -> EitherT Servant.ServantError IO SearchResponse

gif'
  :: GifId
  -> Maybe Key
  -> EitherT Servant.ServantError IO SingleGifResponse

search' :<|> gif' = Servant.client api host
  where host = Servant.BaseUrl Servant.Https "api.giphy.com" 443

-- | Issue a search request for the given query.
--   E.g. <http://api.giphy.com/v1/gifs/search?q=funny+cat&api_key=dc6zaTOxFJmzC>
search
  :: Query
  -> Giphy SearchResponse
search query = do
  key <- Reader.asks configApiKey
  lift $ search' (pure key) (pure query)

-- | Issue a request for a single GIF identified by its 'GifId'.
--   E.g. <http://api.giphy.com/v1/gifs/feqkVgjJpYtjy?api_key=dc6zaTOxFJmzC>
gif
  :: GifId
  -> Giphy SingleGifResponse
gif gifid = do
  key <- Reader.asks configApiKey
  lift $ gif' gifid (pure key)

-- | You need to provide a 'GiphyConfig' to lift a 'Giphy' computation
-- into the 'IO' monad.
runGiphy :: Giphy a -> GiphyConfig -> IO (Either Servant.ServantError a)
runGiphy = (runEitherT .) . Reader.runReaderT
