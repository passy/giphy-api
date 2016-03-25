{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Provides a Giphy monad that can be used to issue selected API calls under a
-- selected API key.
--
-- @
-- import qualified Web.Giphy as Giphy
--
-- let apiKey = Giphy.'Key' "dc6zaTOxFJmzC"
-- let config = Giphy.'GiphyConfig' apiKey
-- resp <- Giphy.'runGiphy' (Giphy.'search' $ Giphy.'query' "puppies") config
-- let fstUrl = resp ^? _Right
--                    . Giphy.'searchItems'
--                    . _head
--                    . Giphy.'gifImages'
--                    . at "original"
--                    . traverse
--                    . Giphy.'imageUrl'
--                    . traverse
-- print fstUrl
-- @

module Web.Giphy
  (
  -- * Request Data Types
  -- $request
    Key(..)
  , Query(..)
  , Phrase(..)
  , Tag(..)
  , Pagination(..)
  , GifId(..)
  -- * Response Data Types
  -- $response
  , Gif(..)
  , Image(..)
  , ImageMap()
  , PaginationOffset(..)
  , SearchResponse(..)
  , SingleGifResponse(..)
  , TranslateResponse(..)
  , RandomResponse(..)
  -- * Giphy Monad
  -- $giphy
  , GiphyConfig(..)
  , Giphy()
  , runGiphy
  -- * Lenses
  -- $lenses
  , gifId
  , gifImages
  , gifSlug
  , gifUrl
  , imageHeight
  , imageUrl
  , imageSize
  , imageWidth
  , imageMp4Url
  , imageMp4Size
  , imageWebpUrl
  , imageWebpSize
  , paginationCount
  , paginationOffset
  , paginationTotalCount
  , searchItems
  , searchPagination
  , singleGifItem
  , translateItem
  , randomGifItem
  -- * API calls
  -- $api
  , search
  , searchOffset
  , translate
  , gif
  , random
  ) where

import           Control.Monad           (MonadPlus (), forM, join, mzero)
import           Control.Monad.Except    (ExceptT, runExceptT)
import qualified Control.Monad.Reader    as Reader
import           Control.Monad.Trans     (MonadIO (), lift, liftIO)
import           Data.Aeson              ((.:), (.:?))
import qualified Data.Aeson.Types        as Aeson
import qualified Data.Map.Strict         as Map
import           Data.Monoid             ((<>))
import qualified Data.Proxy              as Proxy
import qualified Data.Text               as T
import           GHC.Generics            (Generic ())
import qualified Lens.Micro.TH           as Lens
import qualified Network.HTTP.Client     as HTTP
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.URI             as URI
import           Servant.API             ((:<|>) (..), (:>))
import qualified Servant.API             as Servant
import qualified Servant.Client          as Servant
import qualified System.IO.Unsafe        as Unsafe
import qualified Text.Read               as Read
import qualified Web.HttpApiData         as Data

maybeParse :: (Monad m, MonadPlus m) => (a -> Maybe b) -> m a -> m b
maybeParse f = (maybe mzero return . f =<<)

fromURI :: (Monad m, MonadPlus m) => m String -> m Servant.URI
fromURI = maybeParse URI.parseURI

fromInt :: (Monad m, MonadPlus m) => m String -> m Int
fromInt = maybeParse Read.readMaybe

-- $request
--
-- These data types are used to encapsulate otherwise weakly
-- typed arguments.

-- | The API Key. See https://github.com/Giphy/GiphyAPI
newtype Key = Key T.Text
  deriving (Data.ToHttpApiData, Data.FromHttpApiData, Show, Eq)

-- | A search query.
newtype Query = Query T.Text
  deriving (Data.ToHttpApiData, Data.FromHttpApiData, Show, Eq)

-- | A phrase or term used for translation.
newtype Phrase = Phrase T.Text
  deriving (Data.ToHttpApiData, Data.FromHttpApiData, Show, Eq)

-- | A tag to retrieve a random GIF for.
newtype Tag = Tag T.Text
  deriving (Data.ToHttpApiData, Data.FromHttpApiData, Show, Eq)

-- | A unique gif identifier.
newtype GifId = GifId T.Text
  deriving (Data.ToHttpApiData, Data.FromHttpApiData, Show, Eq)

-- | Offset for paginated requests.
newtype PaginationOffset = PaginationOffset Int
  deriving (Data.ToHttpApiData, Data.FromHttpApiData, Show, Eq)

-- $response
--
-- These data types contain are the parsed JSON responses from
-- the Giphy API.

-- | An image contained in a Giphy response.
data Image = Image {
    _imageUrl      :: Maybe URI.URI
  , _imageSize     :: Maybe Int
  , _imageMp4Url   :: Maybe URI.URI
  , _imageMp4Size  :: Maybe Int
  , _imageWebpUrl  :: Maybe URI.URI
  , _imageWebpSize :: Maybe Int
  , _imageWidth    :: Maybe Int
  , _imageHeight   :: Maybe Int
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''Image

parseImageJSON :: T.Text -> Aeson.Object -> Aeson.Parser Image
parseImageJSON prefix o =
  let p = if T.null prefix then "" else prefix <> "_"
  in Image <$> (fromURI <$> (o .:? (p <> "url")))
           <*> (fromInt <$> (o .:? (p <> "size")))
           <*> (fromURI <$> (o .:? (p <> "mp4")))
           <*> (fromInt <$> (o .:? (p <> "mp4_size")))
           <*> (fromURI <$> (o .:? (p <> "webp")))
           <*> (fromInt <$> (o .:? (p <> "webp_size")))
           <*> (fromInt <$> (o .:? (p <> "width")))
           <*> (fromInt <$> (o .:? (p <> "height")))

instance Aeson.FromJSON Image where
  parseJSON (Aeson.Object o) = parseImageJSON "" o
  parseJSON _ = error "Invalid image response."

-- | Mapping from a 'T.Text' identifier to an 'Image'.
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

-- | Metadata about pagination in a response.
data Pagination = Pagination {
    _paginationTotalCount :: Int
  , _paginationCount      :: Int
  , _paginationOffset     :: Int
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''Pagination

instance Aeson.FromJSON Pagination where
  parseJSON (Aeson.Object o) =
    Pagination <$> o .: "total_count"
               <*> o .: "count"
               <*> o .: "offset"
  parseJSON _ = error "Invalid pagination data."

-- | A collection of GIFs as part of a search response.
data SearchResponse = SearchResponse {
    _searchItems      :: [Gif]
  , _searchPagination :: Pagination
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''SearchResponse

instance Aeson.FromJSON SearchResponse where
  parseJSON (Aeson.Object o) =
    SearchResponse <$> o .: "data"
                   <*> o .: "pagination"
  parseJSON _ = error "Invalid search response."

-- | A single GIF as part of a translate response.
newtype TranslateResponse = TranslateResponse {
    _translateItem :: Gif
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''TranslateResponse

instance Aeson.FromJSON TranslateResponse where
  parseJSON (Aeson.Object o) =
    TranslateResponse <$> o .: "data"
  parseJSON _ = error "Invalid translate response."

-- | A single gif as part of a response.
newtype SingleGifResponse = SingleGifResponse {
  _singleGifItem :: Gif
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''SingleGifResponse

instance Aeson.FromJSON SingleGifResponse where
  parseJSON (Aeson.Object o) =
    SingleGifResponse <$> o .: "data"
  parseJSON _ = error "Invalid GIF response."

-- | A single gif as part of a response.
newtype RandomResponse = RandomResponse {
  _randomGifItem :: Gif
} deriving (Show, Eq, Ord, Generic)

Lens.makeLenses ''RandomResponse

randomImageKeys :: [T.Text]
randomImageKeys = [ "fixed_height_downsampled"
                  , "fixed_height_small"
                  , "fixed_width_downsampled"
                  , "fixed_width_small"
                  , "fixed_width"
                  ]

instance Aeson.FromJSON RandomResponse where
  parseJSON (Aeson.Object o) =
    RandomResponse <$> (mkGif =<< (o .: "data"))
    where
      mkGif :: Aeson.Object -> Aeson.Parser Gif
      mkGif d =
        Gif <$> d .: "id"
            <*> pure ""
            <*> fromURI (d .: "url")
            <*> mkImageMap d

      mkImageMap :: Aeson.Object -> Aeson.Parser ImageMap
      mkImageMap = (Map.fromList <$>) . forM keys . uncurry . extractImage
        where
          dup = join (,)
          keys = ("image", "original") : (dup <$> randomImageKeys)

      extractImage :: Aeson.Object -> T.Text -> T.Text -> Aeson.Parser (T.Text, Image)
      extractImage d key tag = (,) <$> pure tag <*> parseImageJSON key d

  parseJSON _ = error "Invalid GIF response."

-- | The Giphy API
type GiphyAPI = "search"
    :> Servant.QueryParam "api_key" Key
    :> Servant.QueryParam "offset" PaginationOffset
    :> Servant.QueryParam "q" Query
    :> Servant.Get '[Servant.JSON] SearchResponse
  :<|> "translate"
    :> Servant.QueryParam "api_key" Key
    :> Servant.QueryParam "s" Phrase
    :> Servant.Get '[Servant.JSON] TranslateResponse
  :<|> Servant.Capture "gif_id" GifId
    :> Servant.QueryParam "api_key" Key
    :> Servant.Get '[Servant.JSON] SingleGifResponse
  :<|> "random"
    :> Servant.QueryParam "api_key" Key
    :> Servant.QueryParam "tag" Tag
    :> Servant.Get '[Servant.JSON] RandomResponse

api :: Proxy.Proxy GiphyAPI
api = Proxy.Proxy

-- From the servant docs:
-- (Yes, the usage of unsafePerformIO is very ugly, we know.
--  Hopefully soon it’ll be possible to do without.)
{-# NOINLINE __manager #-}
__manager :: HTTP.Manager
__manager = Unsafe.unsafePerformIO $ HTTP.newManager tlsManagerSettings

search'
  :: Maybe Key
  -> Maybe PaginationOffset
  -> Maybe Query
  -> ExceptT Servant.ServantError IO SearchResponse

translate'
  :: Maybe Key
  -> Maybe Phrase
  -> ExceptT Servant.ServantError IO TranslateResponse

gif'
  :: GifId
  -> Maybe Key
  -> ExceptT Servant.ServantError IO SingleGifResponse

random'
  :: Maybe Key
  -> Maybe Tag
  -> ExceptT Servant.ServantError IO RandomResponse

search' :<|> translate' :<|> gif' :<|> random' = Servant.client api host __manager
  where
    host = Servant.BaseUrl Servant.Https "api.giphy.com" 443 "v1/gifs/"

-- $api
--
-- Functions that directly access the Giphy API. All these functions run in
-- the 'Giphy' monad.
--

-- | Issue a search request for the given query without specifying an offset.
-- E.g. <https://api.giphy.com/v1/gifs/search?q=funny+cat&api_key=dc6zaTOxFJmzC>
search
  :: Query
  -> Giphy SearchResponse
search query = do
  key <- Reader.asks configApiKey
  lift $ search' (pure key) (pure $ PaginationOffset 0) (pure query)

-- | Issue a search request for the given query by specifying a
-- pagination offset.
-- E.g. <https://api.giphy.com/v1/gifs/search?q=funny+cat&api_key=dc6zaTOxFJmzC&offset=25>
searchOffset
  :: Query
  -> PaginationOffset
  -> Giphy SearchResponse
searchOffset query offset = do
  key <- Reader.asks configApiKey
  lift $ search' (pure key) (pure offset) (pure query)

-- | Issue a request for a single GIF identified by its 'GifId'.
-- E.g. <https://api.giphy.com/v1/gifs/feqkVgjJpYtjy?api_key=dc6zaTOxFJmzC>
gif
  :: GifId
  -> Giphy SingleGifResponse
gif gifid = do
  key <- Reader.asks configApiKey
  lift $ gif' gifid (pure key)

-- | Issue a translate request for a given phrase or term.
-- E.g. <https://api.giphy.com/v1/gifs/translate?s=superman&api_key=dc6zaTOxFJmzC>
translate
  :: Phrase
  -> Giphy TranslateResponse
translate phrase = do
  key <- Reader.asks configApiKey
  lift $ translate' (pure key) (pure phrase)

-- | Issue a request for a random GIF for the given (optional) tag.
-- E.g. <https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=american+psycho>
random
  :: Maybe Tag
  -> Giphy RandomResponse
random tag = do
  key <- Reader.asks configApiKey
  lift $ random' (pure key) tag

-- $giphy
--
-- Use 'runGiphy' to lift the 'Giphy' monad into IO.
--

-- | Contains the key to access the API.
data GiphyConfig = GiphyConfig { configApiKey :: Key }
  deriving (Show, Eq)

-- | The Giphy monad contains the execution context.
type Giphy = Reader.ReaderT GiphyConfig (ExceptT Servant.ServantError IO)

-- | You need to provide a 'GiphyConfig' to lift a 'Giphy' computation
-- into 'MonadIO'.
runGiphy :: MonadIO m => Giphy a -> GiphyConfig -> m (Either Servant.ServantError a)
runGiphy = ((liftIO . runExceptT) .) . Reader.runReaderT

-- $lenses
--
-- You can use these lenses if you prefer them to manually accessing
-- record fields.
