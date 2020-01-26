{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.WebSocket where

import Data.Aeson
import Data.Map.Monoidal (MonoidalMap)
import Data.Semigroup ((<>))
import Data.Some
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Network.URI (URI(..))
import Reflex.Patch
import Reflex.Query.Class

websocketUri :: URI -> URI
websocketUri uri = uri
  { uriScheme = case uriScheme uri of
    "http:" -> "ws:"
    "https:" -> "wss:"
    "file:" -> "ws:"
    p -> error $ "Unrecognized protocol: " <> p
  }

-- | Represents a WebSocket message from one of two channels: ViewSelector declarations or API requests
data WebSocketRequest q r = WebSocketRequest_ViewSelector q
                          | WebSocketRequest_Api (TaggedRequest r)
  deriving (Typeable, Generic)

instance (FromJSON q, FromJSON (Some r)) => FromJSON (WebSocketRequest q r)
instance (ToJSON q, ToJSON (Some r)) => ToJSON (WebSocketRequest q r)

-- | Represents a WebSocket response from one of three channels: incoming 'View's, API responses, or version info
data WebSocketResponse q = WebSocketResponse_View (QueryResult q)
                         | WebSocketResponse_Api TaggedResponse
                         | WebSocketResponse_Version Text
  deriving (Typeable, Generic)

instance FromJSON (QueryResult q) => FromJSON (WebSocketResponse q)
instance ToJSON (QueryResult q) => ToJSON (WebSocketResponse q)

-- | A request tagged with an identifier
data TaggedRequest r = TaggedRequest Int (Some r)
  deriving (Typeable, Generic)

instance FromJSON (Some r) => FromJSON (TaggedRequest r)
instance ToJSON (Some r) => ToJSON (TaggedRequest r)

-- | A response tagged with an identifier matching the one in the 'TaggedRequest'. The identifier is the first argument.
data TaggedResponse = TaggedResponse Int Value
  deriving (Typeable, Generic)

instance FromJSON TaggedResponse
instance ToJSON TaggedResponse

newtype Channels channelId a = Channels { unChannels :: MonoidalMap channelId a }
  deriving (Typeable, Generic)

deriving instance (Eq channelId, Eq q) => Eq (Channels channelId q)
deriving instance (Ord channelId, Semigroup q) => Semigroup (Channels channelId q)
deriving instance (Ord channelId, Monoid q) => Monoid (Channels channelId q)
deriving instance (Ord channelId, Additive q) => Additive (Channels channelId q)
deriving instance (Ord channelId, Group q) => Group (Channels channelId q)
deriving instance (ToJSONKey channelId, ToJSON q) => ToJSON (Channels channelId q)
deriving instance (Ord channelId, FromJSONKey channelId, FromJSON q) => FromJSON (Channels channelId q)

instance (Ord channelId, Query q) => Query (Channels channelId q) where
  type QueryResult (Channels channelId q) = Channels channelId (QueryResult q)
  crop (Channels q) (Channels v) = Channels $ crop q v

-- newtype ChannelView channelId q = ChannelView { unChannelView :: MonoidalMap channelId (QueryResult q) }
--   deriving (Typeable, Generic)

-- newtype ChannelViewChunk channelId q = ChannelViewChunk { unChannelViewChunk :: MonoidalMap channelId (Chunk (QueryResult q)) }
--   deriving (Typeable, Generic)

-- deriving instance (Eq channelId, Eq (QueryResult q)) => Eq (ChannelView channelId q)
-- deriving instance (Ord channelId, Semigroup (QueryResult q)) => Semigroup (ChannelView channelId q)
-- deriving instance (Ord channelId, Monoid (QueryResult q)) => Monoid (ChannelView channelId q)
-- deriving instance (Ord channelId, Additive (QueryResult q)) => Additive (ChannelView channelId q)
-- deriving instance (Ord channelId, Group (QueryResult q)) => Group (ChannelView channelId q)
-- deriving instance (ToJSONKey channelId, ToJSON (QueryResult q)) => ToJSON (ChannelView channelId q)
-- deriving instance (Ord channelId, FromJSONKey channelId, FromJSON (QueryResult q)) => FromJSON (ChannelView channelId q)

-- instance (Ord channelId, Query q) => Query (ChannelQuery channelId q) where
--   type QueryResult (ChannelQuery channelId q) = ChannelView channelId q
--   crop (ChannelQuery vs) (ChannelView v) = ChannelView $ crop vs v

class (ToJSON (Payload a)) => Chunkable a where
  type Payload a :: *
  toChunks :: a -> ([Payload a], Int)

data Chunk a = Chunk
  { _chunk_number      :: {-# UNPACK #-} !Int
  , _chunk_totalChunks :: {-# UNPACK #-} !Int
  , _chunk_payload     :: !(Payload a)
  }
