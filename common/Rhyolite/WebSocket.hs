{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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

import Rhyolite.App

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
data WebSocketResponse channelId q
  = WebSocketResponse_View (Channels channelId (Chunk (QueryResult q)))
  | WebSocketResponse_Api TaggedResponse
  | WebSocketResponse_Version Text
  deriving (Typeable, Generic)

instance (Ord channelId, FromJSONKey channelId, FromJSON (Payload (QueryResult q))) => FromJSON (WebSocketResponse channelId q)
instance (Ord channelId, ToJSONKey channelId, ToJSON (Payload (QueryResult q))) => ToJSON (WebSocketResponse channelId q)

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
  deriving (Typeable, Generic, Functor, Traversable, Foldable)

deriving instance (Eq channelId, Eq q) => Eq (Channels channelId q)
deriving instance (Ord channelId, Semigroup q) => Semigroup (Channels channelId q)
deriving instance (Ord channelId, Monoid q) => Monoid (Channels channelId q)
deriving instance (Ord channelId, Additive q) => Additive (Channels channelId q)
deriving instance (Ord channelId, Group q) => Group (Channels channelId q)
deriving instance (PositivePart q) => PositivePart (Channels channelId q)
deriving instance (ToJSONKey channelId, ToJSON q) => ToJSON (Channels channelId q)
deriving instance (Ord channelId, FromJSONKey channelId, FromJSON q) => FromJSON (Channels channelId q)

instance (Ord channelId, Query q) => Query (Channels channelId q) where
  type QueryResult (Channels channelId q) = Channels channelId (QueryResult q)
  crop (Channels q) (Channels v) = Channels $ crop q v

class (ToJSON (Payload a)) => Chunkable a where
  type Payload a :: *
  toChunks :: a -> [Payload a]
  -- | Reconstruct a value from multiple chunks if possible.
  fromChunks :: [Payload a] -> Maybe a

data Chunk a = Chunk
  { _chunk_messageId    :: {-# UNPACK #-} !Int
  , _chunk_isFinalChunk :: !Bool
  , _chunk_payload      :: !(Payload a)
  }
  deriving (Typeable, Generic)

instance FromJSON (Payload a) => FromJSON (Chunk a)
instance ToJSON (Payload a) => ToJSON (Chunk a)
