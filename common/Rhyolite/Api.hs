{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Rhyolite.Api where

import Data.Aeson
import Data.Some
import Data.Constraint.Forall
import Data.Constraint.Extras

type Request r = (ForallF ToJSON r, Has ToJSON r, FromJSON (Some r), Has FromJSON r)

data ApiRequest :: * -> (k -> *) -> (k -> *) -> k -> * where
  ApiRequest_Public :: public a -> ApiRequest cred public private a
  ApiRequest_Private :: cred -> private a -> ApiRequest cred public private a
  deriving (Show)

instance (ArgDict c public, ArgDict c private) => ArgDict c (ApiRequest cred public private) where
  type ConstraintsFor (ApiRequest cred public private) c = (ConstraintsFor public c, ConstraintsFor private c)
  argDict = \case
    ApiRequest_Public x -> argDict x
    ApiRequest_Private _ x -> argDict x

public :: public t -> ApiRequest cred public private t
public = ApiRequest_Public

private :: cred -> private t -> ApiRequest cred public private t
private = ApiRequest_Private

instance (ToJSON cred, ToJSON (public a), ToJSON (private a)) => ToJSON (ApiRequest cred public private a) where
  toJSON = \case
    ApiRequest_Public x -> toJSON ("public", toJSON x)
    ApiRequest_Private c x -> toJSON ("private", (toJSON x, toJSON c))

instance (FromJSON cred, FromJSON (Some public), FromJSON (Some private)) => FromJSON (Some (ApiRequest cred public private)) where
  parseJSON v = do
    (tag,rest) <- parseJSON v
    case tag of
      "public" -> do
        Some x <- parseJSON rest
        return (Some (ApiRequest_Public x))
      "private" -> do
        (Some x, c) <- parseJSON rest
        return (Some (ApiRequest_Private c x))
      _ -> fail "Request appears neither public nor private"