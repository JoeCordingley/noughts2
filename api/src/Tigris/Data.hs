{-# LANGUAGE DeriveGeneric #-}
module Tigris.Data (Sphere(..), Tile(..), Dynasty(..), Leader(..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Sphere = Temples | Markets | Settlements | Farms deriving (Show, Eq, Ord)
newtype Tile = Tile {tileSphere :: Sphere} deriving (Show, Eq, Ord)
data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)
instance FromJSON Dynasty

instance ToJSON Dynasty
newtype Leader = Leader {leaderSphere :: Sphere} deriving (Show, Eq, Ord)
