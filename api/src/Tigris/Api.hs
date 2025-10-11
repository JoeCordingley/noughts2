{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tigris.Api (Dynasty, chooseDynasty, isReady) where

import BasicPrelude
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Lib
import Lucid (term, toHtml)
import Lucid.Base (Html)
import Lucid.Html5
import Text.Mustache (Template, automaticCompile, substitute)

-- Types

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)

instance FromJSON Dynasty

instance ToJSON Dynasty

chooseDynasty ::
  IO (Map Dynasty (PlayerId, Name) -> PlayerId -> Text)
chooseDynasty = do
  eTemplate <- automaticCompile ["../shared/templates"] "dynastyChoice.mustache"
  case eTemplate of
    Left err -> error $ "Mustache template parse error: " <> show err
    Right tmpl -> pure (render tmpl)
  where
    render :: Template -> Map Dynasty (PlayerId, Name) -> PlayerId -> Text
    render tmpl playerMap player = substitute tmpl context
      where
        context = object ["values" .= map forDynasty [Archer, Bull, Pot, Lion]]
        forDynasty dynasty =
          object
            [ "dynasty" .= show dynasty,
              "mine" .= isMine,
              "taken" .= isTaken,
              "player" .= name
            ]
          where
            (isTaken, isMine, name) = case Map.lookup dynasty playerMap of
              Just (id', playerName) -> (True, id' == player, Just playerName)
              Nothing -> (False, False, Nothing)

isReady :: Map Dynasty a -> Bool
isReady playerMap = Map.size playerMap >= 2
