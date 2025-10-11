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
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Lib
import Lucid (term, toHtml)
import Lucid.Base (Html)
import Lucid.Html5

-- Types

data Dynasty = Archer | Bull | Pot | Lion deriving (Eq, Ord, Generic, Show)

instance FromJSON Dynasty

instance ToJSON Dynasty

chooseDynasty :: Map Dynasty (PlayerId, Name) -> PlayerId -> Html ()
chooseDynasty playerMap player =
  div_ [id_ "board"] $ do
    h2_ "Choose Your Dynasty"
    div_ [class_ "dynasty-grid"]
      $ forM_ [Archer, Bull, Pot, Lion]
      $ dynastyDiv
    when (isReady playerMap) $ button_ [class_ "start-game action", term "hx-vals" (encodeToText (StartGame :: PositionChoice Dynasty)), term "ws-send" mempty] "Start Game"
  where
    dynastyDiv :: Dynasty -> Html ()
    dynastyDiv dynasty = div_ ([class_ "dynasty-box"] ++ if isMine then [class_ "mine"] else []) $ do
      strong_ . toHtml $ show dynasty
      small_ $ toHtml status
      span_ [class_ "button-area"] $ unless isTaken $ button_ [class_ "dynasty", class_ "action", term "hx-vals" jsonVal, term "ws-send" mempty] "Choose"
      where
        (isTaken, isMine, status) = case Map.lookup dynasty playerMap of
          Just (id, name) -> (True, id == player, "Player: " <> name)
          Nothing -> (False, False, "Available")
        jsonVal = encodeToText $ PositionChoice dynasty

isReady :: Map Dynasty a -> Bool
isReady playerMap = Map.size playerMap >= 2
