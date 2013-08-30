{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Squash.Player ( Player(),
    PlayerInfo(playerInfoFirstName, playerInfoSecondName, playerInfoEmail),
    IntMapPlayerStore(),
    PlayerStore,  addPlayer, playerInfo)
where
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Control.Monad.State
import Data.Maybe

data Player = Player Int
  deriving (Show, Eq, Ord)

data PlayerInfo = PlayerInfo {
   playerInfoFirstName :: String,
   playerInfoSecondName :: String,
   playerInfoEmail :: String
   }

type PlayerStoreMap = IntMap PlayerInfo

data PlayerRoster = PlayerRoster Int PlayerStoreMap

emptyRoster = PlayerRoster 1 IntMap.empty

class (MonadTrans t) => PlayerStore t where
  addPlayer :: (Monad m) => PlayerInfo -> t m Player
  playerInfo :: (Monad m) => Player -> t m PlayerInfo

type IntMapPlayerStore = StateT PlayerRoster

instance PlayerStore IntMapPlayerStore where
  addPlayer = intmapAddPlayer
  playerInfo = intmapPlayerInfo

intmapAddPlayer :: (Monad m) => PlayerInfo -> IntMapPlayerStore m Player
intmapAddPlayer roster = do
  PlayerRoster playerId _ <- get
  modify $ \ (PlayerRoster nextId storeMap) ->
    PlayerRoster  (nextId + 1) $
      IntMap.insert playerId roster storeMap
  return (Player playerId)

intmapPlayerInfo :: (Monad m) => Player -> IntMapPlayerStore m PlayerInfo
intmapPlayerInfo (Player key) = do
  PlayerRoster _ store <- get
  return $ fromJust $ IntMap.lookup key store
