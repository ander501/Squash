{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Squash.Player ( Player(),
                       PlayerReader,
                       PlayerInfo,
                       IntMapPlayerStore(),
                       PlayerStore,  addPlayer)
where
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe

data Player = Player Int
  deriving (Show, Eq, Ord)

data PlayerRecord = PlayerRecord {
   playerInfoFirstName :: String,
   playerInfoLastName :: String,
   playerInfoEmail :: String
   }


class PlayerInfo a where
  playerFirstName :: a -> String
  playerLastName  :: a -> String
  playerEmail     :: a -> String

instance PlayerInfo PlayerRecord where
  playerFirstName = playerInfoFirstName
  playerLastName = playerInfoLastName
  playerEmail = playerInfoEmail

type PlayerStoreMap = IntMap PlayerRecord

data PlayerRoster = PlayerRoster Int PlayerStoreMap

emptyRoster = PlayerRoster 1 IntMap.empty

class (MonadTrans t) => PlayerStore t where
  addPlayer :: (Monad m) => PlayerRecord -> t m Player

type IntMapPlayerStore = StateT PlayerRoster

type PlayerReader = ReaderT PlayerStoreMap

instance PlayerStore IntMapPlayerStore where
  addPlayer = intmapAddPlayer

intmapAddPlayer :: (Monad m) => PlayerRecord -> IntMapPlayerStore m Player
intmapAddPlayer roster = do
  PlayerRoster playerId _ <- get
  modify $ \ (PlayerRoster nextId storeMap) ->
    PlayerRoster  (nextId + 1) $
      IntMap.insert playerId roster storeMap
  return (Player playerId)

intmapPlayerInfo :: (Monad m) => Player -> IntMapPlayerStore m PlayerRecord
intmapPlayerInfo (Player key) = do
  PlayerRoster _ store <- get
  return $ fromJust $ IntMap.lookup key store
