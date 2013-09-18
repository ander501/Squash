module Squash.League
where
import Squash.Player
import Squash.Match
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.LocalTime
import Data.List.Zipper (Zipper)
import qualified Data.List.Zipper as Zipper

data LeagueError = LeagueException | NoOfferFound | NoMatchFound | LeagueError String

instance Error LeagueError where
  noMsg = LeagueException
  strMsg s = LeagueError s

type League m a = ReaderT ErrorT LeagueError (StateT LeagueState m) a

data LeagueState = LeagueState {
  leagueMatches :: [Match],
  leagueOffers  :: [OfferTime]
}

data OfferTime = OfferTime {
  offerPlayer :: Player,
  offerStart  :: LocalTime,
  offerEnd    :: LocalTime
  }

possibleTime :: LocalTime -> LocalTime -> OfferTime -> Bool
possibleTime startTime endTime OfferTime {
  offerStart = offerStartTime,
  offerEnd = offerEndTime }
  | (offerStartTime <= startTime) && (startTime <= offerEndTime)
    && (offerStartTime <= endTime) && (endTime <= offerEndTime)
    && (startTime < endTime) = True
  | otherwise = False

type PlayerScores = Map Int

standings :: (Monad m) => League m [ ( Player, Int ) ]
standings = do
  let collect ( player, score ) = Map.insertWith (+) player score
      playerTotals (g:gs) = interleave (matchPairing g) (score g) ++ playerTotals gs
      playerTotals _ = []
  matches <- leagueMatches `liftM` get
  return $ Map.assocs $ foldr collect Map.empty $ playerTotals $ matches

acceptOffer :: (Monad m) => LocalTime -> LocalTime -> Player -> Player -> League m ()
acceptOffer startTime endTime offeringPlayer acceptingPlayer
  = do
      offers <- leagueOffers `liftM` get
      matches <- leagueMatches `liftM` get
      let (frontOffers, tailOffers) = break matchingOfferP offers
      offer <- if (null tailOffers)
               then throwError NoOfferFound
               else return $ head tailOffers
      let (frontMatches, tailMatches) = break matchingMatchP matches
      match <- if (null tailMatches)
               then throwError NoMatchFound
               else return $ head tailMatches
      put LeagueState {
        leagueOffers = frontOffers ++ tail tailOffers,
        leagueMatches = frontMatches ++ [match { matchDuration = Just (startTime, endTime) }] ++ tail tailMatches
        }
  where matchingOfferP offer@(OfferTime { offerPlayer = player}) = (player == offeringPlayer) && (possibleTime startTime endTime offer)
        matchingMatchP match = (unscheduled match) && (forPairing (Pairing (offeringPlayer, acceptingPlayer)) match)
