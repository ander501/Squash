module Squash.League where

import Squash.Utils
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

data LeagueError = LeagueException
                 | NoOfferFound
                 | NoMatchFound
                 | TimeConflict
                 | LeagueError String

instance Error LeagueError where
  noMsg = LeagueException
  strMsg s = LeagueError s

type League m a = PlayerReader (ErrorT LeagueError (StateT LeagueState m)) a

data LeagueState = LeagueState {
  leagueMatches :: [Match],
  leagueOffers  :: [Offer]
}

data Offer = Offer {
  offerPlayer :: Player,
  offerInterval :: TimeInterval
  }

possibleTime :: TimeInterval -> Offer -> Bool
possibleTime interval Offer { offerInterval = offerredInterval }
  = interval `containedIn` offerredInterval

type PlayerScores = Map Int

standings :: (Monad m) => League m [ ( Player, Int ) ]
standings = do
  let collect ( player, score ) = Map.insertWith (+) player score
      playerTotals (g:gs) = interleave (matchPairing g) (score g) ++ playerTotals gs
      playerTotals _ = []
  matches <- leagueMatches `liftM` get
  return $ Map.assocs $ foldr collect Map.empty $ playerTotals $ matches

makeOffer :: (Monad m) => TimeInterval -> Player -> League m ()
makeOffer interval offeringPlayer
  = do
      offers <- leagueOffers `liftM` get
      matches <- leagueMatches `liftM` get
      if (null $ filter isConflictingP matches)
        then put LeagueState {
           leagueOffers = (Offer {
                              offerPlayer = offeringPlayer,
                              offerInterval = interval} : offers ),
           leagueMatches = matches}
        else throwError TimeConflict
  where
    isConflictingP :: Match -> Bool
    isConflictingP match@(Match { matchPairing = Pairing (p1, p2), matchDuration = (Just scheduledInterval)})
          = (p1 == offeringPlayer || p2 == offeringPlayer) && (interval `conflictsWith` scheduledInterval)
    isConflictingP _ = False

acceptOffer :: (Monad m) => TimeInterval -> Player -> Player -> League m ()
acceptOffer interval offeringPlayer acceptingPlayer
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
        leagueMatches = frontMatches ++ [match { matchDuration = Just interval}] ++ tail tailMatches
        }
  where matchingOfferP offer@(Offer { offerPlayer = player}) = (player == offeringPlayer) && (possibleTime interval offer)
        matchingMatchP match = (unscheduled match) && (forPairing (Pairing (offeringPlayer, acceptingPlayer)) match)
