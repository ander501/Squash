module Squash.League
where
import Squash.Player
import Squash.Match
import Control.Monad.State
import Data.Map
import Data.Time.LocalTime


newtype (Monad m) => League m a = StateM LeagueState m a

data LeagueState = LeagueState {
  leagueMatches :: [Match],
  leagueOffers  :: [OfferTime]
}

data OfferTime = OfferTime {
  offerPlayer :: Player,
  offerStart  :: LocalTime,
  offerEnd    :: LocalTime
  }

type PlayerScores = Map Int

standings :: League -> [ ( Player, Int ) ]
standings (League matches) = Map.assocs $ foldr collect Map.empty $ playerTotals matches
  where collect ( player, score ) = Map.insertWith (+) player score
        playerTotals (g:gs) = interleave (matchPairing g) (score g) ++ playerTotals gs
        playerTotals _ = []

acceptOffer :: LocalTime -> LocalTime -> Player -> Player -> League m Bool
acceptOffer startTime endTime acceptingPlayer offeringPlayer
  = do
    offers <- leagueOffers gets
    matches <- leagueMatches gets
