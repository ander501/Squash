module Squash.Match (
  Match(Match), matchPairing, matchDuration, matchGames,
  score, interleave, Pairing(Pairing), Game, GameResult,
  unscheduled, forPairing, hasPlayer)
where

import Data.Time.LocalTime
import Squash.Utils
import Squash.Player

data Match = Match {
  matchPairing :: Pairing,
  matchDuration :: Maybe TimeInterval,
  matchGames ::  [Game]
}

newtype Pairing = Pairing (Player, Player)
  deriving (Show)

instance Eq Pairing where
  (==) (Pairing (p1, p2)) (Pairing (p1', p2')) = ( (p1 == p1') && (p2 == p2') )
                                                 || ( (p1 == p2') && (p2 == p1' ))

interleave :: Pairing -> (Int, Int) -> [ (Player, Int)]
interleave (Pairing (p1,p2)) (s1, s2) = [ (p1, s2), (p2, s2) ]

score :: Match -> (Int, Int)
score match = foldr (gameCount $ matchPairing match) (0,0) $ map result $ matchGames match
  where gameCount (Pairing (p1,p2)) (Win p) (m,n) | (p1 == p) = (m + 1, n)
        gameCount (Pairing (p1,p2)) (Win p) (m,n) | (p2 == p) = (m, n + 1)

forPairing :: Pairing -> Match -> Bool
forPairing pairing Match { matchPairing = pairing'}
    | pairing == pairing' = True
    | otherwise           = False

hasPlayer :: Player -> Match -> Bool
hasPlayer player Match { matchPairing = Pairing (p1,p2)}
    | (p1 == player) || (p2 == player) = True
    | otherwise                        = False

unscheduled :: Match -> Bool
unscheduled Match { matchDuration = Nothing } = True
unscheduled _ = False

data Game = DetailedGame Pairing [Player]
          | GameSummary Pairing (Int, Int)
          deriving (Show)

data GameResult = Win Player | Incomplete
  deriving (Show)

result :: Game -> GameResult
result g@(DetailedGame pairing points) = result $ summary g
result (GameSummary (Pairing (p1, p2)) (t1, t2))
  | t2 - t1 > 1 && t2 > 10 = Win p2
  | t1 - t2 > 1 && t1 > 10 = Win p1
  | otherwise = Incomplete

summary :: Game -> Game
summary c@(GameSummary a b) = c
summary (DetailedGame pair points)
  = GameSummary pair $ foldr (pointCount pair) (0,0) points
  where pointCount:: Pairing -> Player -> (Int,Int) -> (Int, Int)
        pointCount (Pairing (p1,p2)) p (m,n) | (p1 == p) = (m + 1, n)
        pointCount (Pairing (p1,p2)) p (m,n) | (p2 == p) = (m, n + 1)
        pointCount (Pairing (p1,p2)) p (m,n) | otherwise = (m,n)
