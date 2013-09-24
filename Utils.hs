module Squash.Utils where

import Data.Time.LocalTime

data Interval a = Interval (a,a)

overlap :: (Ord a) => Interval a -> Interval a -> Bool
overlap (Interval (a0,a1)) (Interval (b0,b1))
  = ((a0 < b0) && (b0 < a1)) || ((a0 < b1) && (b1 < a1))

containedIn :: (Ord a) => Interval a -> Interval a -> Bool
containedIn (Interval (a0,a1)) (Interval (b0,b1))
  = ((b0 <= a0) && (a0 <= b1) && (b0 <= a1) && (a1 <= b1))

type TimeInterval = Interval LocalTime

conflictsWith :: TimeInterval -> TimeInterval -> Bool
conflictsWith = overlap
