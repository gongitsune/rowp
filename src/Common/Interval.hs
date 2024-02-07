{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Common.Interval
  ( Interval (..),
    contains,
    surrounds,
    empty,
    universe,
  )
where

data Interval a = (Ord a) =>
  Interval
  { lower :: a,
    upper :: a
  }

contains :: (Ord a) => Interval a -> a -> Bool
contains i x = i.lower <= x && x <= i.upper

surrounds :: (Ord a) => Interval a -> a -> Bool
surrounds i x = i.lower < x && x < i.upper

empty :: (Fractional a, Ord a) => Interval a
empty = Interval (1 / 0) ((-1) / 0)

universe :: (Fractional a, Ord a) => Interval a
universe = Interval ((-1) / 0) (1 / 0)
