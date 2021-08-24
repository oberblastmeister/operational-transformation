module Data.Range
  ( Range (RangeV),
    range,
    start,
    end,
    length,
  )
where

import Prelude hiding (length)

data Range = Range !Int !Int
  deriving (Show, Eq, Ord)

range :: Int -> Int -> Range
range x y
  | x > y =
    error $
      "Data.Range: start "
        ++ show x
        ++ " is greater than end "
        ++ show y
  | otherwise = Range x y

start, end :: Range -> Int
start (Range x _) = x
end (Range _ y) = y

length :: Range -> Int
length (Range x y) = y - x

{-# COMPLETE RangeV #-}

pattern RangeV :: Int -> Int -> Range
pattern RangeV x y <- Range x y