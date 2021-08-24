module Data.Change (Change (..)) where

import Data.Range
import Data.Text (Text)

data Change = Change !Range !Text
  deriving (Show, Eq)