{-# LANGUAGE TemplateHaskell #-}

module OperationalTransformation.Internal where

import Data.Foldable (foldl')
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.TH

data Operation
  = Insert !Text
  | Delete !Int
  | Retain !Int
  deriving (Show, Eq)

data OperationSeq = OperationSeq
  { _opSeq :: !(Seq Operation),
    _len :: !Int,
    _lenAfter :: !Int
  }
  deriving (Show, Eq)

makeLenses ''OperationSeq

fromList :: [Operation] -> OperationSeq
fromList = foldl' (flip add) empty

empty :: OperationSeq
empty = OperationSeq {_opSeq = Empty, _len = 0, _lenAfter = 0}

add :: Operation -> OperationSeq -> OperationSeq
add op os = case op of
  Insert t -> addInsert t os
  Delete n -> addDelete n os
  Retain n -> addRetain n os

addRetain :: Int -> OperationSeq -> OperationSeq
addRetain 0 os = os
addRetain n os =
  os
    & opSeq
      %~ \case
        xs :|> Retain m -> xs :|> Retain (n + m)
        xs -> xs :|> Retain n
    & len +~ n
    & lenAfter +~ n

addInsert :: Text -> OperationSeq -> OperationSeq
addInsert t os | T.null t = os
addInsert t os =
  os
    & opSeq
      %~ \case
        xs :|> Insert t' -> xs :|> Insert (t' <> t)
        xs :|> Insert t' :|> del@(Delete _) -> xs :|> Insert (t' <> t) :|> del
        xs :|> del@(Delete _) -> xs :|> Insert t :|> del
        xs -> xs :|> Insert t
    & lenAfter
    +~ T.length t

addDelete :: Int -> OperationSeq -> OperationSeq
addDelete 0 os = os
addDelete n os =
  os
    & opSeq
      %~ \case
        xs :|> Delete m -> xs :|> Delete (m + n)
        xs -> xs :|> Delete n
    & len +~ n

apply :: Text -> OperationSeq -> Text
apply input OperationSeq {_len}
  | T.length input /= _len =
    error $ "Text did not match baseLen of " ++ show _len
apply input operations = go input operations ""
  where
    go it OperationSeq {_opSeq = Empty} ot | T.null it = ot
    go it os@OperationSeq {_opSeq = op :<| ops} ot =
      let os' = os & opSeq .~ ops
       in case op of
            Retain n ->
              let (before, after) = T.splitAt n it
               in go after os' (ot <> before)
            Insert t -> go it os' (ot <> t)
            Delete n -> go (T.drop n it) os' ot
    go _ _ _ = error "unreachable"

invert :: Text -> OperationSeq -> OperationSeq
invert oldInput operationSeq = go oldInput operationSeq empty
  where
    go _ OperationSeq {_opSeq = Empty} osInverse = osInverse
    go t os@OperationSeq {_opSeq = op :<| ops} osInverse =
      let os' = os & opSeq .~ ops
       in case op of
            Retain n -> go (T.drop n t) os' (addRetain n osInverse)
            Insert t' -> go t os' $ addDelete (T.length t') osInverse
            Delete n -> go (T.drop n t) os' $ addInsert (T.take n t) osInverse