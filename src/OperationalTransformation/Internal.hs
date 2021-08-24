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
apply input OperationSeq {_opSeq} = go input _opSeq ""
  where
    go it Empty ot | T.null it = ot
    go it (op :<| ops) ot =
      case op of
        Retain n ->
          let (before, after) = T.splitAt n it
           in go after ops (ot <> before)
        Insert t -> go it ops (ot <> t)
        Delete n -> go (T.drop n it) ops ot
    go _ _ _ = error "unreachable"

invert :: Text -> OperationSeq -> OperationSeq
invert oldInput OperationSeq {_opSeq} = go oldInput _opSeq empty
  where
    go _ Empty osInverse = osInverse
    go t (op :<| ops) osInverse =
      case op of
        Retain n -> go (T.drop n t) ops (addRetain n osInverse)
        Insert t' -> go t ops $ addDelete (T.length t') osInverse
        Delete n -> go (T.drop n t) ops $ addInsert (T.take n t) osInverse

compose :: OperationSeq -> OperationSeq -> OperationSeq
compose OperationSeq {_lenAfter} OperationSeq {_len}
  | _len /= _lenAfter =
    error $
      "The lenAfter of the first was not equal to len of second: "
        ++ show _lenAfter
        ++ " "
        ++ show _len
compose os1 os2 = go (os1 ^. opSeq) (os2 ^. opSeq) empty
  where
    go Empty Empty xs = xs
    go (Delete d :<| as) Empty xs = go as Empty (addDelete d xs)
    go Empty (Insert i :<| bs) xs = go Empty bs (addInsert i xs)
    go aa@(a :<| as) bb@(b :<| bs) xs =
      case (a, b) of
        (Delete n, _) -> go as bb (addDelete n xs)
        (_, Insert n) -> go aa bs (addInsert n xs)
        (Retain n, Retain m) -> case compare n m of
          LT -> go as (Retain (m - n) :<| bs) (addRetain n xs)
          EQ -> go as bs (addRetain n xs)
          GT -> go (Retain (n - m) :<| as) bs (addRetain n xs)
        (Retain n, Delete m) -> case compare n m of
          LT -> go as (Delete (m - n) :<| bs) (addDelete n xs)
          EQ -> go as bs (addDelete m xs)
          GT -> go (Retain (n - m) :<| as) bs (addDelete m xs)
        (Insert t, Retain m) -> case compare (T.length t) m of
          LT -> go as (Retain (m - T.length t) :<| bs) (addInsert t xs)
          EQ -> go as bs (addInsert t xs)
          GT ->
            let (before, after) = T.splitAt m t
             in go (Insert after :<| as) bs (addInsert before xs)
        (Insert t, Delete m) -> case compare (T.length t) m of
          LT -> go as (Delete (m - T.length t) :<| bs) xs
          EQ -> go as bs xs
          GT -> go (Insert (T.drop m t) :<| as) bs xs
    go _ _ _ = error "unreachable"

transform :: OperationSeq -> OperationSeq -> OperationSeq
transform = undefined