{-# LANGUAGE TemplateHaskell #-}

module OperationalTransformation.Internal where

import Control.Monad
import Data.Change
import Data.Either.Combinators
import Data.Foldable (foldl')
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Range (Range (..))
import qualified Data.Range as Range
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Prelude hiding (last)

-- | The core operation that we use
data Operation
  = Insert !Text
  | Delete !Int
  | Retain !Int
  deriving (Show, Eq)

data SpannedOperation
  = SpannedInsert !Int !Text
  | SpannedDelete !Range
  deriving (Show, Eq)

-- | A sequence of operations
-- | Adding operations to the sequence will potentially merge Operations for optimization purposes
data OperationSeq = OperationSeq
  { _opSeq :: !(Seq Operation),
    _len :: !Int,
    _lenAfter :: !Int
  }
  deriving (Show, Eq)

makeLenses ''OperationSeq

fromList :: [Operation] -> OperationSeq
fromList = foldl' (flip add) empty

fromChanges :: Text -> [Change] -> Maybe OperationSeq
fromChanges t changes =
  if disjoint
    then Just $ fromChangesUnchecked t changesSorted
    else Nothing
  where
    changesSorted = L.sort changes

    disjoint =
      zip
        changesSorted
        (drop 1 changesSorted)
        & all
          ( \(Change (RangeV _ end) _, Change (RangeV start _) _) ->
              end <= start
          )

fromChanges' :: Text -> [Change] -> OperationSeq
fromChanges' t = fromJust <$> fromChanges t

fromChangesUnchecked :: Text -> [Change] -> OperationSeq
fromChangesUnchecked t changes = go changes 0 empty
  where
    go [] last os = addRetain (T.length t - last) os
    go (Change r@(RangeV start end) t' : cs) last os =
      let l = Range.length r
          os' = addRetain (start - last) os
       in go cs end $
            if T.null t'
              then addDelete l os'
              else addDelete l $ addInsert t' os'

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
        -- don't create to operations when they are already at the end, merge them
        xs :|> Insert t' -> xs :|> Insert (t' <> t)
        -- make sure inserts always go before deletes if they are directly consecutive
        -- also optimize if there is an insert already before delete
        xs :|> Insert t' :|> del@(Delete _) -> xs :|> Insert (t' <> t) :|> del
        xs :|> del@(Delete _) -> xs :|> Insert t :|> del
        -- finally add to the back if there are no optimizations
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

data ApplyError
  = TextLenMismatch Int Int

instance Show ApplyError where
  show (TextLenMismatch tl l) =
    show $
      "Text length "
        ++ show tl
        ++ " did not match expected length "
        ++ show l
        ++ " of operation"

apply :: Text -> OperationSeq -> Either ApplyError Text
apply input OperationSeq {_len}
  | T.length input /= _len = Left $ TextLenMismatch (T.length input) _len
apply input OperationSeq {_opSeq} = Right $ go input _opSeq ""
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

apply' :: Text -> OperationSeq -> Text
apply' t = fromRight' <$> apply t

invert :: Text -> OperationSeq -> OperationSeq
invert oldInput OperationSeq {_opSeq} = go oldInput _opSeq empty
  where
    go _ Empty osInverse = osInverse
    go t (op :<| ops) osInverse =
      case op of
        Retain n -> go (T.drop n t) ops (addRetain n osInverse)
        Insert t' -> go t ops $ addDelete (T.length t') osInverse
        Delete n -> go (T.drop n t) ops $ addInsert (T.take n t) osInverse

-- | Composing two operation sequences gives a new sequence that preserves the changes of both.
-- | For each input string S and a pair of consequtive sequences A and B
-- | apply(apply(S, A), B) = apply(S, compose(A, B))
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
    go Empty (Insert t :<| bs) xs = go Empty bs (addInsert t xs)
    go aa@(a :<| as) bb@(b :<| bs) xs =
      case (a, b) of
        -- these are the free cases
        -- Why?
        -- Because adding these directly to the OperationSequence will not affect other operations
        -- Obviously we would not want to directly add Retains when we see them, as the point of them is to merge Retains
        -- But what about adding Insert and Delete on the opposite side?
        -- What happens if we add Insert first on the left
        -- For example with this pattern (Insert t, _)
        -- When composing [Insert "hello"] [Retain 5], we will incorrectly get [Insert "hello", Retain 5]
        -- because adding the Insert on the left affects the Retain, we would not want to add it
        -- so we want to handle this case separately in a later pattern match
        -- Similarly when composing [Insert "hello"] [Delete 5] we would expect to get []
        -- because we have just deleted what we have inserted
        -- Instead we get [Insert "hello", Delete 5], which is not what we want
        -- With the pattern (_, Delete n) we have similar issues
        -- When composing [Insert "hello"] [Delete 5] we would expect to get []
        -- Instead we get [Insert "hello", Delete 5]
        -- Thus, you can see that inserting stuff other than these exact operations on the correct side will lead to erroneous compositions
        -- The rest of the cases in the compose function are for dealing with when we don't have these exact operations
        -- In this exact order. Each case will have to be delt with manually by comparing how much is inserted, retained, or deleted
        (Delete n, _) -> go as bb (addDelete n xs)
        (_, Insert t) -> go aa bs (addInsert t xs)
        --
        -- (Insert t, _) -> go as bb (addInsert t xs)
        -- (Insert t, _) -> go as bb (addInsert t xs)
        -- (_, Delete n) -> go aa bs (addDelete n xs)

        -- Now here are the cases where we have to see both sides and decide how to compose them
        -- When both are retain, we add the less one to the sequence
        -- This is because retaining and then retaining again will not do anything for the overlapped part
        -- For composing example, [Retain 4] [Retain 4] would give as [Retain 4]
        -- This is the EQ case
        -- This is why if they are not exactly the same, we add the lesser part to the set, because that is what they overlap on
        -- [Retain 6] [Retain 4] would get Retain 4 added to the sequencce
        -- [Retain 1] [Retain 2] would get one added to the sequence
        -- These are the LT and GT cases
        -- however we have to put the difference as the next peeker
        -- Retain 2 and Retain 1 in the example above, respectively
        -- We have to add them to the peeker and not directly to the set because they can merge with other operations.
        -- For example composing [Retain 4, Insert "he"] and [Retain 6]
        -- we can see that the intersecting retain of the first two elements is Retain 4
        -- The extra Retain 2 will get added back to the peeker, and we get
        -- [Insert "he"] [Retain 2] which will add the Insert to the sequence
        (Retain n, Retain m) -> case compare n m of
          LT -> go as (Retain (m - n) :<| bs) (addRetain n xs)
          EQ -> go as bs (addRetain n xs)
          GT -> go (Retain (n - m) :<| as) bs (addRetain n xs)
        -- these are the cases when a retain is with a non retain operation
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
    go aa bb xs =
      error $
        "unreachable:\n"
          ++ show aa
          ++ "\n"
          ++ show bb
          ++ "\n"
          ++ show xs

transform :: OperationSeq -> OperationSeq -> (OperationSeq, OperationSeq)
transform os1 os2 = go (os1 ^. opSeq) (os2 ^. opSeq) empty empty
  where
    go Empty Empty xs ys = (xs, ys)
    go (Insert t :<| as) Empty xs ys = go as Empty (addRetain (T.length t) xs) (addInsert t ys)
    go Empty (Insert t :<| bs) xs ys = go Empty bs (addInsert t xs) (addRetain (T.length t) ys)
    go aa@(a :<| as) bb@(b :<| bs) xs ys = case (a, b) of
      (Insert t, _) -> go as bb (addInsert t xs) (addRetain (T.length t) ys)
      (_, Insert t) -> go aa bs (addRetain (T.length t) xs) (addInsert t ys)
      (Retain n, Retain m) -> case compare n m of
        LT -> go as (Retain (m - n) :<| bs) (addRetain n xs) (addRetain n ys)
        EQ -> go as bs (addRetain n xs) (addRetain n ys)
        GT -> go (Retain (n - m) :<| as) bs (addRetain m xs) (addRetain m ys)
      (Delete n, Delete m) -> case compare n m of
        LT -> go as (Delete (m - n) :<| bs) xs ys
        EQ -> go as bs xs ys
        GT -> go (Delete (n - m) :<| as) bs xs ys
      (Retain n, Delete m) -> case compare n m of
        LT -> go as (Delete (m - n) :<| bs) xs (addDelete n ys)
        EQ -> go as bs xs (addDelete m ys)
        GT -> go (Delete (n - m) :<| as) bs (addDelete m xs) ys
      (Delete n, Retain m) -> case compare n m of
        LT -> go as (Delete (m - n) :<| bs) (addDelete n xs) ys
        EQ -> go as bs (addDelete n xs) ys
        GT -> go (Delete (n - m) :<| as) bs (addDelete m xs) ys
    go _ _ _ _ = error "unreachable"