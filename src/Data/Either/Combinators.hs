module Data.Either.Combinators
  ( fromLeft',
    fromRight',
  )
where

import GHC.Stack (HasCallStack)

fromLeft' :: (Show b, HasCallStack) => Either a b -> a
fromLeft' (Left a) = a
fromLeft' (Right b) = error $ "got right: " ++ show b

fromRight' :: (Show a, HasCallStack) => Either a b -> b
fromRight' (Right b) = b
fromRight' (Left a) = error $ "got left: " ++ show a