module OperationalTransformationSpec (spec) where

import Data.Text
import GHC.Stack.Types (HasCallStack)
import OperationalTransformation.Internal
import Test.Hspec

fromLeft' :: Either a b -> a
fromLeft' (Left a) = a
fromLeft' (Right b) = error "got right"

fromRight' :: Show a => Either a b -> b
fromRight' (Right b) = b
fromRight' (Left a) = error $ "got left: " ++ show a

checkApply :: HasCallStack => [Operation] -> Text -> Text -> Expectation
checkApply ops before after = fromRight' (apply before os) `shouldBe` after
  where
    os = fromList ops

checkNormalize :: HasCallStack => [Operation] -> [Operation] -> Expectation
checkNormalize ops ops' = fromList ops `shouldBe` fromList ops'

spec :: Spec
spec = parallel $ do
  it "normalize" $ do
    checkNormalize [] []
    checkNormalize [Insert "hello"] [Insert "hello"]
    checkNormalize [Insert "hello", Delete 4] [Insert "hello", Delete 4]
    checkNormalize [Delete 4, Insert "hello"] [Insert "hello", Delete 4]
    checkNormalize [Delete 4, Delete 5] [Delete 9]
    checkNormalize [Insert "h", Retain 1, Retain 2] [Insert "h", Retain 3]
    checkNormalize [Insert "broh", Delete 5, Insert "yoo", Insert "another"] [Insert "brohyooanother", Delete 5]
  -- checkNormalize []

  it "apply" $ do
    checkApply [Insert "hello", Retain 5] " broh" "hello broh"
    checkApply [Delete 2, Insert "wow", Retain 1, Insert "yo"] "wow" "wowwyo"

  it "smoke" $ do
    return @IO ()