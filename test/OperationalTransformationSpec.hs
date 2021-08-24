module OperationalTransformationSpec (spec) where

import Data.Change
import Data.Range
import Data.Text
import OperationalTransformation.Internal
import Test.Hspec
import Test.Hspec (describe)

fromLeft' :: Either a b -> a
fromLeft' (Left a) = a
fromLeft' (Right b) = error "got right"

fromRight' :: Show a => Either a b -> b
fromRight' (Right b) = b
fromRight' (Left a) = error $ "got left: " ++ show a

checkApply :: HasCallStack => [Operation] -> Text -> Text -> Expectation
checkApply ops t t' = apply t os `shouldBe` t'
  where
    os = fromList ops

checkNormalize :: HasCallStack => [Operation] -> [Operation] -> Expectation
checkNormalize ops ops' = fromList ops `shouldBe` fromList ops'

checkInvert :: HasCallStack => [Operation] -> [Operation] -> Text -> Expectation
checkInvert ops ops' t = do
  let ops'' = fromList ops
  let ops''' = fromList ops'
  let inverted = invert t ops''
  inverted `shouldBe` ops'''
  apply (apply t ops'') ops''' `shouldBe` t
  return ()

checkCompose :: HasCallStack => [Operation] -> [Operation] -> [Operation] -> Text -> Text -> Expectation
checkCompose ops ops' expectedComposed t t' = do
  let ops'' = fromList ops
  let ops''' = fromList ops'
  let composed = compose ops'' ops'''
  let expectedComposed' = fromList expectedComposed
  composed `shouldBe` expectedComposed'
  apply (apply t ops'') ops''' `shouldBe` apply t composed
  apply t composed `shouldBe` t'

checkFromChanges :: HasCallStack => Text -> [Change] -> [Operation] -> Expectation
checkFromChanges t cs ops = do
  let os = fromList ops
  fromChanges t cs `shouldBe` os

spec :: Spec
spec = parallel $ do
  describe "normalize" $ do
    it "should work" $ do
      checkNormalize [] []
      checkNormalize [Insert "hello"] [Insert "hello"]
      checkNormalize [Insert "hello", Delete 4] [Insert "hello", Delete 4]
      checkNormalize [Delete 4, Insert "hello"] [Insert "hello", Delete 4]
      checkNormalize [Delete 4, Delete 5] [Delete 9]
      checkNormalize [Insert "h", Retain 1, Retain 2] [Insert "h", Retain 3]
      checkNormalize [Insert "broh", Delete 5, Insert "yoo", Insert "another"] [Insert "brohyooanother", Delete 5]

  it "apply" $ do
    checkApply [Insert "hello", Retain 5] " broh" "hello broh"
    checkApply [Delete 2, Insert "wow", Retain 1, Insert "yo"] "wow" "wowwyo"
    checkApply [Delete 2, Insert "yeahboi", Retain 2] "ah  " "yeahboi  "
    checkApply [Insert "hello", Retain 5] "hello" "hellohello"
    checkApply [Insert "broh", Delete 5] "hello" "broh"

  it "invert" $ do
    checkInvert [Insert "hello"] [Delete 5] ""
    checkInvert [Delete 3, Retain 4, Delete 2] [Insert "wow", Retain 4, Insert "ye"] "wow    ye"
    checkInvert [Retain 4] [Retain 4] "broh"

  it "compose" $ do
    -- cannot do inserts on the left first that will make the retain on right wrong
    checkCompose [Retain 4, Insert "he"] [Retain 6] [Retain 4, Insert "he"] "____" "____he"
    checkCompose [Insert "hello"] [Retain 5] [Insert "hello"] "" "hello"
    checkCompose [Retain 2, Insert "hello"] [Insert "another ", Retain 7] [Insert "another ", Retain 2, Insert "hello"] "br" "another brhello"
    checkCompose [Insert "hello"] [Insert "another ", Retain 5] [Insert "another hello"] "" "another hello"
    checkCompose [Delete 2, Retain 4] [Delete 3, Retain 1] [Delete 5, Retain 1] "we____" "_"
    return @IO ()

  describe "from changes" $ do
    it "it should delete from middle" $ do
      checkFromChanges "hello broh" [Change (range 2 5) ""] [Retain 2, Delete 3, Retain 5]

    it "should delete from start" $ do
      checkFromChanges "asdfasdf" [Change (range 0 4) ""] [Delete 4, Retain 4]

    it "should insert from beginning" $ do
      checkFromChanges "asdf" [Change (range 0 0) "hello "] [Insert "hello ", Retain 4]

    it "should replace" $ do
      checkFromChanges "asdf" [Change (range 0 2) "broh"] [Insert "broh", Delete 2, Retain 2]

  it "transform" $ do
    return @IO ()

  it "smoke" $ do
    return @IO ()