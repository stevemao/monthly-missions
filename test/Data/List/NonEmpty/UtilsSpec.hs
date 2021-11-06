{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Data.List.NonEmpty.UtilsSpec (main, spec) where

import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Data.List.NonEmpty.Utils
import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "findValueIndex" $ do
    it "should not find value if empty list" $ property prop_emptyList
    it "may not find value if non empty list" $ property prop_nonEmptyList

prop_emptyList :: Fun Int Bool -> Bool
prop_emptyList f = isNothing (findValueIndex (applyFun f) [])

prop_nonEmptyList :: Fun Int Bool -> NonEmpty Int -> Bool
prop_nonEmptyList f as = case findValueIndex f' (NE.toList as) of
    Nothing              -> True
    Just (a, index, as') -> find f' as == Just a && index >= 0 && as' == as
    where f' = applyFun f
