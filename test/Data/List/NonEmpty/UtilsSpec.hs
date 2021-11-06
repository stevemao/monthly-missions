{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Data.List.NonEmpty.UtilsSpec (main, spec) where

import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NE
import           Data.List.NonEmpty.Utils
import           Data.Maybe
import           Safe
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
  describe "replace" $ do
    it "should replace at positive or zero index" $ property prop_positive_replace
    it "should do nothing if negative index" $ property prop_negative_replace
  describe "upsertList" $ do
    it "should upsert" $ property prop_upsertList
  describe "foldr2NonEmpty" $ do
    it "should foldr2NonEmpty" $ property prop_foldr2NonEmpty

prop_emptyList :: Fun Integer Bool -> Bool
prop_emptyList f = isNothing (findValueIndex (applyFun f) [])

prop_nonEmptyList :: Fun Integer Bool -> NonEmpty Integer -> Bool
prop_nonEmptyList f as = case findValueIndex f' (NE.toList as) of
    Nothing              -> isNothing (find f' as)
    Just (a, index, as') -> find f' as == Just a && index >= 0 && as' == as
    where f' = applyFun f

prop_positive_replace :: NonNegative Int -> Integer -> NonEmpty Integer -> Bool
prop_positive_replace index a as = length as == length as' && if index' >= length as
                          then as == as'
                          else atMay (NE.toList as') index' == Just a
    where as' :: NonEmpty Integer
          as' = replace index' a as
          index' = getNonNegative index

prop_negative_replace :: Negative Int -> Integer -> NonEmpty Integer -> Bool
prop_negative_replace index a as = length as == length as' && as == as'
    where as' :: NonEmpty Integer
          as' = replace index' a as
          index' = getNegative index

prop_upsertList :: Fun (Maybe Integer) Integer -> Int -> [(Int, Integer)] -> Bool
prop_upsertList f k m = case lookup k m of
            Nothing -> (k, f' Nothing) `elem` m'
            Just v  -> length m' == length m && elem (k, f' (Just v)) m'
            where f' = applyFun f
                  m' = upsertList f' k m

prop_foldr2NonEmpty :: Fun (Integer, [Integer]) (NonEmpty Integer) -> NonEmpty Integer -> Bool
prop_foldr2NonEmpty f as = NE.toList (foldr2NonEmpty f' as) == foldr f'' [] (NE.toList as)
            where f' = applyFun2 f
                  f'' a b = NE.toList $ f' a b
