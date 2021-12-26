{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
module BattleCats.MonthlyMissions.TypesSpec (main, spec) where

import           BattleCats.MonthlyMissions.Types
import           Data.List.NonEmpty               (NonEmpty)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances        ()

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getEnergy" $ do
    it "sum all energies" $ property prop_getEnergy

deriving instance Arbitrary Energy

prop_getEnergy :: NonEmpty Energy -> Bool
prop_getEnergy energies = getEnergy stages == sum energies
    where stages = (\e -> Stage "test category" "test level" (StageName "test stage") e (Schedule Nothing)) <$> energies
