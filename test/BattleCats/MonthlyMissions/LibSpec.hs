{-# LANGUAGE OverloadedStrings #-}
module BattleCats.MonthlyMissions.LibSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           BattleCats.MonthlyMissions.Lib
import           BattleCats.MonthlyMissions.Types
import           Data.List.NonEmpty               (NonEmpty ((:|)))

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "findMinEnergy" $ do
    it "1 group" $ do
      let group = (
            ( Stage
                  ( Level "CotC Ch.2" )
                  ( StageName "The Big Bang" )
                  ( Energy 110 )
              , Enemy
                  ( HpSpawn "100%" )
                  ( FirstSpawn 300 )
                  ( Target "Shibalien Elite" )
                  ( EnemyCode 360 ) :| []
              ) :|
            []
            ) :|
                [
                    ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "The Big Bang" )
                            ( Energy 110 )
                      , Enemy
                          ( HpSpawn "50%" )
                          ( FirstSpawn 0 )
                          ( Target "Star Peng" )
                          ( EnemyCode 361 ) :| []
                      ) :|
                    []
                ,
                    ( Stage
                        ( Level "CotC Ch.2" )
                        ( StageName "The Big Bang" )
                        ( Energy 110 )
                    , Enemy
                        ( HpSpawn "100%" )
                        ( FirstSpawn 200 )
                        ( Target "Cat God" )
                        ( EnemyCode 419 ) :| []
                    ) :| []
                ]

      let expected = (Stage (Level "CotC Ch.2") (StageName "The Big Bang") (Energy 110),Enemy (HpSpawn "100%") (FirstSpawn 300) (Target "Shibalien Elite") ( EnemyCode 360 ) :| [
                  Enemy (HpSpawn "50%") (FirstSpawn 0) (Target "Star Peng") ( EnemyCode 361 )
                , Enemy (HpSpawn "100%") (FirstSpawn 200) (Target "Cat God") ( EnemyCode 419 )
              ]) :| []

      let (MinEnergyStages stages) = findMinEnergy group

      stages `shouldBe` expected

    it "a few groups" $ do
      let group = (
            ( Stage
                ( Level "CotC Ch.2" )
                ( StageName "Jupiter" )
                ( Energy 62 )
            , Enemy
                ( HpSpawn "99%" )
                ( FirstSpawn 0 )
                ( Target "Shibalien Elite" )
                ( EnemyCode 360 ) :| []
            ) :|
            [
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "The Big Bang" )
                    ( Energy 110 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 300 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ]
            ) :|
                [
                    ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "The Big Bang" )
                            ( Energy 110 )
                      , Enemy
                          ( HpSpawn "50%" )
                          ( FirstSpawn 0 )
                          ( Target "Star Peng" )
                          ( EnemyCode 361 ) :| []
                      ) :|
                    []
                ,
                    ( Stage
                        ( Level "CotC Ch.2" )
                        ( StageName "The Big Bang" )
                        ( Energy 110 )
                    , Enemy
                        ( HpSpawn "100%" )
                        ( FirstSpawn 200 )
                        ( Target "Cat God" )
                        ( EnemyCode 419 ) :| []
                    ) :| []
                ]

      let expected = (Stage (Level "CotC Ch.2") (StageName "The Big Bang") (Energy 110),Enemy (HpSpawn "100%") (FirstSpawn 300) (Target "Shibalien Elite") ( EnemyCode 360 ) :| [
                  Enemy (HpSpawn "50%") (FirstSpawn 0) (Target "Star Peng") ( EnemyCode 361 )
                , Enemy (HpSpawn "100%") (FirstSpawn 200) (Target "Cat God") ( EnemyCode 419 )
              ]) :| []

      let (MinEnergyStages stages) = findMinEnergy group

      stages `shouldBe` expected

    it "real world groups" $ do
      let group = (
            ( Stage
                ( Level "CotC Ch.2" )
                ( StageName "Mars" )
                ( Energy 60 )
            , Enemy
                ( HpSpawn "100%" )
                ( FirstSpawn 400 )
                ( Target "Shibalien Elite" )
                ( EnemyCode 360 ) :| []
            ) :|
            [
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Jupiter" )
                    ( Energy 62 )
                , Enemy
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Venus" )
                    ( Energy 64 )
                , Enemy
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Mercury" )
                    ( Energy 64 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 500 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Uranus" )
                    ( Energy 62 )
                , Enemy
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Triton" )
                    ( Energy 65 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 900 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Red Rectangle" )
                    ( Energy 67 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 2 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Calabash Nebula" )
                    ( Energy 74 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 1900 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Sirius" )
                    ( Energy 78 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 1500 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Aldebaran" )
                    ( Energy 83 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 500 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Betelgeuse" )
                    ( Energy 81 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 2000 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Sighter's Star" )
                    ( Energy 82 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 1000 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Corona" )
                    ( Energy 83 )
                , Enemy
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Darararah" )
                    ( Energy 85 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 2 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Thanxbye" )
                    ( Energy 82 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 1200 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Aguham" )
                    ( Energy 85 )
                , Enemy
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "Andromeda" )
                    ( Energy 95 )
                , Enemy
                    ( HpSpawn "99%" )
                    ( FirstSpawn 2 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ,
                ( Stage
                    ( Level "CotC Ch.2" )
                    ( StageName "The Big Bang" )
                    ( Energy 110 )
                , Enemy
                    ( HpSpawn "100%" )
                    ( FirstSpawn 300 )
                    ( Target "Shibalien Elite" )
                    ( EnemyCode 360 ) :| []
                )
            ]
            ) :|
                [
                    ( Stage
                        ( Level "CotC Ch.2" )
                        ( StageName "Mars" )
                        ( Energy 60 )
                    , Enemy
                        ( HpSpawn "100%" )
                        ( FirstSpawn 800 )
                        ( Target "Star Peng" )
                         ( EnemyCode 361 ) :| []
                    ) :|
                    [
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Venus" )
                            ( Energy 64 )
                        , Enemy
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Mercury" )
                            ( Energy 64 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1100 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Red Rectangle" )
                            ( Energy 67 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1100 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Eskimo Nebula" )
                            ( Energy 70 )
                        , Enemy
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Catseye Nebula" )
                            ( Energy 72 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1700 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Egg Nebula" )
                            ( Energy 75 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 2400 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Altair" )
                            ( Energy 80 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 900 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Canopus" )
                            ( Energy 82 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 600 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Aldebaran" )
                            ( Energy 83 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1600 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Cosmic Lounge" )
                            ( Energy 90 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1300 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Corona" )
                            ( Energy 83 )
                        , Enemy
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Thanxbye" )
                            ( Energy 82 )
                        , Enemy
                            ( HpSpawn "99%" )
                            ( FirstSpawn 2 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Mertoz" )
                            ( Energy 80 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 500 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Aguham" )
                            ( Energy 85 )
                        , Enemy
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Skelling" )
                            ( Energy 90 )
                        , Enemy
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "Black Hole" )
                            ( Energy 105 )
                        , Enemy
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1000 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ,
                        ( Stage
                            ( Level "CotC Ch.2" )
                            ( StageName "The Big Bang" )
                            ( Energy 110 )
                        , Enemy
                            ( HpSpawn "50%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" )
                            ( EnemyCode 361 ) :| []
                        )
                    ]
                ,
                    ( Stage
                        ( Level "CotC Ch.2" )
                        ( StageName "The Big Bang" )
                        ( Energy 110 )
                    , Enemy
                        ( HpSpawn "100%" )
                        ( FirstSpawn 200 )
                        ( Target "Cat God" )
                        ( EnemyCode 419 ) :| []
                    ) :| []
                ]

      let expected = (Stage (Level "CotC Ch.2") (StageName "The Big Bang") (Energy 110),Enemy (HpSpawn "100%") (FirstSpawn 300) (Target "Shibalien Elite") ( EnemyCode 360 ) :| [
                  Enemy (HpSpawn "50%") (FirstSpawn 0) (Target "Star Peng") ( EnemyCode 361 )
                , Enemy (HpSpawn "100%") (FirstSpawn 200) (Target "Cat God") ( EnemyCode 419 )
              ]) :| []

      let (MinEnergyStages stages) = findMinEnergy group

      stages `shouldBe` expected
    -- it "is idempotent" $ property $
    --   \str -> strip str === strip (strip str)
