{-# LANGUAGE OverloadedStrings #-}
module BattleCats.MonthlyMissions.LibSpec (main, spec) where

import           BattleCats.MonthlyMissions.Lib
import           BattleCats.MonthlyMissions.Types
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

enemyWithDefault :: HpSpawn -> FirstSpawn -> Target -> Enemy
enemyWithDefault hpSpawn firstSpawn target = Enemy hpSpawn firstSpawn target (EnemyCode 360) (IsBoss 0)

stageWithDefault :: Level -> StageName -> Energy -> Stage
stageWithDefault l s e = Stage (Category "Story Mode") l s e (Schedule Nothing)

spec :: Spec
spec = do
  describe "findMinEnergy" $ do
    it "1 group" $ do
      let group = (
            ( stageWithDefault
                  ( Level "CotC Ch.2" )
                  ( StageName "The Big Bang" )
                  ( Energy 110 )
              , enemyWithDefault
                  ( HpSpawn "100%" )
                  ( FirstSpawn 300 )
                  ( Target "Shibalien Elite" ) :| []
              ) :|
            []
            ) :|
                [
                    ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "The Big Bang" )
                            ( Energy 110 )
                      , enemyWithDefault
                          ( HpSpawn "50%" )
                          ( FirstSpawn 0 )
                          ( Target "Star Peng" ) :| []
                      ) :|
                    []
                ,
                    ( stageWithDefault
                        ( Level "CotC Ch.2" )
                        ( StageName "The Big Bang" )
                        ( Energy 110 )
                    , enemyWithDefault
                        ( HpSpawn "100%" )
                        ( FirstSpawn 200 )
                        ( Target "Cat God" ) :| []
                    ) :| []
                ]

      let expected = (stageWithDefault (Level "CotC Ch.2") (StageName "The Big Bang") (Energy 110), enemyWithDefault (HpSpawn "100%") (FirstSpawn 300) (Target "Shibalien Elite") :| [
                  enemyWithDefault (HpSpawn "50%") (FirstSpawn 0) (Target "Star Peng")
                , enemyWithDefault (HpSpawn "100%") (FirstSpawn 200) (Target "Cat God")
              ]) :| []

      let (MinEnergyStages stages) = findMinEnergy group

      stages `shouldBe` expected

    it "a few groups" $ do
      let group = (
            ( stageWithDefault
                ( Level "CotC Ch.2" )
                ( StageName "Jupiter" )
                ( Energy 62 )
            , enemyWithDefault
                ( HpSpawn "99%" )
                ( FirstSpawn 0 )
                ( Target "Shibalien Elite" ) :| []
            ) :|
            [
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "The Big Bang" )
                    ( Energy 110 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 300 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ]
            ) :|
                [
                    ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "The Big Bang" )
                            ( Energy 110 )
                      , enemyWithDefault
                          ( HpSpawn "50%" )
                          ( FirstSpawn 0 )
                          ( Target "Star Peng" ) :| []
                      ) :|
                    []
                ,
                    ( stageWithDefault
                        ( Level "CotC Ch.2" )
                        ( StageName "The Big Bang" )
                        ( Energy 110 )
                    , enemyWithDefault
                        ( HpSpawn "100%" )
                        ( FirstSpawn 200 )
                        ( Target "Cat God" ) :| []
                    ) :| []
                ]

      let expected = (stageWithDefault (Level "CotC Ch.2") (StageName "The Big Bang") (Energy 110), enemyWithDefault (HpSpawn "100%") (FirstSpawn 300) (Target "Shibalien Elite") :| [
                  enemyWithDefault (HpSpawn "50%") (FirstSpawn 0) (Target "Star Peng")
                , enemyWithDefault (HpSpawn "100%") (FirstSpawn 200) (Target "Cat God")
              ]) :| []

      let (MinEnergyStages stages) = findMinEnergy group

      stages `shouldBe` expected

    it "real world groups" $ do
      let group = (
            ( stageWithDefault
                ( Level "CotC Ch.2" )
                ( StageName "Mars" )
                ( Energy 60 )
            , enemyWithDefault
                ( HpSpawn "100%" )
                ( FirstSpawn 400 )
                ( Target "Shibalien Elite" ) :| []
            ) :|
            [
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Jupiter" )
                    ( Energy 62 )
                , enemyWithDefault
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Venus" )
                    ( Energy 64 )
                , enemyWithDefault
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Mercury" )
                    ( Energy 64 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 500 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Uranus" )
                    ( Energy 62 )
                , enemyWithDefault
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Triton" )
                    ( Energy 65 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 900 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Red Rectangle" )
                    ( Energy 67 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 2 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Calabash Nebula" )
                    ( Energy 74 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 1900 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Sirius" )
                    ( Energy 78 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 1500 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Aldebaran" )
                    ( Energy 83 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 500 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Betelgeuse" )
                    ( Energy 81 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 2000 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Sighter's Star" )
                    ( Energy 82 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 1000 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Corona" )
                    ( Energy 83 )
                , enemyWithDefault
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Darararah" )
                    ( Energy 85 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 2 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Thanxbye" )
                    ( Energy 82 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 1200 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Aguham" )
                    ( Energy 85 )
                , enemyWithDefault
                    ( HpSpawn "99%" )
                    ( FirstSpawn 0 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "Andromeda" )
                    ( Energy 95 )
                , enemyWithDefault
                    ( HpSpawn "99%" )
                    ( FirstSpawn 2 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ,
                ( stageWithDefault
                    ( Level "CotC Ch.2" )
                    ( StageName "The Big Bang" )
                    ( Energy 110 )
                , enemyWithDefault
                    ( HpSpawn "100%" )
                    ( FirstSpawn 300 )
                    ( Target "Shibalien Elite" ) :| []
                )
            ]
            ) :|
                [
                    ( stageWithDefault
                        ( Level "CotC Ch.2" )
                        ( StageName "Mars" )
                        ( Energy 60 )
                    , enemyWithDefault
                        ( HpSpawn "100%" )
                        ( FirstSpawn 800 )
                        ( Target "Star Peng" ) :| []
                    ) :|
                    [
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Venus" )
                            ( Energy 64 )
                        , enemyWithDefault
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Mercury" )
                            ( Energy 64 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1100 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Red Rectangle" )
                            ( Energy 67 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1100 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Eskimo Nebula" )
                            ( Energy 70 )
                        , enemyWithDefault
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Catseye Nebula" )
                            ( Energy 72 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1700 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Egg Nebula" )
                            ( Energy 75 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 2400 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Altair" )
                            ( Energy 80 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 900 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Canopus" )
                            ( Energy 82 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 600 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Aldebaran" )
                            ( Energy 83 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1600 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Cosmic Lounge" )
                            ( Energy 90 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1300 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Corona" )
                            ( Energy 83 )
                        , enemyWithDefault
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Thanxbye" )
                            ( Energy 82 )
                        , enemyWithDefault
                            ( HpSpawn "99%" )
                            ( FirstSpawn 2 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Mertoz" )
                            ( Energy 80 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 500 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Aguham" )
                            ( Energy 85 )
                        , enemyWithDefault
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Skelling" )
                            ( Energy 90 )
                        , enemyWithDefault
                            ( HpSpawn "99%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "Black Hole" )
                            ( Energy 105 )
                        , enemyWithDefault
                            ( HpSpawn "100%" )
                            ( FirstSpawn 1000 )
                            ( Target "Star Peng" ) :| []
                        )
                    ,
                        ( stageWithDefault
                            ( Level "CotC Ch.2" )
                            ( StageName "The Big Bang" )
                            ( Energy 110 )
                        , enemyWithDefault
                            ( HpSpawn "50%" )
                            ( FirstSpawn 0 )
                            ( Target "Star Peng" ) :| []
                        )
                    ]
                ,
                    ( stageWithDefault
                        ( Level "CotC Ch.2" )
                        ( StageName "The Big Bang" )
                        ( Energy 110 )
                    , enemyWithDefault
                        ( HpSpawn "100%" )
                        ( FirstSpawn 200 )
                        ( Target "Cat God" ) :| []
                    ) :| []
                ]

      let expected = (stageWithDefault (Level "CotC Ch.2") (StageName "The Big Bang") (Energy 110), enemyWithDefault (HpSpawn "100%") (FirstSpawn 300) (Target "Shibalien Elite") :| [
                  enemyWithDefault (HpSpawn "50%") (FirstSpawn 0) (Target "Star Peng")
                , enemyWithDefault (HpSpawn "100%") (FirstSpawn 200) (Target "Cat God")
              ]) :| []

      let (MinEnergyStages stages) = findMinEnergy group

      stages `shouldBe` expected
