{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BattleCats.MonthlyMissions.Lib where

import           BattleCats.MonthlyMissions.Types
import           Control.Exception
import           Data.List
import qualified Data.Text                        as T
import           Database.SQLite.Simple

getCode :: EnemyUnitsTSV -> Target -> IO EnemyCode
getCode (EnemyUnitsTSV enemyunits) t@(Target target) = do
  let maybeIdx = subtract 1 <$> findIndex (target `T.isInfixOf`) (T.lines enemyunits)

  maybe (throwIO (error $ "could not find " <> show t :: SomeException)) (return . EnemyCode) maybeIdx

getStages :: Connection -> EnemyUnitsTSV -> Mission -> IO [Stage]
getStages conn enemyunits m@(Mission location target) = do
    EnemyCode enemycode <- getCode enemyunits target
    stages <- case location of
      LocationLevel level -> do
                            -- This is a hack for inaccurate EoC Stages
                            let (level', energyAdjustment, excludedStages :: [T.Text]) = case level of
                                                          "EoC Ch.1" -> ("EoC", 0, ["Moon Ch.2", "Moon Ch.3"])
                                                          "EoC Ch.2" -> ("EoC", 10, ["Moon Ch.1", "Moon Ch.3"])
                                                          "EoC Ch.3" -> ("EoC", 20, ["Moon Ch.1", "Moon Ch.2"])
                                                          l          -> (l, 0, [])

                            let excludedStagesIndex = [0..length excludedStages]
                            let excludedStagesWithIndex = zip excludedStages excludedStagesIndex

                            let extraQuery = foldr (\i acc -> " AND stage != :excludedStage" <> (Query . T.pack . show $ i) <> acc) "" excludedStagesIndex

                            let extraParam = (\(s, i) -> (":excludedStage" <> (T.pack . show $ i)) := s) <$> excludedStagesWithIndex

                            stages <- queryNamed conn ("SELECT level, stage, energy from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND level = :level" <> extraQuery) ([":enemycode" := enemycode, ":level" := level'] <> extraParam)

                            return $ (\(Stage _ n e) -> Stage level n (e + energyAdjustment)) <$> stages
      LocationCategory category -> do
                            queryNamed conn "SELECT level, stage, energy from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND category = :category" [":enemycode" := enemycode, ":category" := category]
    if null stages
    then throwIO (error $ "could not find " <> show m :: SomeException)
    else return $ nub stages

findMinEnergy :: [[Stage]] -> Stages
findMinEnergy stagess = minimum $ Stages <$> nub <$> sequence stagess
