{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BattleCats.MonthlyMissions.Lib where

import           BattleCats.MonthlyMissions.Types
import           Control.Exception
import           Data.List                        (findIndex)
import           Data.List.NonEmpty               (NonEmpty ((:|)), fromList,
                                                   groupBy1, nonEmpty)
import qualified Data.Map.Strict                  as M
import qualified Data.Text                        as T
import           Database.SQLite.Simple

getCode :: EnemyUnitsTSV -> Target -> IO EnemyCode
getCode (EnemyUnitsTSV enemyunits) t@(Target target) = do
  let maybeIdx = subtract 1 <$> findIndex (("\t" <> target <> "\t") `T.isInfixOf`) (T.lines enemyunits)

  maybe (throwIO (error $ "could not find " <> show t :: SomeException)) (return . EnemyCode) maybeIdx

getStages :: Connection -> EnemyUnitsTSV -> Mission -> IO (NonEmpty StageWithEnemy)
getStages conn enemyunits m@(Mission location target) = do
    EnemyCode enemycode <- getCode enemyunits target
    stages <- case location of
      LocationLevel level -> do
                            -- This is a hack for inaccurate EoC stages
                            let (level', energyAdjustment, excludedStages :: [T.Text]) = case level of
                                                          "EoC Ch.1" -> ("EoC", 0, ["Moon Ch.2", "Moon Ch.3"])
                                                          "EoC Ch.2" -> ("EoC", 10, ["Moon Ch.1", "Moon Ch.3"])
                                                          "EoC Ch.3" -> ("EoC", 20, ["Moon Ch.1", "Moon Ch.2"])
                                                          l          -> (l, 0, [])

                            let excludedStagesIndex = [0..length excludedStages - 1]
                            let excludedStagesWithIndex = zip excludedStages excludedStagesIndex

                            let extraQuery = foldr (\i acc -> " AND stage != :excludedStage" <> (Query . T.pack . show $ i) <> acc) "" excludedStagesIndex

                            let extraParam = (\(s, i) -> (":excludedStage" <> (T.pack . show $ i)) := s) <$> excludedStagesWithIndex

                            stages <- queryNamed conn ("SELECT level, stage, energy, u.hpspawn, u.firstspawn from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND level = :level" <> extraQuery) ([":enemycode" := enemycode, ":level" := level'] <> extraParam)

                            return $ (\(FromRowStage _ n e h f) -> FromRowStage level n (e + energyAdjustment) h f) <$> stages
      LocationCategory category -> do
                            queryNamed conn "SELECT level, stage, energy, u.hpspawn, u.firstspawn from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND category = :category" [":enemycode" := enemycode, ":category" := category]
    case nonEmpty stages of
        Nothing -> throwIO (error $ "could not find " <> show m :: SomeException)
        Just nonEmptyStages -> do
          let fromRowStage = groupBy1 (\(FromRowStage levelA nameA energyA _ _) (FromRowStage levelB nameB energyB _ _) -> Stage levelA nameA energyA == Stage levelB nameB energyB) nonEmptyStages

          return $ findFastestEnemy target <$> fromRowStage

findFastestEnemy :: Target -> NonEmpty FromRowStage -> StageWithEnemy
findFastestEnemy t s@(FromRowStage level name energy _ _ :| _) = (Stage level name energy, fastestEnemies)
    where fastestEnemies = (\(FastestEnemy e) -> e) <$> minimum (FastestEnemy <$> enemies) :| []
          enemies = (\(FromRowStage _ _ _ hpSpawn firstSpawn) -> Enemy hpSpawn firstSpawn t) <$> s

findMinEnergy :: NonEmpty (NonEmpty StageWithEnemy) -> MinEnergyStages
findMinEnergy stageEnemies = minimum stages
  where combinitions = sequence stageEnemies
        uniqCombinitions = foldr updateEnemies M.empty <$> combinitions
        -- TODO: find a way to use total functions
        stages = MinEnergyStages <$> fromList <$> M.toList <$> uniqCombinitions

updateEnemies :: StageWithEnemy -> StageEnemies -> StageEnemies
updateEnemies (stage, enemies) = M.alter (Just . combineEnemies enemies) stage

combineEnemies :: NonEmpty Enemy -> Maybe (NonEmpty Enemy) -> NonEmpty Enemy
combineEnemies newEnemies (Just enemies) = newEnemies <> enemies
combineEnemies newEnemies Nothing        = newEnemies
