{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BattleCats.MonthlyMissions.Lib where

import           BattleCats.MonthlyMissions.Types
import           Control.Exception
import           Data.List
import           Data.List.NonEmpty               (NonEmpty ((:|)), groupBy1,
                                                   nonEmpty)
import           Data.List.NonEmpty.Utils
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import           Database.SQLite.Simple
import           System.Directory

getCode :: EnemyUnitsTSV -> Target -> IO EnemyCode
getCode (EnemyUnitsTSV enemyunits) t@(Target target) = do
  let maybeIdx = subtract 1 <$> findIndex (("\t" <> target <> "\t") `T.isInfixOf`) (T.lines enemyunits)

  maybe (throwIO (errorWithoutStackTrace $ "could not find " <> show t :: SomeException)) (return . EnemyCode) maybeIdx

getStages :: Connection -> EnemyUnitsTSV -> Mission -> IO (NonEmpty StageWithEnemy)
getStages conn enemyunits m@(Mission location target) = do
    code@(EnemyCode enemycode) <- getCode enemyunits target
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

                            let extraParams = (\(s, i) -> (":excludedStage" <> (T.pack . show $ i)) := s) <$> excludedStagesWithIndex

                            stages <- queryNamed conn ("SELECT category, level, stage, energy, u.hpspawn, u.firstspawn, u.isboss from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND level = :level"
                                                    <> extraQuery)
                                                    ([":enemycode" := enemycode, ":level" := level'] <> extraParams)

                            return $ (\(FromRowStage c _ n e h f isBoss) -> FromRowStage c level n (e + energyAdjustment) h f isBoss) <$> stages
      LocationCategory category -> do
                            queryNamed conn "SELECT category, level, stage, energy, u.hpspawn, u.firstspawn, u.isboss from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND category = :category"
                                           [":enemycode" := enemycode, ":category" := category]
    case nonEmpty stages of
        Nothing -> throwIO (errorWithoutStackTrace $ "could not find " <> show m :: SomeException)
        Just nonEmptyStages -> do
          let sameStageRows = groupBy1
                              (\(FromRowStage categoryA levelA nameA energyA _ _ _) (FromRowStage categoryB levelB nameB energyB _ _ _) ->
                                        Stage categoryA levelA nameA energyA == Stage categoryB levelB nameB energyB)
                              nonEmptyStages

          return $ findFastestEnemy target code <$> sameStageRows

findFastestEnemy :: Target -> EnemyCode -> NonEmpty FromRowStage -> StageWithEnemy
findFastestEnemy t c s@(FromRowStage category level name energy _ _ _ :| _) = (Stage category level name energy, fastestEnemies)
    where fastestEnemies = (\(FastestEnemy e) -> e) <$> minimum (FastestEnemy <$> enemies) :| []
          enemies :: NonEmpty Enemy
          enemies = (\(FromRowStage _ _ _ _ hpSpawn firstSpawn isBoss) -> Enemy hpSpawn firstSpawn t c isBoss) <$> s

findMinEnergy :: NonEmpty (NonEmpty StageWithEnemy) -> MinEnergyStages
findMinEnergy stageEnemies = minimum stages
  where combinitions = sequence stageEnemies
        uniqCombinitions = loop updateEnemies <$> combinitions
        stages = MinEnergyStages <$> uniqCombinitions

updateEnemies :: StageWithEnemy -> [StageWithEnemy] -> NonEmpty StageWithEnemy
updateEnemies (stage, enemies) = upsertList (combineEnemies enemies) stage

combineEnemies :: NonEmpty Enemy -> Maybe (NonEmpty Enemy) -> NonEmpty Enemy
combineEnemies newEnemies (Just enemies) = newEnemies <> enemies
combineEnemies newEnemies Nothing        = newEnemies

getMinStages :: NonEmpty Mission -> IO MinEnergyStages
getMinStages missions = do
  enemyunitsPath <- getXdgDirectory XdgData "./monthly-missions/data/enemyunits.tsv"
  enemyunits <- TIO.readFile enemyunitsPath

  let eu = EnemyUnitsTSV enemyunits

  dbPath <- getXdgDirectory XdgData "./monthly-missions/data/stages10.2.db"
  conn <- open dbPath

  stagess <- traverse (getStages conn eu) missions

  close conn

  return $ findMinEnergy stagess

-- whereIsStage :: Connection -> Stage -> NonEmpty (NonEmpty Stage)
-- whereIsStage conn (Stage l s _) = queryNamed conn
--                   ("SELECT group_concat(stage, ',') FROM stages WHERE category = 'SoL' GROUP BY 'level'")
--                   [":category" := enemycode, ":category" := category]
