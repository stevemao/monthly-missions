{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
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
import           TextShow

eocHack :: Level -> (DBLevel, Energy, [T.Text])
eocHack level = case level of
  "EoC Ch.1" -> ("EoC", 0, ["Moon Ch.2", "Moon Ch.3"])
  "EoC Ch.2" -> ("EoC", 10, ["Moon Ch.1", "Moon Ch.3"])
  "EoC Ch.3" -> ("EoC", 20, ["Moon Ch.1", "Moon Ch.2"])
  l          -> (toDBLevel l, 0, [])

getCode :: EnemyUnitsTSV -> Target -> IO EnemyCode
getCode (EnemyUnitsTSV enemyunits) t@(Target target) = do
  let maybeIdx = subtract 1 <$> findIndex (("\t" <> target <> "\t") `T.isInfixOf`) (T.lines enemyunits)

  maybe (throwIO (errorWithoutStackTrace $ "could not find " <> show t :: SomeException)) (return . EnemyCode) maybeIdx

toDBLevel :: Level -> DBLevel
toDBLevel (Level l) = DBLevel l

toLevel :: DBLevel -> Level
toLevel (DBLevel l) = Level l

getStages :: Connection -> EnemyUnitsTSV -> Mission -> IO (NonEmpty StageWithEnemy)
getStages conn enemyunits m@(Mission location target) = do
    code@(EnemyCode enemycode) <- getCode enemyunits target
    stages <- case location of
      LocationLevel level -> do
                            let (level', energyAdjustment, excludedStages) = eocHack level

                            let excludedStagesIndex = [0..length excludedStages - 1]
                            let excludedStagesWithIndex = zip excludedStages excludedStagesIndex

                            let extraQuery = foldr (\i acc -> " AND stage != :excludedStage" <> (Query . showt $ i) <> acc) "" excludedStagesIndex

                            let extraParams = (\(s, i) -> (":excludedStage" <> showt i) := s) <$> excludedStagesWithIndex

                            stages <- queryNamed conn ("SELECT category, level, stage, energy, schedule, u.hpspawn, u.firstspawn, u.isboss from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND level = :level"
                                                    <> extraQuery)
                                                    ([":enemycode" := enemycode, ":level" := level'] <> extraParams)

                            return $ (\(FromRowStage c _ n e s h f isBoss) -> AdjustedFromRowStage c level n (e + energyAdjustment) s h f isBoss) <$> stages
      LocationCategory category -> do
                            stages <- queryNamed conn "SELECT category, level, stage, energy, schedule, u.hpspawn, u.firstspawn, u.isboss from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND category = :category"
                                           [":enemycode" := enemycode, ":category" := category]

                            return $ (\(FromRowStage c l n e s h f isBoss) -> AdjustedFromRowStage c (toLevel l) n e s h f isBoss) <$> stages
    case nonEmpty stages of
        Nothing -> throwIO (errorWithoutStackTrace $ "could not find " <> show m :: SomeException)
        Just nonEmptyStages -> do
          let sameStageRows = groupBy1
                              (\(AdjustedFromRowStage categoryA levelA nameA energyA scheduleA _ _ _) (AdjustedFromRowStage categoryB levelB nameB energyB scheduleB _ _ _) ->
                                        Stage categoryA levelA nameA energyA scheduleA == Stage categoryB levelB nameB energyB scheduleB)
                              nonEmptyStages

          return $ findFastestEnemy target code <$> sameStageRows

findFastestEnemy :: Target -> EnemyCode -> NonEmpty AdjustedFromRowStage -> StageWithEnemy
findFastestEnemy t c s@(AdjustedFromRowStage category level name energy schedule _ _ _ :| _) = (Stage category level name energy schedule, fastestEnemies)
    where fastestEnemies = (\(FastestEnemy e) -> e) <$> minimum (FastestEnemy <$> enemies) :| []
          enemies :: NonEmpty Enemy
          enemies = (\(AdjustedFromRowStage _ _ _ _ _ hpSpawn firstSpawn isBoss) -> Enemy hpSpawn firstSpawn t c isBoss) <$> s

findMinEnergy :: NonEmpty (NonEmpty StageWithEnemy) -> MinEnergyStages
findMinEnergy stageEnemies = minimum stages
  where combinitions = sequence stageEnemies
        uniqCombinitions = foldr2NonEmpty updateEnemies <$> combinitions
        stages = MinEnergyStages <$> uniqCombinitions

updateEnemies :: StageWithEnemy -> [StageWithEnemy] -> NonEmpty StageWithEnemy
updateEnemies (stage, enemies) = upsertList (combineEnemies enemies) stage

combineEnemies :: NonEmpty Enemy -> Maybe (NonEmpty Enemy) -> NonEmpty Enemy
combineEnemies newEnemies (Just enemies) = newEnemies <> enemies
combineEnemies newEnemies Nothing        = newEnemies

getMinStages :: NonEmpty Mission -> IO MinEnergyStagesWithMap
getMinStages missions = do
  enemyunitsPath <- getXdgDirectory XdgData "./monthly-missions/data/enemyunits.tsv"
  enemyunits <- TIO.readFile enemyunitsPath

  let eu = EnemyUnitsTSV enemyunits

  dbPath <- getXdgDirectory XdgData "./monthly-missions/data/stages10.2.db"
  conn <- open dbPath

  setTrace conn (Just TIO.putStrLn)

  stagess <- traverse (getStages conn eu) missions

  let MinEnergyStages minEnergyStagess = findMinEnergy stagess

  stagessWithMap <- traverse (\(s, es) -> (s, es, ) <$> whereIsStage conn s) minEnergyStagess

  close conn

  return $ MinEnergyStagesWithMap stagessWithMap

whereIsStage :: Connection -> Stage -> IO Map
whereIsStage conn (Stage c l s _ _) = do
    let (dbLevel, _, excludedStages) = eocHack l
    stages <- queryNamed conn
                  "SELECT group_concat(stage, ';'), level, category FROM stages where category = :category GROUP BY category, level order by stageid desc"
                  [":category" := c]

    case nonEmpty stages of
        Nothing -> throwIO (errorWithoutStackTrace $ "could not find " <> show c :: SomeException)
        Just nonEmptyStages -> do
          aggregatedStages <- traverse (\(FromRowStageCategory (StageNames stageNames) level _) -> do
                                              let ss = filter (`notElem` excludedStages) . T.split (== ';') $ stageNames

                                              case nonEmpty ss of
                                                Nothing -> throwIO (errorWithoutStackTrace $ "no stages found in " <> show c :: SomeException)
                                                Just stages' -> return $ AggregatedStages (StageName <$> stages') level c
               ) nonEmptyStages

          return $ Map $ (\(AggregatedStages sns level _) -> (\sn -> sn == s && level == dbLevel) <$> sns) <$> aggregatedStages
