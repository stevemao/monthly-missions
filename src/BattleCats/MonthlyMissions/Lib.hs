{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BattleCats.MonthlyMissions.Lib where

import           BattleCats.MonthlyMissions.Types
import           Control.Exception
import           Data.List.NonEmpty               (NonEmpty ((:|)),
                                                   groupBy1, nonEmpty)
import qualified Data.Text                        as T
import           Database.SQLite.Simple
import           Data.List

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
        uniqCombinitions = loop updateEnemies <$> combinitions
        stages = MinEnergyStages <$> uniqCombinitions

upsertList :: Eq k => (Maybe v -> v) -> k -> [(k, v)] -> NonEmpty (k, v)
upsertList f k m = case findValueIndex (\(key, _) -> key == k) m of
  Nothing -> (k, f Nothing) :| m
  Just ((_, v), i, ne) -> replace' ne i (k, f (Just v))

findValueIndex :: (a -> Bool) -> [a] -> Maybe (a, Int, NonEmpty a)
findValueIndex _ [] = Nothing 
findValueIndex f x@(a : as) = case find (f . fst) (zip x [0..]) of
  Nothing -> Nothing 
  Just (b, i) -> Just (b, i, a :| as)

replace :: [a] -> Int -> a -> [a]
replace [] _ _ = []
replace (_:xs) 0 a = a : xs
replace (x:xs) n a =
  if n < 0
    then x : xs
    else x: replace xs (n-1) a

replace' :: NonEmpty a -> Int -> a -> NonEmpty a
replace' (_:|xs) 0 a = a :| xs
replace' (x:|xs) n a =
  if n < 0
    then x :| xs
    else x :| replace xs (n-1) a

loop' :: (t -> a) -> [t] -> [a]
loop' _ [] = []
loop' f (a : as) = f a : loop' f as

loop :: (a -> [a] -> NonEmpty a) -> NonEmpty a -> NonEmpty a
loop f (a :| as) = f a as

updateEnemies :: StageWithEnemy -> [StageWithEnemy] -> NonEmpty StageWithEnemy
updateEnemies (stage, enemies) = upsertList (combineEnemies enemies) stage

combineEnemies :: NonEmpty Enemy -> Maybe (NonEmpty Enemy) -> NonEmpty Enemy
combineEnemies newEnemies (Just enemies) = newEnemies <> enemies
combineEnemies newEnemies Nothing        = newEnemies
