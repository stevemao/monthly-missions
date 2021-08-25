{-# LANGUAGE OverloadedStrings #-}
module BattleCats.MonthlyMissions.Lib where

import qualified Data.Text as T
import           Database.SQLite.Simple
import           Control.Exception
import           Data.List
import           BattleCats.MonthlyMissions.Types

getCode :: EnemyUnitsTSV -> Target -> IO EnemyCode
getCode (EnemyUnitsTSV enemyunits) t@(Target target) = do
  let maybeIdx = subtract 1 <$> findIndex (target `T.isInfixOf`) (T.lines enemyunits)

  maybe (throwIO (error $ "could not find " <> show t :: SomeException)) (return . EnemyCode) maybeIdx

getStages :: Connection -> EnemyUnitsTSV -> Mission -> IO [Stage]
getStages conn enemyunits m@(Mission (Location level) target) = do
    EnemyCode enemycode <- getCode enemyunits target

    stages <- queryNamed conn "SELECT level, stage, energy from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND level = :level" [":enemycode" := enemycode, ":level" := level]

    if stages == [] 
    then throwIO (error $ "could not find " <> show m :: SomeException)
    else return $ nub stages

findMinEnergy :: [[Stage]] -> Stages
findMinEnergy stagess = minimum $ Stages <$> nub <$> sequence stagess
