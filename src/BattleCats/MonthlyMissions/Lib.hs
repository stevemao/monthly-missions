{-# LANGUAGE OverloadedStrings #-}
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
                            let (level', energyAdjustment) = case level of
                                                            "EoC Ch.1" -> ("EoC", 0)
                                                            "EoC Ch.2" -> ("EoC", 10)
                                                            "EoC Ch.3" -> ("EoC", 20)
                                                            l          -> (l, 0)

                            stages <- queryNamed conn "SELECT level, stage, energy from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND level = :level" [":enemycode" := enemycode, ":level" := level']

                            return $ (\(Stage _ n e) -> Stage level n (e + energyAdjustment)) <$> stages
      LocationCategory category -> do
                            queryNamed conn "SELECT level, stage, energy from stages s JOIN units u ON u.stageid = s.stageid WHERE u.enemycode = :enemycode AND category = :category" [":enemycode" := enemycode, ":category" := category]
    if null stages
    then throwIO (error $ "could not find " <> show m :: SomeException)
    else return $ nub stages

findMinEnergy :: [[Stage]] -> Stages
findMinEnergy stagess = minimum $ Stages <$> nub <$> sequence stagess
