{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import           Database.SQLite.Simple
import           BattleCats.MonthlyMissions.Types
import           BattleCats.MonthlyMissions.Lib

main :: IO ()
main = do
  let missions = [Mission (Location "EoC") (Target "Hippoe")
                , Mission (Location "EoC") (Target "Pigge")
                , Mission (Location "EoC") (Target "Jackie Peng")]

  enemyunits <- TIO.readFile "./data/enemyunits.tsv"

  let eu = EnemyUnitsTSV enemyunits

  conn <- open "./data/stages10.2.db"

  stagess <- traverse (getStages conn eu) missions

  let stages = findMinEnergy stagess

  print stages

  close conn
