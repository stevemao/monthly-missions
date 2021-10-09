{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BattleCats.MonthlyMissions.Lib
import           BattleCats.MonthlyMissions.Types
import qualified Data.Text.IO                     as TIO
import           Database.SQLite.Simple

eocCh2, eocCh3, itfCh1, itfCh2, sol, cotcCh1, cotcCh2, cotcCh3 :: Location
eocCh2 = LocationLevel "EoC Ch.2"
eocCh3 = LocationLevel "EoC Ch.3"
itfCh1 = LocationLevel "ItF Ch.1"
itfCh2 = LocationLevel "ItF Ch.2"
sol = LocationCategory "SoL"
cotcCh1 = LocationLevel "CotC Ch.1"
cotcCh2 = LocationLevel "CotC Ch.2"
cotcCh3 = LocationLevel "CotC Ch.3"

main :: IO ()
main = do
  let missions = [ Mission cotcCh1 (Target "One Horn")
                 , Mission cotcCh2 (Target "Cat God")
                 , Mission sol (Target "Zomboe") ]

  enemyunits <- TIO.readFile "./data/enemyunits.tsv"

  let eu = EnemyUnitsTSV enemyunits

  conn <- open "./data/stages10.2.db"

  stagess <- traverse (getStages conn eu) missions

  let stages = findMinEnergy stagess

  print stages

  close conn
