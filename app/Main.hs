{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BattleCats.MonthlyMissions.Lib
import           BattleCats.MonthlyMissions.Types
import qualified Data.Text.IO                     as TIO
import           Database.SQLite.Simple

eocCh2 = LocationLevel "EoC Ch.2"
eocCh3 = LocationLevel "EoC Ch.3"
itfCh1 = LocationLevel "ItF Ch.1"
itfCh2 = LocationLevel "ItF Ch.2"
sol = LocationCategory "SoL"

main :: IO ()
main = do
  let missions = [Mission itfCh2 (Target "Corrupted Valkyrie")
                , Mission sol (Target "Camelle")
                , Mission sol (Target "Director Kurosawah")]

  enemyunits <- TIO.readFile "./data/enemyunits.tsv"

  let eu = EnemyUnitsTSV enemyunits

  conn <- open "./data/stages10.2.db"

  stagess <- traverse (getStages conn eu) missions

  let stages = findMinEnergy stagess

  print stages

  close conn
