{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BattleCats.Image
import           BattleCats.MonthlyMissions.Lib
import           BattleCats.MonthlyMissions.Types
import qualified Data.ByteString                   as BS
import           Data.Foldable
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import qualified Data.Text.IO                      as TIO
import           Database.SQLite.Simple
import           Text.Pretty.Simple
import           Text.Pretty.Simple.Internal.Color

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
  let missions = Mission sol (Target "Owlbrow") :|
               [ Mission sol (Target "St. Pigge the 2nd") ]

  enemyunits <- TIO.readFile "./data/enemyunits.tsv"

  let eu = EnemyUnitsTSV enemyunits

  conn <- open "./data/stages10.2.db"

  stagess <- traverse (getStages conn eu) missions

  let (MinEnergyStages stages) = findMinEnergy stagess

  traverse_ (\(stage, enemies) -> do
      BS.putStr "\n"

      pPrint stage

      traverse (\enemy@(Enemy _ _ _ code) -> do
            pPrintOpt CheckColorTty defaultOutputOptionsDarkBg { outputOptionsInitialIndent = 4
                                                               , outputOptionsColorOptions = Just (defaultColorOptionsDarkBg {
                                                                    colorRainbowParens = [ colorBold Vivid Cyan
                                                                                         , colorBold Vivid Yellow
                                                                                         , color Dull Magenta
                                                                                         , color Dull Cyan
                                                                                         , color Dull Yellow
                                                                                         , colorBold Dull Magenta
                                                                                         , colorBold Dull Cyan
                                                                                         , colorBold Dull Yellow
                                                                                         , color Vivid Magenta
                                                                                         , color Vivid Cyan
                                                                                         , color Vivid Yellow
                                                                                         , colorBold Vivid Magenta ] }) } enemy

            BS.putStr "         "
            terminalImage code
            BS.putStr "\n"
        ) enemies
      ) stages

  close conn
