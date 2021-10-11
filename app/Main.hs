{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BattleCats.Image
import           BattleCats.MonthlyMissions.Lib
import           BattleCats.MonthlyMissions.Types
import           Control.Monad.IO.Class
import qualified Data.ByteString                   as BS
import           Data.Foldable
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import           Text.Pretty.Simple
import           Text.Pretty.Simple.Internal.Color

eocCh2, eocCh3, itfCh1, itfCh2, sol, cotcCh1, cotcCh2, cotcCh3 :: Location
eocCh2 = LocationLevel "EoC Ch.2"
eocCh3 = LocationLevel "EoC Ch.3"
itfCh1 = LocationLevel "ItF Ch.1"
itfCh2 = LocationLevel "ItF Ch.2"
cotcCh1 = LocationLevel "CotC Ch.1"
cotcCh2 = LocationLevel "CotC Ch.2"
cotcCh3 = LocationLevel "CotC Ch.3"
sol = LocationCategory "SoL"

main :: IO ()
main = do
  let missions = Mission sol (Target "Owlbrow") :|
               [ Mission sol (Target "St. Pigge the 2nd") ]

  MinEnergyStages stages <- getMinStages missions

  traverse_ (\(stage, enemies) -> do
      BS.putStr "\n"

      pPrint stage

      traverse (\enemy@(Enemy _ _ _ code) -> do
            pPrintIndented enemy

            BS.putStr "         "
            terminalImage code
            BS.putStr "\n"
        ) enemies
      ) stages

pPrintIndented :: (MonadIO m, Show a) => a -> m ()
pPrintIndented = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg
              { outputOptionsInitialIndent = 4
              , outputOptionsColorOptions = Just (defaultColorOptionsDarkBg
                      { colorRainbowParens = [ colorBold Vivid Cyan
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
                                             , colorBold Vivid Magenta ] }) }
