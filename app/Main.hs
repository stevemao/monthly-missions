{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import           BattleCats.Image
import           BattleCats.MonthlyMissions.Lib
import           BattleCats.MonthlyMissions.Types
import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.ByteString                   as BS
import           Data.Foldable
import           Data.List.NonEmpty                (nonEmpty)
import qualified Data.Text                         as T
import           Data.Version
import           Paths_monthly_missions
import           System.Console.CmdArgs
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

data Missions
  = Missions
      { m :: [T.Text]
      }
  deriving (Data, Show, Typeable)

input :: Missions
input = Missions (def &= args &= typ "location target...") &= program "monthly-missions" &=
           help "Find the most energy efficient stages for your monthly missions" &=
           summary ("monthly-missions " <> showVersion version <> ", (C) Steve Mao") &=
           details ["Examples:"
                   , "  monthly-missions cotcCh2 'Cat God' cotcCh2 'Shibalien Elite'"
                   , "  monthly-missions sol 'Shibalien Elite' sol 'Star Peng'", ""
                   , "Manually find the stages that contain the enemies can be a very time consuming task",""
                   ,"This tool can find your enemies very quickly","  You might even find multiple enemies in one stage"
                   ]

pairs :: [b] -> [(b, b)]
pairs []           = []
pairs [_]          = []
pairs (a : b : as) = [(a,b)] <> pairs as

fromString :: T.Text -> Location
fromString "eocCh2"  = LocationLevel "EoC Ch.2"
fromString "eocCh3"  = LocationLevel "EoC Ch.3"
fromString "itfCh1"  = LocationLevel "ItF Ch.1"
fromString "itfCh2"  = LocationLevel "ItF Ch.2"
fromString "cotcCh1" = LocationLevel "CotC Ch.1"
fromString "cotcCh2" = LocationLevel "CotC Ch.2"
fromString "cotcCh3" = LocationLevel "CotC Ch.3"
fromString "sol"     = LocationCategory "SoL"
fromString l         = LocationLevel (Level l)

main :: IO ()
main = do
  as <- cmdArgs input

  let ms = (\(l, e) -> Mission (fromString l) (Target e)) <$> pairs (m as)

  case nonEmpty ms of
    Nothing -> throwIO (error "You must specify at least one location and enemy. try monthly-missions cotcCh2 'Cat God' cotcCh2 'Shibalien Elite'" :: SomeException)
    Just missions -> do
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
