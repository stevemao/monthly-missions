{-# LANGUAGE OverloadedStrings #-}
module BattleCats.Image where
import           BattleCats.MonthlyMissions.Types
import qualified Data.ByteString                  as BS
import qualified ITerm2.ANSI.Image                as I

terminalImage :: EnemyCode -> IO ()
terminalImage (EnemyCode code) = do
    imgdata <- BS.readFile $ "./data/enemy/enemy_icon_" <> padLeft '0' 3 (show code) <> ".png"

    I.print imgdata

{-# INLINE padLeft #-}
padLeft :: a -> Int -> [a] -> [a]
padLeft  c n xs = replicate (n - length xs) c ++ xs
