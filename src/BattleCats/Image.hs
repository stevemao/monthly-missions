{-# LANGUAGE OverloadedStrings #-}
module BattleCats.Image where
import           BattleCats.MonthlyMissions.Types
import qualified Data.ByteString                  as BS
import           Data.ByteString.Base64

getImage :: EnemyCode -> IO BS.ByteString
getImage (EnemyCode code) = do
    imgdata <- BS.readFile $ "./data/enemy/enemy_icon_" <> padLeft '0' 3 (show code) <> ".png"

    let b64 = encodeBase64' imgdata

    return $ "\ESC]1337;File=inline=1:" <> b64 <> "\0007\n"

{-# INLINE padLeft #-}
padLeft :: a -> Int -> [a] -> [a]
padLeft  c n xs = replicate (n - length xs) c ++ xs
