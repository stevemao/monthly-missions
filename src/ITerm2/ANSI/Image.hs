{-# LANGUAGE OverloadedStrings #-}
module ITerm2.ANSI.Image where
import qualified Data.ByteString        as BS
import           Data.ByteString.Base64
import           Data.ByteString.Char8

data Size = Cells Int | Pixels Int | Percent Int | Auto

data Options = Options { width               :: Size
                       , height              :: Size
                       , preserveAspectRatio :: Bool }

defaultOptions :: Options
defaultOptions = Options Auto Auto True

fromSize :: Size -> BS.ByteString
fromSize (Cells c)   = pack (show c)
fromSize (Pixels p)  = pack (show p) <> "px"
fromSize (Percent p) = pack (show p) <> "%"
fromSize Auto        = "auto"

image :: Options -> BS.ByteString -> BS.ByteString
image opts imgdata = "\ESC]1337;File=inline=1" <> ";width=" <> w <> ";height=" <> h <> ";preserveAspectRatio=" <> p <> ":" <> b64 <> "\0007"
        where b64 = encodeBase64' imgdata
              w = fromSize (width opts)
              h = fromSize (height opts)
              p = if preserveAspectRatio opts then "1" else "0"

printOpt :: Options -> BS.ByteString -> IO ()
printOpt opts imgdata = BS.putStr (image opts imgdata)

print :: BS.ByteString -> IO ()
print = printOpt defaultOptions
