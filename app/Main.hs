module Main where

import System.Environment (getArgs)
import Data.Binary (decodeFile)
import qualified Data.ByteString.Lazy as L (readFile, putStr)
import Data.Binary.IPS (IPS, patch)

-------------------------------------------------------------------------------
main :: IO ()
main = do
    [ipsFilename, inFilename] <- getArgs
    ips <- decodeFile ipsFilename :: IO IPS
    inData <- L.readFile inFilename
    L.putStr (patch ips inData)
