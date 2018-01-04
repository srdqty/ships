{-# LANGUAGE TemplateHaskell #-}

module Main where

-------------------------------------------------------------------------------
import Data.Version (showVersion)

-------------------------------------------------------------------------------
import Development.GitRev (gitHash)
import Paths_ships (version)

-------------------------------------------------------------------------------
import CommandLineParser
import Data.Binary.IPS (patch)

-------------------------------------------------------------------------------
main :: IO ()
main = do
    options <- parseCommandLine (showVersion version) $(gitHash)
    ips <- getIPS options
    inData <- getInput options
    putOutput options (patch ips inData)
