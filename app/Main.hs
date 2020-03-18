{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception      as Exception
import           Control.Monad          (filterM, forever)
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Lazy   as BSL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.IO           as T
import           Data.Time
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.IO
import qualified System.IO              as IO
import           Text.Read

import           Lib
import           Types

--------------------------------------------------------------------------------

data Options =
  Options
    { opResDir    :: Maybe String
    , opInputFile :: Maybe String
    , opMode      :: Bool
    }

optionsParser :: Parser Options
optionsParser = Options
  <$> optional (strOption
      ( long    "resources directory"
     <> short   'd'
     <> metavar "DIR"
     <> help    "directory with xml resources" ))
  <*> optional (strOption
      ( long    "translations file"
     <> short   'i'
     <> metavar "FILE"
     <> help    "Path to csv file" ))
  <*> flag False True
      ( long "exporting mode"
     <> short 'e'
     <> long "Switch, to run in export mode (by default = false)")

-- | Description of the utility.
optionsDesc :: InfoMod Options -- ^ parser description
optionsDesc = fullDesc <> headerDesc
  where headerDesc = header "Android Translations Helper"

-- | Parser of the command-line options.
parser :: ParserInfo Options
parser = info (helper <*> optionsParser) optionsDesc


--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser parser
  let inFile  = opInputFile opts
      resDir  = opResDir opts
      exoMode = opMode opts

  case exoMode of
    False -> do
      case inFile of
        Nothing -> do
          putStrLn $ "ERROR: no input files supplied"
          return $ ()
        Just file -> do
          --
          -- do importing
          -- 1. parse csv inti

          readData file
          return $ ()
    True  -> do
      putStrLn $ "Exporting not yet implemented"
      return $ ()
