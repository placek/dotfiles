module Prompt.UDisks.Device (Device (..), parse, isLoop, isMountedPartition, isNotMountedPartition) where

import qualified Text.Parsec as Parsec
import           Data.List             (isPrefixOf)

data Device =
  Device { devicePath :: String
         , deviceType :: String
         , isHotplug  :: Bool
         , size       :: String
         , mountpoint :: Maybe String
         } deriving Show

isLoop :: Device -> Bool
isLoop device = "loop" == deviceType device

isMounted :: Device -> Bool
isMounted device = case mountpoint device of
                     Nothing -> False
                     Just _  -> True

isLoopPartition :: Device -> Bool
isLoopPartition device = isPrefixOf "/dev/loop" (devicePath device)

isMountedPartition :: Device -> Bool
isMountedPartition device = isMounted device && (isHotplug device || isLoopPartition device)

isNotMountedPartition :: Device -> Bool
isNotMountedPartition device = (not . isMounted $ device) && (isHotplug device || isLoopPartition device)

parse = Parsec.parse deviceParser

deviceParser :: Parsec.Parsec String () Device
deviceParser = do
  Parsec.spaces
  dPath <- pathParser
  Parsec.spaces
  dType <- typeParser
  Parsec.spaces
  dHotplug <- hotplugParser
  Parsec.spaces
  dSize <- sizeParser
  Parsec.spaces
  dMountpoint <- mountpointParser
  return $ Device dPath dType dHotplug dSize dMountpoint

pathParser :: Parsec.Parsec String () String
pathParser = do
  Parsec.string "PATH=\""
  value <- Parsec.many1 $ Parsec.noneOf "\""
  Parsec.char '"'
  return value

typeParser :: Parsec.Parsec String () String
typeParser = do
  Parsec.string "TYPE=\""
  value <- Parsec.many1 $ Parsec.noneOf "\""
  Parsec.char '"'
  return value

hotplugParser :: Parsec.Parsec String () Bool
hotplugParser = do
  Parsec.string "HOTPLUG=\""
  value <- Parsec.oneOf "01"
  Parsec.char '"'
  return $ case value of
             '1'       -> True
             otherwise -> False

sizeParser :: Parsec.Parsec String () String
sizeParser = do
  Parsec.string "SIZE=\""
  value <- Parsec.many1 $ Parsec.noneOf "\""
  Parsec.char '"'
  return value

mountpointParser :: Parsec.Parsec String () (Maybe String)
mountpointParser = do
  Parsec.string "MOUNTPOINT=\""
  value <- Parsec.many $ Parsec.noneOf "\""
  Parsec.char '"'
  return $ if null value
           then Nothing
           else Just value
