module Prompt.UDisks.Device (Device(..), parse, (.&.), isLoop, isMounted, isNotMounted, isLuks, hasFileSystem) where

import qualified Text.Parsec      as Parsec
import           Data.List                  (isPrefixOf)

(.&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
a .&. b = \c -> a c && b c

data Device =
  Device { devicePath :: String
         , deviceType :: String
         , isHotplug  :: Bool
         , size       :: String
         , mountPoint :: Maybe String
         , fsType     :: Maybe String
         } deriving Show

isLoop :: Device -> Bool
isLoop device = "loop" == deviceType device

isMounted :: String -> Device -> Bool
isMounted user device = case mountPoint device of
                          Just a    -> isPrefixOf ("/run/media/" ++ user) a
                          otherwise -> False

isNotMounted :: Device -> Bool
isNotMounted device = case mountPoint device of
                        Nothing -> True
                        Just _  -> False

hasFileSystem :: Device -> Bool
hasFileSystem device = case fsType device of
                         Nothing -> False
                         Just _  -> True

isLuks :: Device -> Bool
isLuks device = case fsType device of
                  Just "crypto_LUKS" -> True
                  otherwise          -> False

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
  Parsec.spaces
  dFSType <- fsTypeParser
  return $ Device dPath dType dHotplug dSize dMountpoint dFSType

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

fsTypeParser :: Parsec.Parsec String () (Maybe String)
fsTypeParser = do
  Parsec.string "FSTYPE=\""
  value <- Parsec.many $ Parsec.noneOf "\""
  Parsec.char '"'
  return $ if null value
           then Nothing
           else Just value
