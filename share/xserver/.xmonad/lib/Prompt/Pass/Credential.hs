module Prompt.Pass.Credential (Credential (..), field, parse) where

import qualified Data.Map    as Map
import qualified Text.Parsec as Parsec

data Credential =
  Credential { password :: String
             , fields   :: Map.Map String String
             } deriving Show

field :: String -> Credential -> Either String String
field s c = case Map.lookup s $ fields c of
              Nothing -> Left $ "no such field: " ++ s
              Just f  -> Right f

parse = Parsec.parse credentialParser

credentialParser :: Parsec.Parsec String () Credential
credentialParser = do
  password <- passwordParser
  Parsec.newline
  fields <- fieldParser `Parsec.sepEndBy` Parsec.newline
  return $ Credential password $ Map.fromList fields

passwordParser :: Parsec.Parsec String () String
passwordParser = do
  password <- Parsec.many1 $ Parsec.noneOf "\n"
  return password

fieldParser :: Parsec.Parsec String () (String, String)
fieldParser = do
  key <- Parsec.many1 $ Parsec.noneOf ":"
  Parsec.char ':'
  Parsec.spaces
  value <- Parsec.many1 $ Parsec.noneOf "\n"
  return (key, value)
