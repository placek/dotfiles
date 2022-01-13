module Prompt.Run (runPrompt) where

import           Codec.Binary.UTF8.String (encodeString)
import           Control.Exception        as E
import           Control.Monad            (forM)
import           Data.Char                (toLower)
import           Data.List                (isPrefixOf, sortBy)
import           System.Directory         (getDirectoryContents)
import           System.Environment       (getEnv)
import           System.Posix.Files       (getFileStatus, isDirectory)

import           XMonad                   hiding (config)
import           XMonad.Prompt
import           XMonad.Util.Run

data Run = Run XPConfig

instance XPrompt Run where
  showXPrompt (Run _) = "Run"

  completionToCommand _ = escape
    where escape [] = ""
          escape (x:xs)
              | isSpecialChar x = '\\' : x : escape xs
              | otherwise       = x : escape xs

  nextCompletion _ = getNextCompletion

  completionFunction (Run c) s = do
    cmds <- io getCommands
    return $ filter ((searchPredicate c) s) cmds

  modeAction _ a _ = spawn a

runPrompt :: XPConfig -> X ()
runPrompt xpconfig = mkXPromptWithModes [XPT $ Run xpconfig] xpconfig

getCommands :: IO [String]
getCommands = do
  pathEnv <- getEnv "PATH"
  let paths = filter (/= "") $ split ':' pathEnv
  commands <- forM paths $ \d -> getDirectoryContents d `E.catch` econst []
  return . uniqSort . filter ((/= '.') . head) . concat $ commands

econst :: Monad m => a -> IOException -> m a
econst = const . return

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l  = f : split e (rest ls)
  where (f,ls) = span (/=e) l
        rest s | s == []   = []
               | otherwise = tail s

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem " &\\@\"'#?$*()[]{};"
