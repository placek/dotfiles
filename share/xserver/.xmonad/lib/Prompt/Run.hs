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

econst :: Monad m => a -> IOException -> m a
econst = const . return

data Run = Run
type Predicate = String -> String -> Bool

instance XPrompt Run where
    showXPrompt         Run = "Run"
    completionToCommand _   = escape

runPrompt :: XPConfig -> X ()
runPrompt c = do
    cmds <- io getCommands
    mkXPrompt Run c (getRunCompl cmds $ searchPredicate c) spawn

getRunCompl :: [String] -> Predicate -> String -> IO [String]
getRunCompl cmds p s | s == "" || last s == ' ' = return []
                     | otherwise                = do
    f     <- fmap lines $ runProcessWithInput "bash" [] ("compgen -A file -- "
                                                        ++ s ++ "\n")
    files <- case f of
               [x] -> do fs <- getFileStatus (encodeString x)
                         if isDirectory fs then return [x ++ "/"]
                                           else return [x]
               _   -> return f
    return . sortBy typedFirst . uniqSort $ files ++ commandCompletionFunction cmds p s
    where
    typedFirst x y
        | x `startsWith` s && not (y `startsWith` s) = LT
        | y `startsWith` s && not (x `startsWith` s) = GT
        | otherwise = x `compare` y
    startsWith str ps = isPrefixOf (map toLower ps) (map toLower str)

commandCompletionFunction :: [String] -> Predicate -> String -> [String]
commandCompletionFunction cmds p str | '/' `elem` str = []
                                     | otherwise      = filter (p str) cmds

getCommands :: IO [String]
getCommands = do
    p  <- getEnv "PATH" `E.catch` econst []
    let ds = filter (/= "") $ split ':' p
    es <- forM ds $ \d -> getDirectoryContents d `E.catch` econst []
    return . uniqSort . filter ((/= '.') . head) . concat $ es

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split e l =
    f : split e (rest ls)
        where
          (f,ls) = span (/=e) l
          rest s | s == []   = []
                 | otherwise = tail s

escape :: String -> String
escape []       = ""
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise       = x : escape xs

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem " &\\@\"'#?$*()[]{};"
