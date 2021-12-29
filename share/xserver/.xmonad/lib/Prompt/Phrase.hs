module Prompt.Phrase (phrasePrompt) where

import           System.Directory (getHomeDirectory)
import           System.FilePath  (combine)
import           XMonad.Core
import           XMonad.Prompt    (XPConfig, XPrompt, commandToComplete,
                                   getNextCompletion, mkXPrompt, nextCompletion,
                                   searchPredicate, showXPrompt)

type Predicate = String -> String -> Bool

getPhraseCompl :: [String] -> Predicate -> String -> IO [String]
getPhraseCompl compls p s = return $ filter (p s) compls

data Phrase = Phrase

instance XPrompt Phrase where
  showXPrompt          Phrase = "Phrase"
  commandToComplete _ c       = c
  nextCompletion      _       = getNextCompletion

phrasePrompt :: XPConfig -> X ()
phrasePrompt xpconfig = do
  phrases <- io getPhrases
  mkXPrompt Phrase xpconfig (getPhraseCompl phrases $ searchPredicate xpconfig) selectPhrase

selectPhrase :: String -> X ()
selectPhrase phrase = spawn $ "echo -n \"" ++ escapeQuote phrase ++ "\"|xdotool type --clearmodifiers --file -"

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = "\\\""
        escape x   = [x]

getPhrases :: IO [String]
getPhrases = do
  home    <- getHomeDirectory
  phrases <- readFile $ combine home ".abbr"
  return . lines $ phrases
