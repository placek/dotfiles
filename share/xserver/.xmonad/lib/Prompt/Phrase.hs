module Prompt.Phrase (phrasePrompt) where

import           System.Directory (getHomeDirectory)
import           System.FilePath  (combine)
import           XMonad.Core
import           XMonad.Prompt

data Phrase = Phrase XPConfig

instance XPrompt Phrase where
  showXPrompt (Phrase _) = "Phrase"

  commandToComplete _ c = c

  nextCompletion _ = getNextCompletion

  completionFunction (Phrase c) s = do
    home    <- getHomeDirectory
    phrases <- readFile $ combine home ".abbr"
    return $ filter ((searchPredicate c) s) (lines phrases)

  modeAction _ a _ = spawn $ "echo -n \"" ++ escapeQuote a ++ "\" | xdotool type --clearmodifiers --file -"

phrasePrompt :: XPConfig -> X ()
phrasePrompt xpconfig = mkXPromptWithModes [XPT $ Phrase xpconfig] xpconfig

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = "\\\""
        escape x   = [x]
