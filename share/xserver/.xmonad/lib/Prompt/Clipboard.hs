module Prompt.Clipboard (clipboardPrompt) where

import           XMonad.Core
import           XMonad.Prompt   (XPConfig, XPrompt, commandToComplete,
                                  getNextCompletion, mkXPrompt, nextCompletion,
                                  searchPredicate, showXPrompt)
import           XMonad.Util.Run (runProcessWithInput)

type Predicate = String -> String -> Bool

getClipCompl :: [String] -> Predicate -> String -> IO [String]
getClipCompl compls p s = return $ filter (p s) compls

data Clipboard = Clipboard

instance XPrompt Clipboard where
  showXPrompt       Clipboard = "Clipboard"
  commandToComplete _ c       = c

clipboardPrompt :: XPConfig -> X ()
clipboardPrompt xpconfig = do
  clips <- io getClips
  mkXPrompt Clipboard xpconfig (getClipCompl clips $ searchPredicate xpconfig) selectClip

selectClip :: String -> X ()
selectClip clip = spawn $ "greenclip print \"" ++ escapeQuote clip ++ "\""

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = "\\\""
        escape x   = [x]

getClips :: IO [String]
getClips = do
  clips <- runProcessWithInput "greenclip" ["print"] []
  return . lines $ clips
