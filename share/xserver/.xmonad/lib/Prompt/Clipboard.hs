module Prompt.Clipboard (clipboardPrompt) where

import           XMonad.Core
import           XMonad.Prompt
import           XMonad.Util.Run (runProcessWithInput, safeSpawn)

data Clipboard = Clipboard XPConfig

instance XPrompt Clipboard where
  showXPrompt (Clipboard _) = "Clipboard"

  commandToComplete _ c = c

  nextCompletion _ = getNextCompletion

  completionFunction (Clipboard c) s = do
    clips <- runProcessWithInput "greenclip" ["print"] []
    return $ filter ((searchPredicate c) s) (lines clips)

  modeAction _ a _ = io $ safeSpawn "greenclip" ["print", a]

clipboardPrompt :: XPConfig -> X ()
clipboardPrompt xpconfig = mkXPromptWithModes [XPT $ Clipboard xpconfig] xpconfig
