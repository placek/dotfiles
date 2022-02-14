module Prompt.Search (searchPrompt) where

import           Data.Char                       (toLower)
import           XMonad                   hiding (config)
import           XMonad.Prompt
import           XMonad.Util.Run                 (safeSpawn)

data SearchType = Allegro | Alpha | Ang | Docker | Duck | Hoogle | Maps | Nixpkgs | Pocket | Ruby | Stack | Wiki | Youtube
                  deriving Show

data Search = Search SearchType XPConfig

instance XPrompt Search where
  showXPrompt (Search t _) = "Search " ++ show t

  nextCompletion _ = getNextCompletion

  completionFunction _ _ = return []

  modeAction (Search t _) a _ = safeSpawn "qutebrowser" ["--target", "tab", (toLower <$> show t) ++ " " ++ a]

searchPrompt :: XPConfig -> X ()
searchPrompt xpconfig = mkXPromptWithModes [ XPT $ Search Duck xpconfig
                                           , XPT $ Search Hoogle xpconfig
                                           , XPT $ Search Youtube xpconfig
                                           , XPT $ Search Maps xpconfig
                                           , XPT $ Search Stack xpconfig
                                           , XPT $ Search Nixpkgs xpconfig
                                           , XPT $ Search Wiki xpconfig
                                           , XPT $ Search Allegro xpconfig
                                           , XPT $ Search Alpha xpconfig
                                           , XPT $ Search Ang xpconfig
                                           , XPT $ Search Docker xpconfig
                                           , XPT $ Search Pocket xpconfig
                                           , XPT $ Search Ruby xpconfig
                                           ] xpconfig
