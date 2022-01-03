module Prompt.Apps (appsPrompt) where

import           System.Directory (getHomeDirectory)
import           System.FilePath  (combine, dropExtension, takeExtension)
import           XMonad.Core
import           XMonad.Prompt    (XPConfig, XPrompt, commandToComplete,
                                   getNextCompletion, mkXPrompt, nextCompletion,
                                   searchPredicate, showXPrompt)
import           XMonad.Util.Run  (runProcessWithInput)

type Predicate = String -> String -> Bool

getCompl :: [String] -> Predicate -> String -> IO [String]
getCompl compls p s = return $ filter (p s) compls

data App = App

instance XPrompt App where
  showXPrompt App = "Application"
  commandToComplete _ c = c

appsPrompt :: XPConfig -> X ()
appsPrompt xpconfig = do
  apps <- io getApps
  mkXPrompt App xpconfig (getCompl apps $ searchPredicate xpconfig) selectApp

selectApp :: String -> X ()
selectApp app = spawn $ "opendesktop .local/share/applications/" ++ escapeQuote app ++ ".desktop"

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = "\\\""
        escape x   = [x]

getApps :: IO [String]
getApps = do
  home  <- getHomeDirectory
  files <- runProcessWithInput "find" [combine home ".local/share/applications/", "-type", "f", "-name", "*.desktop", "-printf", "%P\n"] []
  return . map removeDesktopExtension $ lines files

removeDesktopExtension :: String -> String
removeDesktopExtension file | takeExtension file == ".desktop" = dropExtension file
                            | otherwise                        = file
