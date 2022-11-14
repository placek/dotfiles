module Prompt.Projects (projectsPrompt) where

import           System.Directory       (getHomeDirectory)
import           System.FilePath        (combine)
import           System.Posix.Env       (getEnv)
import           XMonad.Core
import           XMonad.Prompt
import           XMonad.Util.Run        (runProcessWithInput)

import           Prompt.Pass.Credential

data PromptType = Select | New

instance Show PromptType where
  show Select = "Select project"
  show New    = "New project"

data Projects = Projects PromptType XPConfig

instance XPrompt Projects where
  showXPrompt (Projects t _) = show t

  commandToComplete _ c = c

  completionFunction (Projects _ c) s = do
    projects <- getProjects
    return $ filter (searchPredicate c $ s) projects

  modeAction (Projects t _) a _ = actionFor t a
    where actionFor :: PromptType -> String -> X ()
          actionFor promptType = io <$> case promptType of
            Select -> selectAction
            New    -> newAction

-- prompt
projectsPrompt :: XPConfig -> X ()
projectsPrompt xpconfig = mkXPromptWithModes ([ XPT $ Projects Select xpconfig
                                              , XPT $ Projects New xpconfig
                                              ]) xpconfig

-- actions
selectAction :: String -> IO ()
selectAction name = do
  dir <- projectDir name
  openProject dir name

newAction :: String -> IO ()
newAction name = do
  dir <- projectDir name
  createProject dir
  openProject dir name

-- utils
openProject :: String -> String -> IO ()
openProject dir name = spawn $ "echo -ne \"launch --title='" ++ name ++ "' vim\ncd " ++ dir ++ "\" | kitty --session -"

createProject :: String -> IO ()
createProject dir = spawn $ "git init " ++ dir

projectDir :: String -> IO String
projectDir name = do
  dir <- projectsDir
  return $ combine dir name

getProjects :: IO [String]
getProjects = do
  dir   <- projectsDir
  files <- runProcessWithInput "find" ["-L", dir, "-type", "d", "-maxdepth", "1", "-printf", "%P\n"] []
  return $ lines files

projectsDir :: IO String
projectsDir =
  getEnv "PROJECTS_DIR" >>= computeProjectsDir
  where computeProjectsDir Nothing         = (`combine` "Projects") <$> getHomeDirectory
        computeProjectsDir (Just storeDir) = return storeDir
