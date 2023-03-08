module Prompt.Pass (passPrompt) where

import           Data.Either            (either)
import qualified Data.List              as List
import qualified Data.Map               as Map
import qualified Data.Maybe             as Maybe
import           System.Directory       (getHomeDirectory)
import           System.FilePath        (combine, dropExtension, takeExtension)
import           System.Posix.Env       (getEnv)
import           XMonad.Core
import           XMonad.Prompt
import           XMonad.Util.Run        (runProcessWithInput)

import           Prompt.Pass.Credential

data PromptType = Autocompletion | Password | Login | Url | Generator | Editor | Removal

instance Show PromptType where
  show Autocompletion = "Password autocompletion"
  show Password       = "Fetch password"
  show Login          = "Fetch login"
  show Url            = "Fetch url"
  show Generator      = "Generate password"
  show Editor         = "Edit password"
  show Removal        = "Remove password"

data Pass = Pass PromptType XPConfig

instance XPrompt Pass where
  showXPrompt (Pass t _) = show t

  commandToComplete _ c = c

  completionFunction (Pass _ c) s = do
    passwords <- getPasswords
    return $ filter (searchPredicate c $ s) passwords

  modeAction (Pass t _) a _ = actionFor t a
    where actionFor :: PromptType -> String -> X ()
          actionFor promptType = case promptType of
            Autocompletion -> autocompletionAction
            Password       -> passwordAction
            Login          -> loginAction
            Url            -> urlAction
            Generator      -> generateAction
            Editor         -> editorAction
            Removal        -> removalAction

-- prompt
passPrompt :: XPConfig -> X ()
passPrompt xpconfig = mkXPromptWithModes ([ XPT $ Pass Autocompletion xpconfig
                                          , XPT $ Pass Password xpconfig
                                          , XPT $ Pass Login xpconfig
                                          , XPT $ Pass Url xpconfig
                                          , XPT $ Pass Generator xpconfig
                                          , XPT $ Pass Editor xpconfig
                                          , XPT $ Pass Removal xpconfig
                                          ]) xpconfig

-- actions
autocompletionAction :: String -> X ()
autocompletionAction passLabel = do
  copyOTP passLabel
  typeLoginAndPassword passLabel

passwordAction :: String -> X ()
passwordAction passLabel = do
  copyOTP passLabel
  typePassword passLabel

loginAction :: String -> X ()
loginAction passLabel = do
  credential <- fetchCredential passLabel
  typeString $ credential >>= field "user"

urlAction :: String -> X ()
urlAction passLabel = do
  credential <- fetchCredential passLabel
  typeString $ credential >>= field "url"

generateAction :: String -> X ()
generateAction passLabel = do
  _ <- io $ runPass ["generate", "--force", "-c", escapeQuote passLabel, "30"]
  return ()

editorAction :: String -> X ()
editorAction passLabel = do
  _ <- io $ runPass ["edit", escapeQuote passLabel]
  return ()

removalAction :: String -> X ()
removalAction passLabel = do
  _<- io $ runPass ["rm", "--force", escapeQuote passLabel]
  return ()

-- utils
copyOTP :: String -> X ()
copyOTP passLabel = spawn $ "pass otp --clip \"" ++ escapeQuote passLabel ++ "\""

typeString :: Either String String -> X ()
typeString input = case input of
                     Right text -> spawn $ clearLineCommand ++ " echo -n \"" ++ escapeQuote text ++ "\" | xdotool type --clearmodifiers --file -"
                     Left  err  -> spawn $ "xmessage \"" ++ escapeQuote err ++ "\""

clearLineCommand :: String
clearLineCommand = "xdotool key ctrl+v Home ctrl+k;"

typeLoginAndPassword :: String -> X ()
typeLoginAndPassword passLabel = do
  credential <- fetchCredential passLabel
  typeString . fmap concat . sequence $ [credential >>= field "user", Right "\t", fmap password credential]

typePassword :: String -> X ()
typePassword passLabel = do
  credential <- fetchCredential passLabel
  typeString $ password <$> credential

fetchCredential :: String -> X (Either String Credential)
fetchCredential passLabel = do
  output <- io $ runPass [escapeQuote passLabel]
  return $ either (Left . show) Right (parse passLabel output)

runPass :: [String] -> IO String
runPass args = runProcessWithInput "pass" args []

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = "\\\""
        escape '$' = "\\$"
        escape x   = [x]

getPasswords :: IO [String]
getPasswords = do
  dir   <- passwordStoreDir
  files <- runProcessWithInput "find" ["-L", dir, "-type", "f", "-name", "*.gpg", "-printf", "%P\n"] []
  return . map removeGpgExtension $ lines files

passwordStoreDir :: IO String
passwordStoreDir =
  getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
  where computePasswordStoreDir Nothing         = (`combine` ".password-store") <$> getHomeDirectory
        computePasswordStoreDir (Just storeDir) = return storeDir

removeGpgExtension :: String -> String
removeGpgExtension file | takeExtension file == ".gpg" = dropExtension file
                        | otherwise                    = file
