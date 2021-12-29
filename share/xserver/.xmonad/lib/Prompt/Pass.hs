module Prompt.Pass ( passPrompt
                   , passAutocompletePrompt
                   , passLoginPrompt
                   , passUrlPrompt
                   , passGeneratePrompt
                   , passRemovePrompt
                   , passEditPrompt
                   ) where

import qualified Data.List        as List
import qualified Data.Maybe       as Maybe
import           System.Directory (getHomeDirectory)
import           System.FilePath  (combine, dropExtension, takeExtension)
import           System.Posix.Env (getEnv)
import           XMonad.Core
import           XMonad.Prompt    (XPConfig, XPrompt, commandToComplete,
                                   getNextCompletion, mkXPrompt, nextCompletion,
                                   searchPredicate, showXPrompt)
import           XMonad.Util.Run  (runProcessWithInput)

type Predicate = String -> String -> Bool

getPassCompl :: [String] -> Predicate -> String -> IO [String]
getPassCompl compls p s = return $ filter (p s) compls

type PromptLabel = String

newtype Pass = Pass PromptLabel

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

passwordStoreFolderDefault :: String -> String
passwordStoreFolderDefault home = combine home ".password-store"

passwordStoreFolder :: IO String
passwordStoreFolder =
  getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
  where computePasswordStoreDir Nothing         = fmap passwordStoreFolderDefault getHomeDirectory
        computePasswordStoreDir (Just storeDir) = return storeDir

mkPassPrompt :: PromptLabel -> (String -> X ()) -> XPConfig -> X ()
mkPassPrompt promptLabel passwordFunction xpconfig = do
  passwords <- io (passwordStoreFolder >>= getPasswords)
  mkXPrompt (Pass promptLabel) xpconfig (getPassCompl passwords $ searchPredicate xpconfig) passwordFunction

-- prompts
passAutocompletePrompt :: XPConfig -> X ()
passAutocompletePrompt = mkPassPrompt "Select credentials" selectCredentials

passPrompt :: XPConfig -> X ()
passPrompt = mkPassPrompt "Select password" selectPassword

passLoginPrompt :: XPConfig -> X ()
passLoginPrompt = mkPassPrompt "Select login" typeLogin

passUrlPrompt :: XPConfig -> X ()
passUrlPrompt = mkPassPrompt "Select URL" typeUrl

passGeneratePrompt :: XPConfig -> X ()
passGeneratePrompt = mkPassPrompt "Generate password" generatePassword

passRemovePrompt :: XPConfig -> X ()
passRemovePrompt = mkPassPrompt "Remove password" removePassword

passEditPrompt :: XPConfig -> X ()
passEditPrompt = mkPassPrompt "Edit password" editPassword

-- actions
selectCredentials :: String -> X ()
selectCredentials passLabel = do
  copyOTP passLabel
  typeLoginAndPassword passLabel

selectPassword :: String -> X ()
selectPassword passLabel = do
  copyOTP passLabel
  typePassword passLabel

generatePassword :: String -> X ()
generatePassword passLabel = void $ runPass ["generate", "--force", "-c", escapeQuote passLabel, "30"]

removePassword :: String -> X ()
removePassword passLabel = void $ runPass ["rm", "--force", escapeQuote passLabel]

editPassword :: String -> X ()
editPassword passLabel = void $ runPass ["edit", escapeQuote passLabel]

copyOTP :: String -> X ()
copyOTP passLabel = spawn $ "pass otp --clip \"" ++ escapeQuote passLabel ++ "\""

typeLoginAndPassword :: String -> X ()
typeLoginAndPassword passLabel = do
  output <- io $ runPass [ escapeQuote passLabel ]
  let (Just login) = extractField "user: " output
  let password     = head output
  typeString $ login ++ "\t" ++ password

typePassword :: String -> X ()
typePassword passLabel = do
  output <- io $ runPass [ escapeQuote passLabel ]
  typeString . head $ output

typeLogin :: String -> X ()
typeLogin passLabel = do
  output <- io $ runPass [ escapeQuote passLabel ]
  Maybe.maybe (return ()) typeString $ extractField "user: " output

typeUrl :: String -> X ()
typeUrl passLabel = do
  output <- io $ runPass [ escapeQuote passLabel ]
  Maybe.maybe (return ()) typeString $ extractField "url: " output

-- utils
void :: IO a -> X ()
void x = () <$ io x

typeString :: String -> X ()
typeString text = spawn $ "echo -n \"" ++ escapeQuote text ++ "\" | xdotool type --clearmodifiers --file -"

extractField :: String -> [String] -> Maybe String
extractField phrase input = (List.\\ phrase) <$> List.find (List.isPrefixOf phrase) input

runPass :: [String] -> IO [String]
runPass inputs = List.lines <$> runProcessWithInput "pass" inputs []

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = "\\\""
        escape x   = [x]

getPasswords :: FilePath -> IO [String]
getPasswords passwordStoreDir = do
  files <- runProcessWithInput "find" ["-L", passwordStoreDir, "-type", "f", "-name", "*.gpg", "-printf", "%P\n"] []
  return . map removeGpgExtension $ lines files

removeGpgExtension :: String -> String
removeGpgExtension file | takeExtension file == ".gpg" = dropExtension file
                        | otherwise                    = file
