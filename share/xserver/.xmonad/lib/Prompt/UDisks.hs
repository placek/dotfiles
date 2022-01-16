module Prompt.UDisks (udisksPrompt) where

import           XMonad.Core
import           XMonad.Prompt          (XPConfig(..), XPrompt(..),  XPType(..), mkXPromptWithModes)
import           XMonad.Prompt.Input    (inputPromptWithCompl)
import           XMonad.Util.Run        (runProcessWithInput)
import           Data.List              (isPrefixOf)

import           Prompt.UDisks.Device

data PromptType = Mount | Unmount | Unlock | Lock | LoopSetup | LoopDelete

instance Show PromptType where
  show Mount      = "Mount device"
  show Unmount    = "Unmount device"
  show Unlock     = "Unlock crypted device"
  show Lock       = "Lock crypted device"
  show LoopSetup  = "Setup loop device"
  show LoopDelete = "Delete loop device"

data UDisks = UDisks PromptType XPConfig

instance XPrompt UDisks where
  showXPrompt (UDisks t _) = show t

  commandToComplete _ c = c

  completionFunction (UDisks Unmount c) s = do
    devices <- getDevices
    let paths = devicePath <$> (filter isMountedPartition devices)
    return $ filter (searchPredicate c $ s) paths

  completionFunction (UDisks LoopDelete c) s = do
    devices <- getDevices
    let paths = devicePath <$> (filter isLoop devices)
    return $ filter (searchPredicate c $ s) paths

  completionFunction (UDisks LoopSetup c) s = do
    files <- compgenFiles s
    return $ filter (searchPredicate c $ s) (lines files)

  completionFunction (UDisks _ c) s = do
    devices <- getDevices
    let paths = devicePath <$> (filter isNotMountedPartition devices)
    return $ filter (searchPredicate c $ s) paths

  modeAction (UDisks t c) a _ = actionFor t a
    where actionFor :: PromptType -> String -> X ()
          actionFor promptType = case promptType of
            Mount      -> mountAction
            Unmount    -> unmountAction
            Unlock     -> unlockAction c
            Lock       -> lockAction
            LoopSetup  -> loopSetupAction
            LoopDelete -> loopDeleteAction

-- prompt
udisksPrompt :: XPConfig -> X ()
udisksPrompt xpconfig = mkXPromptWithModes ([ XPT $ UDisks Mount      xpconfig
                                            , XPT $ UDisks Unmount    xpconfig
                                            , XPT $ UDisks Unlock     xpconfig
                                            , XPT $ UDisks Lock       xpconfig
                                            , XPT $ UDisks LoopSetup  xpconfig
                                            , XPT $ UDisks LoopDelete xpconfig
                                            ]) xpconfig

keyFilePrompt :: XPConfig -> X (Maybe String)
keyFilePrompt xpconfig = inputPromptWithCompl xpconfig "Select key file" compl
  where compl s = lines <$> compgenFiles s

-- actions
mountAction :: String -> X ()
mountAction path = do
  output <- runUDiskCtl ["mount", "-b", path, "--no-user-interaction"]
  notify output path "Mounting succeded" "Mounting failed"

unmountAction :: String -> X ()
unmountAction path = do
  output <- runUDiskCtl ["unmount", "-b", path, "--no-user-interaction"]
  notify output path "Unmounting succeded" "Unmounting failed"

unlockAction :: XPConfig -> String -> X ()
unlockAction xpconfig path = do
  keyFilePath <- keyFilePrompt xpconfig
  case keyFilePath of
    Nothing     -> return ()
    Just kfPath -> do
                     output <- runUDiskCtl ["unlock", "-b", path, "--key-file", kfPath, "--no-user-interaction"]
                     notify output path "Unlocking succeded" "Unlocking failed"

lockAction :: String -> X ()
lockAction path = do
  output <- runUDiskCtl ["lock", "-b", path, "--no-user-interaction"]
  notify output path "Locking succeded" "Locking failed"

loopSetupAction :: String -> X ()
loopSetupAction path = do
  output <- runUDiskCtl ["loop-setup", "-f", path, "--no-user-interaction"]
  notify output path "Seting up loop succeded" "Seting up loop failed"

loopDeleteAction :: String -> X ()
loopDeleteAction path = do
  output <- runUDiskCtl ["loop-delete", "-b", path, "--no-user-interaction"]
  notify output path "Deleting loop succeded" "Deleting loop failed"

-- utils
compgenFiles :: String -> IO String
compgenFiles s = runProcessWithInput "bash" [] $ "bind 'set completion-ignore-case on'; compgen -A file -- " ++ s ++ "\n"

notify :: String -> String -> String -> String -> X ()
notify output path success failure = if isPrefixOf "Error" output then sendWarning else sendNotification
  where sendNotification = spawn $ "notify-send -u normal \""   ++ success ++ "\" \"" ++ path ++ "\""
        sendWarning      = spawn $ "notify-send -u critical \"" ++ failure ++ "\" \"" ++ path ++ "\""

runUDiskCtl :: [String] -> X String
runUDiskCtl args = io $ runProcessWithInput "udisksctl" args []

parseDevice :: String -> Either String Device
parseDevice input = either (Left . show) Right (parse input input)

getDevices :: IO [Device]
getDevices = do
  output <- runProcessWithInput "lsblk" ["-o", "PATH,TYPE,HOTPLUG,SIZE,MOUNTPOINT", "--pairs"] []
  case sequence $ parseDevice <$> lines output of
    Left err -> print err >> fail err
    Right ds -> return ds
