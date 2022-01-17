module Prompt.UDisks (udisksPrompt) where

import           Data.List              (isPrefixOf)
import           System.Posix.Env       (getEnv)
import           XMonad.Core
import           XMonad.Prompt          (XPConfig(..), XPrompt(..),  XPType(..), mkXPromptWithModes)
import           XMonad.Prompt.Input    (inputPromptWithCompl)
import           XMonad.Util.Run        (runProcessWithInput, safeSpawn)

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

  completionFunction (UDisks Mount c) s = do
    devices <- getDevices
    let paths = devicePath <$> (filter (hasFileSystem .&. isNotMounted) devices)
    return $ filter (searchPredicate c $ s) paths

  completionFunction (UDisks Unmount c) s = do
    devices <- getDevices
    mUser <- getEnv "USER"
    case mUser of
      Just user -> do
        let paths = devicePath <$> (filter (hasFileSystem .&. isMounted user) devices)
        return $ filter (searchPredicate c $ s) paths
      Nothing -> return []

  completionFunction (UDisks Unlock c) s = do
    devices <- getDevices
    let paths = devicePath <$> (filter (isNotMounted .&. isLuks) devices)
    return $ filter (searchPredicate c $ s) paths

  completionFunction (UDisks Lock c) s = do
    devices <- getDevices
    let paths = devicePath <$> (filter (isNotMounted .&. isLuks) devices)
    return $ filter (searchPredicate c $ s) paths

  completionFunction (UDisks LoopSetup c) s = do
    files <- compgenFiles s
    return $ filter (searchPredicate c $ s) (lines files)

  completionFunction (UDisks LoopDelete c) s = do
    devices <- getDevices
    let paths = devicePath <$> (filter (isLoop .&. isNotMounted) devices)
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
mountAction path = runUDiskCtl ["mount", "-b", path, "--no-user-interaction"] >>= notify "Mounted"

unmountAction :: String -> X ()
unmountAction path = runUDiskCtl ["unmount", "-b", path, "--no-user-interaction"] >>= notify "Unmounted"

unlockAction :: XPConfig -> String -> X ()
unlockAction xpconfig path = do
  keyFilePath <- keyFilePrompt xpconfig
  case keyFilePath of
    Nothing     -> return ()
    Just kfPath -> runUDiskCtl ["unlock", "-b", path, "--key-file", kfPath, "--no-user-interaction"] >>= notify "Unlocked"

lockAction :: String -> X ()
lockAction path = runUDiskCtl ["lock", "-b", path, "--no-user-interaction"] >>= notify "Locked"

loopSetupAction :: String -> X ()
loopSetupAction path = runUDiskCtl ["loop-setup", "-f", path, "--no-user-interaction"] >>= notify "Mapped"

loopDeleteAction :: String -> X ()
loopDeleteAction path = runUDiskCtl ["loop-delete", "-b", path, "--no-user-interaction"] >>= notify ""

-- utils
compgenFiles :: String -> IO String
compgenFiles s = runProcessWithInput "bash" [] $ "bind 'set completion-ignore-case on'; compgen -A file -- " ++ s ++ "\n"

notify :: String -> String -> X ()
notify prefix output = safeSpawn "notify-send" ["-u", kind, "-i", "/run/current-system/sw/share/icons/Paper/scalable/devices/drive-harddisk-usb-symbolic.svg", "UDisks", output]
  where kind = if isPrefixOf prefix output
               then "normal"
               else "critical"

runUDiskCtl :: [String] -> X String
runUDiskCtl args = io $ runProcessWithInput "udisksctl" args []

parseDevice :: String -> Either String Device
parseDevice input = either (Left . show) Right (parse input input)

getDevices :: IO [Device]
getDevices = do
  output <- runProcessWithInput "lsblk" ["-o", "PATH,TYPE,HOTPLUG,SIZE,MOUNTPOINT,FSTYPE", "--pairs"] []
  case sequence $ parseDevice <$> lines output of
    Left err -> print err >> fail err
    Right ds -> return ds
