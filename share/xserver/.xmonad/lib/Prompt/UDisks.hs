module Prompt.UDisks (udisksPrompt) where

import           XMonad.Core
import           XMonad.Prompt
import           XMonad.Util.Run        (runProcessWithInput)

import           Prompt.UDisks.Device

data PromptType = Mount | Unmount | Unlock | Lock

instance Show PromptType where
  show Mount   = "Mount device"
  show Unmount = "Unmount device"
  show Unlock  = "Unlock crypted device"
  show Lock    = "Lock crypted device"

data UDisks = UDisks PromptType XPConfig

instance XPrompt UDisks where
  showXPrompt (UDisks t _) = show t

  commandToComplete _ c = c

  completionFunction (UDisks Unmount c) s = do
    devices <- getDevices
    let paths = devicePath <$> filter isMounted devices
    return $ filter (searchPredicate c $ s) paths

  completionFunction (UDisks _ c) s = do
    devices <- getDevices
    let paths = devicePath <$> filter (not . isMounted) devices
    return $ filter (searchPredicate c $ s) paths

  modeAction (UDisks t _) a _ = actionFor t a
    where actionFor :: PromptType -> String -> X ()
          actionFor promptType = case promptType of
            Mount   -> mountAction
            Unmount -> unmountAction
            Unlock  -> unlockAction
            Lock    -> lockAction

-- prompt
udisksPrompt :: XPConfig -> X ()
udisksPrompt xpconfig = mkXPromptWithModes ([ XPT $ UDisks Mount xpconfig
                                            , XPT $ UDisks Unmount xpconfig
                                            , XPT $ UDisks Unlock xpconfig
                                            , XPT $ UDisks Lock xpconfig
                                            ]) xpconfig

-- actions
mountAction :: String -> X ()
mountAction = runUDiskCtl "mount"

unmountAction :: String -> X ()
unmountAction = runUDiskCtl "unmount"

unlockAction :: String -> X ()
unlockAction = runUDiskCtl "unlock"

lockAction :: String -> X ()
lockAction = runUDiskCtl "lock"

-- utils
runUDiskCtl :: String -> String -> X ()
runUDiskCtl command path = do
  _ <- io $ runProcessWithInput "udisksctl" [command, "-b", path, "--no-user-interaction"] []
  return ()

parseDevice :: String -> Either String Device
parseDevice input = either (Left . show) Right (parse input input)

parseDevices :: String -> Either String [Device]
parseDevices input = sequence $ parseDevice <$> lines input

filterOutHotplugPartitions :: [Device] -> [Device]
filterOutHotplugPartitions devices = filterHotplugs . filterPartitions $ devices
  where filterHotplugs   = filter isHotplug
        filterPartitions = filter isPartition

getDevices :: IO [Device]
getDevices = do
  output <- runProcessWithInput "lsblk" ["-o", "PATH,TYPE,HOTPLUG,SIZE,MOUNTPOINT", "--pairs"] []
  case parseDevices output of
    Left err -> print err >> fail err
    Right ds -> return . filterOutHotplugPartitions $ ds
