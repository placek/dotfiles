import Data.Monoid
import System.Exit
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myBorderWidth        = 4
myClickJustFocuses   :: Bool
myClickJustFocuses   = False
myEventHook          = mempty
myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True
myFocusedBorderColor = "#3498DB"
myModMask            = mod1Mask -- mod4Mask.
myNormalBorderColor  = "#2C3E50"
myTerminal           = "urxvt"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)                 -- launch a terminal
    , ((modm,               xK_p     ), spawn "rofi -show combi")                     -- launch drun menu
    , ((modm .|. shiftMask, xK_c     ), kill)                                         -- close focused window
    , ((modm,               xK_space ), sendMessage NextLayout)                       -- rotate through the available layouts
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)           -- reset the layouts on the current workspace to default
    , ((modm,               xK_n     ), refresh)                                      -- resize viewed windows to the correct size
    , ((modm,               xK_j     ), windows W.focusDown)                          -- move focus to the next window
    , ((modm,               xK_k     ), windows W.focusUp  )                          -- move focus to the previous window
    , ((modm,               xK_m     ), windows W.focusMaster  )                      -- move focus to the master window
    , ((modm,               xK_Return), windows W.swapMaster)                         -- swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )                         -- swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )                         -- swap the focused window with the previous window
    , ((modm,               xK_h     ), sendMessage Shrink)                           -- shrink the master area
    , ((modm,               xK_l     ), sendMessage Expand)                           -- expand the master area
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)               -- push window back into tiling
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))                   -- increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))                -- deincrement the number of windows in the master area
    , ((modm              , xK_b     ), sendMessage ToggleStruts)                     -- toggle the status bar gap
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))                    -- quit xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- restart xmonad
    , ((modm .|. shiftMask, xK_l     ), spawn "slock")                                -- lock screen
    ]
    ++
    -- mod-[1..9], switch to workspace N
    -- mod-shift-[1..9], move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w       -- set the window to floating mode and move by dragging
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster)) -- raise the window to the top of the stack
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w     -- set the window to floating mode and resize by dragging
                                       >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = avoidStruts . spacingRaw False (Border 2 2 2 2) True (Border 2 2 2 2) True $ layoutHook desktopConfig

myManageHook = composeAll [ className =? "Gimp" --> doFloat ]

myWorkspaces :: [String]
myWorkspaces = clickable . (fmap xmobarEscape) . (fmap show) $ workspaces
  where clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" | (i,ws) <- zip workspaces l, let n = i ]
        workspaces  = [1..7]
        xmobarEscape = concatMap doubleLts
          where doubleLts '<' = "<<"
                doubleLts x   = [x]

myLogHook xmproc = dynamicLogWithPP xmobarPP { ppOutput          = hPutStrLn xmproc
                                             , ppCurrent         = xmobarColor "#3498DB" "" . wrap "[" "]"
                                             , ppHiddenNoWindows = xmobarColor "#ECF0F1" ""
                                             , ppTitle           = xmobarColor "#2ECC71"  "" . shorten 40
                                             , ppVisible         = wrap "(" ")"
                                             , ppUrgent          = xmobarColor "#E74C3C" "#F1C40F"
                                             }
myStartupHook = do
  spawnOnce "xrdb -merge .Xresources &"
  spawnOnce "feh --randomize --bg-fill .wall/* &"

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks (defaults xmproc)

defaults xmproc = desktopConfig { terminal           = myTerminal
                                , focusFollowsMouse  = myFocusFollowsMouse
                                , clickJustFocuses   = myClickJustFocuses
                                , borderWidth        = myBorderWidth
                                , modMask            = myModMask
                                , workspaces         = myWorkspaces
                                , normalBorderColor  = myNormalBorderColor
                                , focusedBorderColor = myFocusedBorderColor
                                , keys               = myKeys
                                , mouseBindings      = myMouseBindings
                                , layoutHook         = myLayout
                                , manageHook         = manageDocks <+> myManageHook <+> manageHook desktopConfig
                                , handleEventHook    = myEventHook
                                , logHook            = myLogHook xmproc
                                , startupHook        = myStartupHook
                                }
