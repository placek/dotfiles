import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal           = "gnome-terminal"
myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True
myClickJustFocuses   :: Bool
myClickJustFocuses   = False
myBorderWidth        = 3
myModMask            = mod1Mask -- mod4Mask.
myWorkspaces         = ["web","term","irc","4","5","6","7","8","9"]
myNormalBorderColor  = "#2c3e50"
myFocusedBorderColor = "#3498db"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
     -- rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    -- move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    -- swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- toggle the status bar gap
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -- quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    -- run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     -- the default number of windows in the master pane
     nmaster = 1
     -- default proportion of screen occupied by master pane
     ratio   = 1/2
     -- percent of screen to increment by when resizing panes
     delta   = 3/100

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

myEventHook = mempty
myLogHook = return ()
myStartupHook = do
  spawnOnce "feh --bg-fill .wall.jpg &"

main = xmonad defaults

defaults = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
