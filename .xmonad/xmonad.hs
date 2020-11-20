import Data.Monoid
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myBorderWidth        = 4
myClickJustFocuses   = False
myEventHook          = mempty
myFocusFollowsMouse  = True
myFocusedBorderColor = "#3498DB"
myModMask            = mod1Mask -- mod4Mask.
myNormalBorderColor  = "#2C3E50"
myTerminal           = "termonad"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)                  -- launch a terminal
    , ((modm .|. shiftMask, xK_p     ), spawn "rofi -show combi")                      -- launch drun menu
    , ((modm,               xK_p     ), shellPrompt dtXPConfig)                        -- launch prompt
    , ((modm .|. shiftMask, xK_c     ), kill)                                          -- close focused window
    , ((modm,               xK_space ), sendMessage NextLayout)                        -- rotate through the available layouts
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)            -- reset the layouts on the current workspace to default
    , ((modm,               xK_n     ), refresh)                                       -- resize viewed windows to the correct size
    , ((modm,               xK_j     ), windows W.focusDown)                           -- move focus to the next window
    , ((modm,               xK_k     ), windows W.focusUp  )                           -- move focus to the previous window
    , ((modm,               xK_m     ), windows W.focusMaster  )                       -- move focus to the master window
    , ((modm,               xK_Return), windows W.swapMaster)                          -- swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )                          -- swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )                          -- swap the focused window with the previous window
    , ((modm,               xK_h     ), sendMessage Shrink)                            -- shrink the master area
    , ((modm,               xK_l     ), sendMessage Expand)                            -- expand the master area
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)                -- push window back into tiling
    , ((modm .|. shiftMask, xK_z     ), killAllOtherCopies)                            -- toggle window state back by killing all copies
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))                    -- increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))                 -- deincrement the number of windows in the master area
    , ((modm              , xK_Right ), nextWS)                                        -- go to next workspace
    , ((modm              , xK_Left  ), prevWS)                                        -- go to previous workspace
    , ((modm              , xK_b     ), sendMessage ToggleStruts)                      -- toggle the status bar gap
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))                     -- quit xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")  -- restart xmonad
    , ((modm .|. shiftMask, xK_x     ), spawn "xkill")                                 -- xkill
    , ((modm .|. shiftMask, xK_4     ), spawn "scrot -q100 /tmp/ss_%Y%m%d_%H%M%S.png") -- screenshot
    ]
    ++
    -- mod-[1..4], switch to workspace N
    -- mod-shift-[1..4], move client to workspace N
    -- mod-shift-control-[1..4], copy client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1..]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
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
    ]

dtXPConfig :: XPConfig
dtXPConfig = def
      { font                = "xft:Iosevka:size=12:antialias=true:hinting=true"
      , bgColor             = "#2C3E50"
      , fgColor             = "#F5F5F5"
      , bgHLight            = "#3498DB"
      , fgHLight            = "#2C3E50"
      , borderColor         = "#3498DB"
      , promptBorderWidth   = 0
      , promptKeymap        = dtXPKeymap
      , position            = Top
   -- , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
   -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
   -- , defaultPrompter     = unwords . map reverse . words  -- reverse the prompt
   -- , defaultPrompter     = drop 5 .id (++ "XXXX: ")  -- drop first 5 chars of prompt and add XXXX:
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to 'Just 5' for 5 rows
      }

promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             , ("s", sshPrompt)          -- ssh prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             ]

dtXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
dtXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

myLayout = avoidStruts . spacingRaw False (Border 2 2 2 2) True (Border 2 2 2 2) True $ t ||| s ||| m ||| f
  where
    f       = Full
    m       = Mirror t
    s       = spiral (6/7)
    t       = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 2/3
    delta   = 3/100

myManageHook = composeAll [ className =? "Gimp" --> doFloat ]

workspaceNames :: [String]
workspaceNames = ["web", "dev", "misc", "kee"]

myWorkspaces :: [String]
myWorkspaces = fmap clickable (zip [1..] workspaceNames)
  where clickable (k, w) = xmobarAction ("xdotool key alt+" ++ show k) "1" w

myLogHook xmproc = dynamicLogWithPP xmobarPP { ppOutput          = hPutStrLn xmproc
                                             , ppCurrent         = xmobarColor "#5F5F5F" myFocusedBorderColor . wrap " " " "
                                             , ppHidden          = xmobarColor myFocusedBorderColor "" . wrap " " " "
                                             , ppHiddenNoWindows = wrap " " " "
                                             , ppVisible         = wrap "(" ")"
                                             , ppTitle           = xmobarColor "#F1C40F"  "" . shorten 40
                                             , ppLayout          = xmobarAction "xdotool key alt+space" "1" . layout
                                             , ppUrgent          = xmobarColor "#E74C3C" "#F1C40F"
                                             , ppWsSep           = ""
                                             , ppSep             = " \xE0B1 "
                                             }
  where layout a = case a of
          "Spacing Tall"        -> "tall"
          "Spacing Mirror Tall" -> "mtall"
          "Spacing Spiral"      -> "spiral"
          "Spacing Full"        -> "full"

myStartupHook = do
  spawnOnce "xsetroot -cursor_name left_ptr"
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
