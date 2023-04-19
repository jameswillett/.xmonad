--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import Data.Monoid
import System.Exit

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

home = "/home/james"
xmonadDir = home ++ "/.xmonad"

exitPrompt :: String -> X () -> X ()
exitPrompt = confirmPrompt $ def
  { position          = Top
  , bgColor           = "#cc241d"
  , borderColor       = "#fb4934"
  , height            = 200
  , promptBorderWidth = 7
  , historySize       = 0
  }

floatRectBig = customFloating $ W.RationalRect (2/6) (1/6) (2/6) (4/6)
floatRectSmol = customFloating $ W.RationalRect (2/12) (1/12) (2/12) (3/12)
scratchpads =
  [ NS "btop"      "kitty --name btop btop" (resource=? "btop"     ) floatRectBig
  , NS "1password" "1password"              (resource=? "1password") floatRectBig
  , NS "emote"     "emote"                  (resource=? "emote"    ) floatRectSmol
  ]

myModMask = mod1Mask .|. shiftMask -- shift + alt

data RofiMode = Drun | Run | Emoji
rofi :: RofiMode -> String
rofi mode = "rofi" ++ combi ++ " -show " ++ _mode ++ " -show-icons"
  where
    (combi, _mode) =
      case mode of
        Drun  -> (" -combi-modi window,drun", "combi")
        Run   -> ("", "run")
        Emoji -> (" -modi emoji", "emoji")

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- for my ergodox the mod keys are as follows
-- modm bottom left thumb
-- modm2 bottom right thumb
-- modm3 top left thumb
-- modm4 top right thumb
modm2 = myModMask .|. controlMask
modm3 = myModMask .|. mod4Mask
modm4 = modm3 .|. controlMask

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ ((shiftMask .|. controlMask, xK_l), nextWS)
    , ((shiftMask .|. controlMask, xK_h), prevWS)

    -- MOD1: shift + alt

    -- modm w, e, r switch screen focus
    -- modm 1..9 switch workplace focus
    , ((modm, xK_n      ), refresh) -- Resize viewed windows to the correct size
    , ((modm, xK_Tab    ), windows W.focusDown) -- Move focus to the next window
    , ((modm, xK_j      ), windows W.focusDown) -- Move focus to the next window
    , ((modm, xK_k      ), windows W.focusUp  ) -- Move focus to the previous window
    , ((modm, xK_m      ), windows W.focusMaster  ) -- Move focus to the master window
    , ((modm, xK_Return ), windows W.swapMaster) -- Swap the focused window and the master window
    , ((modm, xK_h      ), sendMessage Shrink) -- Shrink the master area
    , ((modm, xK_l      ), sendMessage Expand) -- Expand the master area
    , ((modm, xK_t      ), withFocused $ windows . W.sink) -- Push window back into tiling
    , ((modm, xK_space  ), sendMessage NextLayout) -- Rotate through the available layout algorithms
    , ((modm, xK_comma  ), sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
    , ((modm, xK_period ), sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area
    , ((modm, xK_q      ), spawn $ xmonadDir ++ "/recompile.sh") -- Restart xmonad

    -- MOD2: control + shift + alt

    -- modm2 w, e, r move to screen
    -- modm2 1..9  move to workplace
    , ((modm2, xK_Return), spawn $ XMonad.terminal conf) -- launch a terminal
    , ((modm2, xK_space ), setLayout $ XMonad.layoutHook conf) -- Reset the layouts on the current workspace to default
    , ((modm2, xK_j     ), windows W.swapDown  ) -- Swap the focused window with the next window
    , ((modm2, xK_k     ), windows W.swapUp    ) -- Swap the focused window with the previous window
    , ((modm2, xK_q     ), exitPrompt "exit" $ io exitSuccess) -- Quit xmonad
    , ((modm2, xK_c     ), kill) -- close focused window

    -- MOD3: meta + shift + alt

    , ((modm3, xK_l     ), spawn $ rofi Drun) -- select desktop app
    , ((modm3, xK_r     ), spawn $ rofi Run) -- run command
    , ((modm3, xK_e     ), spawn $ rofi Emoji) -- emoji picker
    , ((modm3, xK_3     ), spawn ("import " ++ home ++ "/screenshot-$(date +'%Y-%m-%d_%H-%M-%S').png")) -- screenshot
    , ((modm3, xK_t     ), namedScratchpadAction scratchpads "btop")
    , ((modm3, xK_p     ), namedScratchpadAction scratchpads "1password")

    -- MOD4: meta + control + shift + alt

    , ((modm4, xK_f     ), sendMessage $ JumpToLayout "full") -- jump to full
    , ((modm4, xK_g     ), sendMessage $ JumpToLayout "grid") -- jump to grid
    , ((modm4, xK_s     ), spawn "systemctl suspend; betterlockscreen -l") -- lock and suspend
    ]

    ++

    -- volume/mute control
    -- doesnt use mod keys
    map (\(keyCode, flag) ->
      ((0, keyCode), spawn (xmonadDir ++ "/volume.sh " ++ flag))
    ) [(0x1008FF13, "-u"), (0x1008FF11, "-d"), (0x1008FF12, "-m")]++

    --
    -- modm-[1..9], Switch to workspace N
    -- modm2-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]
    ++

    --
    -- modm-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- modm2-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, controlMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
    [ ((modm, button1),
        \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster) -- mod-button1, Set the window to floating mode and move by dragging
    , ((modm, button2),
        \w -> focus w >> windows W.shiftMaster) -- mod-button2, Raise the window to the top of the stack
    , ((modm, button3),
        \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) -- mod-button3, Set the window to floating mode and resize by dragging

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout =
  smartBorders
  (   named "grid"   (withGap grid)
  ||| named "tall"   (withGap tiled)
  ||| named "3col"   (withGap threeCol)
  ||| named "spiral" (withGap spiral)
  ||| named "fullg"  (withGap $ noBorders Full)
  ||| named "full"   (noBorders Full)
  )
    where
      withGap l = avoidStruts $ _spacingRaw 10 l

      -- percentage per resize message
      r         = 3 / 100

      -- (7/4) resolves to a 2x2 grid on ultrawide monitors.
      -- might need to change to (3/2) for normaler aspect ratios
      grid      = GridRatio (7/4)
      tiled     = Tall 1 r (2/3)
      threeCol  = ThreeColMid 1 r (1/2)
      spiral    = spiralWithDir East CW (3/4)

      sb          px = Border { top = px, bottom = px, left = px, right = px }
      _spacingRaw px = spacingRaw False (sb px) True (sb px) True

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "btop"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , namedScratchpadManageHook scratchpads
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

clampLength :: Int -> String -> String
clampLength n string = clamped ++ (if string /= clamped then ellipses else "")
  where
    ellipses = "..."
    breakpoint = n - 4
    clamped = take breakpoint string


myLogHook handle = dynamicLogWithPP $ def
  { ppLayout  = wrap "(<fc=#e4b63c>"                  "</fc>)"
  , ppTitle   = wrap "<fc=#cccccc>"                   "</fc>" . clampLength 128
  , ppVisible = wrap "("                              ")"
  , ppCurrent = wrap "<fc=#b8473d>[</fc><fc=#7cac7a>" "</fc><fc=#b8473d>]</fc>"
  , ppOutput  = hPutStrLn handle
  }

myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawnOnce $ xmonadDir ++ "/initxmonad.sh &"
  spawnOnce $ xmonadDir ++ "/setupMonitors.sh &"
  spawnOnce "nitrogen --restore &"
  spawnOnce $ "(picom --config " ++ xmonadDir ++ "/picom-xmonad.conf)&"
  spawnOnce $ "(dunst -config " ++ xmonadDir ++ "/dunstrc) &"


main = do
  xmobarProc <- spawnPipe ("xmobar -x 0 " ++ xmonadDir ++ "/xmobar.hs")
  xmonad $ docks $ ewmhFullscreen . ewmh $ def {
      -- simple stuff
        terminal           = "kitty",
        focusFollowsMouse  = True,
        clickJustFocuses   = False,
        borderWidth        = 6,
        modMask            = myModMask,
        workspaces         = map show [1..9],
        normalBorderColor  = "#2d3b41",
        focusedBorderColor = "#7cac7a",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook xmobarProc,
        startupHook        = myStartupHook
    }
