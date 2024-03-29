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

import Graphics.X11.ExtraTypes ( xF86XK_AudioLowerVolume
                               , xF86XK_AudioRaiseVolume
                               , xF86XK_AudioMute
                               , xF86XK_AudioPlay
                               , xF86XK_AudioNext
                               , xF86XK_AudioPrev
                               )

import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Hooks.DynamicLog ( ppLayout, ppTitle, ppVisible, ppCurrent, ppOutput, dynamicLogWithPP, wrap)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (doFloatAt)
import XMonad.Layout.Grid (Grid (GridRatio))
import XMonad.Layout.Magnifier (magnifierczOff, magnifierczOff', MagnifyMsg(Toggle, ToggleOff, MagnifyMore, MagnifyLess))
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Spacing (Border(Border), bottom, left, top, right, spacingRaw)
import XMonad.Layout.Spiral (spiralWithDir, Direction(East), Rotation(CW))
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeCol))
import XMonad.Prompt (position, bgColor, borderColor, height, promptBorderWidth, historySize, XPPosition(Top))
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.StackSet (RationalRect(RationalRect), focusDown, focusUp, swapUp, swapDown, focusMaster, swapMaster, sink, greedyView, shift, view, shiftMaster)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.NamedScratchpad (customFloating, NamedScratchpad(NS), namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

import qualified Data.Map        as M
import XMonad.Actions.UpdatePointer (updatePointer)
import Control.Monad (mapM_)
import XMonad.Hooks.FadeInactive (fadeOut, fadeIn)

home = "/home/james"
xmonadDir = home ++ "/.xmonad"
lockCmd = "systemctl suspend; betterlockscreen -l"

exitPrompt :: String -> X () -> X ()
exitPrompt = confirmPrompt $ def
  { position          = Top
  , bgColor           = "#cc241d"
  , borderColor       = "#fb4934"
  , height            = 200
  , promptBorderWidth = 7
  , historySize       = 0
  }

floatRectBig = customFloating $ RationalRect (2/6) (1/6) (2/6) (4/6)
floatRectSmol = customFloating $ RationalRect (2/12) (1/12) (2/12) (3/12)
scratchpads =
  [ NS "btop"      "kitty --name btop btop" (resource=? "btop"     ) floatRectBig
  , NS "1password" "1password"              (resource=? "1password") floatRectBig
  ]

myModMask = mod1Mask .|. shiftMask -- shift + alt

data RofiMode = Drun | Run | Emoji
rofi :: RofiMode -> String
rofi mode = "rofi" ++ combi ++ " -show " ++ _mode ++ " -show-icons"
  where
    (combi, _mode) =
      case mode of
        Drun  -> (" -combi-modi window,drun", "combi")
        Run   -> ("", "run -run-command '{cmd}'")
        Emoji -> (" -modi emoji", "emoji")

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--


resetMagAndChangeLayout :: Message a => a -> X ()
resetMagAndChangeLayout msg = sendMessage ToggleOff >> sendMessage msg

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  -- for my ergodox the mod keys are as follows
  -- modm left thumb left
  -- modm2 left thumb right
  -- modm3 right thumb left
  -- modm4 right thumb right
  let modm2 = modm .|. controlMask
      modm3 = modm .|. mod4Mask
      modm4 = modm .|. mod4Mask .|. controlMask
  in
  M.fromList $
    [ ((shiftMask .|. controlMask, xK_l), nextWS)
    , ((shiftMask .|. controlMask, xK_h), prevWS)

    -- MOD1: shift + alt

    -- modm w, e, r switch screen focus
    -- modm 1..9 switch workplace focus
    , ((modm, xK_n      ), refresh) -- Resize viewed windows to the correct size
    , ((modm, xK_Tab    ), windows focusDown) -- Move focus to the next window
    , ((modm, xK_j      ), windows focusDown) -- Move focus to the next window
    , ((modm, xK_k      ), windows focusUp  ) -- Move focus to the previous window
    , ((modm, xK_m      ), windows focusMaster  ) -- Move focus to the master window
    , ((modm, xK_Return ), windows swapMaster) -- Swap the focused window and the master window
    , ((modm, xK_h      ), sendMessage Shrink) -- Shrink the master area
    , ((modm, xK_l      ), sendMessage Expand) -- Expand the master area
    , ((modm, xK_t      ), withFocused $ windows . sink) -- Push window back into tiling
    , ((modm, xK_space  ), resetMagAndChangeLayout NextLayout) -- Rotate through the available layout algorithms
    , ((modm, xK_comma  ), sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
    , ((modm, xK_period ), sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area
    , ((modm, xK_q      ), spawn $ "xmonad --recompile && xmonad --restart") -- Restart xmonad

    -- MOD2: control + shift + alt

    -- modm2 w, e, r move to screen
    -- modm2 1..9  move to workplace
    , ((modm2, xK_Return), spawn $ XMonad.terminal conf) -- launch a terminal
    , ((modm2, xK_space ), setLayout $ XMonad.layoutHook conf) -- Reset the layouts on the current workspace to default
    , ((modm2, xK_j     ), windows swapDown  ) -- Swap the focused window with the next window
    , ((modm2, xK_k     ), windows swapUp    ) -- Swap the focused window with the previous window
    , ((modm2, xK_q     ), exitPrompt "exit" $ spawn "killall picom xidlehook redshift nitrogen" >> io exitSuccess) -- Quit xmonad
    , ((modm2, xK_c     ), kill) -- close focused window
    , ((modm2, xK_t     ), withFocused $ fadeOut 0.5) -- make active window transparent
    , ((modm2, xK_o     ), withFocused fadeIn) -- make active window opaque
    , ((modm2, xK_f     ), spawn "kitty ranger") -- launch ranger

    -- MOD3: meta + shift + alt

    , ((modm3, xK_l     ), spawn $ rofi Drun) -- select desktop app
    , ((modm3, xK_r     ), spawn $ rofi Run) -- run command
    , ((modm3, xK_e     ), spawn $ rofi Emoji) -- emoji picker
    , ((modm3, xK_3     ), spawn $ "maim -s " ++ home ++ "/screenshot-$(date +'%Y-%m-%d_%H-%M-%S').png") -- screenshot
    , ((modm3, xK_4     ), spawn "maim -s | xclip -selection clipboard -t image/png") -- screenshot to clipboard
    , ((modm3, xK_t     ), namedScratchpadAction scratchpads "btop")
    , ((modm3, xK_p     ), namedScratchpadAction scratchpads "1password")
    , ((modm3, xK_m     ), sendMessage Toggle) -- toggle magnification of non-fullscreen layouts
    , ((modm3, xK_equal ), sendMessage MagnifyMore) -- increase magnification 
    , ((modm3, xK_minus ), sendMessage MagnifyLess) -- decrease magnification

    -- MOD4: meta + control + shift + alt

    , ((modm4, xK_f     ), resetMagAndChangeLayout $ JumpToLayout "full") -- jump to full
    , ((modm4, xK_g     ), resetMagAndChangeLayout $ JumpToLayout "grid") -- jump to grid
    , ((modm4, xK_s     ), spawn lockCmd) -- lock and suspend
    ]

    ++

    -- volume/mute control
    -- doesnt use mod keys
    map (\(keyCode, flag) ->
      ((0, keyCode), spawn (xmonadDir ++ "/volume.sh " ++ flag))
    ) [(xF86XK_AudioRaiseVolume, "-u"), (xF86XK_AudioLowerVolume, "-d"), (xF86XK_AudioMute, "-m")]++

    -- play/pause/next/previous
    map (\(keyCode, command) ->
      ((0, keyCode), spawn ("playerctl " ++ command))
    ) [(xF86XK_AudioPlay, "play-pause"), (xF86XK_AudioNext, "next"), (xF86XK_AudioPrev, "previous")] ++

    --
    -- modm-[1..9], Switch to workspace N
    -- modm2-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(greedyView, 0), (shift, controlMask)]]
    ++

    --
    -- modm-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- modm2-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f) >> updatePointer (0.5, 0.5) (0,0))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(view, 0), (shift, controlMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
    [ ((modm, button1),
        \w -> focus w >> mouseMoveWindow w >> windows shiftMaster) -- mod-button1, Set the window to floating mode and move by dragging
    , ((modm, button2),
        \w -> focus w >> windows shiftMaster) -- mod-button2, Raise the window to the top of the stack
    , ((modm, button3),
        \w -> focus w >> mouseResizeWindow w >> windows shiftMaster) -- mod-button3, Set the window to floating mode and resize by dragging

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout =
  smartBorders
  (   named "grid"   (withGapMag grid)
  ||| named "tall"   (withGapMag tiled)
  ||| named "3col"   (withGapMag threeCol)
  ||| named "spiral" (withGapMag spiral)
  ||| named "fullg"  (avoidStruts full)
  ||| named "full"   full
  )
    where
      full = noBorders Full
      withGapMag = avoidStruts . _spacingRaw 10 . magnifierczOff 1.15

      -- percentage per resize message
      r         = 3 / 100

      -- (7/4) resolves to a 2x2 grid on ultrawide monitors.
      -- might need to change to (3/2) for normaler aspect ratios
      grid      = GridRatio (7/4)
      tiled     = Tall 1 r (2/3)
      threeCol  = ThreeCol 1 r (1/2)
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
    , className =? "Peek"           --> doFloatLeft
    , className =? "Yad"            --> doFloat -- this is for gksu
    , resource  =? "protonvpn"      --> doFloatLeft
    , resource  =? "btop"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , namedScratchpadManageHook scratchpads
    ]
      where doFloatLeft = doFloatAt 0 0

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


startupCmds =
  [ xmonadDir ++ "/initxmonad.sh &"
  , xmonadDir ++ "/setupMonitors.sh &"
  , "nitrogen --restore &"
  , "(picom --config " ++ xmonadDir ++ "/picom-xmonad.conf)&"
  , "(dunst -config " ++ xmonadDir ++ "/dunstrc) &"
  , "redshift -P &"
  , xidlehook 600 lockCmd ""
  , xidlehook 540 lockNotif clearNotif
  , "nm-applet &"
  , "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
  ]
    where
      xidlehook time cmd canceller = "xidlehook --timer " ++ show time ++ " '" ++ cmd ++ "' '" ++ canceller ++ "' --not-when-audio --not-when-fullscreen --detect-sleep &"
      lockNotif = "dunstify -A \"action,label\" -u \"critical\" -a \"hey\" \"computer will lock in 1 minute\" -r 12345"
      clearNotif = "dunstify -C 12345"

myStartupHook = do
  setDefaultCursor xC_left_ptr
  mapM_ spawnOnce startupCmds

main = do
  xmobarProc <- spawnPipe ("xmobar -x 0 " ++ xmonadDir ++ "/xmobar.hs")
  xmonad $ docks $ ewmhFullscreen $ ewmh $ def {
      -- simple stuff
        terminal           = "kitty",
        focusFollowsMouse  = False,
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
