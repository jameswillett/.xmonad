--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.Cursor
import XMonad.Util.Dzen
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Actions.CycleWS
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

home = "/home/james"
xmonadDir = home ++ "/.xmonad"

floatRectBig = customFloating $ W.RationalRect (2/6) (1/6) (2/6) (4/6)
floatRectSmol = customFloating $ W.RationalRect (2/12) (1/12) (2/12) (3/12)
scratchpads =
  [ NS "btop"      "kitty --name btop btop" (resource=? "btop"     ) floatRectBig
  , NS "1password" "1password"              (resource=? "1password") floatRectBig
  , NS "emote"     "emote"                  (resource=? "emote"    ) floatRectSmol
  ]

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask .|. shiftMask

alert :: String -> X ()
alert message = dzenConfig centered message
  where
    windowW = (*20) $ length message
    centered =
          onCurr (center windowW 100)
          >=> font "-*-monospace-*-r-*-*-30-*-*-*-*-*-*-*"
          >=> addArgs ["-fg", "#ffd7af"]
          >=> addArgs ["-bg", "#262626"]

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

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm2, xK_Return), spawn $ XMonad.terminal conf)

    , ((modm3, xK_l     ), spawn $ rofi Drun)

    , ((modm3, xK_r     ), spawn $ rofi Run)

    , ((modm3, xK_e     ), spawn $ rofi Emoji)

    -- close focused window
    , ((modm2, xK_c     ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    -- jump to full/grid
    , ((modm4, xK_f), sendMessage $ JumpToLayout "full")
    , ((modm4, xK_g), sendMessage $ JumpToLayout "grid")

    --  Reset the layouts on the current workspace to default
    , ((modm2, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm2, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm2, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm2, xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn $ xmonadDir ++ "/recompile.sh")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm2, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    , ((modm3, xK_3 ), spawn ("import " ++ home ++ "/screenshot-$(date +'%Y-%m-%d_%H-%M-%S').png"))
    , ((modm4, xK_s ), spawn "systemctl suspend; betterlockscreen -l")

    , ((shiftMask .|. controlMask, xK_l), nextWS)
    , ((shiftMask .|. controlMask, xK_h), prevWS)
    , ((modm3, xK_t), namedScratchpadAction scratchpads "btop")
    , ((modm3, xK_p), namedScratchpadAction scratchpads "1password")
    ]
    ++

    map (\(keyCode, flag) ->
      ((0, keyCode), runProcessWithInput (xmonadDir ++ "/volume.sh") [flag] "" >>= alert)
    ) [(0x1008FF13, "-u"), (0x1008FF11, "-d"), (0x1008FF12, "-m")]++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, controlMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =
  smartBorders
  (   named "grid"   (withGap grid)
  ||| named "tall"   (withGap tiled)
  ||| named "fullg"  (withGap $ noBorders Full)
  ||| named "full"   (noBorders Full)
  )
    where
      withGap l = avoidStruts $ _spacingRaw 10 l

      -- (7/4) resolves to a 2x2 grid on ultrawide monitors.
      -- might need to change to (3/2) for normaler aspect ratios
      grid      = GridRatio (7/4)
      tiled     = Tall 1 (3 / 100) (2/3)

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

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
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

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawnOnce $ xmonadDir ++ "/initxmonad.sh &"
  spawnOnce $ xmonadDir ++ "/setupMonitors.sh &"
  spawnOnce "nitrogen --restore &"


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmobarProc <- spawnPipe ("xmobar -x 0 " ++ xmonadDir ++ "/xmobar.hs")
  xmonad $ docks $ ewmh def {
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

-- | Finally, a copy of the default bindings in simple textual tabular format.
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
