import Control.Monad
import Data.Monoid
import qualified Data.Map as M

import XMonad
import XMonad.Actions.OnScreen
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)


-- Dashboard
-- Workspace "9" runs the dash.sh. By pressing mod-`, it is made visible.
-- When mod-` is released, the previous workspace is again visible.
-- There is a small problem: when mod is released before `, the flashed
-- workspace stays current.
-- The key-repeat for ` is disabled so we can keep the flashed workspace
-- as long as we want.

import XMonad.Hooks.ManageDocks
import FlashWorkspace (flashWorkspace, flashbackWorkspace)
import SpawnOnceOn (spawnOnceOn)

main = do
  xmonad . docks $ def
    { modMask  = mod4Mask      -- Use Super_L (i.e. Windows) instead of Alt.
    , terminal = "xterm -b -1" -- Allows just enough space for 80 columns with
                               -- the font defined in configuration.nix.
                               -- (i.e. size=9)
    , focusedBorderColor = "#00ffff"

    , startupHook = do
        spawnOnceOn "9" "xterm -b 1 -e screen -c /home/thu/projects/tank/dashboard.screen"
        -- Disable key-repeat for 't', which has a hook for both press/release events.
        -- spawnOnce "xset -r 28"
        -- Disable key-repeat for '`', which has a hook for both press/release events.
        spawnOnce "xset -r 49"
        setWMName "LG3D" -- For Java-based stuff; Rider in my case.

    , handleEventHook = handleEventHook def `mappend` keyUpEventHook

    , layoutHook =
        avoidStruts
          $ noBorders (smartBorders (layoutHook def))

    , manageHook = manageSpawn <+> manageHook def
    }

      `additionalKeys`

    [ ((mod4Mask,               xK_BackSpace), spawn chromium)
    , ((mod4Mask .|. shiftMask, xK_BackSpace), spawn firefox)
    , ((mod4Mask .|. shiftMask, xK_l),         spawn "physlock")
    , ((mod4Mask,               xK_grave),     flashWorkspace)
    , ((mod4Mask .|. shiftMask, xK_k),         spawn kitty)
    , ((mod4Mask,               xK_b),         sendMessage ToggleStruts)
    ]

chromium = "chromium --force-device-scale-factor=1.5 https://start.duckduckgo.com"

firefox = "firefox https://start.duckduckgo.com"

kitty = "kitty -o include=/home/thu/projects/learn-kitty/Spiderman.conf -o font_size=8"


keyUpEventHook :: Event -> X All
keyUpEventHook e = handleKeyRelease e >> return (All True)

handleKeyRelease :: Event -> X ()
handleKeyRelease (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
  | t == keyRelease = withDisplay $ \dpy -> do
    s  <- io (keycodeToKeysym dpy code 0)
    mClean <- cleanMask m
    userCodeDef () $ whenJust (M.lookup (mClean, s) ks) id
handleKeyRelease _ = return ()

ks = M.fromList $
  [ ((mod4Mask, xK_grave), flashbackWorkspace)
    -- Flash back even is we already have released the mod key.
    -- I think this is ok since this will have an effect only when we stored
    -- the workspace previously.
  , ((0, xK_grave), flashbackWorkspace)
  ]
