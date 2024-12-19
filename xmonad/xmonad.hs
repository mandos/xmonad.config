import qualified Data.Map as M
import XMonad as X
-- Actions

-- Hooks

-- Layouts

-- Utils
-- TODO: RemapKeysP is no in my version of xmonad-contrib (0.17)

import XMonad.Actions.FloatSnap (afterDrag, snapMagicMove, snapMagicResize)
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.PhysicalScreens (sendToScreen, viewScreen)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.SpawnOn (spawnOn)
import qualified XMonad.Actions.Submap as SM
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, setEwmhActivateHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook (doAskUrgent, focusUrgent)
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Layout.Fullscreen
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.ManageHook
import qualified XMonad.Prompt as P
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, removeKeysP)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, defaultFloating, namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Util.SpawnOnce (spawnOnOnce)
import XMonad.Util.Ungrab

main :: IO ()
main = xmonad . (setEwmhActivateHook doAskUrgent) . ewmhFullscreen . ewmh . xmobarProp $ myConfig

myConfig =
  def
    { focusFollowsMouse = True,
      handleEventHook = swallowEventHook (className =? "Alacritty") (return True),
      layoutHook = myLayoutHook,
      manageHook = myManageHook,
      modMask = mod4Mask,
      -- startupHook = myStartupHook,
      terminal = "alacritty",
      workspaces = ["1:term", "2:term", "3:term", "4:web", "5:web", "6:app", "7:app", "8:app", "9:app"],
      mouseBindings = newMouse
    }
    `removeKeysP` myRemovedKeys
    `additionalKeysP` myKeys
    `additionalKeys` [ ((mod4Mask .|. mask, key), f sc)
                       | (key, sc) <- zip [xK_w, xK_f, xK_p] [0 ..],
                         (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
                     ]

myRemovedKeys = ["M-q", "M-l", "M-h", "M-j", "M-k"] ++ ["M-w", "M-S-w", "M-e", "M-S-e", "M-r", "M-S-r"] ++ ["M-S-c"]

myKeys =
  [ ("M-<Return>", spawn "alacritty"),
    ("M-r", spawn "rofi -combi-modi run,ssh,drun -terminal alacritty -show-icons -matching fuzzy -show combi -modi combi"),
    ("M-s", SM.submap $ mySearchEngineMap $ S.promptSearchBrowser P.def "brave"),
    ("M-l", windows W.focusUp),
    ("M-h", windows W.focusDown),
    ("M-j", sendMessage Shrink),
    ("M-k", sendMessage Expand),
    ("M-u", focusUrgent),
    ("M-C-q", spawn "xmonad --recompile; pkill xmobar; xmonad --restart"),
    ("M-C-x", kill),
    ("M-C-s", SM.submap $ mySearchEngineMap $ S.selectSearchBrowser "brave"),
    -- Some applications
    ("M-C-w", spawn "brave"),
    ("M-C-p", spawn "env GNUPGHOME=$HOME/.gnupg rofi-pass"),
    -- Scratchpads
    ("M-M1-e", namedScratchpadAction myScratchpads "emacs"),
    ("M-M1-s", namedScratchpadAction myScratchpads "slack"),
    ("M-M1-b", namedScratchpadAction myScratchpads "btop")
  ]

myMouse x =
  [ ((mod4Mask, button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicMove (Just 50) (Just 50) w))),
    ((mod4Mask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicResize [L, R, U, D] (Just 50) (Just 50) w))),
    ((mod4Mask, button3), (\w -> focus w >> mouseResizeWindow w >> afterDrag (snapMagicResize [R, D] (Just 50) (Just 50) w)))
  ]

newMouse x = M.union (mouseBindings def x) (M.fromList (myMouse x))

-- Search Engines
mySearchEngineMap method =
  M.fromList
    [ ((0, xK_g), method S.google),
      ((0, xK_w), method S.wikipedia),
      ((0, xK_m), method S.maps),
      ((0, xK_y), method S.youtube),
      ((0, xK_d), method S.duckduckgo)
    ]

-- Layouts
myLayoutHook = mouseResize $ windowArrange $ layoutHook def

-- Scratchpads
myScratchpads =
  [ NS "slack" "slack" (className =? "Slack") (customFloating $ W.RationalRect 0.0 0.0 1 1),
    NS "emacs" "emacsclient -r" (className =? "Emacs") (customFloating $ W.RationalRect 0.0 0.0 1 1),
    NS "btop" "alacritty --class btop,Alacritty -e btop" (resource =? "btop") (customFloating $ W.RationalRect 0.0 0.0 1 1)
  ]

-- Startup Hook
myStartupHook = do
  spawnOnOnce "5:web" "brave"
  spawnOn "3:term" "alacritty"

-- Manage Hook
myManageHook :: ManageHook
myManageHook =
  manageHook def
    <+> namedScratchpadManageHook myScratchpads

-- composeAll
--   [ className =? "brave-Browser" --> doShift "5 web"
--   -- className =? "Emacs" --> doFloat
--   ]
