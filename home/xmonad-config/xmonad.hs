import XMonad
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen)
import XMonad.Util.Scratchpad (scratchpadSpawnActionTerminal, scratchpadManageHookDefault)
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

myTerminal = "alacritty"

myLayout = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myManageHook =
    composeAll
      [ isFullscreen               --> doFullFloat
      , className =? "Pavucontrol" --> doFloat
      , className =? "pinentry"    --> doFloat
      , namedScratchpadManageHook myScratchPads
      ]

myScratchPads =
  [ NS "terminal" (myTerminal ++ " -t scratchpad") (title =? "scratchpad") defaultFloating
  ]

main :: IO ()
main = xmonad $ ewmhFullscreen $ desktopConfig
    { terminal = myTerminal
    , layoutHook = myLayout
    , manageHook = myManageHook <+> manageHook desktopConfig <+> scratchpadManageHookDefault
    }
    `additionalKeysP`
    [ ("M-r", spawn "rofi -show drun") -- Alt+r => Application runner
    , ("C-S-4", spawn "flameshot gui") -- Ctrl+Shift+4 => Area selection screenshot
    , ("<F12>", namedScratchpadAction myScratchPads "terminal") -- F12 => Toggle terminal scratchpad
    ]
