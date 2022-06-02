import XMonad
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.ManageDocks (avoidStruts)

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

myManageHook :: ManageHook
myManageHook =
    composeAll
      [ className =? "Pavucontrol" --> doFloat
      ]

main :: IO ()
main = xmonad $ desktopConfig
    { terminal = "alacritty"
    , layoutHook = myLayout
    , manageHook = myManageHook <+> manageHook desktopConfig
    }
    `additionalKeysP`
    [ ("M-r", spawn "rofi -show drun") -- Alt+r => Application runner
    , ("C-S-4", spawn "flameshot gui") -- Ctrl+Shift+4 => Area selection screenshot
    ]
