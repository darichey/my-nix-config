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
import qualified XMonad.Util.Hacks as Hacks

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
      , className =? ".blueman-manager-wrapped"    --> doFloat
      , className =? "nm-connection-editor"    --> doFloat
      , className =? "Peek"    --> doFloat
      , namedScratchpadManageHook myScratchPads
      ]

myScratchPads =
  [ NS "terminal" (myTerminal ++ " -t scratchpad") (title =? "scratchpad") defaultFloating
  ]

main :: IO ()
main = xmonad $ Hacks.javaHack $ ewmhFullscreen $ desktopConfig
    { terminal = myTerminal
    , layoutHook = myLayout
    , manageHook = myManageHook <+> manageHook desktopConfig <+> scratchpadManageHookDefault
    , modMask = mod4Mask -- mod = "windows" key
    , startupHook = myStartupHook
    }
    `additionalKeysP`
    -- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Util-EZConfig.html#v:mkKeymap
    [ ("M-r", spawn "rofi -show drun") -- Win+r => Application runner
    , ("C-S-4", spawn "flameshot gui") -- Ctrl+Shift+4 => Area selection screenshot
    , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 10%- unmute")
    , ("<XF86AudioMute>", spawn "amixer set Master mute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 10%+ unmute")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set 10%+")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
    , ("M-<F12>", namedScratchpadAction myScratchPads "terminal") -- Win+F12 => Toggle terminal scratchpad
    , ("M-<F11>", spawn "rofi -show calc -modi calc -no-show-match -no-sort -calc-command \"echo -n '{result}' | xclip -selection clipboard\"")
    ]

myStartupHook :: X ()
myStartupHook = do
  spawn "feh --randomize --bg-fill ~/.wallpapers/*"
