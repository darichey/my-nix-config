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
import XMonad.Util.SpawnOnce (spawnOnOnce, spawnOnce)
import XMonad.Layout.PerWorkspace
import XMonad.Actions.SpawnOn
import XMonad.Actions.OnScreen
import XMonad.Hooks.DynamicProperty

myTerminal = "alacritty"

myLayout = avoidStruts $ smartBorders $ onWorkspace "9" (Mirror tiled) $ tiled ||| Mirror tiled ||| Full
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
      [ isFullscreen                            --> doFullFloat
      , className =? "Pavucontrol"              --> doFloat
      , className =? "pinentry"                 --> doFloat
      , className =? ".blueman-manager-wrapped" --> doFloat
      , className =? "nm-connection-editor"     --> doFloat
      , className =? "Peek"                     --> doFloat
      , className =? "discord"                  --> doShift "9"
      , namedScratchpadManageHook myScratchPads
      ]

-- Move spotify to workspace 9
-- Can't do this in myManageHook (like we do for discord) because spotify is bad:
-- https://github.com/xmonad/xmonad/issues/214#issuecomment-738586824
myHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> doShift "9")

myScratchPads =
  [ NS "terminal" (myTerminal ++ " -t scratchpad") (title =? "scratchpad") defaultFloating
  ]

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myKeys =
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
  ++
  -- "Replacing greedyView with view" section on https://wiki.haskell.org/Xmonad/Frequently_asked_questions
  [ (otherModMasks ++ "M-" ++ [key], action tag)
    | (tag, key)  <- zip myWorkspaces "123456789"
    , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                    , ("S-", windows . W.shift)]
  ]

myStartupHook :: X ()
myStartupHook = do
  -- Spawn feh to pick a random wallpaper
  spawn "feh --randomize --bg-fill ~/.wallpapers/*"

  -- Spawn discord and spotify. They will be moved to workspace 9 by the manage hook
  spawnOnce "discord"
  spawnOnce "spotify"

  -- Move workspace 9 to the secondary monitor
  windows (greedyViewOnScreen 1 "9")


main :: IO ()
main = xmonad $ Hacks.javaHack $ ewmhFullscreen $ desktopConfig
    { terminal = myTerminal
    , layoutHook = myLayout
    , manageHook = myManageHook <+> manageSpawn <+> manageHook desktopConfig <+> scratchpadManageHookDefault
    , modMask = mod4Mask -- mod = "windows" key
    , startupHook = myStartupHook
    , workspaces  = myWorkspaces
    , handleEventHook = myHandleEventHook
    }
    `additionalKeysP` myKeys
