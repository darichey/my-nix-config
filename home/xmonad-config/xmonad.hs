import XMonad
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad $ def
    { terminal = "alacritty" }
    `additionalKeysP`
    [ ("M-r", spawn "rofi -show drun") -- Alt+r => Application runner
    , ("C-S-4", spawn "flameshot gui") -- Ctrl+Shift+4 => Area selection screenshot
    ]
