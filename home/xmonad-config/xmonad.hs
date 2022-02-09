import XMonad
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad $ def
    { terminal = "alacritty" }
    `additionalKeysP`
    [ ("M-r", spawn "rofi -show drun") ]