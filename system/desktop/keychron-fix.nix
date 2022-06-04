# Configuration relating to getting my Keychron K8 keyboard working properly on Linux.
# Based on this excellent gist: https://gist.github.com/andrebrait/961cefe730f4a2c41f57911e6195e444
# Note: Keyboard should be in "Windows/Android" mode
{ config, pkgs, ... }:

{
  # Function keys are used as first key. Pressing 'F8' key will behave like a F8. Pressing 'fn'+'F8' will act as special key (play/pause).
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=2
  '';
}