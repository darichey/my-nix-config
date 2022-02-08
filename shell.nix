{ pkgs ? import <nixpkgs> {} }:
import ./home/xmonad-config/shell.nix { inherit pkgs; }