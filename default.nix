{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc948" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./journalh.nix { }
