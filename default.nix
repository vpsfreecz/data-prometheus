{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./data-prometheus.nix { }
