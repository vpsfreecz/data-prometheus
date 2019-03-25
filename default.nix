{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./data-prometheus.nix { }
