{ compiler ? "ghc843" }:
with builtins;
let
  rev = "61deecdc34fc609d0f805b434101f3c8ae3b807a";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  # nix-prefetch-url --unpack
  sha256 = "147xyn8brvkfgz1z3jbk13w00h6camnf6z0bz0r21g9pn1vv7sb0";
  config =
    { packageOverrides = super:
      let self = super.pkgs;
          lib = super.haskell.lib;
      in {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            ghc861 = super.haskell.packages.ghc861.override {
              overrides = self: super: {
                free-algebras = super.callPackage ./free-algebras-0.0.5.0.nix {};
              };
            };
            ghc843 = super.haskell.packages.ghc843.override {
              overrides = self: super: {
                free-algebras = super.callPackage ./free-algebras-0.0.5.0.nix {};
              };
            };
            ghc822 = super.haskell.packages.ghc822.override {
              overrides = self: super: {
                free-algebras = super.callPackage ./free-algebras-0.0.5.0.nix {};
              };
            };
            ghc802 = super.haskell.packages.ghc802.override {
              overrides = self: super: {
                concurrent-output = super.callPackage ./concurrent-output-1.9.2.nix {};
                ansi-terminal = super.callPackage ./ansi-terminal-0.6.3.1.nix {};
                async = super.callPackage ./async-2.1.1.1.nix {};
                lifted-async = super.callPackage ./lifted-async-0.9.3.3.nix {};
                exceptions = super.callPackage ./exceptions-0.8.3.nix {};
                free-algebras = super.callPackage ./free-algebras-0.0.5.0.nix {};
              };
            };
          };
        };
      };
    };
  nixpkgs = import (fetchTarball { inherit url sha256; }) { inherit config; };
in nixpkgs
