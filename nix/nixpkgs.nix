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
            ghc802 = super.haskell.packages.ghc802.override {
              overrides = self: super: {
                # hedgehog = super.callPackage ./hedgehog-0.6.1.nix {};
                semigroupoids = super.semigroupoids_5_3_1;
                base-orphans = super.base-orphans_0_8;
              };
            };
          };
        };
      };
    };
  nixpkgs = import (fetchTarball { inherit url sha256; }) { inherit config; };
in nixpkgs
