{ compiler ? "ghc843"
, haddock ? true
, test ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix {};
  default = import ./default.nix {inherit compiler haddock test;};
in
  {
    heyting-algebras = if nixpkgs.lib.inNixShell
      then default.heyting-algebras.env
      else default.heyting-algebras;
  }

