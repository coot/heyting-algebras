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
    heyting-algebra = if nixpkgs.lib.inNixShell
      then default.heyting-algebra.env
      else default.heyting-algebra;
  }

