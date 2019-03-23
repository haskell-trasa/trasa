{ }:

let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    owner = "layer-3-communications";
    repo = "nixpkgs";
    rev = "2ac764de78a1e63009143e2ecd88aa378002190f";
    sha256 = "0j0hrzr9b57ifwfhggpzm43zcf6wcsj8ffxv6rz7ni7ar1x99x2c";
  };

in
  nixpkgs
