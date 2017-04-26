{ nixpkgs ? import <nixpkgs> {} }:
let cabal2nixResult = src: nixpkgs.runCommand "cabal2nixResult" {
      buildCommand = ''
        cabal2nix file://"${src}" >"$out"
      '';
      buildInputs = with nixpkgs; [
        cabal2nix
      ];
    } "";
    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        trasa = self.callPackage (cabal2nixResult ./.) {};
      };
    };
    drv = haskellPackages.trasa;
in if nixpkgs.lib.inNixShell then
  drv.env
else
  drv
