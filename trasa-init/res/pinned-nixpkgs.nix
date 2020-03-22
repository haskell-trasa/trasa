{ system ? builtins.currentSystem, config ? {} }:
let
  json = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/${json.rev}.tar.gz";
    inherit (json) sha256;
  };
in
  import src { inherit system config; }
