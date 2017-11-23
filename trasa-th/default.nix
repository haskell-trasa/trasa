{ test ? true, frontend ? false }:
(import ../nix/default.nix {
  package = "interface";
  inherit frontend test;
}).trasa
