{ test ? true, frontend ? false }:
(import ../nix/default.nix { 
  package = "trasa";
  inherit frontend test;
}).trasa
