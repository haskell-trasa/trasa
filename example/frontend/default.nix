{ test ? true, frontend ? false }:
(import ../../nix/default.nix { 
  package = "frontend"; 
  inherit frontend test;
}).trasa
