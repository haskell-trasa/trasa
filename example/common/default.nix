{ test ? true, frontend ? false }:
(import ../../nix/default.nix {
  package = "common";
  inherit frontend test;
}).trasa
