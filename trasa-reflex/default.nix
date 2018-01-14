{ test ? true, frontend ? false }:
(import ../nix/default.nix {
  package = "trasa-reflex";
  inherit frontend test;
}).drv
