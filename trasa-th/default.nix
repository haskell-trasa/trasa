{ test ? true, frontend ? false }:
(import ../nix/default.nix {
  package = "trasa-th";
  inherit frontend test;
}).drv
