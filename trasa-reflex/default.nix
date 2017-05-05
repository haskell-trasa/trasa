{ test ? "true", frontend ? "false" }:
let parseBool = str: with builtins;
  let json = fromJSON str; in if isBool json then json else throw "nix parseBool: ${str} is not a bool.";
  frontendB = parseBool frontend;
in
import ../nix/default.nix { package = "trasa-reflex"; frontend = frontendB; test = !frontendB && parseBool test; }
