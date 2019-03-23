# trasa

This library is a solution for http-based routing and dispatch. Its goals are similar to the goals of servant, however, trasa relies on very different mechanisms to accomplish those goals. All typeclasses in this library are optional. All of the real work is accomplished with GADTs, universal quantification, and plain old haskell data types.

# [Examples](./example/)

## Hacking

Nix users can have all dependencies of `trasa` and `trasa-*` cached when hacking on this project using `cachix`:

```sh
# install nix
$ bash <(curl https://nixos.org/nix/install)

# install cachix client
$ nix-env -iA cachix -f https://cachix.org/api/v1/install

# start using the binary cache
$ cachix use layer-3-cachix
```

Nix commands will use the cache:

```sh
$ nix-build
copying path '/nix/store/n1gwpmvmcgsbnr0a8ncflhvc59db775h-myproject-1.0.0' from 'https://layer-3-cachix.cachix.org'
```
