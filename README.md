# trasa

This library is a solution for http-based routing and dispatch. Its goals are similar to the goals of servant, however, trasa relies on very different mechanisms to accomplish those goals. All typeclasses in this library are optional. All of the real work is accomplished with GADTs, universal quantification, and plain old haskell data types.

An example application featuring server and client can be seen in the [example folder](./example/).
