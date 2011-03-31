This is an attempt at implementing an R-tree library. Actually, 2 differents libraries are included : one using the object system offered by Ocaml (the 'O' module), and the other one not using it (the 'M' module). So far, only it is only possible to add and search within the tree. I hope to add the removing and maybe splitting (or partitioning) features at a later time, if I need it for my own projects.

To install the library, from the library root directory :

$ ./build.sh install-bin

To install the dev files (after installing the library) :

$ ./build.sh install-api

The documentation generation is broken (an issue with how I organized the source tree).
There's preliminary support for debian packaging (I don't use the dh_ocaml script yet).

To generate the debian packages (again from the library root directory) :

$ debuild binary

The test application (test_rtree) can be built either once the library is installed, or directly within the library source directory :

$ ocamlbuild tests/test_rtree.native

It requires the 'lablgl' ocaml library, providing opengl bindings.

Please note that this code is released under the LGPL (the library) and GPL (the test app) licences.

Have fun,

didier
