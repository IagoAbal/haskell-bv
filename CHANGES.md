
0.4.0
=====

This is a maintainance release, but it introduces changes to the API that required a new major version.
Remarkably, I have fixed a few bugs, optimized a few functions, and added a few more properties (tests).
Apart from that, and the usual clean up, there are also a handful of new API functions that I judged useful.

For performance reasons, this release introduces an explicit dependency with _integer-gmp_.
I could make the library depend on an abstract integer implementation, if necessary,
but that would add complexity and for now depending on GMP seems fine (let me know if you have an opinion).


Dependencies
-----------

* Depend on the _ghc-prim_ package, the GHC's internal representation of primitive types.
* Depend on the _integer-gmp_ package, the Haskell bindings for GNU's GMP library.
* Use _MagicHash_ extension to work with unboxed machine integers.

API
---

* Added _bitVecs_ (list of bit-vector literals).
* Added _@:_ (indexing of multiple bits).
* Added _pow_ as an optimized exponentiation function.
* Fixed _bitVec_ (value must fit bit-with).
* Fixed _negate_ (wrong on zero bit-vector).
* Define _join_ for the case of an empty list (it must be equivalent to _mconcat_).
* Define _and_ and _or_ for the case of an empty list.
* Declared Monoid instance for bit-vector (monoid under concanetation).
* Optimized (using GMP internals): _fromBits_, _fromInteger_, and _lg2_.
* Remove uninteresting _maxNat_ function from export list.

0.3.0
=====

This is a maintainance release, but it introduces changes to the API that required a new major version.

Dependencies
-----------

* Increase base version to 4.6.
* Support base 4.7 (new methods were added to the _Bits_ type-class).
* Use of _CPP_ extension for conditional compilation.

API
---

* Replace assertions by errors when checking preconditions of exported functions.
* Use proper names for functions and encourage qualified import, names ended with underscore are now deprecated.
* Add _lsb1_ function to complement _msb1_.
* Tweak code and documentation.

