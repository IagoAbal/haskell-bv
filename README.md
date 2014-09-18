A library for bit-vector arithmetic in Haskell
=========================================

Bit-vector arithmetic inspired by [SMT-LIB](http://smt-lib.org/) and [Cryptol](http://cryptol.net/).

Bit-vectors are represented as a pair size and value, where sizes are of type _Int_ and values are _Integer_.

* Bit-vectors are interpreted as unsigned integers (i.e. natural numbers) except for some specific signed operations.
* Most operations are in some way size-polymorphic and, if required, will perform padding to adjust the size of input bit-vectors.

Other libraries
-------------

There exist many Haskell libraries to handle bit-vectors, but to the best of my knowledge _bv_ is the only one that focuses on bit-vector arithmetic.

If you do not need bit-vector arithmetic, then you probably should use any of the other libraries, which could offer more efficient implementations of bit arrays.

Importing and name clashes
-----------------------

Many exported functions name-clash with Prelude functions, it is therefore recommended to do a qualified import:

    import           Data.BitVector ( BV )
    import qualified Data.BitVector as BV

Running the test suite
--------------------

If you wish to run the test suite simply install (or just configure) the package with the flag _test_ enabled and an executable _bv-tester_ will be generated.

    cabal instal bv -ftest

Performance
----------

The _BV_ datatype is simply a pair of an _Int_, to represent the size, and an arbitrary-precision _Integer_, to represent the value of a bit-vector. Both fields are strict, and further GHC is being instructed to unbox strict fields within the module.

Most bit-vector operations are simple wrappers around _Integer_ operations, and thus we rely heavily on inlining. I hope this library is as efficient as the underlyning implementation of _Integer_, plus some small overhead, but I did not test it rigorously. Please feel free to comment.

