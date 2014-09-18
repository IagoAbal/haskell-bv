
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

