#!/bin/bash

set -euxo pipefail

cabal sandbox init
cabal install -ftest --only-dependencies

cabal clean
cabal configure -f -gmp -ftest
cabal build
dist/build/bv-tester/bv-tester -a 10000

cabal clean
cabal configure -fgmp -ftest
cabal build
dist/build/bv-tester/bv-tester -a 10000
