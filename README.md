SensorNet-Compiler
==================

Dependencies
------------

Get Haskell and Cabal from https://www.haskell.org/platform/ or your distro's 
repositories, and make sure cabal is at least version 1.18.

Install
-------

      Make sure ~/.cabal/bin is in your path, then: 
      
      $ cabal install happy
      $ cabal sandbox init
      $ cabal install --enable-tests
      $ cabal configure --enable-tests

If `cabal install` only gives you a list of packages, then use this command
instead:

      $ cabal install --enable-tests --force-reinstall

Build 
-----

In the parent directory:

      $ cabal build

Test
----

In the parent directory:

      $ cabal test
