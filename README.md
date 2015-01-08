SensorNet-Compiler
==================

Dependencies
------------

Get Haskell and Cabal from https://www.haskell.org/platform/ or your distro's 
repositories, and make sure cabal is at least version 1.18.

Install
-------

      Make sure ~/.cabal/bin is early in your path, then: 
     
      $ cabal update
      $ cabal install happy
      $ cabal install alex
      $ cabal sandbox init
      $ cabal install --enable-tests
      $ cabal configure --enable-tests

If `cabal install` only gives you a list of packages, then use this command
instead:

      $ cabal install --enable-tests --force-reinstall

####To download code onto a Pinoccio:
      
* Follow [Pinoccio's instructions](https://pinocc.io/solo) for setting up the development environment.
* Download the [LinkedList library](https://github.com/ivanseidel/LinkedList) and import it into Arduino Beta (installed during the Pinoccio setup) by ```Sketch->Import Library```.
* Download the [Arduino Buffered Serial](https://code.google.com/p/arduino-buffered-serial/) library and import it into Arduino Beta by ```Sketch->Import Library```.


Build 
-----

In the parent directory:

      $ cabal build

Test
----

In the parent directory:

      $ cabal test
