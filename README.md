BitcoinTrading
===============

A program that automatically buys bitcoins when they are cheap, and sells
them when they are expensive.

Running
=======
To run, run the following from the top directory (it should contain this
README, src/, target/, etc.)

  $ sbt
  > run -option

options are:
  -c Test coinbase
  -f Test fake markets
  -h Run the heuristic program to learn parameters
  -r Run the real trading program

