# DualityOfFunction
This Racket code implements the [Fredman Khachiyan algorithm](https://pdfs.semanticscholar.org/cf09/761a7a863915f91346881df95484b5bee617.pdf) for detecting if two given monotone formulas in DNF are dual.

# usage
To interface with the software, use DrRacket's built in REPL. To access the top level functionality of the entire package, run the REPL through the file ```interface.rkt```. Explanations of various available commands are also included there. 
If you wish to modify anything, here's a picture of where everything is implemented:

# implementation


+ ``DNF.rkt`` : here is where basic operations and data structures on monotone boolean functions are defined. Also includes pattern matching code for MBFs on 4 variables.
+ ``fk-1.rkt`` : the ``FK-A`` algorithm, with certificate generation and pivot/tiebreaking rules is defined here. Note that this code does not collect data, it only checks duality.
+ ``getfunctions.rkt`` : This snippet of code does some data wrangling to obtain MBF's listen on [ Takeaki Uno's website](http://research.nii.ac.jp/~uno/dualization.html)
+ ``profilegen.rkt`` : Implements code to generate all profiles of monotone boolean functions on _n_ variabes, inspired by [this paper](http://people.math.sfu.ca/~tamon/Papers/r7.pdf)
+ ``dualgen.rkt`` uses the duality checking code in ``fk-1.rkt`` to generate duals of monotone boolean functions.
+ ``statgen.rkt`` Builds the recursion tree for the fk-a algorithm, with functionality to generate svg files from the tree, count the depth, number of leaves, etc. find occurances of a given pattern, generate grids etc.
