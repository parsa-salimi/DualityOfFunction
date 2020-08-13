---
layout: default
title: Installation and running
nav_order: 2
---

The code is written in [Racket](https://racket-lang.org/). To run the code, install Racket first.
After cloning the repository, the code can be run either from the DrRacket IDE, or through the command line.

Note that the code uses several modules from the `main-distribution` of racket, so in particular the code will not work properly on minimal racket. If you have minimal racket and want to get the required libraries, run 
```raco pkg install main-distribution``

## DrRacket instructions
Open the `interface.rkt` file with DrRacket, which is a blank file including all the module imports. Then eitehr use the REPL, or write some code and run it with the `run` Button. You can also open any of the files and run them directly, although running in, say `DNF.rkt` won't give you access to functions in `fk-1.rkt`. 

## Command line instructions
To run a REPL from the command line, navigate to the root directory and run `racket`. a REPL environment will open. Type
`(enter! "interface.rkt")`
to access all provided functions.

----

## Accesing all functions
The above steps provide only the functions specified by the `provide` statemtns at the top of each file. If for some reason you need to use a function that is not provided, replace "interface.rkt" in the above with the file containing the function.

## Defining new functions
To define new functions, either define them in the file where it is most natural, and then use it in the same environment (by running DrRacket in the same file or `(enter! <filename>)`. Alternatively, provide it at the top of the file and use `interface.rkt`, or define it in `interface.rkt` and use it there.
