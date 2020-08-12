---
layout: default
title: Installation and running
nav_order: 2
---

The code is written in [Racket](https://racket-lang.org/). To run the code, install Racket first.
After cloning the repository, the code can be run either from the DrRacket IDE, or through the command line.

## DrRacket instructions
Open the `interface.rkt` file with DrRacket, which is a blank file including all the module imports. Then eitehr use the REPL, or write some code and run it with the `run` Button. You can also open any of the files and run them directly, although running in, say `DNF.rkt` won't give you access to functions in `fk-1.rkt`. 

## Command line instructions
To run a REPL from the command line, run:
```
racket --repl -t DNF.rkt -t generator.rkt -t fk-1.rkt -t statgen.rkt
```
