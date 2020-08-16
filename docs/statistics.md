---
layout: default
title: generating statistics and operations on recursion trees
nav_order: 4
---

  Gathering data about a run of the algorithm is handled in `statgen.rkt`. This file does not run the implementation in `fk-a.rkt` directly, rather, it steps through the algorithm, building a binary recusrion tree containing the state of the algorithm at each step. A sample of such a tree was shown in the examples section. All subsequent operations on `statgen` make use of this tree.

---
## Basic operations on trees

The recursion tree is one of :
 - a list `(node left right)` where `node` is again a list of the two function being checked. `left` and `right` are both also trees.
 - a list `(dual? (f g))` where `dual?` is a boolean value signifying whether `f` and `g` are dual, and `(f g) is the list of the two functions being checked.
 
 These properties are not explicitly used in the rest of the code, if you wish to the data structure, you are free to do so as long as you re-implement the basic operations described below.
 
 There are various basic operations on these trees(note: only available from `statgen.rkt`). all of these take a tree as an argument

 - `(node tree)`  returns a list (f g) of functions.
 - `(left-tree tree)`  returns the left branch.
 - `(right-tree tree)`  returns the right branch.
 - `(empty-tree? tree)`  returns true if the tree is a leaf node
 - `(tree->list tree)`  converts the tree to a list of all the function pairs that occur in it, i.e a list of all the nodes.
 
 In addition, the following functions are useful for counting the elements of a tree:
 
 `(treecount tree f)` returns the count of the nodes of `tree` that satisfy `f`. `f` is a function taking a list of two MBF's and returning either true or false.
 
 `(leafcount tree)` returns the count of the leaf nodes in the tree.
 
 ### Generating trees
 
 The trees re generted by the `FK-treelist` procedure. This procedure takes the following arguments:
 - f and g : boolean functions
 - pivot   : pivot rule to apply (discussed in the next section)
 - tiebreaker : The tiebreaking rule to apply after the pivot rule (again discussed in the next section)
 - vars       : the set of variables these function are defined on. Since for dual functions `f` and `g` must have the same 
 variables, you can use `(vars f)` as an argument.
 
This procedure is built on top of `FK-treelist-guided`, which takes an extra argument: a list of variables. The algorithm then decomposes on these variables first, ignoring the pivot rules.

---
## Top level functions for visualization and aggregate statistics
 
 
 

