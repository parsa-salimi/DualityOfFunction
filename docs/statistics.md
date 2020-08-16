---
layout: default
title: generating statistics and operations on recursion trees
nav_order: 4
---

  Gathering data about a run of the algorithm is handled in `statgen.rkt`. This file does not run the implementation in `fk-a.rkt` directly, rather, it steps through the algorithm, building a binary recusrion tree containing the state of the algorithm at each step. A sample of such a tree was shown in the examples section. All subsequent operations on `statgen` make use of this tree.

---
## Basic operations on trees.

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

 
 

