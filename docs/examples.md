---
layout: default
title: examples
nav_order: 3
---
## Representation of functions
A Boolean function is stored in it's prime disjunctive normal form(which can also be interpreted as a family of sets, or a *hypergraph*) as a list of clauses, where the clauses are also represented as unordered lists of
integer indices. For example, The [Gurvich-Khachiyan](https://www.sciencedirect.com/science/article/pii/S0012365X96000908) exampples `f_2` and `g_2` are represented as:
```
(f-n 2) -> '((2 3) (2 4) (1 3) (1 4) (6 7) (6 8) (5 7) (5 8))
(g-n 2) -> '((3 4 5 6) (3 4 7 8) (1 2 5 6) (1 2 7 8))
```
