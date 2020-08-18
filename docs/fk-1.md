---
layout: default
title: basic duality checking and pivot heuristics
nav_order: 5
---

One limitation of `statgen.rkt` is that it doesn't actually do duality checking, although one can infer this by looking at all the leaf nodes and seeing if they are all `(#t <f>)`. To actually do duality checking, use the provided `FK` function. It takes the same arguments as `FK-treelist`, namely, the two functions, a pivot rule, and a tiebreaking rule. It returns a list `(#t/#f cert)` where the first element returns the result of duality checking. If the two functions are not dual, then `cert` is a list of clauses *c-1,...c-n* such that *f(c-i) = g(NOT(c-i))*. This can then be used in dual generation (see next section).

## Pivot and tiebreaking rules

