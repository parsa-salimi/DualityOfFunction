---
layout: default
title: Generating all MBFs on n variables
parent: miscellaneous functionality
nav_order: 4
---

The function `(duals n)` in the file `profilegen.rkt` generates all the mbfs on n variables. This is only practical for small `n`. To remove duplicates from this list, run the companion function, `(distinct-duals n)`. 
