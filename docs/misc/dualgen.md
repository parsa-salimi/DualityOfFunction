---
layout: default
title: Dual generation
parent: miscellaneous functionality
nav_order: 2
---

The Fredman-Khachiyan algorithm can be used to generate duals as follows: to find the dual of *F*, check the duality with the 0 function, generating a certificate(unless *F* is zero), add clauses to 0 such that the certificate no longer works, and repeat until *F* is dual to this generated function.

This method is usually slow in practice, but is implemented in `dualgen.rkt` as the function `(dualgen f pivot tiebreaker)` where `pivot` and `tiebreaker` are the underlying pivoting and tiebreaking rules that the `FK` algorithm uses.

However, it is usually more practical to use the provided `dual` function, which essentially uses the same recursion as `FK` to generate the dual, combining the duals of the left branch and the right  branch.
