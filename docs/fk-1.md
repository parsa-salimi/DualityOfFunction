---
layout: default
title: basic duality checking and pivot heuristics
nav_order: 5
---

One limitation of `statgen.rkt` is that it doesn't actually do duality checking, although one can infer this by looking at all the leaf nodes and seeing if they are all `(#t <f>)`. To actually do duality checking, use the provided `FK` function. It takes the same arguments as `FK-treelist`, namely, the two functions, a pivot rule, and a tiebreaking rule. It returns a list `(#t/#f cert)` where the first element returns the result of duality checking. If the two functions are not dual, then `cert` is a list of clauses *c-1,...c-n* such that *f(c-i) = g(NOT(c-i))*. This can then be used in dual generation (see next section).

## Pivot and tiebreaking rules

At each stage of the algorithm, we must choose a variable to branch on. Finding the best possible order(an order that generates the smallest tree) is generally computationally unfeasable, but we can adopt some simple heuristics to choose a variable *x* from *Var(F)*, the list of variables of *F*.

This heuristic consists of two parts :
 - a *pivot rule* that narrows down the list *var(F)* according to some critereon.
 - a *tiebreaking rule*, which then chooses a variable from the list returned by the pivot rule.
 
 By our conventions, a pivot rule must be a function of the form `(f b1 b2 v)` where `b1` and `b2` are the pair of functions in the current iteration of FK-A, and `v` is the list of present variables. we sort `v` before passing it on to the pivot rule. The function must return a sublist of `v`. The tiebreaking rule `(t v')` takes this sublist and returns a single element from the list. 
 
 So long as you adhere to these conventions, you can define your own pivot and tiebreaking rules in `fk-1.rkt` and add them to the `provide` list at the beginning of the file.
 
  The provided pivot rules are:
  - `fnone` : just returns v.
  - `fthresh` : returns variables passing the frequency threshold `1/(log(|b1|+|b2|)`. Fredman and Khachiyan have shown that such variables always exist.
  - `fcons`   : chooses variables passing the frequency thershold from the smallest clause in `b1` and `b2`. Once again this is guaranteed to exist. 
  - `fmax`    : chooses all the variables having maximal total frequency(maximum frequency in either `b1` or `b2`.
  
  And the provided tiebreking rules are:
  - `tbfirst`  : chooses the first variable in the list.
  - `tblex`    : chooses the lexicographically first variable in the list. equivelant to `tbfirst` under all our pivot rules.
  - `tblast`   : chooses the last variable in the list.
  - `tbrand`   : chooses a random variable in the list.

