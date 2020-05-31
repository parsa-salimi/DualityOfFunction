#lang racket
; f and g are lists of lists of int
(define (FK f g)
  (define (FK-irr f g)
    (cond [ (not (sanitycheck f g)) false]
          [ (<= (* (clause-len f) (clause-len g)) 1) (easydual f g)]
          [ else (letrec ((x (frequent f g))
                          (f-0 (remove-var f x))
                          (f-1 (remove-clause f x))
                          (g-0 (remove-var g x))
                          (g-1 (remove-clause g x))))
                 (and (FK f1 (disjunction g0 g1)) (FK g1 (disjunction f0 f1)))]))
  (FK-irr (irredundant f) (irredundant g)))
                   