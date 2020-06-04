#lang racket
(require "DNF.rkt")
(require "generator.rkt")
(require "fk-1.rkt")
(require pict)
(require pict/tree-layout)
(require file/convertible)



;the following does not return true or false, it just returns a tree.
(define counters (map (lambda (x) (box 0)) (range 0 100)))
(define (FK f g accum depth)
  (if (>= accum depth) false
      (begin (set-box! (list-ref counters accum) (+ 1 (unbox (list-ref counters accum))))
    (cond [ (not (sanitycheck f g)) (tree-layout #:pict(disk #:color "blue" 15))]
          [ (<= (* (clause-len f) (clause-len g)) 1) (if (easydual f g) (tree-layout #:pict(disk #:color "red" 15)) (tree-layout #:pict(disk #:color "green" 15)))]
          [ else (letrec ((x (frequent f g))
                          (f0 (remove-var f x))
                          (f1 (remove-clause f x))
                          (g0 (remove-var g x))
                          (g1 (remove-clause g x)))
                 (tree-layout #:pict(text (number->string x)) (FK (reduce f1) (reduce (disjunction g0 g1)) (+ accum 1) depth) (FK (reduce g1) (reduce (disjunction f0 f1)) (+ accum 1) depth)))]))))
(define (fk-run f g depth) (naive-layered (FK f g 0 depth)))
  


;f and g are the formulas to generate
;filename is the file name
;depth is the maximum depth of the generated tree
;spacing is the y-spacing between the nodes, set to #f for default spacing
(define (generate-svg f g filename depth spacing) (fprintf (open-output-file filename) (bytes->string/utf-8
  (convert (naive-layered (FK f g 0 depth) #:x-spacing 1 #:y-spacing spacing) 'svg-bytes))))


                   