#lang racket
(require "DNF.rkt")
(require "generator.rkt")
(require "fk-1.rkt")
(require pict)
(require pict/tree-layout)
(require file/convertible)

; A tree is recursively defined as :
; #f (a terminal node
; a list '(node l r), where node is a node(currently stores frequency/ index information, and l and r are trees

(define (node tree) (car tree))
(define (left-tree tree) (cadr tree))
(define (right-tree tree) (caddr tree))
(define (node-leaf? tree) (member #t tree))
(define (empty-tree? tree) (not (list? tree)))
(define (leafcount tree)
  (cond [(empty-tree? tree) 0]
        [(equal? (cdr tree) '(#t #t)) 1]
        [(node-leaf? tree) (+ 1 (leafcount (left-tree tree)) (leafcount (right-tree tree)))]
        [else (+ (leafcount (left-tree tree)) (leafcount (right-tree tree)))]))
;keeps track of nodes of each depth
(define counters (map (lambda (x) (box 0)) (range 0 50)))

;generates a #tree-layout from a given tree, which can be used to visualise the tree
(define (tree->pict tree)
  (cond [(empty-tree? tree) (tree-layout #:pict(disk #:color "blue" 15))]
        [else (tree-layout #:pict(text (number->string (first (node tree)))) (tree->pict (left-tree tree)) (tree->pict (right-tree tree)))]))

;the following returns a computation tree
(define (FK-treelist f g accum pivot tiebreaker)
    (cond [ (not (sanitycheck f g)) (list "sanity-check")]
          [ (<= (* (clause-len f) (clause-len g)) 1) (if (easydual f g) #t #f)]
          [ else
            (begin (set-box! (list-ref counters accum) (+ 1 (unbox (list-ref counters accum))))
            (letrec ((x (tiebreaker (pivot (vars f) f g)))
                          (f0 (remove-var f x))
                          (f1 (remove-clause f x))
                          (g0 (remove-var g x))
                          (g1 (remove-clause g x)))
                 (list (list x (max (frequency x f) (frequency x g))) (FK-treelist (reduce f1) (reduce (disjunction g0 g1)) (+ accum 1) pivot tiebreaker)
                       (FK-treelist (reduce g1) (reduce (disjunction f0 f1)) (+ accum 1) pivot tiebreaker))))]))

;f and g are the formulas to generate
;filename is the file name
;depth is the maximum depth of the generated tree
;spacing is the y-spacing between the nodes, set to #f for default spacing
(define (generate-svg treepict filename depth spacing) (fprintf (open-output-file filename) (bytes->string/utf-8
  (convert (naive-layered treepict) #:x-spacing 1 #:y-spacing spacing) 'svg-bytes)))


(define (gridgen pivotlist tblist)
  (define possibilities (cartesian-product pivotlist tblist))
  (for-each (lambda (x) (begin (display x) (printf ":  ~a \n" (leafcount (FK-treelist (f-n 3) (g-n 3) 0 (first x) (second x)))))) possibilities))




                   