#lang racket
(require "DNF.rkt")
(provide is typelist)

;hand-crafted pattern matching for all functions on 4 variables. 
(define (list-distinct? l)
  (= (length l) (length (remove-duplicates l))))

(define (isc4 formula)
  (define (isc4h f)
    (and (= (length f) 2) (= (length (first f)) 2) (= (length (second f)) 2) (list-distinct? (first f)) (list-distinct? (second f)) (list-distinct? (flatten f))))
  (or (isc4h (first formula)) (isc4h (second formula))))

(define (isk2dd formula)
  (define (isk2ddh f)
    (and (= (length f) 2) (= (length (first f)) 3) (= (length (second f)) 3) (list-distinct? (first f)) (list-distinct? (second f)) (= (length (remove-duplicates (flatten f))) 4)
         (= (length (set-intersect (first f) (second f))) 2)))
  (or (isk2ddh (first formula)) (isk2ddh (second formula))))

(define (ispan3b formula)
  (define (ispan3bh f)
    (define sortedf (sort f (lambda (x y) (< (length x) (length y)))))
    (and (= (length sortedf) 2) (= (length (first sortedf)) 2) (= (length (second sortedf)) 3) (list-distinct? (first sortedf)) (list-distinct? (second sortedf))
            (= (length (remove-duplicates (flatten sortedf))) 4) (= (length (set-intersect (first f) (second f))) 1)))
  (or (ispan3bh (first formula)) (ispan3bh (second formula))))

(define (isstar3 formula)
  (define (helper f)
    (define sortedf (sort f (lambda (x y) (< (length x) (length y)))))
    (and (= (length sortedf) 2) (= (length (first sortedf)) 1) (= (length (second sortedf)) 3)
         (list-distinct? (second sortedf))
         (= (length (remove-duplicates (flatten sortedf))) 4) (= (length (set-intersect (first f) (second f))) 0)))
  (or (helper (first formula)) (helper (second formula))))

(define (isd41 formula)
  (define (helper f)
    (and (= (length f) 1) (= (length (first f)) 4)))
  (or (helper (first formula)) (helper (second formula))))

(define (ispan3 f)
  (define (helper f) (not (equal? (function-profile f 4) '(0 4 0 0))))
  (and (not (isc4 f)) (or (helper (first f)) (helper (second f)))))

(define (isk3d f)
  (define (helper f) (and (= (length (vars f)) 4) (equal? (function-profile f 4) '(0 0 3 0))))
  (or (helper (first f)) (helper (second f))))

(define (ispath4 f)
  (define (helper f)
    (and (equal? (function-profile f 4) '(0 3 0 0)) (= (length (vars f)) 4)))
  (and (not (isstar3 f)) (or (helper (first f)) (helper (second f)))))

(define (isk4- f)
  (define (helper f) (equal? (function-profile f 4) '(0 5 0 0)))
  (or (helper (first f)) (helper (second f))))

(define (ishw3 f)
  (define (helper f) (equal? (function-profile f 4) '(0 3 1 0)))
  (or (helper (first f)) (helper (second f))))

(define (isk4 f)
  (define (helper f) (equal? (function-profile f 4) '(0 6 0 0)))
  (or (helper (first f)) (helper (second f))))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define typelist '(c4 k2dd pan3b d41 star3 pan3 k3d path4 k4- hw3 k4)) 

;(eval `(,(string->symbol (string-append "is" "c4"))))
; function is a string "c4" "d41" "hw3" "k4" etc
(define (is f function)
   (eval `(,(string->symbol (string-append "is" (symbol->string function))) (quote ,f)) ns))
