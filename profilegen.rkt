#lang racket
(require "fk-1.rkt" "DNF.rkt" "generator.rkt" "dualgen.rkt" math/number-theory)

;implements a two-dimnesional hash table(with n rows) as a vector of hash tables
(define-syntax K
  (syntax-rules (:= in)
    [(K(a b) := c in hash) (hash-set! (vector-ref hash a) b c)]
    [(K(a b) in hash)  (hash-ref (vector-ref hash  a) b)]))
(define (make-table n)
  (build-vector (+ 1 n) (lambda (m) (make-hash))))


;implementing a multidimentional hash with n columns. A vector of n elements, each is a hash table
;generates all the profiles of MBFs of length n (Yusun and Stephen, section 4)
(define (profiles n)
  (define (init-profiles n)
    (foldl (lambda (x y)
           (cons (cons x (make-list (- n 1) 0)) y)) '()
           (range 0 (+ 1 n))))
  ;step 1: fill hash table with lower bounds
  (define myhash (make-table n))
  (define k 0)
  (for [(s (range 0 (+ 1 (binomial n (floor (/ n 2))))))]
    (K(0 s) := 0 in myhash))
  (for [(r (range 1 (+ 1 n)))]
    (set! k r)
    (K(r 0) := 0 in myhash)
    (for [(s (range 1 (+ 1 (binomial n r))))]
      (when (>= s (binomial k r))
        (set! k (+ k 1)))
      (K(r s) := (+ (binomial (- k 1) (- r 1))
                  (K((- r 1) (- s (binomial (- k 1) r))) in myhash))
                  in myhash)))
  (define p (init-profiles n))
  p)
          
  
