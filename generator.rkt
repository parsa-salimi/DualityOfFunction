#lang racket
(require "DNF.rkt")
(provide f-n g-n selfgen selfgen-d)


(define (f-n k)
  (define (N n) (expt 2 (- (* 2 n) 1)))
  (define (vargen a b) (range a (+ 1 b)))
  (define (f-list k varlist)
    (cond [(= k 1) (map list varlist)]
          [else
           (letrec ((newk (N (- k 1)))
                 (list-1 (take varlist newk))
                 (list-2 (take (drop varlist newk) newk))
                 (list-3 (take (drop varlist (* 2 newk))  newk))
                 (list-4 (take (drop varlist (* 3 newk))  newk)))
           (add (mult (f-list (- k 1) list-1) (f-list (- k 1) list-2))
                (mult (f-list (- k 1) list-3) (f-list (- k 1) list-4))))]))
  (f-list k (vargen 1 (N k))))
(define (g-n k)
  (define (N n) (expt 2 (- (* 2 n) 1)))
  (define (vargen a b) (range a (+ 1 b)))
  (define (g-list k varlist)
    (cond [(= k 1) (list  varlist)]
          [else
           (letrec ((newk (N (- k 1)))
                 (list-1 (take varlist newk))
                 (list-2 (take (drop varlist newk) newk))
                 (list-3 (take (drop varlist (* 2 newk))  newk))
                 (list-4 (take (drop varlist (* 3 newk))  newk)))
           (mult (add (g-list (- k 1) list-1) (g-list (- k 1) list-2))
                 (add (g-list (- k 1) list-3) (g-list (- k 1) list-4))))]))
  (g-list k (vargen 1 (N k))))

(define (selfgen n)
  (define a-list (combinations (range 1 n) 4))
  (define b-list (map (lambda (x) (cons n x)) (combinations (range 1 n) 2)))
  (append a-list b-list))
(define (selfgen-d n)
  (define a-list (combinations (range 1 n) (- n 2)))
  (define b-list (map (lambda (x) (cons n x)) (combinations (range 1 n) (- n 4))))
  (append a-list b-list))