#lang racket
(provide reduce minimum-clause maximum-clause remove-var remove-clause clause-len
         vars disjunction add mult)
; This file implements the DNF representations and generators

; a formula is a list of list of int
;for example (x1 /\ x2 /\ x3)\/(x3 x4 x5) is represented by '((1 2 3) (3 4 5)). There is no ambiguity because we have assumed that our
;clauses are in DNF and moreover since we are only dealing with prime DNF's, we can safely ignore complements.
; 0 is represented by '(), and 1 is represented by '(())
(define (disjunction f g) (remove-duplicates (append f g) equal?))
(define (clause-len c) (length c))
(define (vars f)
  (remove-duplicates (flatten f) =))

(define (remove-var f x)
  (define (remove-help f x)
    (cond [(empty? f) empty]
          [(member x (first f)) (cons (remove x (first f)) (remove-var (rest f) x))]
          [else (cons (first f) (remove-var (rest f) x))]))
   (remove-help f x))
(define (remove-clause f x)
  (define (remove-clause-help f x)
    (cond [(empty? f) empty]
          [(member x (first f)) (remove-clause (rest f) x)]
          [else (cons (first f) (remove-clause (rest f) x))]))
   (remove-clause-help f x))
;finding minimum and maximum clauses is very similar, so we write this helper function for both of them
(define (list-iter f func accum)
  (cond [(empty? f) accum]
        [(func (length (first f)) (length accum)) (list-iter (rest f) func (first f))]
        [else (list-iter (rest f) func accum)]))
(define (minimum-clause f)
  (if (empty? f) '()
  (list-iter (rest f) < (first f))))
(define (maximum-clause f)
  (if (empty? f) '()
  (list-iter (rest f) > (first f))))
;reduces a given formula: dleetes redundant and non minimal elemtns. O(n^2)
(define (reduce formula)
  (define (minimal? clause formula)
    (if (empty? formula) true
        (and (not (subset? (car formula) clause)) (minimal? clause (cdr formula)))))
  (define (reduce-help num formula)
    (cond [(= num (length formula)) empty]
          [(minimal? (list-ref formula num) (remove (list-ref formula num) formula)) (cons (list-ref formula num) (reduce-help (+ num 1) formula))]
          [else (reduce-help (+ num 1) formula)]))
  ;if a DNF contains an empty clause, then it is trivially the only minimal clause
  (if (member '() formula) '(())
     (reduce-help 0 (remove-duplicates formula equal? ))))

;in DNF, adding formula's is straightforward
(define (add f g) (disjunction f g))
;multiplication is a little bit harder
(define (mult f g)
  (define (mult-clause c g)
    (map (lambda (x) (remove-duplicates (append c x) =)) g))
  (define (mult-accum f accum)
    (if (empty? f) accum
        (mult-accum (cdr f) (disjunction (mult-clause (car f) g) accum))))
  (reduce (mult-accum f '())))

