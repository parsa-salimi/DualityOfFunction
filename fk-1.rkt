#lang racket
; f and g are lists of lists of int
;for example (x1 /\ x2 /\ x3)\/(x3 x4 x5) is represented by '((1 2 3) (3 4 5)). There is no ambiguity because we have assumed that our
;clauses are in DNF and moreover since we are only dealing with prime DNF's, we can safely ignore complements.

(define (reduce formula)
  (define (minimal? clause formula)
    (if (empty? formula) true
        (and (not (subset? (car formula) clause)) (minimal? clause (cdr formula)))))
  (define (reduce-help num formula)
    (cond [(= num (length formula)) empty]
          [(minimal? (list-ref formula num) (remove (list-ref formula num) formula)) (cons (list-ref formula num) (reduce-help (+ num 1) formula))]
          [else (reduce-help (+ num 1) formula)]))
  (reduce-help 0 (remove-duplicates formula equal? ))) ;we have to remove duplicates first because otherwise both instances will be removes by a process of mutually assisted destruction(MAD)

(define (clause-len I) (length I))

(define (sanitycheck f g) f)

;we might end up with a redundant DNF but that's okay. We will deal with it in the main recursion
(define (remove-var f x)
  (cond [(empty? f) empty]
        [(member x (first f)) (cons (remove x (first f)) (remove-var (rest f) x))]
        [else (cons (first f) (remove-var (rest f) x))]))

(define (remove-clause f x)
  (cond [(empty? f) empty]
        [(member x (first f)) (remove-clause (rest f) x)]
        [else (cons (first f) (remove-clause (rest f) x))]))

;If |F||G| <= 1, we do duality checking as follows:
;if both are zero, then they are clearly dual
;if both have length one, then by our sanity check conditions, the length of both implicants is also 1, and they have the same variable, so they are two identical variables, and therefore dual
;otherwise, we have an empty formula, and a singleton implicant, which are clearly not dual
(define (easydual f g)
  (cond [(= (length f) (length g)) true]
        [else false]))

;we can just choose a random variable, or a variable that satisfies the frequency conditions in Fredman Khachyian. The following searches the entire min-length clause, and thus can potentially return
;a better variable than one that merely satisfies the FK frequency criterea
(define (frequent f g)
  (define (minimum-clause f)
    (define (minimum-help f accum)
      (cond [(empty? f) accum]
            [(< (length (first f)) (length accum)) (minimum-help (rest f) (first f))]
            [else (minimum-help (rest f) accum)]))
    (minimum-help (rest f) (first f)))
  ;clause is the clause of minimum length, f is the other formula, for now just return the first element
  (define (frequent-help clause f) (car clause))
  (let ((min-f (minimum-clause f ))
        (min-g (minimum-clause g )))
    (if (< (length min-f) (length min-g)) (frequent-help min-f g) (frequent-help min-g f))))

(define (disjunction f g) (remove-duplicates (append f g) equal?))
  
(define (FK f g)
  (define (FK-irr f g)
    (cond [ (not (sanitycheck f g)) false]
          [ (<= (* (clause-len f) (clause-len g)) 1) (easydual f g)]
          [ else (letrec ((x (frequent f g))
                          (f0 (remove-var f x))
                          (f1 (remove-clause f x))
                          (g0 (remove-var g x))
                          (g1 (remove-clause g x)))
                 (and (FK f1 (disjunction g0 g1)) (FK g1 (disjunction f0 f1))))]))
  (FK-irr (reduce f) (reduce g)))
                   