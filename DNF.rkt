#lang racket
(provide reduce minimum-clause maximum-clause remove-var remove-clause clause-len size
         vars disjunction add mult insert function-profile make-table K first-clause rest-clauses representation)

;implements a two-dimnesional hash table(with n rows) as a vector of hash tables
(define-syntax K
  (syntax-rules (:= in +=)
    [(K(a b) := c in hash) (hash-set! (vector-ref hash a) b c)]
    [(K(a b) += c in hash) (begin (hash-ref (vector-ref hash  a) b (lambda () (hash-set! (vector-ref hash a) b 0)))
                           (hash-set! (vector-ref hash a) b (+ c (hash-ref (vector-ref hash  a) b 0))))]
    [(K(a b) in hash)  (hash-ref (vector-ref hash  a) b 0)]))
(define (make-table n)
  (build-vector (+ 1 n) (lambda (m) (make-hash))))
(define (set-same? a b)
  (and (subset? a b) (subset? b a)))

; This file implements the list of lists DNF representation

; a formula is a list of list of int
;for example (x1 /\ x2 /\ x3)\/(x3 x4 x5) is represented by '((1 2 3) (3 4 5)). There is no ambiguity because we have assumed that our
;clauses are in DNF and moreover since we are only dealing with prime DNF's, we can safely ignore complements.
; 0 is represented by '(), and 1 is represented by '(())
(define (clause-len c) (length c))
(define (size f) (length f))
;(define (vars f)
;  (remove-duplicates (flatten f) =))
(define (vars f)
  (define (merge l1 l2)
    (cond [(empty? l1) l2]
          [(empty? l2) l1]
          [(< (first l1) (first l2)) (cons (first l1) (merge (rest l1) l2))]
          [(> (first l1) (first l2)) (cons (first l2) (merge l1 (rest l2)))]
          [(cons (first l1) (merge (rest l1) (rest l2)))]))
  (foldl merge '() f))
(define (first-clause f) (first f))
(define (rest-clauses f) (rest f))

;in DNF, adding formulas is straightforward
(define (disjunction f g) (append f g))
(define add disjunction)
;multiplication is a little bit harder
(define (mult f g)
  (define (mult-clause c g)
    (map (lambda (x) (remove-duplicates (append c x) =)) g))
  (define (mult-accum f accum)
    (if (empty? f) accum
        (mult-accum (rest f) (disjunction (mult-clause (first f) g) accum))))
  (reduce (mult-accum f '())))

(define (remove-var f x)
    (cond [(empty? f) empty]
          [(member x (first f)) (cons (remove x (first f)) (remove-var (rest f) x))]
          [else (remove-var (rest f) x)]))
(define (remove-clause f x)
    (cond [(empty? f) empty]
          [(member x (first f)) (remove-clause (rest f) x)]
          [else (cons (first f) (remove-clause (rest f) x))]))
;finding minimum and maximum clauses is very similar, so we write this helper function for both of them
(define (list-iter lst func accum)
  (foldl (lambda (x accum) (if (func (length x) (length accum)) x accum)) accum lst))

;returns a clause of minimum/maximum length
(define (minimum-clause f)
  (if (empty? f) '()
  (list-iter (rest-clauses f) < (first-clause f))))
(define (maximum-clause f)
  (if (empty? f) '()
  (list-iter (rest-clauses f) > (first-clause f))))

;reduces a given formula: deletes redundant and non minimal elemtns. O(n^2)
(define (reduce f)
 (define (sub-ordered-list l1 l2) ;length l1 <= length l2
    (cond [(empty? l1) #t]
          [(empty? l2) (empty? l1)]
          [(< (first l1) (first l2)) #f]
          [(> (first l1) (first l2)) (sub-ordered-list l1 (rest l2))]
          [else (sub-ordered-list (rest l1) (rest l2))]))
  
  (define (minimal? clause f)
    (cond [(empty? f) #t]
          [(>= (clause-len (first-clause f)) (clause-len clause)) (minimal? clause (rest-clauses f))]
          [else (and (not (sub-ordered-list (first-clause f) clause))
                     (minimal? clause (rest-clauses f)))]))
  (define (reduce-help num f)
    (cond [(= num (length f)) empty]
          [(minimal? (list-ref f num)  f) (cons (list-ref f num) (reduce-help (+ num 1) f))]
          [else (reduce-help (+ num 1) f)]))
  ;if a DNF contains an empty clause, then it is trivially the only minimal clause
  (if (member '() f) '(())
     (reduce-help 0  f)))



;sets the value of <position> to <value> in <certificate>
(define (insert certificate position value)
  (if (= value 0) certificate
      (if (list? certificate) (cons position certificate) certificate)))

;generates the profile of a given MBF
(define (function-profile f vars)
  (define (vector-inc! vector pos)
    (vector-set! vector pos (+ 1 (vector-ref vector pos))))
  (letrec [(profilelength vars)
           (profile-vector (make-vector profilelength 0))
           (lengthlist (map length f))]
    (for ([i lengthlist])
      (vector-inc! profile-vector (- i 1)))
    (vector->list profile-vector)))


(define (list-distinct? l) (= (length l) (length (remove-duplicates l))))


(define (representation f) f)


;The following is the old pattern matching code, it's more versatile but
;it is way too slow to be of practical use. manual code for 4 variable functions
;has been written in patternmatcher.rkt

;(define (M f pattern condition)
;  `(match ',f (,pattern
;               #:when ,condition
;               #t)
;             (else #f)))
;
;(define (normalize-form f)
;  (sort (map (lambda (x) (sort x <)) f) (lambda (x y) (< (length x) (length y)))))
;(define (perm-mapper x permlist origlist)
;  (define index (index-of origlist x))
;  (list-ref permlist index))
;
;(define (match-formula f pattern condition)
;  (define fvars (sort (vars f) <))
;  (define formula (normalize-form f))
;  (define (permute-function permutation)
;    (map (lambda (cl) (map (lambda (v) (perm-mapper v permutation fvars)) cl)) formula))
;   ;(eval (M f pattern condition)))
; (ormap (lambda (perm) (eval (M perm pattern condition))) (permutations formula)))
;
;
;(define var4list
;  '(;(star2 (list (list c1)(list a1 b1)) #t (list a1 b1 c1))
;    (star3 (list (list-no-order a1 b1)(list-no-order a2 c1)(list-no-order a3 d1)) (and (= a1 a2 a3)) (list a1 b1 c1 d1))
;    ;(hw3   (list (list-no-order a1 d1)(list-no-order b1 d2)(list-no-order c1 d3)(list-no-order a2 b2 c2)) (and (= a1 a2) (= b1 b2) (= c1 c2) (= d1 d2 d3)) (list a1 b1 c1 d1))
;    ;(k3    (list (list-no-order a1 c1)(list-no-order b1 c2)(list-no-order a2 b2)) (and (= a1 a2) (= b1 b2) (= c1 c2)) (list a1 b1 c1))
;    ;(k4-   (list (list-no-order c1 d1)(list-no-order a1 d2)(list-no-order a2 c2)(list-no-order b1 d3)(list-no-order b2 c3)) (and (= c1 c2 c3) (= d1 d2 d3) (= a1 a2) (= b1 b2)) (list a1 b1 c1 d1))
;    ;(k4    (list (list-no-order a1 b1) (list-no-order b2 c1) (list-no-order b3 d1) (list-no-order a2 c2) (list-no-order c3 d2) (list-no-order a3 d3))
;     ;      (and (= a1 a2 a3) (= b1 b2 b3) (= c1 c2 c3) (= d1 d2 d3)) (list a1 b1 c1 d1))
;    (c4    (list (list-no-order a1 c1) (list-no-order b1 c2) (list-no-order b2 d1) (list-no-order a2 d2)) (and (= a1 a2) (= b1 b2) (= c1 c2) (= d1 d2)) (list a1 b1 c1 d1))
;    ;(path4 (list (list-no-order a1 c1)(list-no-order b1 c2)(list-no-order b2 d1)) (and (= c1 c2) (= b1 b2)) (list a1 b1 c1 d1))
;    (k2dd  (list (list-no-order a1)(list-no-order b1)(list-no-order c1 d1)) #t (list a1 b1 c1 d1))
;    ;(k3d   (list (list-no-order b1)(list-no-order a1 c1)(list-no-order c2 d1)(list-no-order a2 d2)) (and (= a1 a2) (= c1 c2) (= d1 d2)) (list a1 b1 c1 d1))
;    (pan3b (list (list-no-order b1 c1)(list-no-order b2 d1)(list-no-order a1)) (and (= b1 b2)) (list a1 b1 c1 d1))
;    ;(pan3  (list (list-no-order a1 b1)(list-no-order a2 c1)(list-no-order a3 d1)(list-no-order c2 d2)) (and (= a1 a2 a3) (= c1 c2) (= d1 d2)) (list a1 b1 c1 d1))
;    ;(d21   (list (list-no-order a1) (list-no-order b1)) #t (list a1 b1))
;    ;(d31   (list (list-no-order a) (list-no-order b) (list-no-order c)) #t (list a b c))
;    (d41   (list (list-no-order a b c d)) #t (list a b c d))
;    ;(d11   (list (list a)) #t (list a))))
;    ))
;
;(define (isa f type)
;  ;(display f) (display type)
;   (match-formula f (second (assoc type var4list)) (third (assoc type var4list))))



        



