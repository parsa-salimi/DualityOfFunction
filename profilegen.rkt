#lang racket
(require "fk-1.rkt" "DNF.rkt" "generator.rkt" "dualgen.rkt" math/number-theory)
(provide genfunctions profiles duals)




(define (numzeroes list) (foldl (lambda (x y) (if (= x 0) (+ y 1) y)) 0 list))
;implementing a multidimentional hash with n columns. A vector of n elements, each is a hash table
;generates all the profiles of MBFs of length n (Yusun and Stephen, section 4)
(define (profiles n)
  (define (init-profiles n)
    (foldl (lambda (x y)
           (cons (cons x (make-list (- n 1) 0)) y)) '()
           (range 0 (+ 1 n))))
  ;step 1: fill hash table with lower bounds
  (define myhash (make-table n))
  (define (p-sub r s p)
    (define (allzeroes? lst) (= (length lst) (numzeroes lst)))
    (define bound (K(r s) in myhash))
    (filter (lambda (x) (and (>= (list-ref x (- r 2)) bound) (allzeroes? (drop x (- r 1)))))
            p))  
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
  ;step 2: generate the profiles
  (define p-n (init-profiles n))
  (for [(r (range 2 (+ 1 n)))]
    (for [(s (range 1 (+ 1 (binomial n r))))]
      (let [(p-rs (p-sub r s p-n))]
      (set! p-n (set-union p-n
                           (map (lambda (profile) (list-set
                            (list-update profile (- r 2) (lambda (x) (- x (K(r s) in myhash)))) (- r 1) s)) p-rs))))))
  p-n)

(define (genfunction profile)
  (define (set-same? a b) (and (subset? a b) (subset? b a)))
  (define (single-entry? profile) (= (length profile) (+ 1 (numzeroes profile))))
  (define (generate-single profile numclauses clauselen)
    (let [(combos (sequence->list (in-combinations (range 1 (+ 1 (length profile))) clauselen)))]
    (cond [(= numclauses 0) '(())]
          [(= numclauses 1) (map list combos)]
          [else (let [(prev (generate-single profile (- numclauses 1) clauselen))]
                  (remove-duplicates (foldl (lambda (clause accum)
                         (append (map (lambda (f) (append (list clause) f))
                                 (filter (lambda (f) (not (member clause f))) prev)) accum)) '() combos) set-same?))])))
  (define (disjoint-formulas? oldf newf)
    (let [(combinations (cartesian-product oldf newf))]
      (andmap (lambda (lst) (not (subset? (first lst) (second lst)))) combinations)))
  (define (gen-iter profile stage accum)
    (cond [(= stage  (length profile)) accum]
          [else (letrec [ (stageprofile (list-set (make-list (length profile) 0) stage (list-ref profile stage)))
                          (newaccum (generate-single stageprofile (list-ref profile stage) (+ 1 stage)))
                          (composition (foldl (lambda (f acc)
                                               (append (map (lambda (g) (append f g))
                                                            (filter (lambda (g) (disjoint-formulas? f g)) newaccum)) acc))
                                                       '() accum))]
                  ;(printf "stageprofile:~a\n newaccum: ~a\n composition: ~a\n" stageprofile newaccum composition)
                  (gen-iter profile (+ 1 stage) composition))]))
  (gen-iter profile 0 '(())))

(define (genfunctions profiles)
  (foldl (lambda (profile accum) (append (genfunction profile) accum)) '() profiles))

                  
(define (duals n) (map (lambda (f) (list f (dual f))) (rest (genfunctions (profiles n)))))
(define (distinct-duals n)
  (let [(duals (duals n))]
    (remove-duplicates duals (lambda (x y) (and (subset? x y) (subset? y x))))))



  
