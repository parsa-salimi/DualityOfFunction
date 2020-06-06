#lang racket
(require "DNF.rkt")
(require "generator.rkt")
(provide sanitycheck easydual frequency fthresh fcons fmin fmax fconsmax tbnaive tbrand tbnaivelast tblex)


(define (sanitycheck f g)
  ;naive implementation, will make it efficient later
  (define (samevars f g)
    (equal? (sort (vars f) <) (sort (vars g) <)))
  (define (maxlength-property f g)
    (<= (length (maximum-clause f)) (length g)))
  (define (intersection-property f g)
    (define (nonempty-clause-formula c f)
      (cond [(empty? f) true]
            [(empty? (set-intersect c (first f))) false]
            [else (nonempty-clause-formula c (rest f))]))
      (cond [(empty? f) true]
            [(nonempty-clause-formula (first f) g) (intersection-property (rest f) g)]
            [else false]))
  (define (inequality-property f g)
    (define (sum-formula f) (foldl + 0 (map (lambda (x) (expt 2 (- (clause-len x)))) f)))
    (>= (+ (sum-formula f) (sum-formula g)) 1))
  (and (samevars f g)
       (intersection-property f g)
       (maxlength-property f g)
       (maxlength-property g f)
       (inequality-property f g)))

;If |F||G| <= 1, we do duality checking as follows:
;if either is '() (representing 0) , the other has to be '(()) (representing 1)
;if both have length one, and one is an empty clause, then they are not dual
;otehrwise they are dual
(define (easydual f g)
  (cond [(empty? f) (equal? g '(()))]
        [(empty? g) (equal? f '(()))]
        [(or (empty? (first f)) (empty? (first g))) false]
        [else true]))



(define (frequency var formula)
  (if (empty? formula) 0
  (/ (length (filter (lambda (x) (member var x)) formula)) (length formula))))
(define (total-frequency var f g)
  (max (frequency var f) (frequency var g)))

;Here we implement several algorithms for choosing the splitting variable.
;When there was a sacrifice between coding something in a robust, easy to modify way vs an efficient implementation, I chose the more robust method.
(define (fthresh varlist f g)
    (define guarantee (/ 1 (+ (length f) (length g))))
    (filter (lambda (x) (>= (total-frequency x f g) guarantee)) varlist))

(define (fcons varlist f g)
    (define guarantee (/ 1 (+ (length f) (length g))))
    (define (frequent-help clause formula)
      (filter (lambda (x) (>= (frequency x formula) guarantee)) clause))
    (let ((min-f (minimum-clause f))
          (min-g (minimum-clause g)))
      (if (< (length min-f) (length min-g)) (frequent-help min-f g) (frequent-help min-g f))))

(define (fmin varlist f g) varlist)

(define (fmax varlist f g)
    (define (compute-max-frequency)
      (define (iterate accum list)
        (cond [(empty? list) accum]
              [(> (total-frequency (car list) f g) accum) (iterate (total-frequency (car list) f g) (cdr list))]
              [else (iterate accum (cdr list))]))
      (iterate 0 varlist))
    (define maxfrequency (compute-max-frequency))
    (filter (lambda (x) (= (total-frequency x f g) maxfrequency)) varlist))

(define (fconsmax varlist f g)
    (fmax (fcons varlist f g) f g))

;now for tiebreakers
(define (tbnaive varlist) (first varlist))
(define (tblex varlist)   (argmin (lambda (x) x) varlist))
(define (tbnaivelast varlist) (last varlist))
(define (tbrand varlist) (list-ref varlist (random (length varlist))))

(define (FK f g pivot tiebreaker)
    (begin
     ;debuging info goes here
    (cond [ (not (sanitycheck f g)) false (error "sanity check failed")]
          [ (<= (* (clause-len f) (clause-len g)) 1) (easydual f g)]
          [ else (letrec ((x (tiebreaker (pivot (vars f) f g))) 
                          (f0 (remove-var f x))
                          (f1 (remove-clause f x))
                          (g0 (remove-var g x))
                          (g1 (remove-clause g x)))
                 (and (FK (reduce f1) (reduce (disjunction g0 g1)) pivot tiebreaker) (FK (reduce g1) (reduce (disjunction f0 f1)) pivot tiebreaker)))])))





  



                   