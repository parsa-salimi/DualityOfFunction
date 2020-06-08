#lang racket
(require "DNF.rkt")
(require "generator.rkt")
(provide sanitycheck easydual frequency fthresh fcons fmin fmax fconsmax tbnaive tbrand tbnaivelast tblex)

;returns a list, which tells us what check we didn't pass exactly(if we didn't pass some test), or is just '(#t #t #t #t) if we pass all tests.
;first element : same variables property. If false returns a list of variables that are in f but not in g.
;second element:intersection property. returns the clause of empty intersection if false
;third and fourth  elemnts:  maaximum clause. returns aa clause with length greater than |g| if false.
(define (sanitycheck-list f g)
  (define (samevars f g)
    (define diff (if (> (length f) (length g)) (set-subtract f g) (set-subtract g f)))
    (if (empty? diff) #t diff))
  (define (maxlength-property f g)
    (define max (maximum-clause f))
    (if (<= (length max) (length g)) #t max))
  (define (intersection-property f g)
    (define (nonempty-clause-formula c f)
      (cond [(empty? f) true]
            [(empty? (set-intersect c (first f))) false]
            [else (nonempty-clause-formula c (rest f))]))
      (cond [(empty? f) true]
            [(nonempty-clause-formula (first f) g) (intersection-property (rest f) g)]
            [else (first f)]))
  (define (inequality-property f g)
    (define (sum-formula f) (foldl + 0 (map (lambda (x) (expt 2 (- (clause-len x)))) f)))
    (if (>= (+ (sum-formula f) (sum-formula g)) 1) #t 'inequality))
  (list (samevars f g)
       (intersection-property f g)
       (maxlength-property f g)
       (maxlength-property g f)
       (inequality-property f g)))
;for backward compatibility 
(define (sanitycheck f g) (equal? (sanitycheck-list f g) '(#t #t #t #t #t)))

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

(define (gencertificate f g failedlist)
         ;if empty-intersection fails, the characteristic vector of the certificate clause is an answer:
  (cond [(list? (second failedtest)) (char-vector (second failedtest) f)]
        ;next check the samevars property. remember that we have alist of variables not contained in the other formula
        [(list? (first failedtest))

(define (FK f g pivot tiebreaker)
    (begin
      (define sanitylist (sanitycheck-list f g))
    (cond [ (not (equal? sanitylist '(#t #t #t #t))) (gencertificate f g sanitylist)]
          [ (<= (* (clause-len f) (clause-len g)) 1) (easydual f g)]
          [ else (letrec ((x (tiebreaker (pivot (vars f) f g))) 
                          (f0 (remove-var f x))
                          (f1 (remove-clause f x))
                          (g0 (remove-var g x))
                          (g1 (remove-clause g x)))
                 (and (FK (reduce f1) (reduce (disjunction g0 g1)) pivot tiebreaker) (FK (reduce g1) (reduce (disjunction f0 f1)) pivot tiebreaker)))])))





  



                   