#lang racket
(require "DNF.rkt")
(require "generator.rkt")
(provide sanitycheck easydual frequency fthresh fcons fmin fmax fconsmax tbnaive tbrand tbnaivelast tblex fthomas)

;returns a list, which tells us what check we didn't pass exactly(if we didn't pass some test), or is just '(#t #t #t #t #t) if we pass all tests.
;first element : same variables property. If false, returns a pair (var formula) indicating a variable that is either only in f or only in g, and either 'f or 'g to signal whether it is only in f or only in g
;second element:intersection property. IF false. returns the clause of empty intersection.
;third and fourth  elemnts:  maaximum clause. If false, returns a clause with length greater than |g|.
(define (sanitycheck-list f g)
  (define (samevars f g)
    (define var-f (vars f))
    (define var-g (vars g))
    (define diff (if (> (length var-f) (length var-g)) (set-subtract var-f var-g) (set-subtract var-g var-f)))
    (if (empty? diff) #t (list (first diff) (if (> (length var-f) (length var-g)) 'f 'g))))
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
(define (sanitycheck f g)
  (equal? (sanitycheck-list f g) '(#t #t #t #t #t)))

;If |F||G| <= 1, we do duality checking as follows:
;if either is '() (representing 0) , the other has to be '(()) (representing 1)
;if both have length one, and one is an empty clause, then they are not dual
;otehrwise they are dual(notice that we are guaranteed that the formulas pass the sanity-checks, which makes things much easier)
(define (easydual f g varlist)
   ;generate certificate if the other formula is 0
   (define (cert-0 formula) (first formula))
   (define (cert-1 formula) (set-subtract varlist (first formula)))
  (cond
        [(and (empty? f) (empty? g)) (list #f '())] ;TODO: could be wrong here
        [(empty? f) (if  (equal? g '(())) '(#t 'nocert) (list #f (cert-0 g)))]
        [(empty? g) (if  (equal? f '(())) '(#t 'nocert) (list #f (cert-0 f)))]
        ;neither is empty and either f is 1 or g is 1
        [(empty? (first f)) (list #f (cert-1 g))]
        [(empty? (first g)) (list #f (cert-1 f))]
        [else '(#t 'nocert)]))

(define (frequency var formula)
  (if (empty? formula) 0
  (/ (length (filter (lambda (x) (member var x)) formula)) (length formula))))
(define (total-frequency var f g)
  (max (frequency var f) (frequency var g)))

;Here we implement several algorithms for choosing the splitting variable.
;When there was a sacrifice between coding something in a robust, easy to modify way vs an efficient implementation, I chose the more robust method.

;list of all variables passing threshold
(define (fthresh varlist f g)
    (define guarantee (/ 1 (+ (length f) (length g))))
    (filter (lambda (x) (>= (total-frequency x f g) guarantee)) varlist))

;constructive algorithm inspired by Lemma 2.2 in Fredman and Khachiyan's paper, returns the variables in the min-clause satisfying the threshold.
(define (fcons varlist f g)
    (define guarantee (/ 1 (+ (length f) (length g))))
    (define (frequent-help clause formula)
      (filter (lambda (x) (>= (frequency x formula) guarantee)) clause))
    (let ((min-f (minimum-clause f))
          (min-g (minimum-clause g)))
      (if (< (length min-f) (length min-g)) (frequent-help min-f g) (frequent-help min-g f))))

;naive implementation, just returns the list
(define (fmin varlist f g) varlist)

;returns all the elements with maximal frequency. remark: fcons usually works better.
(define (fmax varlist f g)
    (define (compute-max-frequency)
      (foldl (lambda (a accum) (if (> (total-frequency a f g) accum) (total-frequency a f g) accum)) 0 varlist))
    (define maxfrequency (compute-max-frequency))
    (filter (lambda (x) (= (total-frequency x f g) maxfrequency)) varlist))

(define (fconsmax varlist f g)
    (fmax (fcons varlist f g) f g))

;this just returns a list of one element; meant to replicate Thomas' code
(define (fthomas  varlist f g)
  (define (iter-max lst)  ;accum is a pair: (variable, frequency), returns a pair maximizing the frequency
    (foldl (lambda (a accum) (if (> (total-frequency a f g) (second accum)) (list a (total-frequency a f g)) accum))
           (list 0 0) lst))
  (list (first (iter-max (sort (fthresh (sort varlist <) f g) <)))))

;now for the tiebreakers
(define (tbnaive varlist) (first varlist))
(define (tblex varlist)   (argmin (lambda (x) x) varlist))
(define (tbnaivelast varlist) (last varlist))
(define (tbrand varlist) (list-ref varlist (random (length varlist))))

(define (gencertificate f g failedlist varlist)
         ;if empty-intersection fails, the characteristic vector of the certificate clause is an answer:
  (cond [(list? (second failedlist))  (second failedlist)] ;in our representation the characteristic vector is just the clause
        ;next check the samevars property. remember that we have alist of variables not contained in the other formula
        [(list? (first failedlist)) f]
        [else g]))


(define (simpledisjunction? f g)
  (define (listofones g)
    (cond [(empty? g) #t]
          [(= (length (first g)) 1) (listofones (rest g))]
          [else #f]))
  (and (= (length f) 1)
       (listofones g)))
;returns a pair (res cert). res is either true or false. if false, cert is a certificate. If true, cert is 'nocert
(define (FK-help f g pivot tiebreaker vars)
    (begin
      (define sanitylist (sanitycheck-list f g))
    (cond [(not (equal? sanitylist '(#t #t #t #t #t))) '(#f (gencertificate f g sanitylist vars))] ;(gencertificate f g sanitylist)]
          [(<= (* (clause-len f) (clause-len g)) 1) (easydual f g vars)]
          [(simpledisjunction? g f) '(#t 'nocert)] ;TODO: generate certificate for when this is false
          [(simpledisjunction? f g) '(#t 'nocert)]
          [ else (letrec ((x (tiebreaker (pivot vars f g))) 
                          (f0 (remove-var f x))
                          (f1 (remove-clause f x))
                          (g0 (remove-var g x))
                          (g1 (remove-clause g x))
                          (first-recursion (FK-help (reduce f1) (reduce (disjunction g0 g1)) pivot tiebreaker (remove x vars))))
                   (if  (first first-recursion)
                        (let ((second-recursion (FK-help (reduce (disjunction f0 f1)) (reduce g1) pivot tiebreaker (remove x vars))))
                          (if (first second-recursion)
                               '(#t 'nocert)
                              ;otherwise the second recursion has failed and we have a certificate g_1(y') = f_0(y) \/ f_1(y)
                              (list #f (insert (second second-recursion) x 1))))
                        ;otherwise the first recursion has failed and we have a certificate f_1(y') = g_0(y) \/ g_1(y)
                        (list #f (insert (second first-recursion) x 0))))])))

(define (FK f g pivot tiebreaker)
  (FK-help f g pivot tiebreaker (vars f)))





  



                   