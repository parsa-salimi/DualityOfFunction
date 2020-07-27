#lang racket

(require "DNF.rkt" "generator.rkt" "getfunctions.rkt")
(provide sanitycheck easydual frequency fthresh fcons fnone fmax fconsmax tbfirst tbrand tblast tblex FK)

;combines a list of lists of certificates into a single list of scertificates
(define-syntax makelist
  (syntax-rules ()
    [(makelist a) (if (list? a) a '())]
    [(makelist a b ...) (if (list? a) (append a (makelist b ...))
                            (makelist b ...))]))
;returns a list, which tells us what check we didn't pass exactly(if we didn't pass some test), or is just '(#t #t #t #t #t) if we pass all tests.
;first element : same variables property. If false, returns a pair (var formula) indicating a variable that is either only in f or only in g, and either 'f or 'g to signal whether it is only in f or only in g
;second element:intersection property. IF false. returns the clause of empty intersection.
;third and fourth  elemnts:  maaximum clause. If false, returns a clause with length greater than |g|.
(define (sanitycheck-list f g)
  (define (samevars f g)
    (define var-f (vars f))
    (define var-g (vars g))
    (letrec ((fminusg (set-subtract var-f var-g))
          (gminusf (set-subtract var-g var-f))
          (diff (if (not (empty? fminusg)) (list 'f fminusg) (list 'g gminusf))))
      (if (and (empty? fminusg) (empty? gminusf)) #t (map (lambda (x) (list x (first diff))) (second diff)))))
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
    (if (>= (+ (sum-formula f) (sum-formula g)) 1) #t '(inequality)))
  (list (samevars f g)
       (intersection-property g f)
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
    (define guarantee (/ 1 (log (+ (length f) (length g)) 2)))
    (filter (lambda (x) (>= (total-frequency x f g) guarantee)) varlist))

;constructive algorithm inspired by Lemma 2.2 in Fredman and Khachiyan's paper, returns the variables in the min-clause satisfying the threshold.
(define (fcons varlist f g)
    (define guarantee (/ 1 (log (+ (length f) (length g)) 2)))
    (define (frequent-help clause formula)
      (filter (lambda (x) (>= (frequency x formula) guarantee)) clause))
    (let ((min-f (minimum-clause f))
          (min-g (minimum-clause g)))
      (if (< (length min-f) (length min-g)) (sort (frequent-help min-f g) <) (sort (frequent-help min-g f) <))))

;naive implementation, just returns the list
(define (fnone varlist f g) varlist)

;returns all the elements with maximal frequency. remark: fcons usually works better.
(define (fmax varlist f g)
    (define (compute-max-frequency)
      (foldl (lambda (a accum) (if (> (total-frequency a f g) accum) (total-frequency a f g) accum)) 0 varlist))
    (define maxfrequency (compute-max-frequency))
    (filter (lambda (x) (= (total-frequency x f g) maxfrequency)) varlist))

(define (fconsmax varlist f g)
    (fmax (fcons varlist f g) f g))

;now for the tiebreakers
(define (tbfirst varlist) (first varlist))
(define (tblex varlist)   (argmin (lambda (x) x) varlist))
(define (tblast varlist) (last varlist))
(define (tbrand varlist) (list-ref varlist (random (length varlist))))

(define (gencertificate f g failedlist varlist)
  (define (conflicting-assignment-maxlength clause formula)
    (define subsets (map (lambda (x) (remove x clause)) clause))
    (define (nonempty-clause-formula c f)
      (cond [(empty? f) true]
            [(empty? (set-intersect c (first f))) false]
            [else (nonempty-clause-formula c (rest f))]))
    (filter (lambda (x) (nonempty-clause-formula x formula)) subsets))
  (define (find-clauses-containing-var formula var)
    (filter (lambda (x) (member var x)) formula))
  (define (choose-cert certdata selector function)
    (if (selector certdata) (function certdata) #t))
  (define (conflicting-assignment-var x)
    (if (eq? (second x) 'f) (map (lambda (y) (remove (first x) y)) (find-clauses-containing-var f (first x)))
                                                (map (lambda (y) (set-subtract varlist (remove (first x) y))) (find-clauses-containing-var g (first x)))))
         ;if empty-intersection fails, the characteristic vector of the certificate clause is an answer:
  (cond [(list? (second failedlist))  (list 'intersection (second failedlist))] ;this conflicting assignment is different from the others, f(x)=g(x')=1. remove (x') from g instead.
        [else  (filter list? (makelist
                    (choose-cert (first failedlist) list? (lambda (conflictingvars) (foldl append '() (map (lambda (x) (conflicting-assignment-var x)) conflictingvars ))))
                    (choose-cert (third failedlist) list? (lambda (x) (conflicting-assignment-maxlength x g)))
                    (choose-cert (fourth failedlist) list? (lambda (x) (set-subtract varlist (conflicting-assignment-maxlength x f))))
                    ;(choose-cert (fifth failedlist) list? (lambda (x) (generate-with-expectations f g varlist)))
                    ))]))
(define (generate-with-expectations f g varlist)
  ;proceed as in lemma 1 of FK
  (define (E zerovars onevars)
    ;remove all implicants with zero and remove variables with 1 from implicants
    (define newf (foldl (lambda (x result) (remove-var result x)) (foldl (lambda (x result) (remove-var result x)) f onevars) zerovars))
    (define newg (foldl (lambda (x result) (remove-var result x)) (foldl (lambda (x result) (remove-var result x)) g zerovars) onevars))
    (define (sum-formula f) (foldl + 0 (map (lambda (x) (expt 2 (- (clause-len x)))) f)))
    (+ (sum-formula f) (sum-formula g)))
  ;for every variable in varlist, we first put it in zerovars, then in onevars, and see which gives a smaller expected value
  (define (generate-helper varlst ones zeroes)
    (cond [(empty? varlst) ones]
          [else (let ((E-0 (E (cons (first varlst) zeroes) ones))
                      (E-1 (E zeroes (cons (first varlst) ones))))
                  (if (> E-1 E-0) (generate-helper (rest varlst) ones (cons (first varlst) zeroes))
                      (generate-helper (rest varlst) (cons (first varlst) ones) zeroes)))]))
  (generate-helper varlist '() '()))
  
  


(define (simpledisjunction? f g)
  (define (listofones g)
    (cond [(empty? g) #t]
          [(= (length (first g)) 1) (listofones (rest g))]
          [else #f]))
  (and (= (length f) 1)
       (listofones g)))
  (define (set-same? s1 s2)
    (if (and (list? s1) (list? s2))
    (and (subset? s1 s2) (subset? s2 s1)) #f))

(define dk1 0)
(define d1k 0)
(define var4types (make-hash))
(for [(type var4list)]
  (hash-set! var4types (car type) 0))
;(define (update-typecount f g)
  ;(for [(type var4list)]
  ;  (when (or (isa f (car type)) (isa g (car type)))
  ;      (hash-set! var4types (car type) (+ 1 (hash-ref var4types (car type)))))))
;returns a pair (res cert). res is either true or false. if false, cert is a certificate. If true, cert is 'nocert
(define (FK-help f g pivot tiebreaker varlist)
    (begin
      (define sanitylist (sanitycheck-list f g))
      ;(when (<= (length (vars f)) 4) (update-typecount f g))
      ;(printf "~a\n" sanitylist)
    (cond [(not (equal? sanitylist '(#t #t #t #t #t))) (let ([certs (gencertificate f g sanitylist varlist)])
                                                             (list #f certs))]
          [(<= (* (clause-len f) (clause-len g)) 1)  (easydual f g varlist)]
          [(simpledisjunction? g f) (set! dk1 (+ 1 dk1)) '(#t 'nocert)] 
          [(simpledisjunction? f g) (set! d1k (+ 1 d1k)) '(#t 'nocert)]
          [ else (letrec ((x (tiebreaker (pivot (sort (vars f) <) f g))) 
                          (f0 (remove-var f x))
                          (f1 (remove-clause f x))
                          (g0 (remove-var g x))
                          (g1 (remove-clause g x))
                          (first-recursion (FK-help (reduce f1) (reduce (disjunction g0 g1)) pivot tiebreaker (remove x varlist))))
                   (if  (first first-recursion)
                        (let ((second-recursion (FK-help (reduce (disjunction f0 f1)) (reduce g1) pivot tiebreaker (remove x varlist))))
                          (if (first second-recursion)
                               '(#t 'nocert)
                              ;otherwise the second recursion has failed and we have a certificate g_1(y') = f_0(y) \/ f_1(y)
    
                               (list #f (map (lambda (cert) (insert cert x 1)) (second second-recursion)))))
                        ;otherwise the first recursion has failed and we have a certificate f_1(y') = g_0(y) \/ g_1(y)
                        (list #f (map (lambda (cert) (insert cert x 0)) (second first-recursion)))))])))

(define (FK f g pivot tiebreaker)
  (FK-help f g pivot tiebreaker (vars f)))
(define (FK-def f g)
  (FK f g fconsmax tblast))

(define (print-occurances f)
  (define (f-occurances func var)
    (define mylist '())
    (for [(i (range 0 (length func)))
          (cl func)]
      (when (member var cl) (set! mylist (append mylist (list i)))))
    mylist)
  (for [(v (sort (vars f) <))]
   (when (odd? v)
   (printf "~a.\t\t" v)
     (printf "(")
     (for [(var (f-occurances f v))]
       (printf "~a," var))
     (printf ")\n")
     )))
(define (prn f lst)
  (let ([newf (foldl (lambda (x acc)
           (remove-clause acc x)) f lst)])
    (print-occurances newf)))

(define (print-covers n)
  (for [(i (combinations (map (λ(x) (+ 1 (* 2 x))) (range 0 15)) n))]
  (define f (prn (g-n 3) i)) 
  (when (not (empty? f))
    (printf "~a.\t~a\n" i f))))








    





  



                   