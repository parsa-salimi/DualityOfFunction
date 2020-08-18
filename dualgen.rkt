#lang racket
(require "fk-1.rkt" "getfunctions.rkt" "generator.rkt" "DNF.rkt")
(provide dual)


;formula is in DNF
;search if there if an implicant in formula which is a subset of the given implicant
(define (istrue implicant formula)
  (define mappedlist (map (lambda (x) (set-subtract x implicant)) formula))
  (member '() mappedlist))

(define (isfalse implicant formula) (not (istrue implicant formula)))

(define (maximise-clause clause varlist g)
  (define (maximise-clause-help accum varlist g)
  (cond [(empty? varlist) accum]
        ;check to see if X-bar still makes g true by flipping the first switch on
        [(isfalse  (cons (first varlist) accum) g) (maximise-clause-help (cons (first varlist) accum) (rest varlist) g)]
        [else (maximise-clause-help accum (rest varlist) g)]))
  (maximise-clause-help clause varlist g))
;we just use fconsmax with naivelast tiebreaker
(define (dualgen f pivot tiebreaker)
  (define (set-same? s1 s2)
    (if (and (list? s1) (list? s2))
    (and (subset? s1 s2) (subset? s2 s1)) #f))
  (define varlist (vars f))
  ;(define out (open-output-file "err.txt"))
  (define (dual-helper f partial accum)
    (define newclause (FK f partial pivot tiebreaker))
    ;(fprintf out "clause: ~a\n\n\n" newclause)
    (cond [(eq? (first newclause) #t) (list partial accum)] 
          [(eq? (first (second newclause)) 'intersection)  (display "intersection")] ;(dual-helper f (filter (lambda (x) (not (set-same? x (second (second newclause))))) partial) (+ 1 accum))]
          [else (letrec [(cert (second newclause))
                         (maxcert (map (lambda (x) (set-subtract varlist (maximise-clause x (set-subtract varlist x) f))) cert))
                         (newpartial (append partial (remove-duplicates maxcert set-same?)))]
                 (if (eq? newpartial partial) (display "e") (display ""))
              (dual-helper f newpartial (+ 1 accum)))]))
  (dual-helper f '() 1))

(define (dual f)
  (define (nonsup f g)
    (filter (lambda (c)
                (andmap (lambda (x) (not (subset? x c))) g)) f))
  (cond [(equal? f '()) '(())]
        [(equal? f '(())) '()]
        [else
         (letrec ((x (first (sort (vars f) <)))
                       (f1  (remove-var f x))
                       (f0  (remove-clause f x))
                       (g0 (dual (reduce (disjunction f0 f1))))
                       (g0-or-g1 (dual f0))
                       (g1 (nonsup g0-or-g1 g0)))
                (reduce (disjunction g0 (mult `((,x)) g1))))]))

(define (dualgen-default f) (dualgen f fcons tbfirst))
(define (cert-dual f) (first (dualgen-default f)))



  
          