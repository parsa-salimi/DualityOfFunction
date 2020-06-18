#lang racket
(require "fk-1.rkt" "getfunctions.rkt" "generator.rkt" "DNF.rkt")


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
    (and (subset? s1 s2) (subset? s2 s1)))
  (define varlist (vars f))
  (define (dual-helper f partial accum)
    (define newclause (FK f partial pivot tiebreaker))
    (cond [(eq? (first newclause) #t) (list partial accum)] 
          [(eq? (first (second newclause)) 'intersection) (display "i") (dual-helper f (filter (lambda (x) (not (set-same? x (second (second newclause))))) partial) (+ 1 accum))]
          [else (letrec [(cert (second newclause))
                 (maxcert (map (lambda (x) (set-subtract varlist (maximise-clause x (set-subtract varlist x) f))) cert))]
              (dual-helper f (append partial (remove-duplicates maxcert set-same?)) (+ 1 accum)))]))
  (dual-helper f '() 1))
(define (dualgen-def f) (dualgen f fcons tbnaive))

(define (benchmark f pivotlist tblist filename)
  (define output (open-output-file filename))
  (define possibilities (cartesian-product pivotlist tblist))
  (for-each (lambda (possibility)
              (define res (call-with-values (thunk (time-apply dualgen (list f (first possibility) (second possibility)))) list)) 
              (fprintf output (string-replace (string-replace (format "~a-~a-f\t\n" (first possibility) (second possibility)) "#<procedure:" "") ">" ""))
                       (fprintf output "calls to FK: ~a \t CPU time: ~a Real time: ~a\n" (second (first (first res))) (second res) (third res)))
            possibilities)
  (close-output-port output))
  
          