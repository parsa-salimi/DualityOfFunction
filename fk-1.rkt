#lang racket
(require "DNF.rkt")
(require "generator.rkt")
(provide sanitycheck easydual frequent)


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

;we can just choose a random variable, or a variable that satisfies the frequency conditions in Fredman Khachyian. The following searches the entire min-length clause, and thus can potentially return
;a better variable than one that merely satisfies the FK frequency criterea
(define (frequent f g)
  ;clause is the clause of minimum length, f is the other formula, for now just return the first element
  (define (frequent-help clause f)
    (define (iterate func list accum lastfrequency)
      (cond [(empty? list) accum]
            [else (let ((frequency (func (first list))))
                    (if (> frequency lastfrequency) (iterate func (rest list) (first list) frequency)
                        (iterate func (rest list) accum lastfrequency)))]))
    (define (frequency var formula)
      (length (filter (lambda (x) (member var x)) formula)))
    (iterate (lambda (x) (frequency x f)) (rest clause) (first clause) 0))
  (let ((min-f (minimum-clause f ))
        (min-g (minimum-clause g )))
    (if (< (length min-f) (length min-g)) (frequent-help min-f g) (frequent-help min-g f))))



(define (FK f g tree)
    (begin
     ;(cond [(= tree 0) (printf "left:\t f:~a g:~a\n" f g)]
     ;      [(= tree 1) (printf "\t\t right:\t f:~a g:~a\n" f g)]
     ;      [else (printf "\troot:\t f:~a g:~a\n" f g)])
    (cond [ (not (sanitycheck f g)) false (printf "sanity check failed")]
          [ (<= (* (clause-len f) (clause-len g)) 1) (easydual f g)]
          [ else (letrec ((x (frequent f g))
                          (f0 (remove-var f x))
                          (f1 (remove-clause f x))
                          (g0 (remove-var g x))
                          (g1 (remove-clause g x)))
                 (and (FK (reduce f1) (reduce (disjunction g0 g1)) 0) (FK (reduce g1) (reduce (disjunction f0 f1)) 1)))])))
(define (fk-run f g) (FK f g 2))



  



                   