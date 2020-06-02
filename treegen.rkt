#lang racket
(require pict)
(require pict/tree-layout)
(require file/convertible)
; f and g are lists of lists of int
;for example (x1 /\ x2 /\ x3)\/(x3 x4 x5) is represented by '((1 2 3) (3 4 5)). There is no ambiguity because we have assumed that our
;clauses are in DNF and moreover since we are only dealing with prime DNF's, we can safely ignore complements.
(define (disjunction f g) (remove-duplicates (append f g) equal?))
(define (clause-len I) (length I))
;we might end up with a redundant DNF but that's okay. We will deal with it in the main recursion
(define (remove-var f x)
  (cond [(empty? f) empty]
        [(member x (first f)) (cons (remove x (first f)) (remove-var (rest f) x))]
        [else (cons (first f) (remove-var (rest f) x))]))

(define (remove-clause f x)
  (cond [(empty? f) empty]
        [(member x (first f)) (remove-clause (rest f) x)]
        [else (cons (first f) (remove-clause (rest f) x))]))
(define (vars f)
  (remove-duplicates (flatten f) =))

(define (list-iter f func accum)
  (cond [(empty? f) accum]
        [(func (length (first f)) (length accum)) (list-iter (rest f) func (first f))]
        [else (list-iter (rest f) func accum)]))
(define (minimum-clause f)
  (if (empty? f) '()
  (list-iter (rest f) < (first f))))
(define (maximum-clause f)
  (if (empty? f) '()
  (list-iter (rest f) > (first f))))
  


(define (reduce formula)
  (define (minimal? clause formula)
    (if (empty? formula) true
        (and (not (subset? (car formula) clause)) (minimal? clause (cdr formula)))))
  (define (reduce-help num formula)
    (cond [(= num (length formula)) empty]
          [(minimal? (list-ref formula num) (remove (list-ref formula num) formula)) (cons (list-ref formula num) (reduce-help (+ num 1) formula))]
          [else (reduce-help (+ num 1) formula)]))
  ;if a DNF contains an empty clause, then it is trivially the only minimal clause
  (if (member '() formula) '(())
     (reduce-help 0 (remove-duplicates formula equal? )))) ;we have to remove duplicates first because otherwise both instances will be removed by a process of mutually assured destruction(MAD)



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
(define (frequent-test f g) (first (vars f)))


;the following does not return true or false, it just returns a tree.
(define counter (box 0))
(define (FK f g accum depth)
  (if (>= accum depth) false
      (begin (set-box! counter (+ 1 (unbox counter)))
    (cond [ (not (sanitycheck f g)) (tree-layout #:pict(disk #:color "blue" 15))]
          [ (<= (* (clause-len f) (clause-len g)) 1) (if (easydual f g) (tree-layout #:pict(disk #:color "red" 15)) (tree-layout #:pict(disk #:color "green" 15)))]
          [ else (letrec ((x (frequent f g))
                          (f0 (remove-var f x))
                          (f1 (remove-clause f x))
                          (g0 (remove-var g x))
                          (g1 (remove-clause g x)))
                 (tree-layout #:pict(text (number->string x)) (FK (reduce f1) (reduce (disjunction g0 g1)) (+ accum 1) depth) (FK (reduce g1) (reduce (disjunction f0 f1)) (+ accum 1) depth)))]))))
(define (fk-run f g depth) (naive-layered (FK f g 0 depth)))
  

;hardcoded for now(for testing) will autogenerate later
(define f-1 '((1) (2)))
(define g-1 '((1 2)))
(define f-2 '((1 3) (1 4) (2 3) (2 4) (5 7) (5 8) (6 7) (6 8)))
(define g-2 '((1 2 5 6) (1 2 7 8) (3 4 5 6) (3 4 7 8)))

;in DNF, adding formula's is straightforward
(define (add f g) (disjunction f g))
;multiplication is a little bit harder
(define (mult f g)
  (define (mult-clause c g)
    (map (lambda (x) (remove-duplicates (append c x) =)) g))
  (define (mult-accum f accum)
    (if (empty? f) accum
        (mult-accum (cdr f) (disjunction (mult-clause (car f) g) accum))))
  (reduce (mult-accum f '())))
(define (f-n k)
  (define (N n) (expt 2 (- (* 2 n) 1)))
  (define (vargen a b) (range a (+ 1 b)))
  (define (f-list k varlist)
    (cond [(= k 1) (map list varlist)]
          [else
           (letrec ((newk (N (- k 1)))
                 (list-1 (take varlist newk))
                 (list-2 (take (drop varlist newk) newk))
                 (list-3 (take (drop varlist (* 2 newk))  newk))
                 (list-4 (take (drop varlist (* 3 newk))  newk)))
           (add (mult (f-list (- k 1) list-1) (f-list (- k 1) list-2))
                (mult (f-list (- k 1) list-3) (f-list (- k 1) list-4))))]))
  (f-list k (vargen 1 (N k))))
(define (g-n k)
  (define (N n) (expt 2 (- (* 2 n) 1)))
  (define (vargen a b) (range a (+ 1 b)))
  (define (g-list k varlist)
    (cond [(= k 1) (list varlist)]
          [else
           (letrec ((newk (N (- k 1)))
                 (list-1 (take varlist newk))
                 (list-2 (take (drop varlist newk) newk))
                 (list-3 (take (drop varlist (* 2 newk))  newk))
                 (list-4 (take (drop varlist (* 3 newk))  newk)))
           (mult (add (g-list (- k 1) list-1) (g-list (- k 1) list-2))
                 (add (g-list (- k 1) list-3) (g-list (- k 1) list-4))))]))
  (g-list k (vargen 1 (N k))))

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))
(define (fk-save f g) (save-pict (fk-run f g) "img.png" 'png))

(define (generate-svg f g filename depth spacing) (fprintf (open-output-file filename) (bytes->string/utf-8
  (convert (naive-layered (FK f g 0 depth) #:x-spacing 1 #:y-spacing spacing) 'svg-bytes))))
;creates a list of length [depth] containing the accumulative number of vertices at each depth. NOT EFFICIENT
(define (densitylist depth)
  (define (densityhelp a accum)
    (cond [(= a accum) '()]
          [else (set-box! counter 0)
                 (FK (f-n 3) (g-n 3) 0 a)
                 (cons (unbox counter) (densityhelp (+ a 1) accum))]))
  (densityhelp 0 depth))
                   