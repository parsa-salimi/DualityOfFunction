#lang racket
(require net/url)
(provide getf)
;download file
(define (databytes urlstring) (port->string (get-pure-port (string->url urlstring))))
(define (getf urlstring)
  ;(define out (open-output-file filename))
  (define data (databytes urlstring))
  ;a list of strings, each representing a clause
  (define liststring (string-split data "\n"))
  ;a list of lists of strings, each string being a number
  (define listliststring  (map (lambda (string) (string-split string)) liststring))
  (define (liststringnum->listnum lst) (map string->number lst))
  (map liststringnum->listnum listliststring))
  