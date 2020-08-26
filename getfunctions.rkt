#lang racket
(require net/url)
(provide getf get-function-file)
;download file
(define (databytes urlstring) (port->string (get-pure-port (string->url urlstring))))

(define (file-to-list data)
  ;(define out (open-output-file filename))
  ;a list of strings, each representing a clause
  (define liststring (string-split data "\n"))
  ;a list of lists of strings, each string being a number
  (define listliststring  (map (lambda (string) (string-split string)) liststring))
  (define (liststringnum->listnum lst) (map string->number lst))
  (map liststringnum->listnum listliststring))

(define (getf urlstring) (file-to-list (databytes urlstring)))
(define (get-function-file filename)
  (define in (open-input-file filename))
  (define list (file-to-list (port->string in)))
  (close-input-port in)
  list)
    
