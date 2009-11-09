#lang scheme
(require scheme/serialize)

(define (read/string str)
  (read (open-input-string str)))
(define (write/string v)
  (define str (open-output-string))
  (write v str)
  (get-output-string str))

; lowercase-symbol! : (or/c string bytes) -> symbol
(define (lowercase-symbol! s)
  (string->symbol
   (string-downcase
    (if (bytes? s)
        (bytes->string/utf-8 s)
        s))))

(provide/contract
 [lowercase-symbol! ((or/c string? bytes?) . -> . symbol?)]
 [read/string (string? . -> . serializable?)]
 [write/string (serializable? . -> . string?)])
