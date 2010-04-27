#lang scheme
(require scheme/serialize)

(define (read/string str)
  (read (open-input-string str)))
;; Eli: Same comments as `read/bytes'.

(define (write/string v)
  (define str (open-output-string))
  (write v str)
  (get-output-string str))
;; Eli: Same comments as `write/string', and worse -- this is the same as
;;   (format "~s" v)

; lowercase-symbol! : (or/c string bytes) -> symbol
(define (lowercase-symbol! s)
  (string->symbol
   (string-downcase
    (if (bytes? s)
        (bytes->string/utf-8 s)
        s))))
;; Eli: This doesn't make any sense at all.  Why is the `!' in the name?  Why
;;   does it accept bytes?  Why does a function in a "string" library accept
;;   bytes?  How can I guess that this creates a new symbol from that name?
;;   (Which makes me think that this is (compose string->symbol string-downcase
;;   symbol->string))

(provide/contract
 [lowercase-symbol! ((or/c string? bytes?) . -> . symbol?)]
 [read/string (string? . -> . serializable?)]
 [write/string (serializable? . -> . string?)])
