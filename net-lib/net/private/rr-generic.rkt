#lang racket/base
;; Utilities for parsing and unparsing various pieces of DNS wire formats.

(provide number->octet-pair
         octet-pair->number
         octet-quad->number
         parse-name
         name->octets
         rr-data
         rr-type
         rr-name)

(define (number->octet-pair n)
  (list (arithmetic-shift n -8)
        (modulo n 256)))

(define (octet-pair->number a b)
  (+ (arithmetic-shift a 8) b))

(define (octet-quad->number a b c d)
  (+ (arithmetic-shift a 24)
     (arithmetic-shift b 16)
     (arithmetic-shift c 8)
     d))

(define (parse-name start reply)
  (define v (car start))
  (cond
    [(zero? v)
     ;; End of name
     (values #f (cdr start))]
    [(zero? (bitwise-and #xc0 v))
     ;; Normal label
     (let loop ([len v] [start (cdr start)] [accum null])
       (if (zero? len)
         (let-values ([(s start) (parse-name start reply)])
           (define s0 (list->bytes (reverse accum)))
           (values (if s (bytes-append s0 #"." s) s0)
                   start))
         (loop (sub1 len) (cdr start) (cons (car start) accum))))]
    [else
     ;; Compression offset
     (define offset (+ (arithmetic-shift (bitwise-and #x3f v) 8)
                       (cadr start)))
     (define-values [s ignore-start] (parse-name (list-tail reply offset) reply))
     (values s (cddr start))]))

;; Bytes -> LB
;; Convert the domain name into a sequence of labels, where each
;; label is a length octet and then that many octets
(define (name->octets s)
  (define (do-one s) (cons (bytes-length s) (bytes->list s)))
  (let loop ([s s])
    (define m (regexp-match #rx#"^([^.]*)[.](.*)" s))
    (if m
        (append (do-one (cadr m)) (loop (caddr m)))
        ;; terminate with zero length octet
        (append (do-one s) (list 0)))))

(define (rr-data rr)
  (cadddr (cdr rr)))

(define (rr-type rr)
  (cadr rr))

(define (rr-name rr)
  (car rr))
