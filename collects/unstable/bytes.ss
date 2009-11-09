#lang scheme
(require scheme/serialize)

(provide/contract
 [read/bytes (bytes? . -> . serializable?)]
 [write/bytes (serializable? . -> . bytes?)]
 [bytes-ci=? (bytes? bytes? . -> . boolean?)])

(define (bytes-ci=? b0 b1)
  (string-ci=? (bytes->string/utf-8 b0)
               (bytes->string/utf-8 b1)))

(define (read/bytes bs)
  (read (open-input-bytes bs)))

(define (write/bytes v)
  (define by (open-output-bytes))
  (write v by)
  (get-output-bytes by))
