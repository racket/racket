#lang racket/base
(require racket/contract
         file/gzip
         file/gunzip)

(provide/contract
 [gzip/bytes (bytes? . -> . bytes?)]
 [gunzip/bytes (bytes? . -> . bytes?)])

(define (gzip/bytes b)
  (define gzb-p (open-output-bytes))
  (gzip-through-ports
   (open-input-bytes b)
   gzb-p #f (current-seconds))
  (get-output-bytes gzb-p))

(define (gunzip/bytes gzb)  
  (define b-p (open-output-bytes))
  (gunzip-through-ports
   (open-input-bytes gzb) b-p)
  (get-output-bytes b-p))
