#lang racket/base
(require racket/contract
         file/md5)
(provide/contract
 [make-labeling (bytes? . -> . (-> symbol?))])

;; REQUIREMENT: The label code must be non-numeric.
;; REQUIREMENT: The first numeric character following the label code
;;              indicates the start of the unique suffix identifying
;;              the closure struct type.

;; make-labeling: bytes -> (-> symbol)
;; produce the labeling function for a particular program
(define (make-labeling pgm)
  (define count (box 0))
  (define tag (md5 pgm))
  (lambda ()
    (begin0
      (string->symbol (format "~a:~a" tag (unbox count)))
      (set-box! count (add1 (unbox count))))))
