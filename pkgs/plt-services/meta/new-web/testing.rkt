#lang racket/base

(provide rewrite-for-testing)

;; Adjust root URLs for testing purposes.
;; Root URLs must be written into roots, not
;; paths within a URLs
(define (rewrite-for-testing s)
  s)
