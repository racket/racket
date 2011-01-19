#lang racket/base

(require unstable/function)

(provide index? exact-rational?)

;; this is required for template in numeric-tower.rkt

;; we assume indexes are 2 bits shorter than fixnums
;; We're generating a reference to fixnum? rather than calling it, so
;; we're safe from fixnum size issues on different platforms.
(define (index? x) (and (fixnum? x) (fixnum? (* x 4))))

(define exact-rational? (conjoin rational? exact?))
