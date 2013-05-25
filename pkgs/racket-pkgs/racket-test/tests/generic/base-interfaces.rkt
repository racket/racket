#lang racket/base

;; Test the existence of forged generic functions for
;; gen:custom-write and gen:equal+hash

(require racket/generic)

(struct tuple (ref)
  #:methods gen:custom-write
  [(define/generic super write-proc)
   (define (write-proc v p w)
     (super v p w))]
  #:methods gen:equal+hash
  [(define/generic super-eq equal-proc)
   (define/generic hash1 hash-proc)
   (define/generic hash2 hash2-proc)
   (define (equal-proc a b e)
     (super-eq (tuple-ref a) (tuple-ref b) e))
   (define (hash-proc a h)
     (hash1 (tuple-ref a) h))
   (define (hash2-proc a h)
     (hash2 (tuple-ref a) h))])

;; ok if these don't raise unbound id errors
(write (tuple 5))
(equal? (tuple 5) (tuple 5))
(equal-hash-code (tuple 5))
(equal-secondary-hash-code (tuple 5))
