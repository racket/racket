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

(module+ test
  (require rackunit racket/port)

  ;; ok if these don't raise unbound id errors
  (check-equal? (with-output-to-string (lambda () (write (tuple 5)))) "#0=#0#")
  (check-equal? (tuple 5) (tuple 5))
  (check-equal? (equal-hash-code (tuple 5)) 53)
  (check-equal? (equal-secondary-hash-code (tuple 5)) 44))
