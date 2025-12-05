#lang racket/base
(require racket/place
         rackunit)

(module+ test
  (go))

(define (go)
  (define p (place id
              (define v (place-channel-get id))
              (define v2 (place-channel-get id))
              (place-channel-put id (and (andmap eq? v v2)
                                         v2))))
  (define (chars)
    (for/list ([c (in-range 0 #x110000 100)]
               #:unless (<= #xD800 c #xDFFF))
      (integer->char c)))
  (place-channel-put p (chars))
  (place-channel-put p (chars))
  (define ls (place-channel-get p))
  (unless (and ls (andmap eq? ls (chars)))
    (error "failed")))
