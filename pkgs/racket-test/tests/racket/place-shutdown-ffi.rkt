#lang racket/base
(require racket/place)

(module in-place racket/base
  (require racket/place
           ffi/unsafe
           ffi/unsafe/custodian)

  (provide main)

  (define malloc (get-ffi-obj 'malloc #f
                              (_fun #:in-original-place? #t _int -> _pointer)
                              (lambda () void)))

  (define (main p)
    (register-custodian-shutdown
     malloc
     (λ (ignored) (malloc 10)))
    (place-channel-put p 'ready)
    (place-channel-get p)))

(define c (make-custodian))

(define pl
  (parameterize ([current-custodian c])
    (dynamic-place '(submod (lib "tests/racket/place-shutdown-ffi.rkt") in-place)
                   'main)))
(unless (eq? (place-channel-get pl) 'ready)
  (error "place is not ready"))

(custodian-shutdown-all c)
