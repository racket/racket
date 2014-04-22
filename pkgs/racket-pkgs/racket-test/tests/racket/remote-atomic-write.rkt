#lang racket/base
(require ffi/unsafe
         racket/place)

;; Check that atomic memory allocated in one place can be written
;; in another place without triggering a write barrier (and crashing)

(define c-memset
  (get-ffi-obj 'memset #f
               (_fun #:in-original-place? #t _pointer _int _size -> _void)))

(define N 50)

(define (go)
  (place 
   ch
   (define s (malloc N 'atomic-interior))
   (collect-garbage)
   (c-memset s 65 N)
   (place-channel-put ch (ptr-ref s _byte (quotient N 2)))))

(module+ main
  (define p (go))
  (unless (equal? 65 (place-channel-get p))
    (error "crash"))
  'ok)
