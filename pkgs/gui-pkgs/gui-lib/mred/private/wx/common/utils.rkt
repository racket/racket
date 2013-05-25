#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/atomic
         "once.rkt")

(provide (protect-out define-mz

                      remember-to-free-later
                      free-remembered-now))

(define-ffi-definer define-mz #f)

;; ----------------------------------------

(define to-free null)

;; Remember to free an object that might currently be in use during a
;; callback:
(define (remember-to-free-later o)
  (start-atomic) 
  (set! to-free (cons o to-free))
  (end-atomic))

;; Called outside the event loop to actually free objects that might
;; otherwise be in use during a callback:
(define (free-remembered-now free)
  (start-atomic)
  (for ([o (in-list (begin0
                     to-free
                     (set! to-free null)))])
    (free o))
  (end-atomic))
