#lang racket/base
(provide use-force)

;; `force` is not exported, so it's an internal definition in
;; a demodularized form of this module; it must not collide with
;; `force` from `racket/promise` when the demodularized form
;; is itself demodularized again

(define (force v)
  ;; recursive, so that it's not inlined away
  (if (procedure? v)
      (force (v))
      v))

(define (use-force v)
  (list 'forced (force v)))
