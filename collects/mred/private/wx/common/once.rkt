#lang racket/base
(require ffi/unsafe)

(provide (protect-out scheme_register_process_global))

;; This module must be instantiated only once:

(define scheme_register_process_global
  (get-ffi-obj 'scheme_register_process_global #f (_fun _string _pointer -> _pointer)))

(let ([v (scheme_register_process_global "GRacket-support-initialized"
                                         (cast 1 _scheme _pointer))])
  (when v
    (error "cannot instantiate `racket/gui/base' a second time in the same process")))
