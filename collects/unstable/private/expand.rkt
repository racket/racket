#lang racket/base
(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Trampoline Expansion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide #%trampoline)

(define-syntax (#%trampoline stx)
  (syntax-case stx ()
    [(_ thunk)
     (procedure? (syntax-e #'thunk))
     (#%app (syntax-e #'thunk))]))
