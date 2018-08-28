#lang racket/base
(require (for-syntax racket/base)
         racket/fixnum)

(provide define-fixnum)

;; Representing a mutable, fixnum-valued variable with an fxvector can
;; avoid a write barrier on assignment

(define-syntax-rule (define-fixnum id v)
  (begin
    (define cell (fxvector v))
    (define-syntax id
      (make-set!-transformer
       (lambda (stx)
         (syntax-case stx (set!)
           [(set! _ r) #'(fxvector-set! cell 0 r)]
           [(... (_ ...)) (raise-syntax-error stx "bad use" stx)]
           [_ #'(fxvector-ref cell 0)]))))))
