#lang racket/base
(require (for-syntax racket/base)
         racket/fixnum)

(provide define-fixnum

         ;; to cooperate with macros that explicitly
         ;; manage closures, as in "object.rkt"
         capture-fixnum
         (for-syntax make-fixnum-transformer))

;; Representing a mutable, fixnum-valued variable with an fxvector can
;; avoid a write barrier on assignment

(define-syntax-rule (define-fixnum id v)
  (begin
    (define cell (fxvector v))
    (define-syntax id (make-fixnum-transformer #'cell))))

(define-for-syntax (make-fixnum-transformer cell-id)
  (with-syntax ([cell cell-id])
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! _ r) #'(fxvector-set! cell 0 r)]
         [(_ #:capture-fixnum) #'cell] ; see `capture-fixnum`
         [(... (_ ...)) (raise-syntax-error stx "bad use" stx)]
         [_ #'(fxvector-ref cell 0)])))))

(define-syntax-rule (capture-fixnum id)
  (id #:capture-fixnum))
