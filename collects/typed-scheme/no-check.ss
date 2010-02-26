#lang scheme/base

#;(require "private/prims.ss")
(provide (all-from-out scheme/base)
	 (all-defined-out)
	 #;(all-from-out "private/prims.ss"))

(define-syntax-rule (define-type-alias . _) (begin))

(define-syntax-rule (define: nm _ _ . body)
  (define nm . body))

(define-syntax-rule (ann e . rest) e)
(define-syntax-rule (inst e . rest) e)

(define-syntax-rule (require/typed mod [id . _] ...)
  (require (only-in mod id ...)))

(define-syntax-rule (: . args) (begin))

(define-syntax let:
  (syntax-rules ()
    [(_ ([id _ _ . rest] ...) . b)
     (let ([id . rest] ...) . b)]
    [(_ id _ _ ([ids _ _ e] ...) . b)
     (let id ([ids e] ...) . b)]))

(define-syntax-rule (lambda: ([id . rest] ...) . b)
  (lambda (id ...) . b))

(define-syntax-rule (λ: . arg) (lambda: . arg))
