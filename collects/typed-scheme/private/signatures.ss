#lang scheme/base
(require scheme/unit)
(provide (all-defined-out))

(define-signature dmap^
  (dmap-meet))

(define-signature promote-demote^
  (var-promote var-demote))

(define-signature constraints^
  (exn:infer?
   fail-sym
   ;; inference failure - masked before it gets to the user program
   (define-syntaxes (fail!)
     (syntax-rules ()
       [(_ s t) (raise fail-sym)]))
   cset-meet cset-meet*
   no-constraint
   empty-cset
   insert
   cset-combine
   c-meet))

(define-signature restrict^
  (restrict))

(define-signature infer^
  (infer infer/vararg infer/dots))



;; cycle 2

(define-signature typechecker^
  (type-check tc-toplevel-form))

(define-signature tc-expr^
  (tc-expr tc-expr/check tc-expr/check/t check-below tc-literal tc-exprs tc-exprs/check tc-expr/t #;check-expr))

(define-signature check-subforms^
  (check-subforms/ignore check-subforms/with-handlers check-subforms/with-handlers/check))

(define-signature tc-if^
  (tc/if-onearm tc/if-twoarm tc/if-onearm/check tc/if-twoarm/check))

(define-signature tc-lambda^
  (tc/lambda tc/lambda/check tc/rec-lambda/check))

(define-signature tc-app^
  (tc/app tc/app/check tc/funapp))

(define-signature tc-let^
  (tc/let-values tc/letrec-values tc/let-values/check tc/letrec-values/check))

(define-signature tc-dots^
  (tc/dots))

