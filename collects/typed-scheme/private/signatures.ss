#lang scheme/base
(require scheme/unit)
(provide (all-defined-out))

;; cycle 1

(define-signature type-printer^
  (print-type has-name print-effect)) ;; done

(define-signature infer^
  (unify1 fv fv/list unfold)) ;; done 

(define-signature subst^
  (subst subst-all)) ;; done

(define-signature type-equal^
  (type-equal? type-compare type<? rename tc-result-equal?)) ;; done

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

