#lang racket/base
(require racket/unit racket/contract
         "../utils/utils.rkt" "../utils/unit-utils.rkt"
         (rep type-rep) (types utils))
(provide (all-defined-out))

(define-signature tc-expr^
  ([cond-contracted tc-expr (syntax? . -> . tc-results?)]
   [cond-contracted tc-literal (->* (syntax?) ((or/c #f Type/c)) Type/c)]
   [cond-contracted tc-expr/check (syntax? tc-results? . -> . tc-results?)]
   [cond-contracted tc-expr/check/t (syntax? tc-results? . -> . Type/c)]
   [cond-contracted tc-exprs ((listof syntax?) . -> . tc-results?)]
   [cond-contracted tc-exprs/check ((listof syntax?) tc-results? . -> . tc-results?)]
   [cond-contracted tc-expr/t (syntax? . -> . Type/c)]
   [cond-contracted single-value ((syntax?) ((or/c tc-results? #f)) . ->* . tc-results?)]))

(define-signature check-subforms^
  ([cond-contracted check-subforms/ignore (syntax? . -> . any)]
   [cond-contracted check-subforms/with-handlers (syntax? . -> . any)]
   [cond-contracted check-subforms/with-handlers/check (syntax? tc-results? . -> . any)]))

(define-signature tc-if^
  ([cond-contracted tc/if-twoarm ((syntax? syntax? syntax?) (tc-results?) . ->* . tc-results?)]))

(define-signature tc-lambda^
  ([cond-contracted tc/lambda (syntax? syntax? syntax? . -> . tc-results?)]
   [cond-contracted tc/lambda/check (syntax? syntax? syntax? tc-results? . -> . tc-results?)]
   [cond-contracted tc/rec-lambda/check (syntax? syntax? syntax? syntax? (listof Type/c) tc-results? . -> . tc-results?)]))

(define-signature tc-app^
  ([cond-contracted tc/app (syntax? . -> . tc-results?)]
   [cond-contracted tc/app/check (syntax? tc-results? . -> . tc-results?)]
   [cond-contracted tc/app-regular (syntax? (or/c tc-results? #f) . -> . tc-results?)]))

(define-signature tc-apply^
  ([cond-contracted tc/apply (syntax? syntax? . -> . tc-results?)]))

(define-signature tc-let^
  ([cond-contracted tc/let-values ((syntax? syntax? syntax? syntax?) ((or/c #f tc-results?)) . ->* . tc-results?)]
   [cond-contracted tc/letrec-values ((syntax? syntax? syntax? syntax?) ((or/c #f tc-results?)) . ->* . tc-results?)]))

(define-signature tc-dots^
  ([cond-contracted tc/dots (syntax? . -> . (values Type/c symbol?))]))

