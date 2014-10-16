#lang racket/base
(require "../utils/utils.rkt" 
         racket/unit 
         (contract-req)
         (utils unit-utils) (rep type-rep) (types utils))
(provide (all-defined-out))

(define-signature tc-expr^
  ([cond-contracted tc-expr (syntax? . -> . full-tc-results/c)]
   [cond-contracted tc-expr/check (syntax? (or/c tc-results/c #f) . -> . full-tc-results/c)]
   [cond-contracted tc-expr/check/t (syntax? tc-results/c . -> . Type/c)]
   [cond-contracted tc-body/check (syntax? (or/c tc-results/c #f) . -> . full-tc-results/c)]
   [cond-contracted tc-expr/t (syntax? . -> . Type/c)]
   [cond-contracted single-value ((syntax?) ((or/c tc-results/c #f)) . ->* . full-tc-results/c)]))

(define-signature check-subforms^
  ([cond-contracted check-subforms/ignore (syntax? . -> . void?)]
   [cond-contracted check-subforms/with-handlers (syntax? (or/c tc-results/c #f) . -> . full-tc-results/c)]))

(define-signature check-class^
  ([cond-contracted check-class (syntax? (or/c tc-results/c #f) . -> . full-tc-results/c)]))

(define-signature tc-if^
  ([cond-contracted tc/if-twoarm ((syntax? syntax? syntax?) ((or/c tc-results/c #f)) . ->* . full-tc-results/c)]))

(define-signature tc-literal^
  ([cond-contracted tc-literal (->* (syntax?) ((or/c Type/c #f)) Type/c)]))

(define-signature tc-send^
  ([cond-contracted tc/send ((syntax? syntax? syntax? syntax?) ((or/c tc-results/c #f)) . ->* . full-tc-results/c)]))

(define-signature tc-expression^
  ([cond-contracted tc/#%expression ((syntax?) ((or/c tc-results/c #f)) . ->* . full-tc-results/c)]))

(define-signature tc-lambda^
  ([cond-contracted tc/lambda (syntax? syntax? syntax? (or/c tc-results/c #f) . -> . full-tc-results/c)]
   [cond-contracted tc/rec-lambda/check (syntax? syntax? syntax? (listof Type/c) tc-results/c . -> .
                                                 (values full-tc-results/c full-tc-results/c))]))

(define-signature tc-app^
  ([cond-contracted tc/app (syntax? (or/c tc-results/c #f) . -> . full-tc-results/c)]
   [cond-contracted tc/app-regular (syntax? (or/c tc-results/c #f) . -> . full-tc-results/c)]))

(define-signature tc-apply^
  ([cond-contracted tc/apply (syntax? syntax? . -> . full-tc-results/c)]))

(define-signature tc-let^
  ([cond-contracted tc/let-values ((syntax? syntax? syntax?) ((or/c #f tc-results/c)) . ->* . full-tc-results/c)]
   [cond-contracted tc/letrec-values ((syntax? syntax? syntax?) ((or/c #f tc-results/c)) . ->* . full-tc-results/c)]))

(define-signature tc-dots^
  ([cond-contracted tc/dots (syntax? . -> . (values Type/c symbol?))]))

