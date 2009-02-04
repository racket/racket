#lang scheme/base
(require scheme/unit scheme/contract "../utils/utils.ss")
(require (rep type-rep)
         (utils unit-utils)
         (private type-utils))
(provide (all-defined-out))

(define-signature typechecker^
  ([cnt type-check (syntax? . -> . syntax?)] 
   [cnt tc-toplevel-form (syntax? . -> . any)]))

(define-signature tc-expr^
  ([cnt tc-expr (syntax? . -> . tc-result?)]
   [cnt tc-expr/check (syntax? Type? . -> . tc-result?)]
   [cnt tc-expr/check/t (syntax? Type? . -> . Type?)]
   [cnt check-below (->d ([s (or/c Type? tc-result?)] [t Type?]) () [_ (if (Type? s) Type? tc-result?)])]
   [cnt tc-literal (any/c . -> . Type?)]
   [cnt tc-exprs ((listof syntax?) . -> . tc-result?)]
   [cnt tc-exprs/check ((listof syntax?) Type? . -> . tc-result?)]
   [cnt tc-expr/t (syntax? . -> . Type?)]))

(define-signature check-subforms^
  ([cnt check-subforms/ignore (syntax? . -> . any)]
   [cnt check-subforms/with-handlers (syntax? . -> . any)]
   [cnt check-subforms/with-handlers/check (syntax? Type? . -> . any)]))

(define-signature tc-if^
  ([cnt tc/if-twoarm (syntax? syntax? syntax? . -> . tc-result?)]   
   [cnt tc/if-twoarm/check (syntax? syntax? syntax? Type? . -> . tc-result?)]))

(define-signature tc-lambda^
  ([cnt tc/lambda (syntax? syntax? syntax? . -> . tc-result?)]
   [cnt tc/lambda/check (syntax? syntax? syntax? Type? . -> . tc-result?)]
   [cnt tc/rec-lambda/check (syntax? syntax? syntax? syntax? (listof Type?) Type? . -> . Type?)]))

(define-signature tc-app^
  ([cnt tc/app (syntax? . -> . tc-result?)] 
   [cnt tc/app/check (syntax? Type? . -> . tc-result?)]
   [cnt tc/funapp (syntax? syntax? tc-result? (listof tc-result?) (or/c #f Type?) . -> . tc-result?)]))

(define-signature tc-let^
  ([cnt tc/let-values (syntax? syntax? syntax? syntax? . -> . tc-result?)]
   [cnt tc/letrec-values (syntax? syntax? syntax? syntax? . -> . tc-result?)]
   [cnt tc/let-values/check (syntax? syntax? syntax? syntax? Type? . -> . tc-result?)]
   [cnt tc/letrec-values/check (syntax? syntax? syntax? syntax? Type? . -> . tc-result?)]))

(define-signature tc-dots^
  ([cnt tc/dots (syntax? . -> . (values Type? symbol?))]))

