#lang scheme/base
(require scheme/unit scheme/contract "../utils/utils.ss")
(require (rep type-rep)
         (utils unit-utils)
         (types utils))
(provide (all-defined-out))

(define-signature typechecker^
  ([cnt type-check (syntax? . -> . syntax?)] 
   [cnt tc-toplevel-form (syntax? . -> . any)]))

(define-signature tc-expr^
  ([cnt tc-expr (syntax? . -> . tc-results?)]
   [cnt tc-expr/check (syntax? tc-results? . -> . tc-results?)]
   [cnt tc-expr/check/t (syntax? Type/c . -> . Type/c)]
   [cnt check-below (->d ([s (or/c Type/c tc-results?)] [t (or/c Type/c tc-results?)]) () [_ (if (Type? s) Type/c tc-results?)])]
   ;[cnt tc-literal (any/c . -> . Type/c)]
   [cnt tc-exprs ((listof syntax?) . -> . tc-results?)]
   [cnt tc-exprs/check ((listof syntax?) Type/c . -> . tc-results?)]
   [cnt tc-expr/t (syntax? . -> . Type/c)]))

(define-signature check-subforms^
  ([cnt check-subforms/ignore (syntax? . -> . any)]
   [cnt check-subforms/with-handlers (syntax? . -> . any)]
   [cnt check-subforms/with-handlers/check (syntax? Type/c . -> . any)]))

(define-signature tc-if^
  ([cnt tc/if-twoarm (syntax? syntax? syntax? . -> . tc-results?)]   
   [cnt tc/if-twoarm/check (syntax? syntax? syntax? Type/c . -> . tc-results?)]))

(define-signature tc-lambda^
  ([cnt tc/lambda (syntax? syntax? syntax? . -> . tc-results?)]
   [cnt tc/lambda/check (syntax? syntax? syntax? Type/c . -> . tc-results?)]
   [cnt tc/rec-lambda/check (syntax? syntax? syntax? syntax? (listof Type/c) Type/c . -> . Type/c)]))

(define-signature tc-app^
  ([cnt tc/app (syntax? . -> . tc-results?)] 
   [cnt tc/app/check (syntax? tc-results? . -> . tc-results?)]
   [cnt tc/funapp (syntax? syntax? tc-result? (listof tc-results?) (or/c #f Type/c) . -> . tc-results?)]))

(define-signature tc-let^
  ([cnt tc/let-values (syntax? syntax? syntax? syntax? . -> . tc-results?)]
   [cnt tc/letrec-values (syntax? syntax? syntax? syntax? . -> . tc-results?)]
   [cnt tc/let-values/check (syntax? syntax? syntax? syntax? tc-results? . -> . tc-results?)]
   [cnt tc/letrec-values/check (syntax? syntax? syntax? syntax? tc-results? . -> . tc-results?)]))

(define-signature tc-dots^
  ([cnt tc/dots (syntax? . -> . (values Type/c symbol?))]))

