#lang scheme/base
(require scheme/unit scheme/contract
         "../utils/utils.rkt"
         (rep type-rep)
         (utils unit-utils)
         (types utils))
(provide (all-defined-out))

(define-signature typechecker^
  ([cnt type-check (syntax? . -> . syntax?)] 
   [cnt tc-toplevel-form (syntax? . -> . any)]))

(define-signature tc-expr^
  ([cnt tc-expr (syntax? . -> . tc-results?)]
   [cnt tc-literal (->* (syntax?) ((or/c #f Type/c)) Type/c)]
   [cnt tc-expr/check (syntax? tc-results? . -> . tc-results?)]
   [cnt tc-expr/check/t (syntax? tc-results? . -> . Type/c)]
   [cnt check-below (->d ([s (or/c Type/c tc-results?)] [t (or/c Type/c tc-results?)]) () [_ (if (Type? s) Type/c tc-results?)])]
   [cnt tc-exprs ((listof syntax?) . -> . tc-results?)]
   [cnt tc-exprs/check ((listof syntax?) tc-results? . -> . tc-results?)]
   [cnt tc-expr/t (syntax? . -> . Type/c)]
   [cnt single-value ((syntax?) ((or/c tc-results? #f)) . ->* . tc-results?)]))

(define-signature check-subforms^
  ([cnt check-subforms/ignore (syntax? . -> . any)]
   [cnt check-subforms/with-handlers (syntax? . -> . any)]
   [cnt check-subforms/with-handlers/check (syntax? tc-results? . -> . any)]))

(define-signature tc-if^
  ([cnt tc/if-twoarm ((syntax? syntax? syntax?) (tc-results?) . ->* . tc-results?)]))

(define-signature tc-lambda^
  ([cnt tc/lambda (syntax? syntax? syntax? . -> . tc-results?)]
   [cnt tc/lambda/check (syntax? syntax? syntax? tc-results? . -> . tc-results?)]
   [cnt tc/rec-lambda/check (syntax? syntax? syntax? syntax? (listof Type/c) tc-results? . -> . tc-results?)]))

(define-signature tc-app^
  ([cnt tc/app (syntax? . -> . tc-results?)] 
   [cnt tc/app/check (syntax? tc-results? . -> . tc-results?)]
   [cnt tc/funapp (syntax? syntax? tc-results? (listof tc-results?) (or/c #f tc-results?) . -> . tc-results?)]))

(define-signature tc-let^
  ([cnt tc/let-values ((syntax? syntax? syntax? syntax?) ((or/c #f tc-results?)) . ->* . tc-results?)]
   [cnt tc/letrec-values (syntax? syntax? syntax? syntax? . -> . tc-results?)]
   [cnt tc/letrec-values/check (syntax? syntax? syntax? syntax? tc-results? . -> . tc-results?)]))

(define-signature tc-dots^
  ([cnt tc/dots (syntax? . -> . (values Type/c symbol?))]))

