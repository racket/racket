#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match
         unstable/list syntax/stx
         (typecheck signatures tc-funapp)
         (types abbrev utils)
         (private type-annotation)
         (rep type-rep filter-rep)
         (utils tc-utils)

         ;; fixme - don't need to be bound in this phase - only to make tests work
         racket/bool
         '#%paramz
         ;; end fixme

         (for-template racket/base racket/bool '#%paramz))


(import tc-expr^)
(export tc-app-special^)

(define-tc/app-syntax-class (tc/app-special expected)
  #:literals (#%plain-app #%plain-lambda extend-parameterization quote
              false? not call-with-values list)
  ;; parameterize
  (pattern (extend-parameterization pmz args ...)
    (let loop ([args (syntax->list #'(args ...))])
      (if (null? args) (ret Univ)
          (let* ([p (car args)]
                 [pt (single-value p)]
                 [v (cadr args)])
            (match pt
              [(tc-result1: (Param: a b))
               (tc-expr/check v (ret a))
               (loop (cddr args))]
              [(tc-result1: t)
               (single-value v)
               (tc-error/expr #:return (or expected (ret Univ)) "expected Parameter, but got ~a" t)
               (loop (cddr args))])))))
  ;; use the additional but normally ignored first argument to make-sequence
  ;; to provide a better instantiation
  (pattern ((~var op (id-from 'make-sequence 'racket/private/for))
            (~and quo (quote (i:id ...))) arg:expr)
    #:when (andmap type-annotation (syntax->list #'(i ...)))
    (match (single-value #'op)
        [(tc-result1: (and t Poly?))
         (tc-expr/check #'quo (ret Univ))
         (tc/funapp #'op #'(quo arg)
                    (ret (instantiate-poly t (extend (list Univ Univ)
                                                     (stx-map type-annotation #'(i ...))
                                                     Univ)))
                    (list (ret Univ) (single-value #'arg))
                    expected)]))
  ;; special-case for not - flip the filters
  (pattern ((~or false? not) arg)
    (match (single-value #'arg)
      [(tc-result1: t (FilterSet: f+ f-) _)
       (ret -Boolean (make-FilterSet f- f+))]))
  ;; special case for (current-contract-region)'s default expansion
  ;; just let it through without any typechecking, since module-name-fixup
  ;; is a private function from syntax/location, so this must have been
  ;; (quote-module-name) originally.
  (pattern (op src path)
    #:declare op (id-from 'module-name-fixup 'syntax/location)
    (ret Univ))
  ;; special case for `delay'
  (pattern (mp1 (#%plain-lambda ()
                  (#%plain-app mp2 (#%plain-app call-with-values (#%plain-lambda () e) list))))
    #:declare mp1 (id-from 'make-promise 'racket/promise)
    #:declare mp2 (id-from 'make-promise 'racket/promise)
    (ret (-Promise (tc-expr/t #'e)))))
