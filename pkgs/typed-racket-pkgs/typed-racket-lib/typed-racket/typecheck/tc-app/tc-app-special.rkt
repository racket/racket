#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match
         unstable/list syntax/stx
         unstable/sequence
         (typecheck signatures tc-funapp)
         (types abbrev utils)
         (private type-annotation)
         (rep type-rep filter-rep)
         (utils tc-utils)

         (for-label racket/base racket/bool '#%paramz))


(import tc-expr^)
(export tc-app-special^)

(define-literal-set special-literals #:for-label
  (extend-parameterization false? not call-with-values list))

(define-tc/app-syntax-class (tc/app-special expected)
  #:literal-sets (kernel-literals special-literals)
  ;; parameterize
  (pattern (extend-parameterization pmz (~seq params args) ...)
    (begin
      (for ([param (in-syntax #'(params ...))]
            [arg (in-syntax #'(args ...))])
        (match (single-value param)
          [(tc-result1: (Param: a b))
           (tc-expr/check arg (ret a))]
          [(tc-result1: t)
           (single-value arg)
           (tc-error/delayed "expected Parameter, but got ~a" t)]))
      (ret Univ)))
  ;; use the additional but normally ignored first argument to make-sequence
  ;; to provide a better instantiation
  (pattern ((~var op (id-from 'make-sequence 'racket/private/for))
            (~and quo (quote (i:id ...))) arg:expr)
    #:when (andmap type-annotation (syntax->list #'(i ...)))
    (match (single-value #'op)
        [(tc-result1: (and t Poly?))
         (tc-expr/check #'quo (ret Univ))
         (tc/funapp #'op #'(quo arg)
                    (instantiate-poly t (extend (list Univ Univ)
                                                (stx-map type-annotation #'(i ...))
                                                Univ))
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
