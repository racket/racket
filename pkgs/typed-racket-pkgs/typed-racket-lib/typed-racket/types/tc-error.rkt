#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep)
         (utils tc-utils)
         "base-abbrev.rkt"
         (prefix-in c: (contract-req))
         racket/match)

(provide/cond-contract
 [tc-error/expr ((string?) (#:return c:any/c #:stx syntax?) #:rest (c:listof c:any/c)
                 . c:->* . c:any/c)]
 [tc-error/expr/fields ((string?) (#:more (c:or/c string? #f) #:return c:any/c #:stx syntax?)
                        #:rest (c:listof c:any/c) . c:->* . c:any/c)]

 [lookup-fail (identifier? . c:-> . Type/c)]
 [lookup-type-fail (identifier? . c:-> . Type/c)])

;; produce a type-checking error, and also return a result (e.g., a type)
(define (tc-error/expr msg
                       #:return [return -Bottom]
                       #:stx [stx (current-orig-stx)]
                       . rest)
  (apply tc-error/delayed #:stx stx msg rest)
  return)

;; like `tc-error/expr`, but with modern error syntax
(define (tc-error/expr/fields msg
                              #:more [more #f]
                              #:stx [stx (current-orig-stx)]
                              #:return [return -Bottom]
                              . rst)
  (apply tc-error/fields #:more more #:stx stx #:delayed? #t msg rst)
  return)

;; error for unbound variables
(define (lookup-fail e)
  (match (identifier-binding e)
    ['lexical (tc-error/expr/fields "missing type for identifier"
                                    #:more "consider adding a type annotation with `:'"
                                    "identifier" (syntax-e e))]
    [#f (tc-error/expr "untyped top-level identifier ~a" (syntax-e e))]
    [(list _ _ nominal-source-mod nominal-source-id _ _ _)
     (define-values (mod-path base-path)
       (module-path-index-split nominal-source-mod))
     (cond [(and (not mod-path) (not base-path))
            (tc-error/expr/fields "missing type for identifier"
                                  #:more "consider adding a type annotation with `:'"
                                  "identifier" (syntax-e e))]
           [(equal? mod-path '(lib "typed/racket"))
            (tc-error/expr/fields
             "missing type for identifier"
             #:more
             (string-append "The `racket' language does not seem"
                            " to have a type for this identifier;"
                            " please file a bug report")
             "identifier" (syntax-e e)
             "from module" mod-path)]
           [else
            (tc-error/expr/fields "missing type for identifier"
                                  #:more "consider using `require/typed' to import it"
                                  "identifier" (syntax-e e)
                                  "from module" mod-path)])]))

(define (lookup-type-fail i)
  (tc-error/expr "~a is not bound as a type" (syntax-e i)))
