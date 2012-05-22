#lang racket/base
(require (rename-in "private/generics.rkt"
                    [define-generics define-generics/pre])
         (for-syntax racket/base racket/local))

;; Convenience layer on top of racket/private/generics.
;; To avoid circular dependencies, racket/private/generics cannot use
;; `parse-keyword-options' (which depends on racket/dict). So we do
;; keyword argument parsing here.
;; Files that use racket/private/generics _must_ pass _all_ keyword
;; arguments to define-generics _in_order_.

(provide define-generics define/generic)

(define-syntax (define-generics stx) ; allows out-of-order / optional kw args
  (syntax-case stx () ; can't use syntax-parse, since it depends on us
    [(_ (name) (generic . generics-args) ...)
     #'(define-generics (name #:defined-table defined-table)
         (generic . generics-args) ...)]
    [(_ (name #:defined-table defined-table)
        (generic . generics-args) ...)
     (local [(define name-str (symbol->string (syntax-e #'name)))
             (define (id . strs)
               (datum->syntax
                #'name (string->symbol (apply string-append strs)) #'name))]
      (with-syntax ([name? (id name-str "?")]
                    [gen:name (id "gen:" name-str)])
       #'(define-generics/pre (name gen:name prop:name name?
                                    #:defined-table defined-table
                                    ;; the following is not public
                                    #:prop-defined-already? #f)
           (generic . generics-args) ...)))]))
