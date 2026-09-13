#lang racket/base

(provide provide/contract
         provide/contract-for-contract-out
         (protect-out (for-syntax true-provide/contract))

         ;; typed-racket expects these export to come from this file,
         ;; so reprovide them here
         (for-syntax contract-neg-party-property
                     provide/contract-info?
                     provide/contract-info-contract-id
                     provide/contract-info-original-id
                     contract-rename-id-property
                     contract-lifted-property))

(require (for-syntax racket/base)
         "module-boundary-ctc.rkt"
         "in-out.rkt"
         syntax/location)

(define-for-syntax (true-provide/contract provide-stx just-check-errors? who)
  (define-values (p/c-clauses unprotected-submodule-name)
    (syntax-case provide-stx ()
      [(_ #:unprotected-submodule modname . more)
       (identifier? #'modname)
       (values (syntax->list #'more) (syntax-e #'modname))]
      [(_ #:unprotected-submodule x . more)
       (raise-syntax-error who
                           "expected a module name to follow #:unprotected-submodule"
                           provide-stx
                           (if (pair? (syntax-e #'more))
                               (car (syntax-e #'more))
                               #f))]
      [(_ p/c-ele ...) (values (syntax->list #'(p/c-ele ...)) #f)]))

  (define-values (code remappings)
    (generate-in/out-code who provide-stx p/c-clauses
                          #'(quote-module-name)
                          #:unprotected-submodule-name unprotected-submodule-name
                          #:just-check-errors? just-check-errors?
                          #:provide? #t))
  #`(begin
      #,code
      #,@(for/list ([remapping (in-list remappings)])
           (define orig-id (car remapping))
           (define export-id (cdr remapping))
           #`(provide (rename-out [#,orig-id #,export-id])))))

(define-for-syntax (provide/contract-for-whom stx who)
  (define s-l-c (syntax-local-context))
  (case s-l-c
   [(module-begin)
    #`(begin ;; force us into the 'module' local context
             #,stx)]
   [(module) ;; the good case
    (true-provide/contract stx #f who)]
   [else ;; expression or internal definition
    (raise-syntax-error who
                        (format "not allowed in a ~a context"
                                (if (pair? s-l-c)
                                    "internal definition"
                                    s-l-c))
                        stx)]))

(define-syntax (provide/contract stx)
  (provide/contract-for-whom stx 'provide/contract))
(define-syntax (provide/contract-for-contract-out stx)
  (provide/contract-for-whom stx 'contract-out))
