#lang racket/base

(require (except-in racket/unit struct/ctc)
         racket/contract
         (submod racket/unit compat)
         (for-syntax racket/base
                     racket/private/unit-compiletime
                     racket/private/unit-syntax))

(provide (rename-out [module-begin #%module-begin]
                     [struct~s struct])
         (except-out (all-from-out racket/base)
                     #%module-begin)
         (all-from-out racket/unit)
         (all-from-out racket/contract)
         (for-syntax (all-from-out racket/base)))

(define-for-syntax (make-name s)
  (string->symbol
   (string-append (regexp-replace "-sig$" (symbol->string s) "")
                  "^")))

;; Recognizes racket require forms.
(define-for-syntax split-racket-requires
  (split-requires* (list #'require #'#%require)))

(define-syntax (module-begin stx)
  (parameterize ((error-syntax stx))
    (with-syntax ((name (datum->syntax stx
                                       (make-name (syntax-property stx 'enclosing-module-name)))))
      (syntax-case stx ()
        ((_ . x)
         (with-syntax ((((reqs ...) . (body ...))
                        (split-racket-requires (checked-syntax->list #'x))))
           (datum->syntax
            stx
            (syntax-e #'(#%module-begin
                         reqs ...
                         (provide name)
                         (define-signature name (body ...))))
            stx)))))))
