#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "private/sc.rkt"))
(provide define-simple-macro
         define-syntax-parser
         (for-syntax (all-from-out syntax/parse)))

(define-syntax (define-simple-macro stx)
  (syntax-parse stx
    [(define-simple-macro (~and (macro:id . _) pattern) . body)
     #`(define-syntax macro
         (syntax-parser/template
          #,((make-syntax-introducer) stx)
          [pattern . body]))]))

(define-simple-macro (define-syntax-parser macro:id option-or-clause ...)
  (define-syntax macro
    (syntax-parser option-or-clause ...)))

