#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "private/sc.rkt"))
(provide define-syntax-parse-rule
         define-syntax-parser
         (rename-out [define-syntax-parse-rule define-simple-macro])
         (for-syntax (all-from-out syntax/parse)))

(define-syntax (define-syntax-parse-rule stx)
  (syntax-parse stx
    [(_ (macro:id . pattern) . body)
     #`(define-syntax macro
         (syntax-parser/template
          #,((make-syntax-introducer) stx)
          [((~var macro id) . pattern) . body]))]))

(define-syntax-parse-rule (define-syntax-parser macro:id option-or-clause ...)
  (define-syntax macro
    (syntax-parser option-or-clause ...)))

