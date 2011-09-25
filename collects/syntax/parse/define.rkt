#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "private/sc.rkt"))
(provide define-simple-macro
         (for-syntax (all-from-out syntax/parse)))

(define-syntax (define-simple-macro stx)
  (syntax-parse stx
    [(define-simple-macro (~and (macro:id . _) pattern) . body)
     #`(define-syntax macro
         (syntax-parser/template
          #,((make-syntax-introducer) stx)
          [pattern . body]))]))
