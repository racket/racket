#lang racket/base
(require (for-syntax racket/base))

(provide only except prefix rename tag
         import export init-depend link
         extends contracted)

(define-syntax-rule (define-syntax-for-error name message)
  (define-syntax name
    (make-set!-transformer
     (lambda (stx)
       (raise-syntax-error 
        #f
        message
        stx)))))

(define-syntax-for-error only
  "misuse of unit import keyword")
(define-syntax-for-error except
  "misuse of unit import keyword")
(define-syntax-for-error prefix
  "misuse of unit import and export keyword")
(define-syntax-for-error rename
  "misuse of unit import and export keyword")
(define-syntax-for-error tag
  "misuse of unit import and export keyword")
(define-syntax-for-error import
  "misuse of unit keyword")
(define-syntax-for-error export
  "misuse of unit keyword")
(define-syntax-for-error init-depend
  "misuse of unit keyword")
(define-syntax-for-error link
  "misuse of compound-unit keyword")
(define-syntax-for-error extends
  "misuse of define-signature keyword")
(define-syntax-for-error contracted
  "misuse of define-signature keyword")
