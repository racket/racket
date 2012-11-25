#lang racket/base

(require (for-syntax racket/base)
         (only-in typed/racket/base define:))

(provide rename-defines)

(define-syntax (rename-defines stx)
  (syntax-case stx ()
    [(_ e)
     (let ([expanded  (local-expand #'e (syntax-local-context) #f)])
       (syntax-case expanded (define-values)
         [(define-values (x ...) expr)
          (with-syntax ([(y ...)  (generate-temporaries #'(x ...))])
            (syntax/loc stx
              (begin
                (define-syntax x (make-rename-transformer #'y)) ...
                (define-values (y ...) expr))))]
         [_  #'e]))]))
