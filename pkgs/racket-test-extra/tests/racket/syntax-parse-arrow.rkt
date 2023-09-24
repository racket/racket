#lang racket/base

(require syntax/parse
         drracket/check-syntax)

(provide stx check-syntax-annotations)

(define stx
  #'(module anonymous racket/base
      (require syntax/parse)
      (lambda (stx)
        (syntax-parse stx
          [e #'e]
          [e #'e]))))

(define check-syntax-annotations
  (show-content stx))

(module+ test
  (require racket/match rackunit)
  ;; check that #'e points to e at L13
  (check-match check-syntax-annotations
               `(,_ ...
                 #(syncheck:add-arrow/name-dup/pxpy 246 247 ,_ ,_ 250 251 ,_ ,_ ,_ ...)
                 ,_ ...))
  ;; check that #'e points to e at L14
  (check-match check-syntax-annotations
               `(,_ ...
                 #(syncheck:add-arrow/name-dup/pxpy 264 265 ,_ ,_ 268 269 ,_ ,_ ,_ ...)
                 ,_ ...)))
