#lang scheme/base

(provide build-expand-context
         generate-expand-context)

(define (build-expand-context v)
  (let ([c (syntax-local-context)])
    (if (pair? c)
        (cons v c)
        (list v))))

(define (generate-expand-context)
  (build-expand-context (gensym 'internal-define)))

