#lang racket/base

(provide build-expand-context
         generate-expand-context)

(define (build-expand-context v)
  (let ([c (syntax-local-context)])
    (if (pair? c)
        (cons v c)
        (list v))))

(struct in-liberal-define-context ()
  #:property prop:liberal-define-context #t)

(define (generate-expand-context [liberal-definitions? #f])
  (build-expand-context (if liberal-definitions?
                            (in-liberal-define-context)
                            (gensym 'internal-define))))
