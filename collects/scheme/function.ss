#lang scheme/base

(provide negate curry curryr)

(define (negate f)
  (unless (procedure? f) (raise-type-error 'negate "procedure" f))
  (let-values ([(arity) (procedure-arity f)]
               [(required-kws accepted-kws) (procedure-keywords f)])
    (define negated
      (if (and (null? required-kws) (null? accepted-kws))
        ;; simple function
        (if (equal? arity 1) ; optimize common case
          (lambda (x) (not (f x)))
          (lambda xs (not (apply f xs))))
        ;; keyworded function
        (make-keyword-procedure
         (lambda (kws kw-args . rest)
           (not (keyword-apply f kws kw-args rest))))))
    negated))

(define (curry f . args)
  (unless (procedure? f) (raise-type-error 'curry "procedure" f))
  (let loop ([args args])
    (define curried
      (if (null? args) ; always at least one step
        (lambda more (loop more))
        (lambda more
          (let ([args (append args more)])
            (if (procedure-arity-includes? f (length args))
              (apply f args)
              (loop args))))))
    curried))

(define (curryr f . args)
  (unless (procedure? f) (raise-type-error 'curry "procedure" f))
  (let loop ([args args])
    (define curried-right
      (if (null? args) ; always at least one step
        (lambda more (loop more))
        (lambda more
          (let ([args (append more args)])
            (if (procedure-arity-includes? f (length args))
              (apply f args)
              (loop args))))))
    curried-right))
