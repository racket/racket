#lang racket/base

(require (for-syntax racket/base)
         (only-in typed/racket/base inst Index))

(provide array/syntax)

(define-for-syntax (syntax-vector-shape e-stx)
  (syntax-case e-stx ()
    [#[]  (list 0)]
    [#[e0 e ...]
     (let ([lst  (syntax->list #'(e0 e ...))])
       (define d (length lst))
       (define ds (syntax-vector-shape (car lst)))
       (if ds
           (let loop ([lst  (cdr lst)])
             (cond [(null? lst)  (cons d ds)]
                   [(equal? ds (syntax-vector-shape (car lst)))
                    (loop (cdr lst))]
              [else  #f]))
           #f))]
    [_  null]))

(define-for-syntax (syntax-vector-flatten e-stx)
  (reverse
   (let loop ([e-stx e-stx] [acc null])
     (syntax-case e-stx ()
       [#[e ...]
        (let ([lst  (syntax->list #'(e ...))])
          (for/fold ([acc acc]) ([lst  (in-list lst)])
            (loop lst acc)))]
       [else
        (cons e-stx acc)]))))

(define-syntax (array/syntax stx)
  (syntax-case stx ()
    [(_ orig-name constr ->array e)
     (let ([ds  (syntax-vector-shape #'e)])
       (unless ds
         (raise-syntax-error (syntax->datum #'orig-name) "expected rectangular data" stx #'e))
       (with-syntax ([(d ...)  ds]
                     [(v ...)  (syntax-vector-flatten #'e)])
         (syntax/loc stx
           (->array ((inst vector Index) d ...) (constr v ...)))))]))
