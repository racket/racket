#lang racket/base

(require (for-syntax racket/base))

(provide array/syntax)

(define-for-syntax (square-bracket? e-stx)
  (eq? #\[ (syntax-property e-stx 'paren-shape)))

(define-for-syntax (syntax-list-shape e-stx)
  (define lst (syntax->list e-stx))
  (cond [(or (not lst) (not (square-bracket? e-stx)))  null]
        [(null? lst)  (list 0)]
        [else
         (define d (length lst))
         (define ds (syntax-list-shape (car lst)))
         (if ds
             (let loop ([lst  (cdr lst)])
               (cond [(null? lst)  (cons d ds)]
                     [(equal? ds (syntax-list-shape (car lst)))
                      (loop (cdr lst))]
                     [else  #f]))
             #f)]))

(define-for-syntax (syntax-list-flatten e-stx)
  (reverse
   (let loop ([e-stx e-stx] [acc null])
     (define lst (syntax->list e-stx))
     (cond [(and lst (square-bracket? e-stx))
            (for/fold ([acc acc]) ([lst  (in-list lst)])
              (loop lst acc))]
           [else
            (cons e-stx acc)]))))

(define-syntax (array/syntax stx)
  (syntax-case stx ()
    [(_ orig-name constr ->array e)
     (let ([ds  (syntax-list-shape #'e)])
       (unless ds
         (raise-syntax-error (syntax->datum #'orig-name) "expected rectangular data" stx #'e))
       (with-syntax ([(d ...)  ds]
                     [(v ...)  (syntax-list-flatten #'e)])
         (syntax/loc stx
           (->array (vector d ...) (constr v ...)))))]))
