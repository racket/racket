#lang typed/racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/list)
         "array-struct.rkt")

(provide array strict-array)

(define-for-syntax (square-bracket? e-stx)
  (eq? #\[ (syntax-property e-stx 'paren-shape)))

(define-for-syntax (syntax-list-shape e-stx)
  (define lst (syntax->list e-stx))
  (cond [(or (not lst) (not (square-bracket? e-stx)))  empty]
        [(empty? lst)  (list 0)]
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
  (define lst (syntax->list e-stx))
  (cond [(and lst (square-bracket? e-stx))
         (append* (map syntax-list-flatten lst))]
        [else
         (list e-stx)]))

(define-syntax (make-array/stx stx)
  (syntax-case stx ()
    [(_ orig-name constr ->array e)
     (let ([ds  (syntax-list-shape #'e)])
       (unless ds
         (raise-syntax-error (syntax->datum #'orig-name) "expected rectangular data" stx #'e))
       (with-syntax ([(d ...)  ds]
                     [(v ...)  (syntax-list-flatten #'e)])
         (syntax/loc stx
           (->array (vector d ...) (constr v ...)))))]))

(: flat-list->view-array (All (A) ((Vectorof Integer) (Listof A) -> (View-Array A))))
(define (flat-list->view-array ds lst)
  (array-view (make-strict-array ds (list->vector lst))))

(define-syntax (strict-array stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx (make-array/stx strict-array vector make-strict-array e))]
    [(_ e:expr T:expr)
     (syntax/loc stx (make-array/stx strict-array (inst vector T) make-strict-array e))]
    [_:id  (raise-syntax-error 'strict-array "not allowed as an expression" stx)]))

(define-syntax (array stx)
  (syntax-parse stx
    [(_ e:expr)  (syntax/loc stx (make-array/stx array list flat-list->view-array e))]
    [_:id  (raise-syntax-error 'array "not allowed as an expression" stx)]))
