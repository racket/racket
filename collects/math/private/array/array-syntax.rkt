#lang typed/racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/list)
         racket/promise
         racket/performance-hint
         "array-struct.rkt"
         "utils.rkt")

(provide
 ;; Construction
 array
 strict-array
 ;; Conditionals
 array-if
 array-and
 array-or)

;; ===================================================================================================
;; Construction

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
  (reverse
   (let loop ([e-stx e-stx] [acc empty])
     (define lst (syntax->list e-stx))
     (cond [(and lst (square-bracket? e-stx))
            (for/fold ([acc acc]) ([lst  (in-list lst)])
              (loop lst acc))]
           [else
            (cons e-stx acc)]))))

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

;; ===================================================================================================
;; Conditionals

(begin-encourage-inline
  
  (: make-array-if (All (A B C) ((Array A) (-> (Array B)) (-> (Array B)) -> (View-Array (U B C)))))
  (define (make-array-if arr1 arr2-thnk arr3-thnk)
    (let ([arr1  (array-view arr1)])
      (define ds (array-shape arr1))
      (define g1 (unsafe-array-proc arr1))
      (define g2 (delay (define arr2 (array-view (arr2-thnk)))
                        (check-equal-array-shape! 'array-if ds (array-shape arr2))
                        (unsafe-array-proc arr2)))
      (define g3 (delay (define arr3 (array-view (arr3-thnk)))
                        (check-equal-array-shape! 'array-if ds (array-shape arr3))
                        (unsafe-array-proc arr3)))
      (unsafe-view-array ds (λ: ([js : Indexes]) (if (g1 js) ((force g2) js) ((force g3) js))))))
  
  (: make-array-and (All (A B) ((Array A) (-> (Array B)) -> (View-Array (U B #f)))))
  (define (make-array-and arr1 arr2-thnk)
    (let ([arr1  (array-view arr1)])
      (define ds (array-shape arr1))
      (define g1 (unsafe-array-proc arr1))
      (define g2 (delay (define arr2 (array-view (arr2-thnk)))
                        (check-equal-array-shape! 'array-or ds (array-shape arr2))
                        (unsafe-array-proc arr2)))
      (unsafe-view-array ds (λ: ([js : Indexes]) (if (g1 js) ((force g2) js) #f)))))
  
  (: make-array-or (All (A B) ((Array A) (-> (Array B)) -> (View-Array (U A B)))))
  (define (make-array-or arr1 arr2-thnk)
    (let ([arr1  (array-view arr1)])
      (define ds (array-shape arr1))
      (define g1 (unsafe-array-proc arr1))
      (define g2 (delay (define arr2 (array-view (arr2-thnk)))
                        (check-equal-array-shape! 'array-or ds (array-shape arr2))
                        (unsafe-array-proc arr2)))
      (unsafe-view-array ds (λ: ([js : Indexes]) (let ([or-part  (g1 js)])
                                                   (if or-part or-part ((force g2) js)))))))
  
  ) ; begin-encourage-inline

(define-syntax-rule (array-if arr1 arr2 arr3)
  (make-array-if arr1 (λ () arr2) (λ () arr3)))

(define-syntax (array-and stx)
  (syntax-case stx ()
    [(_ arr1)  (syntax/loc stx arr1)]
    [(_ arr1 arr2)  (syntax/loc stx (make-array-and arr1 (λ () arr2)))]
    [(_ arr1 arr2 arr3 . arrs)  (syntax/loc stx (array-and arr1 (array-and arr2 arr3 . arrs)))]))

(define-syntax (array-or stx)
  (syntax-case stx ()
    [(_ arr1)  (syntax/loc stx arr1)]
    [(_ arr1 arr2)  (syntax/loc stx (make-array-or arr1 (λ () arr2)))]
    [(_ arr1 arr2 arr3 . arrs)  (syntax/loc stx (array-or arr1 (array-or arr2 arr3 . arrs)))]))
