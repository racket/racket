#lang scheme/base
(require syntax/stx
         (for-template scheme/base
                       syntax/stx
                       scheme/stxparam
                       unstable/struct
                       "runtime.ss"))
(provide (all-defined-out))

(define (get-kind kind)
  (syntax-case kind ()
    [#:pair pairK]
    [#:vector vectorK]
    [#:box boxK]
    [(#:pstruct key)
     (make-kind #`(lambda (x)
                    (let ([xkey (prefab-struct-key x)])
                      (and xkey (equal? xkey (quote key)))))
                (list (lambda (s d)
                        #`(datum->syntax #,s (struct->list #,d) #,s)))
                (list #'dfc-add-unpstruct))]))

;; A Kind is
;;   (make-kind id (listof (id id -> stx)) (listof expr))

(define-struct kind (predicate selectors frontier-procs) #:transparent)

(define pairK
  (make-kind #'pair?
             (list (lambda (s d) #`(car #,d))
                   (lambda (s d) #`(datum->syntax #,s (cdr #,d) #,s)))
             (list #'dfc-add-car
                   #'dfc-add-cdr)))

(define vectorK
  (make-kind #'vector?
             (list (lambda (s d)
                     #`(datum->syntax #,s (vector->list #,d) #,s)))
             (list #'dfc-add-unvector)))

(define boxK
  (make-kind #'box?
             (list (lambda (s d) #`(unbox #,d)))
             (list #'dfc-add-unbox)))
