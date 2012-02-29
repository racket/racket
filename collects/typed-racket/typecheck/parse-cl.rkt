#lang racket/base
(require syntax/parse racket/dict syntax/id-table (for-template '#%kernel))
(provide rebuild-let*)
(define-syntax-class cl-rhs
  #:literals (if)
  #:attributes (i j)
  [pattern i:id
           #:attr j #f]
  [pattern (if j:id i:id e:expr)])

(define-syntax-class rebuild-let*
  #:literals (let-values)
  #:attributes (mapping flag-mapping)
  (pattern (let-values ([(x) e:cl-rhs]) body:rebuild-let*)
           #:attr mapping (dict-set (attribute body.mapping) #'e.i #'x)
           #:attr flag-mapping (if (attribute e.j)
                                   (dict-set (attribute body.flag-mapping) #'e.i #'e.j)
                                   (attribute body.flag-mapping)))
  (pattern (let-values ([(x) e:cl-rhs]) body:expr)
           #:attr mapping (dict-set (make-immutable-free-id-table) #'e.i #'x)
           #:attr flag-mapping (if (attribute e.j)
                                   (dict-set (make-immutable-free-id-table) #'e.i #'e.j)
                                   (make-immutable-free-id-table))))
