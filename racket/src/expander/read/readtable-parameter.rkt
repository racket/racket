#lang racket/base
(require "../common/contract.rkt")

(provide current-readtable
         prop:readtable prop:readtable?)
         
(define-values (prop:readtable prop:readtable? prop:readtable-ref)
  (make-struct-type-property 'readtable))

(define/who current-readtable (make-parameter #f
                                              (lambda (v)
                                                (check who prop:readtable? #:or-false v)
                                                v)))
