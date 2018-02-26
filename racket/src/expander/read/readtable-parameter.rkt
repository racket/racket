#lang racket/base

(provide current-readtable
         prop:readtable prop:readtable?)
         
(define-values (prop:readtable prop:readtable? prop:readtable-ref)
  (make-struct-type-property 'readtable))

(define current-readtable (make-parameter #f
                                          (lambda (v)
                                            (unless (or (not v)
                                                        (prop:readtable? v))
                                              (raise-argument-error 'current-readtable
                                                                    "(or/c readtable? #f)"
                                                                    v))
                                            v)))
