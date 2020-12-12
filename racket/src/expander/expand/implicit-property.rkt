#lang racket/base
(require "../syntax/original.rkt")

(provide implicit-made-explicit-properties
         original-implicit-made-explicit-properties)

(define implicit-made-explicit-properties
  (hasheq 'implicit-made-explicit #t))

(define original-implicit-made-explicit-properties
  (hash-set implicit-made-explicit-properties
            original-property-sym #t))
