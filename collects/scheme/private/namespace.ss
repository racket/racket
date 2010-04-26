#lang racket/base

(provide make-base-empty-namespace
         make-base-namespace)
  
(define orig-varref (#%variable-reference orig-varref))

(define (make-base-empty-namespace)
  (let* ([this-ns (variable-reference->empty-namespace orig-varref)]
         [ns (parameterize ([current-namespace this-ns]) ; ensures correct phase
               (make-empty-namespace))])
    (namespace-attach-module this-ns
                             'scheme/base 
                             ns)
    ns))

(define (make-base-namespace)
  (let ([ns (make-base-empty-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'scheme/base))
    ns))
