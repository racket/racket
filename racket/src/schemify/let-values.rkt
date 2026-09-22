#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "simple.rkt")

(provide convert-simple-let-values-bindings)

(define (convert-simple-let-values-bindings idss rhss prim-knowns knowns imports mutated simples unsafe-mode?)
  (and (for/and ([ids (in-list idss)]
                 [rhs (in-list rhss)])
         (match rhs
           [`(values ,args ...) (and (= (length ids) (length args))
                                     (for/and ([arg (in-list args)])
                                       (simple? #:pure? #f arg prim-knowns knowns imports mutated simples unsafe-mode?)))]
           [`,_ #f]))
       (for/list ([ids (in-list idss)]
                  [rhs (in-list rhss)]
                  #:do [(define args
                          (match rhs
                            [`(values ,args ...) args]))]
                  [id (in-list ids)]
                  [arg (in-list args)])
         `[(,id) ,arg])))
