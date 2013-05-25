#lang scheme/base
(require "private/random-mutator.rkt"
         scheme/contract
         "private/gc-core.rkt")

(provide/contract
 [save-random-mutator 
  (->* (path-string?
        string?)
       (#:iterations 
        exact-positive-integer?
        #:heap-values (cons/c heap-value? (listof heap-value?))
        #:program-size exact-positive-integer?
        #:heap-size exact-positive-integer?
        #:gc2? boolean?)
       void?)]
 [find-heap-values
  (-> (or/c path-string? input-port?)
      (listof heap-value?))])
