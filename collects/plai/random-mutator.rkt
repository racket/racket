#lang scheme/base
(require "private/random-mutator.ss"
         scheme/contract
         "private/gc-core.ss")

(provide/contract
 [save-random-mutator 
  (->* (path-string?
        string?)
       (#:iterations 
        exact-positive-integer?
        #:heap-values (cons/c heap-value? (listof heap-value?))
        #:program-size exact-positive-integer?
        #:heap-size exact-positive-integer?)
       void?)]
 [find-heap-values
  (-> (or/c path-string? input-port?)
      (listof heap-value?))])
