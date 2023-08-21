#lang racket/base
(require "private/struct.rkt"
         racket/contract/base)

(provide
 (except-out (all-from-out "private/struct.rkt")
             build-struct-names))

(provide/contract
 [build-struct-names
  (->* (identifier? (listof identifier?) boolean? boolean?)
       ((or/c #f syntax?) 
        #:constructor-name (or/c #f identifier?))
       (listof identifier?))])
