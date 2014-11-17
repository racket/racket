#lang racket/base
(require data/enumerate
         racket/contract/base)
(provide enum
         enum?
         size
         (contract-out
          (encode (-> enum? any/c exact-nonnegative-integer?))
          (decode (-> enum? exact-nonnegative-integer? any/c)))
         empty/e
         const/e
         from-list/e
         fin/e
         disj-sum/e
         disj-append/e
         cons/e
         elegant-cons/e
         dep/e
         dep2/e ;; requires size (eventually should replace dep/e with this)
         map/e
         filter/e ;; very bad, only use for small enums
         except/e 
         thunk/e
         fix/e
         many/e
         many1/e
         list/e
         vec/e

         cantor-vec/e
         cantor-list/e

         box-vec/e
         box-list/e
         
         traverse/e
         hash-traverse/e
         
         fail/e
         
         approximate
         to-list
         to-stream
         take/e
         fold-enum

         nat/e
         range/e
         slice/e
         nat+/e

         ;; Base type enumerators
         any/e
         var/e
         var-prefix/e
         num/e
         integer/e
         bool/e
         real/e
         string/e)
