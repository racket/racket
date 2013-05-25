#lang racket/base
(require "main.rkt"
         racket/contract
         texpict/private/convertible)

(provide pict-convert pict-convertible?)
(provide/contract
 [prop:pict-convertible (struct-type-property/c (-> pict-convertible? pict?))]
 [prop:pict-convertible? (struct-type-property/c predicate/c)])
