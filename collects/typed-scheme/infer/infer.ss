#lang scheme/base

(require (except-in "../utils/utils.ss" infer))
(require "infer-unit.ss" "constraints.ss" "dmap.ss" "signatures.ss"
         "restrict.ss" "promote-demote.ss"
         scheme/contract
         (rep type-rep)
         (utils unit-utils))

(provide/contract
 [infer (((listof symbol?) (listof Type?) (listof Type?) (or/c (one-of/c #f) Type?) (listof symbol?)) 
         ((or/c (one-of/c #f) Type?))
         . ->* . 
         (listof (list/c symbol? Type?)))]
 [infer/vararg (((listof symbol?) (listof Type?) (listof Type?) Type? (or/c (one-of/c #f) Type?) (listof symbol?)) 
                ((or/c (one-of/c #f) Type?))
                . ->* . 
                (listof (list/c symbol? Type?)))]
 [infer/dots (((listof symbol?) symbol? (listof Type?) (listof Type?) Type? (or/c (one-of/c #f) Type?) (listof symbol?)) 
              (#:expected (or/c (one-of/c #f) Type?))
              . ->* . 
              (listof (list/c symbol? Type?)))])
         
(provide restrict)

(define-values/link-units/infer
  infer@ constraints@ dmap@ restrict@ promote-demote@)
