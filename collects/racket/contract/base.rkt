#lang racket/base

;; A stripped down version of scheme/contract for use in
;; the PLT code base where appropriate.

(require "private/arrow.rkt"
         "private/arr-i.rkt"
         "private/base.rkt"
         "private/box.rkt"
         "private/hash.rkt"
         "private/vector.rkt"
         "private/misc.rkt"
         "private/provide.rkt"
         "private/guts.rkt"
         "private/legacy.rkt"
         "private/ds.rkt"
         "private/opt.rkt")

(provide
 opt/c define-opt/c ;(all-from-out "private/opt.rkt")
 (except-out (all-from-out "private/ds.rkt")
             lazy-depth-to-look)

 (except-out (all-from-out "private/arrow.rkt")
             making-a-method
             procedure-accepts-and-more?
             check-procedure
             check-procedure/more
             make-contracted-function)
 (all-from-out "private/arr-i.rkt")
 (all-from-out "private/box.rkt")
 (all-from-out "private/hash.rkt")
 (all-from-out "private/vector.rkt")
 (except-out (all-from-out "private/misc.rkt")
             check-between/c
             check-unary-between/c)
 (all-from-out "private/provide.rkt")
 (all-from-out "private/base.rkt")
 (all-from-out "private/legacy.rkt")
 (except-out (all-from-out "private/guts.rkt")
             check-flat-contract
             check-flat-named-contract))
