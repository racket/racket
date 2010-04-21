#lang racket/base

;; A stripped down version of scheme/contract for use in
;; the PLT code base where appropriate.

(require "private/arrow.ss"
         "private/base.ss"
         "private/misc.ss"
         "private/provide.ss"
         "private/guts.ss"
         "private/legacy.ss"
         "private/ds.ss"
         "private/opt.ss")

(provide 
 opt/c define-opt/c ;(all-from-out "private/opt.ss")
 (except-out (all-from-out "private/ds.ss")
             lazy-depth-to-look)
 
 (except-out (all-from-out "private/arrow.ss")
             making-a-method
             procedure-accepts-and-more?
             check-procedure
             check-procedure/more)
 (except-out (all-from-out "private/misc.ss")
             check-between/c
             check-unary-between/c)
 (all-from-out "private/provide.ss")
 (all-from-out "private/base.ss")
 (all-from-out "private/legacy.ss")
 (except-out (all-from-out "private/guts.ss")
             check-flat-contract
             check-flat-named-contract))

