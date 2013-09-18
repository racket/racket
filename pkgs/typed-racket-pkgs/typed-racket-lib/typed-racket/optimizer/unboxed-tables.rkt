#lang racket/base

(require syntax/id-table)

(provide
  unboxed-vars-table 
  unboxed-funs-table)

;; contains the bindings which actually exist as separate bindings for each component
;; associates identifiers to lists (real-binding imag-binding orig-binding-occurrence)
(define unboxed-vars-table (make-free-id-table))

;; associates the names of functions with unboxed args (and whose call sites have to
;; be modified) to the arguments which can be unboxed and those which have to be boxed
;; entries in the table are of the form:
;; ((unboxed ...) (boxed ...))
;; all these values are indices, since arg names don't make sense for call sites
;; the new calling convention for these functions have all real parts of unboxed
;; params first, then all imaginary parts, then all boxed arguments
(define unboxed-funs-table (make-free-id-table))
