#lang racket/base

(require syntax/id-table syntax/parse racket/dict 
         "../utils/utils.rkt"
         (utils tc-utils))

(provide
  add-unboxed-fun!
  unboxed-fun
  add-unboxed-var!
  unboxed-var)

;; contains the bindings which actually exist as separate bindings for each component
;; associates identifiers to lists (real-binding imag-binding orig-binding-occurrence)
(define unboxed-vars-table (make-free-id-table))

(define (add-unboxed-var! orig-binding real-binding imag-binding)
  (dict-set! unboxed-vars-table orig-binding
             (list real-binding imag-binding orig-binding)))

(define-syntax-class unboxed-var
  #:attributes (real-binding imag-binding)
  (pattern v:id
    #:with unboxed-info (dict-ref unboxed-vars-table #'v #f)
    #:when (syntax->datum #'unboxed-info)
    #:with (real-binding imag-binding orig-binding) #'unboxed-info
    ;; we need to introduce both the binding and the use at the same time
    #:do [(add-disappeared-use (syntax-local-introduce #'v))
          (add-disappeared-binding (syntax-local-introduce #'orig-binding))]))

;; associates the names of functions with unboxed args (and whose call sites have to
;; be modified) to the arguments which can be unboxed and those which have to be boxed
;; entries in the table are of the form:
;; ((unboxed ...) (boxed ...))
;; all these values are indices, since arg names don't make sense for call sites
;; the new calling convention for these functions have all real parts of unboxed
;; params first, then all imaginary parts, then all boxed arguments
(define unboxed-funs-table (make-free-id-table))

(define (add-unboxed-fun! fun-name unboxed-args)
  (dict-set! unboxed-funs-table fun-name unboxed-args))

(define-syntax-class unboxed-fun
  #:attributes ((unboxed 1) unboxed-info)
  (pattern op:id
    #:do [(define unboxed-args (dict-ref unboxed-funs-table #'op #f))]
    #:when unboxed-args
    #:with ((unboxed ...) (boxed ...))
           (list
             (for/list ([i (in-naturals)] [unboxed? unboxed-args] #:when unboxed?) i)
             (for/list ([i (in-naturals)] [unboxed? unboxed-args] #:unless unboxed?) i))
    #:with (~and unboxed-info (unboxed-args ...)) unboxed-args))
