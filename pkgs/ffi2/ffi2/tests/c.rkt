#lang racket/base
(require ffi2
         rackunit)

(define c-lib (ffi2-lib #f))

(define-ffi2-type ours_t* void_t*)

;; most test use `define-ffi2-procedure`, so try plain `ffi2-procedure` here
(define c-malloc (ffi2-procedure (ffi2-lib-ref c-lib "malloc")
                                 (size_t . -> . void_t*)))
(define c-free (ffi2-procedure (ffi2-lib-ref c-lib "free")
                               (void_t* . -> . void_t
                                        #:abi cdecl_abi)))

(define p (c-malloc 100))
(check-true (void_t*? p))
(check-false (void_t*/gcable? p))
(check-false (ours_t*? p))
(c-free p)
