#lang racket/base
(require (for-syntax racket/base)
         ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path
         "libcrypto.rkt")

(define-runtime-path legacy-so
  '(so "ossl-modules/legacy"))

(define-ffi-definer define-crypto libcrypto)

(define-crypto OSSL_PROVIDER_load (_fun _pointer _string -> _pointer)
  #:fail (lambda () void))

(void
 (begin
   (OSSL_PROVIDER_load #f (if (absolute-path? legacy-so)
                              legacy-so
                              "legacy"))
   (OSSL_PROVIDER_load #f "default")))
