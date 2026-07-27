#lang racket/base
(require ffi/unsafe/runtime-lib)

;; The intent of this module is just to load some Windows DLLs and
;; pull them along in standard executables, and not to export the
;; libraries or bindings

(define-runtime-lib vcruntime
  #:ffi-lib-args (#:fail (lambda () #f))
  [(and windows 64)
   (so "vcruntime140.dll")
   (so "vcruntime140_1.dll")]
  [else #f])
