#lang scheme/base
(require ffi/objc
         scheme/foreign
         "utils.rkt"
         "const.rkt"
         "types.rkt")
(unsafe!)
(objc-unsafe!)

(provide pool)

(import-class NSAutoreleasePool)

(define pool (tell (tell NSAutoreleasePool alloc) init))
