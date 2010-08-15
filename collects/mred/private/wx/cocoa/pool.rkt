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

;; This pool manages all objects that would otherwise not
;; have a pool, which makes them stick around until the
;; process exits.
(define pool (tell (tell NSAutoreleasePool alloc) init))
