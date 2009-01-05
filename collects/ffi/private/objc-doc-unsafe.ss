#lang scheme/base

(require ffi/objc)

(error 'objc-unsafe! "only `for-label' use in the documentation")

(objc-unsafe!)

(provide (protect-out (all-defined-out))
         (all-from-out ffi/objc))
