#lang scheme/base

(require scheme/foreign)

(error 'unsafe! "only `for-label' use in the documentation")

(unsafe!)

(provide (protect-out (all-defined-out))
         (all-from-out scheme/foreign))

