;; Implements the Beginner Scheme language, at least in terms of the
;; forms and procedures. The reader-level aspects of the language
;; (e.g., case-sensitivity) are not implemented here.

#lang scheme/base
(require mzlib/etc
         mzlib/list
         syntax/docprovide
         "private/rewrite-error-message.rkt"
         (for-syntax "private/rewrite-error-message.rkt")
         (for-syntax scheme/base))

(require "private/provide-and-scribble.rkt")


;; Implements the forms:
(require "private/teach.rkt"
         "private/teach-module-begin.rkt"
         test-engine/racket-tests)

;; syntax:
(provide (rename-out
          [beginner-define define]
          [beginner-define-struct define-struct]
          [beginner-lambda lambda]
          [beginner-app #%app]
          [beginner-top #%top]
          [beginner-cond cond]
          [beginner-else else]
          [beginner-if if]
          [beginner-and and]
          [beginner-or or]
          [beginner-quote quote]
          [beginner-module-begin #%module-begin]
          [beginner-require require]
          [beginner-dots ..]
          [beginner-dots ...]
          [beginner-dots ....]
          [beginner-dots .....]
          [beginner-dots ......]
          [beginner-true true]
          [beginner-false false]
          )
         check-expect
         check-within
         check-error
         check-member-of
         check-range
         ;; define-wish
         #%datum
         #%top-interaction
         empty
         
         ;  signature : -> mixed one-of predicate combined
         ;  Number Real Rational Integer Natural Boolean True False String Symbol Char Empty-list Any
         ;  cons-of
         ;  Property
         ;  check-property for-all ==> expect expect-within expect-member-of expect-range
         )
 
;; procedures:
(provide-and-scribble
 procedures
 (begin)
 (all-from beginner: (submod lang/private/beginner-funs with-wrapper) procedures))
