
(module htdp-beginner-abbr scheme/base
  (require mzlib/etc
	   mzlib/list
	   mzlib/math
	   syntax/docprovide
           test-engine/scheme-tests)

  ;; Implements the forms:
  (require "private/teach.ss"
	   "private/teachprims.ss"
	   "private/contracts/contracts-module-begin.ss")

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
            [beginner-require require]
            [beginner-dots ..]
            [beginner-dots ...]
            [beginner-dots ....]
            [beginner-dots .....]
            [beginner-dots ......]
            [intermediate-quote quote]
            [intermediate-quasiquote quasiquote]
            [intermediate-unquote unquote]
            [intermediate-unquote-splicing unquote-splicing]
            [beginner-module-begin #%module-begin])
           check-expect
           check-within
           check-error
           check-member-of
           check-range
	   #%datum
           #%top-interaction
	   empty true false

	   contract : -> mixed one-of predicate combined
	   Number Real Rational Integer Natural Boolean True False String Symbol Char Empty-list
	   property)

  ;; procedures:
  (provide-and-document
   procedures
   (all-from beginner: lang/htdp-beginner procedures))

  )
