
(module htdp-intermediate-lambda scheme/base
  (require "private/teach.ss"
	   "private/contracts/contracts-module-begin.ss"
	   mzlib/etc
	   mzlib/list
	   syntax/docprovide
           test-engine/scheme-tests)
  
  ;; syntax:
  (provide (rename-out
            [intermediate-lambda-define define]
            [intermediate-define-struct define-struct]
            [intermediate-lambda lambda]
            [intermediate-lambda λ]
            [advanced-app #%app]
            [beginner-top #%top]
            [intermediate-local local]
            [intermediate-let let]
            [intermediate-let* let*]
            [intermediate-letrec letrec]
            [intermediate-recur recur]
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
            [intermediate-time time]
            [intermediate-module-begin #%module-begin]
            )
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
   (all-from intermediate: lang/htdp-intermediate procedures))
  )
