
(module htdp-beginner-abbr mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
	   (lib "math.ss")
	   (lib "docprovide.ss" "syntax"))

  ;; Implements the forms:
  (require "private/teach.ss"
	   "private/contract-forms.ss"
	   "private/teachprims.ss")

  ;; syntax:
  (provide (rename beginner-define define)
	   (rename beginner-define-struct define-struct)
	   (rename beginner-lambda lambda)
	   (rename beginner-app #%app)
	   (rename beginner-top #%top)
	   (rename beginner-cond cond)
	   (rename beginner-else else)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
           (rename beginner-require require)
	   ;; (rename beginner-contract contract)
	   ;; (rename beginner-define-data define-data)
	   (rename intermediate-quote quote)
	   (rename intermediate-quasiquote quasiquote)
	   (rename intermediate-unquote unquote)
	   (rename intermediate-unquote-splicing unquote-splicing)
	   (rename beginner-module-begin #%module-begin)
	   #%datum
           #%top-interaction
	   empty true false)

  ;; procedures:
  (provide-and-document
   procedures
   (all-from beginner: (lib "htdp-beginner.ss" "lang") procedures)))
