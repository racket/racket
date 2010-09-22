
;; Implements the Beginner Scheme language, at least in terms of the
;; forms and procedures. The reader-level aspects of the language
;; (e.g., case-sensitivity) are not implemented here.

(module htdp-beginner scheme/base
  (require mzlib/etc
	   mzlib/list
	   syntax/docprovide
           (for-syntax scheme/base))

  ;; Implements the forms:
  (require "private/teach.ss"
	   "private/teach-module-begin.ss"
           test-engine/scheme-tests)

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
            )
           check-expect
           check-within
           check-error
           check-member-of
           check-range
	   #%datum
           #%top-interaction
	   empty true false

	   signature : -> mixed one-of predicate combined
	   Number Real Rational Integer Natural Boolean True False String Symbol Char Empty-list Any
	   cons-of
	   Property
	   check-property for-all ==> expect expect-within expect-member-of expect-range)

  (require (for-syntax "private/firstorder.ss"))
    
  ;; This is essentially a specialized version of `define-primitive'
  ;;  that refines the error messages for built-in things, which
  ;;  we might like to call "contructor" or "predicate" instead of
  ;;  just "primitive".
  (define-syntax (in-rator-position-only stx)
    (syntax-case stx ()
      [(_ new-name orig-name)
       (let ([new (syntax new-name)]
             [orig (syntax orig-name)])
         ;; Some things are not really functions:
         (if (memq (syntax-e orig) '(beginner:pi beginner:e beginner:null beginner:eof))
             #'(define new-name orig-name)
	     (with-syntax ([(what something)
			    (case (syntax-e orig)
			      [(beginner:make-posn)
			       #'("constructor"
				  "called with values for the structure fields")]
			      [(beginner:posn-x beginner:posn-y)
			       #'("selector"
				  "applied to a structure to get the field value")]
			      [(beginner:posn?)
			       #'("predicate"
				  "applied to an argument")]
			      [else
			       #'("primitive operator"
				  "applied to arguments")])])
	       #'(define-syntax new-name 
                   (make-first-order
                    (lambda (stx)
                      (syntax-case stx ()
                        [(id . args)
                         (syntax/loc stx (beginner-app orig-name . args))]
                        [_else
                         (raise-syntax-error
                          #f
                          (format
                           "this ~a must be ~a; expected an open parenthesis before the ~a name"
                           what
                           something
                           what)
                          stx)]))
                    #'orig-name)))))]))
      
  ;; procedures:
  (provide-and-document/wrap
   procedures
   in-rator-position-only
   (all-from beginner: lang/private/beginner-funs procedures))
  
  )
