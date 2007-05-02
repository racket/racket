
;; Implements the Beginner Scheme language, at least in terms of the
;; forms and procedures. The reader-level aspects of the language
;; (e.g., case-sensitivity) are not implemented here.

(module htdp-beginner mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
	   (lib "docprovide.ss" "syntax"))

  ;; Implements the forms:
  (require "private/teach.ss"
	   "private/contract-forms.ss")

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
	   (rename beginner-quote quote)
	   (rename beginner-module-begin #%module-begin)
           (rename beginner-require require)
	   ; (rename beginner-contract contract)
	   ; (rename beginner-define-data define-data)
	   #%datum
           #%top-interaction
	   empty true false)

  (require-for-syntax "private/firstorder.ss")
    
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
   (all-from beginner: (lib "beginner-funs.ss" "lang" "private") procedures)))
