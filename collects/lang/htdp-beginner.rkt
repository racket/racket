;; Implements the Beginner Scheme language, at least in terms of the
;; forms and procedures. The reader-level aspects of the language
;; (e.g., case-sensitivity) are not implemented here.

(module htdp-beginner scheme/base
  (require mzlib/etc
           mzlib/list
           syntax/docprovide
           "private/rewrite-error-message.rkt"
           (for-syntax "private/rewrite-error-message.rkt")
           (for-syntax scheme/base))

  ;; Implements the forms:
  (require "private/teach.rkt"
           "private/teach-module-begin.rkt"
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

; 	   signature : -> mixed one-of predicate combined
; 	   Number Real Rational Integer Natural Boolean True False String Symbol Char Empty-list Any
; 	   cons-of
; 	   Property
; 	   check-property for-all ==> expect expect-within expect-member-of expect-range
	   )

  (require (for-syntax "private/firstorder.rkt"))


  (define-syntax (in-rator-position-only stx)
    (syntax-case stx ()
      [(_ new-name orig-name)
       (let ([new (syntax new-name)]
             [orig (syntax orig-name)])
         ;; Some things are not really functions:
         (if (memq (syntax-e orig) '(beginner:pi beginner:e beginner:null beginner:eof))
             #'(define new-name orig-name)
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
                         "expected a function call, but there is no open parenthesis before this function")
                        stx)]))
                  #'orig-name))))]))
  
  ;; procedures:
  (provide-and-document/wrap
   procedures
   in-rator-position-only
   (all-from beginner: lang/private/beginner-funs procedures))
  
  )
