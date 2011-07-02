#lang typed-scheme

#;(require mzlib/etc)
;(require "prims.rkt")
(require mzlib/match)

(define-typed-struct pt ([x : Number] [y : Number]))

(require (for-syntax scheme/base))


(define-match-expander blah #:match (lambda (stx) (syntax-case stx ()
						    [(_ . a) #'($ . a)])))

(define: (pt-add/match/blah [v : Any]) : Number
  (match v
	 [(blah pt #{x : Number} #{y : Number}) (+ x y)]
	 [_ 0]))


