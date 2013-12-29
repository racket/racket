#lang racket/base
(require "test-suite-utils.rkt")

(module test racket/base)

(define (test-creation class name)
  (test
   name
   (lambda (x) (eq? 'passed x))
   (lambda ()
     (queue-sexp-to-mred
      `(let* ([f (make-object frame:basic% "test canvas" #f 300 300)]
	      [c (make-object ,class (send f get-area-container))])
	 (send c set-editor (make-object text:wide-snip%))
	 (send f show #t)))
      (wait-for-frame "test canvas")
      (queue-sexp-to-mred
       `(send (get-top-level-focus-window) show #f))
      'passed)))

(test-creation '(canvas:basic-mixin editor-canvas%)
	       'canvas:basic-mixin-creation)
(test-creation 'canvas:basic%
	       'canvas:basic%-creation)

(test-creation '(canvas:wide-snip-mixin canvas:basic%)
	       'canvas:wide-snip-mixin-creation)
(test-creation 'canvas:wide-snip%
	       'canvas:wide-snip%-creation)
