#lang racket
(require racket/vector racket/gui/dynamic)

(require "main.ss")
(current-namespace (make-base-namespace))
(define exec (make-parameter go/text))
(define unit-only? (make-parameter #f))
(command-line
 #:once-each
 ["--unit" "run just the unit tests" (unit-only? #t)]
 ["--gui" "run using the gui" 
  (current-namespace ((gui-dynamic-require 'make-gui-namespace)))
  (exec go)])
(unless (= 0 ((exec) (unit-only?)))
  (error "Typed Scheme Tests did not pass."))
