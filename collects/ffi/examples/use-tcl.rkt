#! /usr/bin/env racket

#lang racket/base

(require "tcl.rkt")

(define (tcldemo . strs)
  (for-each (lambda (s)
              (printf "> ~a\n" s)
              (with-handlers ([void (lambda (e)
                                      (display (if (exn? e) (exn-message e) e)
                                               (current-error-port))
                                      (newline (current-error-port)))])
                (printf "~a\n" (eval-tcl s))))
            strs))

(tcldemo "puts 123"
         "puts $a"
         "set a {this is some stupid string}"
         "set b [list a a]"
         "set c {[list $a $a]}"
         "puts \"a = \\\"$a\\\"\""
         "puts \"b = \\\"$b\\\"\""
         "puts \"c = \\\"$c\\\"\""
         "puts \"even better... \\\"[expr $c]\\\"\"")
