#! /usr/bin/env racket

#lang racket/base

(require "tcl.rkt")

(define (tcldemo . strs)
  (for ([s (in-list strs)])
    (printf "> ~a\n" s)
    (with-handlers ([void (lambda (e)
                            (eprintf "~a\n" (if (exn? e) (exn-message e) e)))])
      (printf "~a\n" (eval-tcl s)))))

(tcldemo "puts 123"
         "puts $a"
         "set a {this is some stupid string}"
         "set b [list a a]"
         "set c {[list $a $a]}"
         "puts \"a = \\\"$a\\\"\""
         "puts \"b = \\\"$b\\\"\""
         "puts \"c = \\\"$c\\\"\""
         "puts \"even better... \\\"[expr $c]\\\"\"")
