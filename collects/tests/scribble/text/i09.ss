#!/bin/env mzscheme
#lang scribble/text

@(begin
   ;; This is a somewhat contrived example, showing how to use lists
   ;; and verbatim to control the added prefix
   (define (item . text)
     ;; notes: the `flush' makes the prefix to that point print so the
     ;; verbatim "* " is printed after it, which overwrites the "| "
     ;; prefix
     (cons flush (prefix "| " (cons (verbatim "* ") text))))
   ;; note that a simple item with spaces is much easier:
   (define (simple . text) @list{* @text}))

start
  @item{blah blah blah
        blah blah blah
        @item{more stuff
              more stuff
              more stuff}
        blah blah blah
        blah blah blah}
  @simple{more blah
          blah blah}
end
