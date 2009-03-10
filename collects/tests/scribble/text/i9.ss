#!/bin/env mzscheme
#lang scribble/text

@(begin
   ;; This is a somewhat contrived example, showing how to use lists
   ;; and verbatim to control the added prefix
   (define (item . text)
     ;; notes: the "" makes the prefix to that point print so the
     ;; prefix is added after it, and the "* " is wrapped in verbatim
     ;; so that line doesn't get the "| " prefix
     (cons "" (prefix "| " (cons (verbatim "* ") text))))
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
