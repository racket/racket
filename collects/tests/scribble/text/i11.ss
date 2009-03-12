#!/bin/env mzscheme
#lang scribble/text

@(define (block x)
   @splice{{
     blah(@x);
   }})

start
  @splice{foo();
          loop:}
  @list{if (something) @block{stuff}}
end
