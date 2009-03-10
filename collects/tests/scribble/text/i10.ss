#!/bin/env mzscheme
#lang scribble/text

@(define (((if . c) . t) . e)
   @list{if (@c)
           @t
         else
           @e
         fi})

function foo() {
  @prefix["//"]{ comment1
                 comment2 @list{comment3
                                comment4}}
  var x = [@list{item1,
                 item2}]
  bar1
  @list{if (1 < 2)
          @list{something1
                something2
                something3}
        else
          @@@if{2 < 3}{something_else}{something_completely_different}
          @@@if{3 < 4}{
            another_something_else1
            another_something_else2
          }{
            another_something_completely_different
          }
        fi
        }
  return;
}
