#!/bin/env mzscheme
#lang scribble/text

@(define (((if . c) . t) . e)
   @list{
     if (@c)
       @t
     else
       @e
     fi})

@; indentation works even when coming from a function

function foo() {
  @list{if (1 < 2)
          something1
        else
          @@@if{2<3}{something2}{something3}
          repeat 3 {
            @@@if{2<3}{something2}{something3}
            @@@if{2<3}{
              @list{something2.1
                    something2.2}
            }{
              something3
            }
          }
        fi}
  return
}
