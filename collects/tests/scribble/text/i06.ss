#!/bin/env mzscheme
#lang scribble/text

@; indentation works with a list, even a single string with a newline
@; in a list, but not in a string by itself

function foo() {
  prefix
  @list{if (1 < 2)
          something1
        else
          @list{something2
                something3}
          @'("something4\nsomething5")
          @"something6\nsomething7"
        fi}
  return
}

@; can be used with a `display', but makes sense only at the top level
@; or in thunks (not demonstrated here)
@;
@(display 123) foo @list{bar1
                         bar2
                         bar2}
