#!/bin/env mzscheme
#lang scribble/text

@; demonstrates how indentation is preserved inside lists

begin
  a
  b
  @list{c
        d
        @list{e
                f
              g}
        h
          i
          @list{j
                  k
                l}
          m
        n
        o}
  p
  q
end
