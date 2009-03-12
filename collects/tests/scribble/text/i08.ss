#!/bin/env mzscheme
#lang scribble/text

@; using verbatim
@(define (((foo . var) . expr1) . expr2)
   @list{int var;
         @verbatim{#ifdef FOO}
         var = [@expr1,
                @expr2];
         @verbatim{#else}
         var = [@expr2,
                @expr1];
         @verbatim{#endif}})

int blah() {
  @@@foo{i}{something}{something_else}
}
