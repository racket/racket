#!/bin/env mzscheme
#lang scribble/text

@; demonstrates using a prefix

function foo() {
  var lst = [@list{item1,
                   item2}]
  @prefix["//"]{ comment1
                 comment2
                   comment3
                 @list{comment4
                         comment5
                       comment6}
                 @prefix["*"]{ more
                               stuff}}
  return
}
