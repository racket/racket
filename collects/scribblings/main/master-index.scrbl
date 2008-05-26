#lang scribble/doc
@(require scribble/basic
          scribble/decode
          "private/utils.ss")

@main-page['index #t]

@(make-splice (index-blocks))
