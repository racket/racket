#lang scribble/manual
@(require "utils.rkt" (for-label racket unstable/markparam))

@title{Mark Parameters}
@unstable[@author+email["Jay McCarthy" "jay@racket-lang.org"]]

@defmodule[unstable/markparam]

This library provides a simplified version of parameters that are backed by continuation marks, rather than parameterizations. This means they are slightly slower, are not inherited by child threads, do not have initial values, and cannot be imperatively mutated.

@defstruct*[mark-parameter ()]{
 The struct for mark parameters. It is guaranteed to be serializable and transparent. If used as a procedure, it calls @racket[mark-parameter-first] on itself.
 }

@defproc[(mark-parameter-first [mp mark-parameter?]
                               [tag continuation-prompt-tag? default-continuation-prompt-tag])
         any/c]{
 Returns the first value of @racket[mp] up to @racket[tag].
 }
               
@defproc[(mark-parameter-all [mp mark-parameter?]
                             [tag continuation-prompt-tag? default-continuation-prompt-tag])
         list?]{
 Returns the values of @racket[mp] up to @racket[tag].
 }   
  
@defproc[(mark-parameters-all [mps (listof mark-parameter?)]
                              [none-v [any/c #f]]
                              [tag continuation-prompt-tag? default-continuation-prompt-tag])
         (listof vector?)]{
  Returns the values of the @racket[mps] up to @racket[tag]. The length
  of each vector in the result list is the same as the length of
  @racket[mps], and a value in a particular vector position is the value
  for the corresponding mark parameter in @racket[mps]. Values for
  multiple mark parameter appear in a single vector only when the mark
  parameters are for the same continuation frame in the current
  continuation. The @racket[none-v] argument is used for vector elements
  to indicate the lack of a value.
}

@defform[(mark-parameterize ([mp expr] ...) body-expr ...)]{
  Parameterizes @racket[(begin body-expr ...)] by associating each
  @racket[mp] with the evaluation of @racket[expr] in the
  parameterization of the entire expression.
}
