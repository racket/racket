#lang scribble/doc

@title{@bold{Futures}: Fine-grained Parallelism}

@; ----------------------------------------------------------------------

@(require scribble/manual
          scribble/urls
          scribble/struct
          (for-label scheme/base
                     scheme/contract
                     '#%futures))

@; ----------------------------------------------------------------------

PLT's future support is only enabled if you pass
@DFlag{enable-futures} to @exec{configure} when you build PLT (and that
build currently only works with @exec{mzscheme}, not with
@exec{mred}).

@defmodule['#%futures]{}

@defproc[(future [thunk (-> any)]) future?]{
  Starts running @scheme[thunk] in parallel.
}

@defproc[(touch [f future?]) any]{
  Returns the value computed in the future @scheme[f], blocking
  to let it complete if it hasn't yet completed.
}

@defproc[(future? [x any/c]) boolean?]{
  Returns @scheme[#t] if @scheme[x] is a future.                               
}

