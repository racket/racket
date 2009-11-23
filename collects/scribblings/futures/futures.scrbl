#lang scribble/doc

@title{@bold{Futures}: Fine-grained Parallelism}

@; ----------------------------------------------------------------------

@(require scribble/manual
          scribble/urls
          scribble/struct
          scheme/class
          (for-label scheme/base
                     scheme/gui/base
                     scheme/class
                     scheme/contract))

@; ----------------------------------------------------------------------

PLT's future support is only enabled if you pass @tt{--enable-futures} to @tt{configure} when
you build PLT (and that build currently only works with @tt{mzscheme}, not with @tt{mred}).

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

