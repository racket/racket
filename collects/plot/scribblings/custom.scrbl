#lang scribble/manual

@(require "common.rkt")

@title[#:tag "custom"]{Making Custom Plot Renderers}

@defmodule[plot/custom]

Eventually, enough of the underlying PLoT API will be exposed that anyone can create new @tech{renderers}.
However, the underlying API still changes too often.
As soon as it settles, @racketmodname[plot/custom] will export it, and this page will document how to use it.
