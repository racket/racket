#lang scribble/base

@(require "shared.rkt")

@title{Size Matters}

@; -----------------------------------------------------------------------------
@section{Code Units}

Keep functions small. Keep classes small. Keep units small. Keep modules small.

Anytime a unit of code looks incomprehensible, it is probably too
large. Break it up into smaller units. To bring across what these smaller
units compute, implement or serve, use meaningful names. Conversely, if you
can't come up with a good name for such units, you are probably looking at
the wrong kind of division; consider alternatives.
