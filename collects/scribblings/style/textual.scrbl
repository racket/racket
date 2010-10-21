#lang scribble/base

@(require "shared.rkt")

@title{Textual Matters}

@; -----------------------------------------------------------------------------
@section{Indentation}

DrRacket indents code. Use it. If you wish to be a real friend of PLT, use
DrRacket all the time, and do request features and changes for things you
don't like.

Real friends of PLT use DrRacket to write their programs, and they don't
override DrRacket's indentation style. They accept it and leave it alone.
The resulting uniformity helps guide eyes.

@margin-note{Okay, okay. We are using emacs to write this guide.} Minor
friends of PLT use Emacs and/or vi(m).  Nevertheless, these minor friends
adjust their chosen editor so that it follows DrRacket's way of indenting
code.

One of the most important disagreements may concern @scheme[if]. So once
and for all:

@(racketmod
racket
#:file good
(if (positive? x)
    (send rocket-object launch)
    (redirect (- x)))
)

@(racketmod
racket
#:file bad
(if (positive? x)
  (send rocket-object launch)
  (redirect (- x)))
)

Also note that the then- and else-branches are separate entities, and each
entity deserves at least one line.

@; -----------------------------------------------------------------------------
@section{Line Width}

A line in Racket is at most 80 characters wide.

If you use Emacs to edit, create a line with ";; " followed by ctrl-U 77
and "-". Okay, use DrRacket to create such lines by holding down the dash
key.

@; -----------------------------------------------------------------------------
@section{Where to Put Parentheses}

Racket isn't C. Put all closing parentheses on one line.
