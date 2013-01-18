#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          (for-label ;unstable/2d/cond
                     ;unstable/2d/match
                     racket/file
                     racket/contract
                     racket/base))

@title[#:tag "2d"]{2D Cond and Match}

@defmodulelang[unstable/2d]{The @racketmodname[unstable/2d] language installs
@litchar{#2d} reader support in the readtable, and then chains to the reader of
another language that is specified immediately after
@racketmodname[unstable/2d].}

The @litchar{#2d} syntax extension adds the ability use 
two dimensional grid syntax. That is, you can drawn an ASCII-art
grid and then treat that as a conditional expression. For example,
here is a simple equality function that operates on pairs and
numbers:
@codeblock{
  #lang unstable/2d racket

  (define (same? a b)
    #2dcond
    ╔═════════════╦═══════════════════════╦═════════════╗
    ║             ║          (pair? a)    ║ (number? a) ║
    ╠═════════════╬═══════════════════════╬═════════════╣
    ║ (pair? b)   ║ (and (same? (car a)   ║     #f      ║
    ║             ║             (car b))  ║             ║
    ║             ║      (same? (cdr a)   ║             ║
    ║             ║             (cdr b))) ║             ║
    ╠═════════════╬═══════════════════════╬═════════════╣
    ║ (number? b) ║           #f          ║   (= a b)   ║
    ╚═════════════╩═══════════════════════╩═════════════╝)
}

This notation works in two stages: reading, and parsing (just as in
Racket in general). The reading stage converts anything that begins
with @litchar{#2d} into a parenthesized expression (possibly signaling
errors if the @litchar{═} and @litchar{║} and @litchar{╬}
characters do not line up in the right places).

Since the first line contains @litchar{#2dcond}, the reader will 
produce a sequence whose first position is the identifier @racket[2dcond].

That macro will take over and then expand into ordinary conditional
expressions, in this case figuring out whether or not the inputs
are pairs or numbers and selecting the appropriate cell.

@section{2D Cond}

@defmodule[unstable/2d/cond]

@defform[(2dcond . stuff)]{}

@section{2D Match}

@defmodule[unstable/2d/match]

@defform[(2dmatch . stuff)]{}
