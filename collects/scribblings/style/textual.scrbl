#lang scribble/base

@(require "shared.rkt")

@(define-syntax-rule
  (good form code ...)
  (racketmod #:file (tt "good") racket form code ...))

@title{Textual Matters}

Simple textual conventions help eyes find pieces of code quickly. Here are
some of those that are easy to check---some automatically and some
manually. If you find yourself editing a file that violates some of the
constraints below, edit it into the proper
shape. @margin-note{@bold{Warning}: On rare occasion a unit test may depend
on the indentation of a file. This is extremely rare but you should be
aware of it.}

@; -----------------------------------------------------------------------------
@section{Indentation}

DrRacket indents code and it is the only tool that everyone in PLT agrees
on. So use DrRacket's indentation style. Here is what this means.
@nested[#:style 'inset]{
 For every file in the repository, DrRacket's "indent all" functions leaves
 the file alone.}
That's all there is to it. @margin-note{See @secref{correctness}. If you
really believe that DrRacket indents some construct improperly, submit a
bug report. When the bug report is closed, the discussion is finished.}

If you prefer to use some other editor (emacs, vi/m, etc), program it so
that it follows DrRacket's indentation style.

Examples:

@racketmod[#:file
@tt{good}
racket

(if (positive? x)
    (send rocket-object launch)
    (redirect (- x)))
]

@racketmod[#:file
@tt{bad}
racket

(if (positive? x)
  (send rocket-object launch)
  (redirect (- x)))
]

@margin-note{we need more of these rules}

@; -----------------------------------------------------------------------------
@section{Line Breaks}

Next to indentation, proper line breaks are critical.

For an @scheme[if] expression, put each alternative on a separate line.

@racketmod[#:file
@tt{good}
racket

(if (positive? x)
    (send rocket-object launch)
    (redirect (- x)))
]

@racketmod[#:file
@tt{bad}
racket

(if (positive? x) (send rocket-object launch)
    (redirect (- x)))
]

Each definition and each local definition deserves at least one line.

@racketmod[#:file
@tt{good}
racket

(define (start-reactor x)
  (define width (* 10 x))
  (define height (* 3 x))
  ...)
]

@racketmod[#:file
@tt{bad}
racket

(define (start-reactor x)
  (define width (* 10 x)) (define height (* 3 x))
  ...)
]
@margin-note{we need more of these rules}

@; -----------------------------------------------------------------------------
@section{Line Width}

A line in a Racket file is at most 102 characters wide.

When you create a file, add a line with ";; " followed by ctrl-U 99 and "-".
When you separate "sections" of code in a file, insert the same line. This
provides some line-width orientation in the middle of a file, too.

@; -----------------------------------------------------------------------------
@section{Where to Put Parentheses}

Racket isn't C. Put all closing parentheses on one line.
