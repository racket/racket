#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label scribble/bnf))

@title[#:tag "bnf"]{BNF Grammars}

@defmodule[scribble/bnf]{The @scheme[scribble/bnf] library
provides utilities for typesetting grammars.}

See also @scheme[schemegrammar].

@defproc[(BNF [prod (cons element? (listof element?))] ...) table?]{

Typesets a grammar table. Each production starts with an element
(typically constructed with @scheme[nonterm]) for the non-terminal
being defined, and then a list of possibilities (typically constructed
with @scheme[BNF-seq], etc.) to show on separate lines.}

@defproc[(nonterm (pre-content any/c) ...) element?]{

Typesets a non-terminal: italic in angle brackets.}

@defproc[(BNF-seq [elem element?] ...) element?]{

Typesets a sequence.}

@defproc[(BNF-group [pre-content any/c] ...) element?]{

Typesets a group surrounded by curly braces (so the entire group can
be repeated, for example).}

@defproc[(optional [pre-content any/c] ...) element?]{

Typesets an optional element: in square brackets.}

@defproc[(kleenestar [pre-content any/c] ...) element?]{

Typesets a 0-or-more repetition.}

@defproc[(kleeneplus [pre-content any/c] ...) element?]{

Typesets a 1-or-more repetition.}

@defproc[(kleenerange [n any/c] [m any/c] [pre-content any/c] ...) element?]{

Typesets a @scheme[n]-to-@scheme[m] repetition. The @scheme[n] and
@scheme[m] arguments are converted to a string using @scheme[(format
"~a" n)] and @scheme[(format "~a" m)].}

@defproc[(BNF-alt [elem element?] ...) element?]{

Typesets alternatives for a production's right-hand side to appear on
a single line. The result is normally used as a single possibility in
a production list for @scheme[BNF].}

@defthing[BNF-etc string?]{

A string to use for omitted productions or content.}
