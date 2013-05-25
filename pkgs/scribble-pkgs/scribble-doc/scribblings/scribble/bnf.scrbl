#lang scribble/doc
@(require scribble/manual "utils.rkt" scribble/bnf
          (for-label scribble/bnf))

@title[#:tag "bnf"]{BNF Grammars}

@defmodule[scribble/bnf]{The @racket[scribble/bnf] library
provides utilities for typesetting grammars.}

For example,

@verbatim[#:indent 2]|{
@(let ([open @litchar{(}]
       [close @litchar{)}])
   @BNF[(list @nonterm{expr}
              @nonterm{id}
              @BNF-seq[open @kleeneplus[@nonterm{expr}] close]
              @BNF-seq[open @litchar{lambda}
                        open @kleenestar[@nonterm{id}] close
                        @nonterm{expr} close]
              @nonterm{val})
        (list @nonterm{val} 
              @BNF-alt[@nonterm{number} @nonterm{primop}])
        (list @nonterm{id}
              @elem{any name except for @litchar{lambda}})])
}|

produces the output

@(let ([open @litchar{(}]
       [close @litchar{)}])
   @BNF[(list @nonterm{expr}
              @nonterm{id}
              @BNF-seq[open @kleeneplus[@nonterm{expr}] close]
              @BNF-seq[open @litchar{lambda}
                        open @kleenestar[@nonterm{id}] close
                        @nonterm{expr} close]
            @nonterm{val})
      (list @nonterm{val} 
            @BNF-alt[@nonterm{number} @nonterm{primop}])
        (list @nonterm{id}
              @elem{any name except for @litchar{lambda}})])

See also @racket[racketgrammar].

@defproc[(BNF [prod (cons element? (listof (or/c block? element?)))] ...) table?]{

Typesets a grammar table. Each production starts with an element
(typically constructed with @racket[nonterm]) for the non-terminal
being defined, and then a list of possibilities (typically constructed
with @racket[BNF-seq], etc.) to show on separate lines.}

@defproc[(nonterm (pre-content any/c) ...) element?]{

Typesets a non-terminal: italic in angle brackets.}

@defproc[(BNF-seq [elem element?] ...) element?]{

Typesets a sequence.}

@defproc[(BNF-seq-lines [elems (listof element?)] ...) block?]{

Typesets a sequence that is broken into multiple lines, where each
@racket[elems] is one line.}

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

Typesets a @racket[n]-to-@racket[m] repetition. The @racket[n] and
@racket[m] arguments are converted to a string using @racket[(format
"~a" n)] and @racket[(format "~a" m)].}

@defproc[(BNF-alt [elem element?] ...) element?]{

Typesets alternatives for a production's right-hand side to appear on
a single line. The result is normally used as a single possibility in
a production list for @racket[BNF].}

@defthing[BNF-etc string?]{

A string to use for omitted productions or content.}
