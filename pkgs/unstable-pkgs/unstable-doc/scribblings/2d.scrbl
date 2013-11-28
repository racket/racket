#lang scribble/doc
@(require scribble/base
          scribble/manual
          "utils.rkt"
          scribble/core
          (for-label unstable/2d/cond
                     unstable/2d/match
                     racket/file
                     racket/contract
                     racket/base))

@title[#:tag "2d"]{2D Syntax}

@defmodulelang[unstable/2d]{The @racketmodname[unstable/2d] language installs
@litchar{#2d} reader support in the readtable, and then chains to the reader of
another language that is specified immediately after
@racketmodname[unstable/2d].}

The @litchar{#2d} syntax extension adds the ability use
a two-dimensional grid syntax. That is, you can draw an ASCII-art
grid and then treat that as an expression. For example,
here is a simple equality function that operates on pairs and
numbers, written using a @litchar{#2d} conditional expression:
@codeblock{
  #lang unstable/2d racket
  (require unstable/2d/cond)

  (define (same? a b)
    #2dcond
    ╔═════════════╦═══════════════════════╦═════════════╗
    ║             ║       (pair? a)       ║ (number? a) ║
    ╠═════════════╬═══════════════════════╬═════════════╣
    ║ (pair? b)   ║ (and (same? (car a)   ║     #f      ║
    ║             ║             (car b))  ║             ║
    ║             ║      (same? (cdr a)   ║             ║
    ║             ║             (cdr b))) ║             ║
    ╠═════════════╬═══════════════════════╬═════════════╣
    ║ (number? b) ║          #f           ║   (= a b)   ║
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
are pairs or numbers and evaluating the code in the appropriate cell.

At the reader level, the syntax @litchar{#2d} notation checks
the number of columns in the first row and uses that as a guide
for where subsequent rows may appear. Once that first row is set,
it serves as a guide to where the columns may appear in subsequent 
rows, although following columns may be merged. 

This merging can simplify
some uses of @litchar{#2d} expressions. For example, consider this
expression that captures subtyping relationships between a few of the
Typed Racket numeric types, this time using a @litchar{#2d} match
expression:
@codeblock{
  #lang unstable/2d racket
  (require unstable/2d/match)
  
  (define (subtype? a b)
    #2dmatch
    ╔══════════╦══════════╦═══════╦══════════╗
    ║   a  b   ║ 'Integer ║ 'Real ║ 'Complex ║
    ╠══════════╬══════════╩═══════╩══════════╣
    ║ 'Integer ║             #t              ║
    ╠══════════╬══════════╗                  ║
    ║ 'Real    ║          ║                  ║
    ╠══════════╣          ╚═══════╗          ║
    ║ 'Complex ║        #f        ║          ║
    ╚══════════╩══════════════════╩══════════╝)
}

There are a number of cell walls missing here, but this is still a
well-formed @litchar{#2d} expression. In this case, the @racket[2dmatch]
treats any of the situations that fall into the larger regions as
the same.

In general, a @litchar{#2d} expression, when read, turns into an expression
with at least two sub-pieces (not counting the initial name). The first is
a sequence of numbers giving the widths of the top row of cells;
the second is also a sequence of numbers, this time giving the heights
of the leftmost column of cells. The remaining sequence describe the cells
content. The first element of each is itself a sequence of coordinates,
one for each of the cells that are connected together. The remaining elements
are the subexpressions in the given cells.

For example, this:

@codeblock{
  #lang unstable/2d racket
  '#2dex
  ╔══════════╦══════════╗
  ║    0     ║    1     ║
  ╠══════════╬══════════╣
  ║    2     ║    3     ║
  ╚══════════╩══════════╝
}

evaluates to
@racketblock['(2dex (10 10)
                    (2 2)
                    (((0 0)) 0)
                    (((0 1)) 2)
                    (((1 0)) 1)
                    (((1 1)) 3))]

and this
@codeblock{
  #lang unstable/2d racket
  '#2dex
  ╔══════════╦══════════╦══════════╗
  ║    0     ║    1 2   ║   3 4    ║
  ╠══════════╬══════════╩══════════╣
  ║    5     ║         6           ║
  ╚══════════╩═════════════════════╝
}
evaluates to
@racketblock['(2dex (10 10 10)
                    (2 2)
                    (((0 0)) 0)
                    (((0 1)) 5)
                    (((1 0)) 1 2)
                    (((1 1) (2 1)) 6)
                    (((2 0)) 3 4))]

In addition, the cells coordinates pairs have source locations of the first
character that is inside the corresponding cell. (Currently the span
is always @racket[1], but that may change.)

@section{Editing 2D}

DrRacket provides a number of keybindings to help editing @litchar{#2d} expressions.
See @seclink["Keyboard Shortcuts" #:doc '(lib "scribblings/drracket/drracket.scrbl") #:indirect? #t]{DrRacket's keyboard shortcuts}.

@section{2D Cond}

@defmodule[unstable/2d/cond]

@defform/subs[(2dcond cond-content)
              ([cond-content (code:line question-row
                                        body-row
                                        ⋮)
                             (code:line question-row
                                        body-row
                                        ⋮
                                        else-row)]
               [question-row (code:line empty-cell question-cell ⋯)
                             (code:line empty-cell question-cell ⋯ else-cell)]
               [body-row (code:line question-cell exprs-cell ⋯)]
               [else-row (code:line question-cell exprs-cell ⋯ else-cell)]
               [question-cell (code:line ╔═════════════╗
                                         ║question-expr║
                                         ╚═════════════╝)]
               
               [empty-cell (code:line ╔═══╗
                                      ║   ║
                                      ╚═══╝)]
               
               [exprs-cell (code:line ╔═════════════╗
                                      ║expr expr ...║
                                      ╚═════════════╝)]
               [else-cell (code:line ╔══════╗
                                     ║ else ║
                                     ╚══════╝)])]{
  Evaluates the first row of question expressions until 
  one of them returns a true value (signaling an error if none do),
  then evaluates the first column of question expressions until
  one of them returns a true value (signaling an error if none do),
  and then evaluates the cell in the middle where both point to,
  returning the result of the last expression in that cell.
}

@section{2D Match}

@defmodule[unstable/2d/match]

@defform/subs[(2dmatch match-content)
              ([match-content (code:line match-first-row
                                         match-row
                                         ⋮)]
               [match-first-row (code:line two-expr-cell match-pat-cell ⋯)]
               [match-row (code:line match-pat-cell exprs-cell ⋯)]
               [two-expr-cell (code:line ╔═════════════════╗
                                         ║col-expr row-expr║
                                         ╚═════════════════╝)]
               
               [match-pat-cell (code:line ╔═════╗
                                          ║ pat ║
                                          ╚═════╝)]
               
               [exprs-cell (code:line ╔═════════════╗
                                      ║expr expr ...║
                                      ╚═════════════╝)])]{
  Matches @racket[col-expr] against each of patterns 
  in the first column of the table and matches @racket[row-expr]
  against each of the patterns in the row row, and then evaluates
  the corresponding @racket[exprs-cell], returning the value of the 
  last expression in that cell.
}

@section{2D Tabular}

@defmodule[unstable/2d/tabular]

@defform/subs[(2dmatch tabular-content)
                                           
              ([tabular-content (code:line tabular-row
                                           ⋮)
                                (code:line tabular-row
                                           ⋮
                                           style-cell)]
               [tabular-row (code:line tabular-cell ⋯)]
               [tabular-cell (code:line ╔════════════════╗
                                        ║tabular-expr ...║
                                        ╚════════════════╝)]
               [style-cell (code:line ╔═════════════════╗
                                      ║style-content ...║
                                      ╚═════════════════╝)]
               [style-content (code:line #:style style-expr)
                              (code:line #:sep sep-expr)
                              #:ignore-first-row])
              
              #:contracts ([style-expr style?]
                           [sep-expr (or/c block? content? #f)]
                           [tabular-expr (or/c block? content?)])]{
  Constructs a @racket[tabular] matching the given cells. 
          
  If a cell spans multiple columns, then the resulting
  @racket[tabular] has @racket['cont] in the corresponding 
  list element. No cells may span rows.
               
  The @racket[#:style] and @racket[#:sep] arguments are just passed
  to @racket[tabular]. 
  
  If the @racket[#:ignore-first-row] keyword is provided, then the first
  row of the @racket[2dtabular] expression is ignored. This can be used
  in case the first row of the rendered table should not have all of the
  columns (as @litchar{#2d} syntax requires that the first row contain
  a cell for each column that appears in the table).
}
