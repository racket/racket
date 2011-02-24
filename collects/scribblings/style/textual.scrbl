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
@section{Where to Put Parentheses}

Racket isn't C. Put all closing parentheses on one line, the last line of
your code.

@compare[
 @racketmod[#:file
 @tt{good}
 racket

 (define (conversion f)
   (* 5/9 (- f 32)))
]
@racketmod[#:file
 @tt{really bad}
 racket

 (define (conversion f)
   (* 5/9 (- f 32)
     )
   )
 ]
]

You are allowed to place all closing parenthesis on a line by itself at the
end of long sequences, be those definitions or pieces of data.

@compare[
 @racketmod[#:file
 @tt{acceptable}
 racket

 (define modes
   '(edit
     help
     debug
     test
     trace
     step
     )) ;; <--- bug in scribble: the last two are on their own line
]
@racketmod[#:file
 @tt{also acceptable}
 racket

 (define turn%
   (class object%
     (init-field state)

     (super-new)

     (define/public (place where tile)
       (send state where tile))

     (define/public (is-placable? place)
       (send state legal? place))
     )) ;; <--- bug in scribble: the last two are on their own line
 ]
]

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

@compare[
         @racketmod[#:file
                    @tt{good}
                    racket

                    (if (positive? (rocket-x r))
                        (launch r)
                        (redirect (- x)))
                                         ]

          @racketmod[#:file
                     @tt{bad}
                     racket

                     (if (positive? (rocket-x r))
                       (launch r)
                       (redirect (- x)))
 ]
]

@margin-note{We need more of these rules}

@; -----------------------------------------------------------------------------
@section{Line Breaks}

Next to indentation, proper line breaks are critical.

For an @scheme[if] expression, put each alternative on a separate line.

@compare[
@racketmod[#:file
@tt{good}
racket

(if (positive? x)
    (launch r)
    (redirect (- x)))
                                         ]

@racketmod[#:file
@tt{bad}
racket

(if (positive? x) (launch r)
    (redirect (- x)))
]
]

Each definition and each local definition deserves at least one line.

@compare[
@racketmod[#:file
@tt{good}
racket

(define (launch x)
  (define wdt (* 10 x))
  (define hgt (* 3 x))
  ...)
]

@racketmod[#:file
@tt{bad}
racket

(define (launch x)
  (define wdt (* 10 x)) (define hgt (* 3 x))
  ...)
]
]

All of the arguments to a function belong on a single line unless the line
becomes too long, in which case you want to put each argument expression on
its own line

@compare[
@racketmod[#:file
@tt{good}
racket

(place-image img 10 10 background)

(code:comment #, @t{and})

(composition img
             (- width  hdelta)
             (- height vdelta)
	     bg)

]

@racketmod[#:file
@tt{bad}
racket

(composition ufo-with-flames
             10 v-delta bg)

]]

Here is an exception:
@racketmod[#:file
@tt{good}
racket

(overlay/offset (rectangle 100 10 "solid" "blue")
                10 10
                (rectangle 10 100 "solid" "red"))
]
 In this case, the two arguments on line 2 are both short and conceptually
 related.

@margin-note{We need more of these rules}

@; -----------------------------------------------------------------------------
@section{Line Width}

A line in a Racket file is at most 102 characters wide.

This number is a compromise. People used to recommend a line width of 80 or
72 column. The number is a historical artifact. It is also a good number if
you wish to print code or project it at a reasonably large font size in a
typical class room. In reality, we don't print code anymore and we don't
show much of our code base to a classroom full of students. We regularly
read code on monitors that accommodate close to 200 columns, and on
occasion, our monitors are even wider. It is time to allow for somewhat
more width in exchange for meaning full identifiers.

So, when you create a file, add a line with ";; " followed by ctrl-U 99 and
"-". When you separate "sections" of code in a file, insert the same line.
These lines help both writers and readers to orient themselves in a file.

@; -----------------------------------------------------------------------------
@section[#:tag "names"]{Names}

Use meaningful names. The Lisp convention is to use full English words
separated by dashes. Racket code benefits from the same convention.

In addition to regular alphanumeric characters, Racketeers use a few
special characters.

@column-table[
 @col[? ! "@" ^ %]
 @col[1 2 3 4 5]
 @col[1 2 3 4 5] ]

@row-table[
 @row[? predicates boolean?]
 @row[! setters    set!]
 @row[% classes    a%]
 @row["@" units      a@]
 @row[^ signatures a^]
]
