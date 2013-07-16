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
on the indentation of a file. This is extremely rare and must be noted at
the top so that readers do not accidentally re-indent the file.}

@; -----------------------------------------------------------------------------
@section{Where to Put Parentheses}

Racket isn't C. Put all closing parentheses on one line, the last line of
your code.

@compare[
 @racketmod[#:file
 @tt{good}
 racket
@;%
 (define (conversion f)
   (* 5/9 (- f 32)))
]
 @filebox[@tt{really bad}
 @codeblock{#lang racket
 (define (conversion f)
   (* 5/9 (- f 32)
     )
   )
 }]
]

You are allowed to place all closing parenthesis on a line by itself at the
end of long sequences, be those definitions or pieces of data.

@compare[
 @filebox[@tt{acceptable}
 @codeblock{#lang racket
 (define modes
   '(edit
     help
     debug
     test
     trace
     step
     ))
 }]
 @filebox[@tt{also acceptable}
 @codeblock{#lang racket
 (define turn%
   (class object%
     (init-field state)

     (super-new)

     (define/public (place where tile)
       (send state where tile))

     (define/public (is-placable? place)
       (send state legal? place))
     ))
 }]
]
 Doing so is most useful when you expect to add, delete, or swap items in
 such sequences.

@; -----------------------------------------------------------------------------
@section{Indentation}

DrRacket indents code and it is the only tool that everyone in PLT agrees
 on. So use DrRacket's indentation style. Here is what this means.
 @nested[#:style 'inset]{
 For every file in the repository, DrRacket's "indent all" functions leaves
 the file alone.}

If you prefer to use some other editor (emacs, vi/m, etc), program it so
that it follows DrRacket's indentation style.

Examples:

@compare[
         @racketmod[#:file
                    @tt{good}
                    racket

		    (code:comment #, @t{drracket style})
                    (if (positive? (rocket-x r))
                        (launch r)
                        (redirect (- x)))
                                         ]

          @racketmod[#:file
                     @tt{bad}
                     racket

		     (code:comment #, @t{.el emacs-file if})
                     (if (positive? (rocket-x r))
                         (launch r)
                       (redirect (- x)))
 ]
]

@bold{Caveat 1}: Until language specifications come with fixed indentation
rules, we need to use the @emph{default} settings of DrRacket's indentation
for this rule to make sense. If you add new constructs, say a for loop,
please contact Robby for advice on how to add a default setting for the
indentation functionality. If you add entire languages, say something on
the order of Typed Racket, we will need to wait for Matthew and Robby to
provide an indentation protocol on the @verbatim{#lang} line; please be
patient and use the existing indentation tool anyway.

@bold{Caveat 2}: This rule does not apply to scribble code.

@; -----------------------------------------------------------------------------
@section{Tabs}

Do not use tab characters in your code.  Tabs make it hard to use textual
 tools like git or diff effectively.  To disable tabs,
@itemlist[
@item{in DrRacket: you are all set. It doesn't insert tabs.}
@item{in Emacs: add (setq indent-tabs-mode nil) to your emacs initialization file.}
@item{in vi: set expandtab.}
]

@; -----------------------------------------------------------------------------
@section{Line Width}

A line in a Racket file is at most @LINEWIDTH[] characters wide.

If you prefer a narrower width than @LINEWIDTH[], and if you stick to this
width ``religiously,'' add a note to the top of the file---right below the
purpose statement---that nobody should violate your file-local rule.

This number is a compromise. People used to recommend a line width of 80 or
72 column. The number is a historical artifact. It is also a good number
for several different reasons: printing code in text mode, displaying code
at reasonable font sizes, comparing several different pieces of code on a
monitor, and possibly more. So age doesn't make it incorrect. We regularly
read code on monitors that accommodate close to 250 columns, and on
occasion, our monitors are even wider. It is time to allow for somewhat
more width in exchange for meaningful identifiers.

So, when you create a file, add a line with ";; " followed by ctrl-U 99 and
"-". When you separate "sections" of code in a file, insert the same line.
These lines help both writers and readers to orient themselves in a file.

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

It is acceptable to have an entire @racket[if] expressions on one line if
it fits within the specified line width (@LINEWIDTH[]):
@codebox[
@racketmod[#:file
@tt{also good}
racket

(if (positive? x) x (- x))
]
]

Each definition and each local definition deserves at least one line.

@compare[
@racketmod[#:file
@tt{good}
racket

(define (launch x)
  (define w 9)
  (define h 33)
  ...)
]

@racketmod[#:file
@tt{bad}
racket

(define (launch x)
  (define w 9) (define h 33)
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

(above img
       (- width  hdelta)
       (- height vdelta)
       bg)
]

@racketmod[#:file
@tt{bad}
racket

(above ufo
       10 v-delta bg)

]]

Here is an exception:
@codebox[
@racketmod[#:file
@tt{good}
racket

(overlay/offset (rectangle 100 10 "solid" "blue")
                10 10
                (rectangle 10 100 "solid" "red"))
]]
 In this case, the two arguments on line 2 are both conceptually
 related and short.


@; -----------------------------------------------------------------------------
@section[#:tag "names"]{Names}

Use meaningful names. The Lisp convention is to use full English words
separated by dashes. Racket code benefits from the same convention.

@compare[
@;%
@(begin
#reader scribble/comment-reader
[racketmod #:file
@tt{good}
racket

render-game-state

send-message-to-client

traverse-forest
])

@; -----------------------------------------------------------------------------
@;%
@(begin
#reader scribble/comment-reader
[racketmod #:file
@tt{bad}
racket

rndr-st

sendMessageToClient

traverse_forest
])
]
@;
 Note that _ (the underline character) is also classified as bad
 Racketeering within names. It is an acceptable placeholder in syntax
 patterns, match patterns, and parameters that don't matter.

Another widely used convention is to @emph{prefix} a function name with the data
 type of the main argument. This convention generalizes the selector-style
 naming scheme of @racket[struct].
@codebox[
@(begin
#reader scribble/comment-reader
[racketmod #:file
@tt{good}
racket

board-free-spaces      board-closed-spaces    board-serialize
])]
 In contrast, variables use a @emph{suffix} that indicates their type:
@codebox[
@(begin
#reader scribble/comment-reader
[racketmod #:file
@tt{good}
racket

(define (win-or-lose? game-state)
  (define position-nat-nat (game-state-position game-state))
  (define health-level-nat (game-state-health game-state))
  (define name-string      (game-state-name game-state))
  (define name-symbol      (string->symbol name-string))
  ...)
])]
 The convention is particularly helpful when the same piece of data shows
 up in different guises, say, symbols and strings.

Names are bad if they heavily depend on knowledge about the context of the
 code. It prevents readers from understanding a piece of functionality at
 an approximate level without also reading large chunks of the surrounding
 and code.

Finally, in addition to regular alphanumeric characters, Racketeers use a
 few special characters by convention, and these characters indicate
 something about the name:

@row-table[
 @row[symbol kind example]
 @row[?    "predicates and boolean-valued functions" boolean?]
 @row[!    "setters and field mutators"              set!]
 @row[%    "classes"                                 game-state%]
 @row[<%>  "interfaces"                              dc<%>]
 @row[^    "unit signatures"                         game-context^]
 @row["@"  "units"                                   testing-context@]
 @row["#%" "kernel identifiers"                      #%app]
]
 @margin-note*{Identifiers with this prefix are mostly used in modules that
 define new languages.}  The use of ``#%'' to prefix names from the kernel
 language warns readers that these identifiers are extremely special and
 they need to watch out for subtleties. No other identifiers start with
 ``#'' and, in particular, all tokens starting with ``#:'' are keywords.

@; -----------------------------------------------------------------------------
@section{Graphical Syntax}

Do not use graphical syntax (comment boxes, XML boxes, etc).

The use of graphical syntax makes it impossible to read files in
alternative editors. It also messes up some revision control systems.
When we figure out how to save such files in an editor-compatible way, we
may relax this constraint.

@section{Spaces}

Don't pollute your code with spaces at the end of lines.

If you find yourself breaking long blocks of code with blank lines to aid
readability, consider refactoring your program to introduce auxiliary
functions so that you can shorten these long blocks of code. If nothing
else helps, consider using (potentially) empty comment lines.

@; -----------------------------------------------------------------------------
@section{End of File}

End files with a newline.
