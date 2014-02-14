#lang scribble/manual

@(require "../utils.rkt" (for-label (only-meta-in 0 typed/racket)))
@(provide typed-mod)

@title[#:tag "quick"]{Quick Start}

Given a module written in the @racketmodname[racket] language, using
Typed Racket requires the following steps:

@itemize[#:style
         'ordered
         @item{Change the language to @racketmodname[typed/racket].}
         @item{Change the uses of @racket[(require mod)] to
           @racket[(require typed/mod)].}
         @item{Annotate structure definitions and top-level
           definitions with their types.} ]

Then, when the program is run, it will automatically be typechecked
before any execution, and any type errors will be reported.  If there
are any type errors, the program will not run.

Here is an example program, written in the @racketmodname[racket]
language:

@(define typed-mod
@racketmod[
typed/racket
(struct: pt ([x : Real] [y : Real]))

(: distance (-> pt pt Real))
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))
]
)

@racketmod[
racket
(struct pt (x y))

(code:contract distance : pt pt -> real)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))
]

Here is the same program, in @racketmodname[typed/racket]:

@|typed-mod|

@section{Using Typed Racket from the Racket REPL}

It is possible to use Typed Racket from the Racket REPL. To do so, start Racket
with the following command line:
@commandline{racket -I typed/racket}
