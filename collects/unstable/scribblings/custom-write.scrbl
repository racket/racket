#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
          "utils.rkt"
          (for-label unstable/custom-write
                     racket/base
                     racket/contract
                     racket/pretty))

@title[#:tag "custom-write"]{Struct Printing}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/custom-write]

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/custom-write racket/pretty))

@defproc[(make-constructor-style-printer
            [get-constructor (-> any/c (or/c symbol? string?))]
            [get-contents    (-> any/c sequence?)])
         (-> any/c output-port? (or/c #t #f 0 1) void?)]{

Produces a function suitable as a value for @racket[prop:custom-write]. The
function prints values in ``constructor style.'' When the value is
@racket[print]ed as an expression, it is shown as an application of the
constructor (as returned by @racket[get-constructor]) to the contents (as
returned by @racket[get-contents]). When given to @racket[write], it is shown as
an unreadable value with the constructor separated from the contents by a colon.

@examples[#:eval the-eval
(struct point (x y)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (obj) 'point)
   (lambda (obj) (list (point-x obj) (point-y obj)))))
(print (point 1 2))
(write (point 1 2))
]

The function also cooperates with @racket[pretty-print]:

@examples[#:eval the-eval
(parameterize ((pretty-print-columns 10))
  (pretty-print (point #e3e6 #e4e6)))
(parameterize ((pretty-print-columns 10))
  (pretty-write (point #e3e6 #e4e6)))
]
}


@defthing[prop:auto-custom-write
          (struct-type-property/c 'constructor)]{

When attached to a struct type, automatically generates a printer using
@racket[make-constructor-style-printer] and attaches it to the struct type's
@racket[prop:custom-write] property. It also sets the
@racket[prop:custom-print-quotable] property to @racket['never].

@examples[#:eval the-eval
(struct point3 (x y z)
  #:property prop:auto-custom-write 'constructor)
(print (point3 3 4 5))
(write (point3 3 4 5))
]
}


@close-eval[the-eval]
