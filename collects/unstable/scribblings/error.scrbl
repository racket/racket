#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket/base racket/contract unstable/error))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/error))

@title[#:tag "error"]{Errors}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/error]

@defproc[(raise-misc-error [name symbol?]
                           [message string?]
                           [field (let ([option/c (or/c 'value 'multi 'maybe)])
                                    (or/c string? (cons/c string? (listof option/c))))]
                           [value any/c] ... ...
                           [#:continued continued-message (or/c string? (listof string?)) null]
                           [#:constructor constructor
                                          (-> string? continuation-mark-set? exn?) 
                                          exn:fail])
         any]{

Raises an exception with a message composed according to the Racket
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{error
message convention}. The exception is created with
@racket[constructor], which is @racket[exn:fail] by default. 

The composed error message includes details consisting of the
alternating @racket[field] and @racket[value] arguments. By default,
@racket[value] is formatted as if by @racket[display] unless it is
@racket[#f], in which case the detail line is omitted. The following
options affect the formatting of the detail line:

@itemlist[

@item{@racket['multi] formats each element in the corresponding value,
which must be a list, as a separate line}

@item{@racket['value] formats the value using
@racket[error-value->string-handler]; the detail line is not omittable
unless @racket['maybe] or @racket['multi] is also provided}

]

@examples[#:eval the-eval

(raise-misc-error 'mcbean "too many stars upon thars"
                  '("given" value) 'star-bellied-sneetch
                  '("stars" value) 3)

(raise-misc-error 'hal "unable to open pod bay doors"
                  #:continued
                  "this mission is too important to let you jeopardize it"
                  "threat" "David Bowman"
                  "detection" "lip reading")

(raise-misc-error 'car "missing car keys"
                  '("searched" multi)
                  (list "dresser" "desk" "kitchen table" "under sofa"
                        "behind microwave" "in washing machine")
                  "last seen"
                  #f)
]
}

@defproc[(compose-error-message
                         [name symbol?]
                         [message string?]
                         [field (let ([option/c (or/c 'value 'multi 'maybe)])
                                  (or/c string? (cons/c string? (listof option/c))))]
                         [value any/c] ... ...
                         [#:continued continued-message (or/c string? (listof string?)) null])
         string?]{

Like @racket[raise-misc-error], but produces a string conforming to
the Racket @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{error message convention}.
}

@(close-eval the-eval)
