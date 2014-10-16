#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket/base racket/contract unstable/error))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/error))

@title[#:tag "error"]{Errors}
@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[unstable/error]

@defproc[(error* [name symbol?]
                 [message string?]
                 [field (let ([option/c (or/c 'value 'multi 'maybe)])
                          (or/c string? (cons/c string? (listof option/c))))]
                 [value any/c] ... ...
                 [#:continued continued-message (or/c string? (listof string?)) null])
         any]{

Raises an exception with a message composed according to the Racket
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{error
message convention}. The raised exception is an instance of
@racket[exn:fail].

The composed error message includes details consisting of the
alternating @racket[field] and @racket[value] arguments. By default,
@racket[value] is formatted as if by @racket[display] unless it is
@racket[#f], in which case the detail line is omitted. The following
options affect the formatting of the detail line:

@itemlist[

@item{@racket['multi] formats each element in the corresponding @racket[value],
which must be a list, as a separate line; if @racket['maybe] is also provided,
then the detail line is omitted if the list is empty}

@item{@racket['value] formats the value using
@racket[error-value->string-handler]; the detail line is not omittable
unless @racket['maybe] or @racket['multi] is also provided}

]

@examples[#:eval the-eval

(error* 'mcbean "too many stars upon thars"
        '("given" value) 'star-bellied-sneetch
        '("stars" value) 3)

(error* 'hal "unable to open pod bay doors"
        #:continued "this mission is too important to let you jeopardize it"
        "threat" "David Bowman"
        "detection" "lip reading")

(error* 'car "missing car keys"
        '("searched" multi)
        (list "dresser" "desk" "kitchen table" "under sofa"
              "behind microwave" "in washing machine")
        "last seen"
        #f)
]
}

@defproc[(raise-syntax-error* 
              [message string?]
              [expr (or/c syntax? #f)]
              [sub-expr (or/c syntax? #f)]
              [field (let ([option/c (or/c 'value 'multi 'maybe)])
                       (or/c string? (cons/c string? (listof option/c))))]
              [value any/c] ... ...
              [#:continued continued-message (or/c string? (listof string?)) null])
         any]{

Like @racket[raise-syntax-error] but with the formatting of
@racket[error*]. The raised exception is an instance of
@racket[exn:fail:syntax]. Like @racket[raise-syntax-error], the
inclusion of @racket[expr] and @racket[sub-expr] in the details of the
error message is controlled by the
@racket[error-print-source-location] paramter; if they included, they
are included before the other details specified by @racket[field] and
@racket[value]. Unlike @racket[raise-syntax-error], both @racket[expr]
and @racket[sub-expr] are mandatory arguments.
}

@defproc[(compose-error-message
                         [name (or/c symbol? #f)]
                         [message string?]
                         [field (let ([option/c (or/c 'value 'multi 'maybe)])
                                  (or/c string? (cons/c string? (listof option/c))))]
                         [value any/c] ... ...
                         [#:continued continued-message (or/c string? (listof string?)) null])
         string?]{

Like @racket[error*], but produces a string conforming to
the Racket @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{error message convention}.
}

@defproc[(compose-error-detail
             [field string?]
             [options (listof (or/c 'value 'multi 'maybe))]
             [value any/c])
         string?]{

Formats a single detail for an error message. The @racket[options]
behave as described in @racket[error*].

The resulting string begins with a newline unless it is empty, so it
can be appended to the end of a base error message.
}

@(close-eval the-eval)

@addition{Jay McCarthy}

@defproc[(exn:not-break? [x any/c]) boolean?]{Identifies values that are not @racket[exn:break?], i.e. values that are safe to catch with @racket[with-handlers].}

@defproc[(error-display [x any/c]) void?]{Calls @racket[(error-display-handler)] with the proper arguments whehter @racket[x] is an exception, or not.}
