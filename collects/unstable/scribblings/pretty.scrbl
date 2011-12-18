#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/pretty))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/pretty unstable/pretty))

@title{Pretty-Printing}
@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defmodule[unstable/pretty]

This module provides tools for pretty-printing.

@defproc[(pretty-format/write [x any/c]
                              [columns
                               (or/c exact-nonnegative-integer? 'infinity)
                               (pretty-print-columns)])
         string?]{

This procedure behaves like @racket[pretty-format], but it formats values
consistently with @racket[write] instead of @racket[print].

@examples[#:eval the-eval
(struct both [a b] #:transparent)
(pretty-format/write (list (both (list 'a 'b) (list "a" "b"))))
]

}

@defproc[(pretty-format/display [x any/c]
                                [columns
                                 (or/c exact-nonnegative-integer? 'infinity)
                                 (pretty-print-columns)])
         string?]{

This procedure behaves like @racket[pretty-format], but it formats values
consistently with @racket[display] instead of @racket[print].

@examples[#:eval the-eval
(struct both [a b] #:transparent)
(pretty-format/display (list (both (list 'a 'b) (list "a" "b"))))
]

}

@defproc[(pretty-format/print [x any/c]
                              [columns
                               (or/c exact-nonnegative-integer? 'infinity)
                               (pretty-print-columns)])
         string?]{

This procedure behaves the same as @racket[pretty-format], but is named
more explicitly to describe how it formats values.  It is included for
symmetry with @racket[pretty-format/write] and @racket[pretty-format/display].

@examples[#:eval the-eval
(struct both [a b] #:transparent)
(pretty-format/print (list (both (list 'a 'b) (list "a" "b"))))
]

}

@addition{@author+email["Vincent St-Amour" "stamourv@racket-lang.org"]}
@defproc[(break-lines [s string?] [columns exact-nonnegative-integer? (pretty-print-columns)])
         string?]{
Splits the string @racket[s] into multiple lines, each of width at most
@racket[columns], splitting only at whitespace boundaries.
@examples[#:eval the-eval
(display (break-lines "This string is more than 80 characters long. It is 98 characters long, nothing more, nothing less."))
]
}


@(close-eval the-eval)
