#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/pretty))

@title{Pretty-Printing}

@defmodule[unstable/pretty]

This module provides tools for pretty-printing.

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defproc[(pretty-format/write [x any/c]
                              [columns
                               (or/c exact-nonnegative-integer? 'infinity)
                               (pretty-print-columns)])
         string?]{

This procedure behaves like @scheme[pretty-format], but it formats values
consistently with @scheme[write] instead of @scheme[print].

@examples[#:eval (eval/require 'racket/pretty 'unstable/pretty)
(struct both [a b] #:transparent)
(pretty-format/write (list (both (list 'a 'b) (list "a" "b"))))
]

}

@defproc[(pretty-format/display [x any/c]
                                [columns
                                 (or/c exact-nonnegative-integer? 'infinity)
                                 (pretty-print-columns)])
         string?]{

This procedure behaves like @scheme[pretty-format], but it formats values
consistently with @scheme[display] instead of @scheme[print].

@examples[#:eval (eval/require 'racket/pretty 'unstable/pretty)
(struct both [a b] #:transparent)
(pretty-format/display (list (both (list 'a 'b) (list "a" "b"))))
]

}

@defproc[(pretty-format/print [x any/c]
                              [columns
                               (or/c exact-nonnegative-integer? 'infinity)
                               (pretty-print-columns)])
         string?]{

This procedure behaves the same as @scheme[pretty-format], but is named
more explicitly to describe how it formats values.  It is included for
symmetry with @scheme[pretty-format/write] and @scheme[pretty-format/display].

@examples[#:eval (eval/require 'racket/pretty 'unstable/pretty)
(struct both [a b] #:transparent)
(pretty-format/print (list (both (list 'a 'b) (list "a" "b"))))
]

}
