#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "vectors"]{Vectors}

A @deftech{vector} is a fixed-length array of arbitrary
values. Unlike a list, a vector supports constant-time access and
update of its elements.

A vector prints similar to a list---as a parenthesized sequence of its
elements---but a vector is prefixed with @litchar{#} after
@litchar{'}, or it uses @racketresult[vector] if one of its elements
cannot be expressed with @racket[quote].

For a vector as an expression, an optional length can be
supplied. Also, a vector as an expression implicitly @racket[quote]s
the forms for its content, which means that identifiers and
parenthesized forms in a vector constant represent symbols and lists.

@refdetails/gory["parse-vector"]{the syntax of vectors}

@examples[
(eval:alts @#,racketvalfont{#("a" "b" "c")} #("a" "b" "c"))
(eval:alts @#,racketvalfont{#(name (that tune))} #(name (that tune)))
(vector-ref #("a" "b" "c") 1)
(vector-ref #(name (that tune)) 1)
]

Like strings, a vector is either mutable or immutable, and vectors
written directly as expressions are immutable.

Vector can be converted to lists and vice versa via
@racket[list->vector] and @racket[vector->list]; such conversions are
particularly useful in combination with predefined procedures on
lists. When allocating extra lists seems too expensive, consider
using looping forms like @racket[for/fold], which recognize vectors as
well as lists.

@examples[
(list->vector (map string-titlecase
                   (vector->list #("three" "blind" "mice"))))
]

@refdetails["vectors"]{vectors and vector procedures}
