#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "vectors"]{Vectors}

A @defterm{vector} is a fixed-length array of arbitarary
values. Unlike a list, a vector supports constant-time access and
update of its elements.

A vector prints similar to a list---as a parenthesized sequence of its
elements---but a vector is prefixed with @litchar{#} and the length
of the vector. The vector length is optional for a vector as an
expression. Also, a vector as an expression implicitly quotes the
forms for its content, which means that identifiers and parenthesized
forms in a vector constant represent symbols and lists.

@refdetails/gory["mz:parse-vector"]{the syntax of vectors}

@examples[
(eval:alts #, @schemevalfont{#("a" "b" "c")} #("a" "b" "c"))
(eval:alts #, @schemevalfont{#(name (that tune))} #(name (that tune)))
(vector-ref #("a" "b" "c") 1)
(vector-ref #(name (that tune)) 1)
]

When the last @math{n} vector elements of a vector are the same value
(as determined by @scheme[eq?]), then the last @math{n-1} instances
are omitted from the printed form. The vector length shown after the
leading @litchar{#} effectively indicates when repeated trailing
elements are omitted. The same conventions apply for vectors as
expressions.

@examples[
(define v (make-vector 100 "."))
v
(vector-set! v 1 "!")
v
(vector-ref #10("." "?") 8)
]

Like strings, a vector is either mutable or immutable, and vectors
written directly as expressions are immutable.

Vector can be converted to lists and vice-versa via
@scheme[list->vector] and @scheme[vector->list]; such conversions are
particularly useful in combination with predefined procedures on
lists. When allocating extra lists seems too expensive, use consider
using looping forms like @scheme[fold-for], which recognize vectors as
well as lists.

@examples[
(list->vector (map string-titlecase
                   (vector->list #("three" "blind" "mice"))))
]

@refdetails["mz:vectors"]{vectors and vector procedures}
