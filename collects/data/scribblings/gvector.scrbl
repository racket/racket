#lang scribble/manual
@(require scribble/eval
          (for-label data/gvector
                     racket/contract
                     racket/dict
                     racket/base))

@title[#:tag "gvector"]{Growable Vectors}

@(define the-eval (make-base-eval))
@(the-eval '(require data/gvector))
@(the-eval '(require racket/dict))

@defmodule[data/gvector]

@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

A growable vector (gvector) is a mutable sequence whose length can
change over time. A gvector also acts as a dictionary (@racket[dict?]
from @racketmodname[racket/dict]), where the keys are zero-based
indexes and the values are the elements of the gvector. A gvector can
be extended by adding an element to the end, and it can be shrunk by
removing any element, although removal can take time linear in the
number of elements in the gvector.

Two gvectors are @racket[equal?] if they contain the same number of
elements and if the contain equal elements at each index.

Operations on gvectors are not thread-safe.

@defproc[(make-gvector [#:capacity capacity exact-positive-integer? 10])
         gvector?]{

Creates a new empty gvector with an initial capacity of
@racket[capacity].
}

@defproc[(gvector [elem any/c] ...)
         gvector?]{

Creates a new gvector containing each @racket[elem] in order.
}

@defproc[(gvector? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a gvector, @racket[#f] otherwise.
}

@defproc[(gvector-ref [gv gvector?]
                      [index exact-nonnegative-integer?]
                      [default any/c (error ....)])
         any/c]{

Returns the element at index @racket[index], if @racket[index] is less
than @racket[(gvector-count gv)]. Otherwise, @racket[default] is
invoked if it is a procedure, returned otherwise.
}

@defproc[(gvector-add! [gv gvector?]
                       [value any/c] ...)
         void?]{

Adds each @racket[value] to the end of the gvector @racket[gv].
}

@defproc[(gvector-set! [gv gvector?]
                       [index (and/c exact-nonnegative-integer? 
                                     (</c (+ 1 (gvector-count gv))))]
                       [value any/c])
         void?]{

Sets the value at index @racket[index] to be @racket[value]. If
@racket[index] is @racket[(gvector-count gv)]---that is, one more than
the greatest used index---the effect is the same as
@racket[(gvector-add! gv value)].
}

@defproc[(gvector-remove! [gv gvector?]
                          [index (and/c exact-nonnegative-integer?
                                        (</c (gvector-count gv)))])
         void?]{

Removes the item at @racket[index], shifting items at higher indexes
down. Takes time proportional to @racket[(- (gvector-count gv)
index)].
}

@defproc[(gvector-remove-last! [gv gvector?])
         any/c]{
Removes the element at the end and returns it.  Takes constant time.
}


@defproc[(gvector-count [gv gvector?])
         exact-nonnegative-integer?]{

Returns the number of items in @racket[gv].
}

@defproc[(gvector->vector [gv gvector?])
         vector?]{

Returns a vector of length @racket[(gvector-count gv)] containing the
elements of @racket[gv] in order.
}

@defproc[(gvector->list [gv gvector?])
         list?]{

Returns a list of length @racket[(gvector-count gv)] containing the
elements of @racket[gv] in order.
}

@defproc[(in-gvector [gv gvector?])
         sequence?]{

Returns a sequence whose elements are the elements of
@racket[gv]. Mutation of @racket[gv] while the sequence is running
changes the elements produced by the sequence. To obtain a sequence
from a snapshot of @racket[gv], use @racket[(in-vector
(gvector->vector gv))] instead.
}

@deftogether[[
@defform[(for/gvector (for-clause ...) body ...+)]
@defform[(for*/gvector (for-clause ...) body ...+)]]]{

Analogous to @racket[for/list] and @racket[for*/list], but constructs
a gvector instead of a list.

Unlike @racket[for/list], the @racket[body] may return zero or
multiple values; all returned values are added to the gvector, in
order, on each iteration.
}


@close-eval[the-eval]
