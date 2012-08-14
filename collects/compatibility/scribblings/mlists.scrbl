#lang scribble/doc
@(require scribblings/reference/mz scribble/racket
          (for-label compatibility/mlist))

@title[#:tag "mlists"]{Mutable List Functions}

@(define reference '(lib "scribblings/reference/reference.scrbl"))

@defmodule[compatibility/mlist]

This @racketmodname[compatibility/mlist] library provides support for
@tech[#:doc reference]{mutable list}s.
Support is provided primarily to help porting Lisp/Scheme code to Racket.

Use of mutable lists for modern Racket code is @bold{@italic{strongly}}
discouraged.
Instead, consider using @tech[#:doc reference]{list}s.


For functions described in this section, contracts are not directly
enforced. In particular, when a @tech[#:doc reference]{mutable list}
is expected, supplying any other kind of value (or mutating a value that
starts as a @tech[#:doc reference]{mutable list})
tends to produce an exception from @racket[mcar] or @racket[mcdr].

@defproc[(mlist? [v any/c]) boolean?]{Returns @racket[#t] if
@racket[v] is a @tech[#:doc reference]{mutable list}: either the empty list,
or a @tech[#:doc reference]{mutable pair} whose second element is a
@tech[#:doc reference]{mutable list}.}


@defproc[(mlist [v any/c] ...) mlist?]{Returns a newly allocated
@tech[#:doc reference]{mutable list} containing the @racket[v]s as its
elements.}


@defproc[(list->mlist [lst list?]) mlist?]{

Returns a newly allocated @tech[#:doc reference]{mutable list} with the same
elements as @racket[lst].}


@defproc[(mlist->list [mlst mlist?]) list?]{

Returns a newly allocated list with the same elements as
@racket[mlst].}


@defproc[(mlength [mlst mlist?])
         exact-nonnegative-integer?]{

Returns the number of elements in @racket[mlst].}


@defproc[(mlist-ref [mlst mlist?] [pos exact-nonnegative-integer?])
         any/c]{

Like @racket[list-ref], but for @tech[#:doc reference]{mutable lists}.}


@defproc[(mlist-tail [mlst mlist?] [pos exact-nonnegative-integer?])
         any/c]{

Like @racket[list-tail], but for @tech[#:doc reference]{mutable lists}.}


@defproc*[([(mappend [mlst mlist?] ...) mlist?]
           [(mappend [mlst mlist?] ... [v any/c]) any/c])]{

Like @racket[append], but for @tech[#:doc reference]{mutable lists}.}


@defproc*[([(mappend! [mlst mlist?] ...) mlist?]
           [(mappend! [mlst mlist?] ... [v any/c]) any/c])]{

The @racket[mappend!] procedure appends the given
@tech[#:doc reference]{mutable lists} by mutating
the tail of each to refer to the next, using @racket[set-mcdr!]. Empty
lists are dropped; in particular, the result of calling
@racket[mappend!] with one or more empty lists is the same as the
result of the call with the empty lists removed from the set of
arguments.}


@defproc[(mreverse [mlst mlist?]) mlist?]{

Like @racket[reverse], but for @tech[#:doc reference]{mutable lists}.}


@defproc[(mreverse! [mlst mlist?]) mlist?]{

Like @racket[mreverse], but destructively reverses the
@tech[#:doc reference]{mutable list} by using all of the mutable pairs in
@racket[mlst] and changing them with @racket[set-mcdr!].}


@defproc[(mmap [proc procedure?] [mlst mlist?] ...+)
         mlist?]{

Like @racket[map], but for @tech[#:doc reference]{mutable lists}.}


@defproc[(mfor-each [proc procedure?] [mlst mlist?] ...+)
         void?]{

Like @racket[for-each], but for @tech[#:doc reference]{mutable lists}.}


@defproc[(mmember [v any/c] [mlst mlist?])
         (or/c mlist? #f)]{

Like @racket[member], but for @tech[#:doc reference]{mutable lists}.}


@defproc[(mmemv [v any/c] [mlst mlist?])
         (or/c mlist? #f)]{

Like @racket[memv], but for @tech[#:doc reference]{mutable lists}.}


@defproc[(mmemq [v any/c] [mlst mlist?])
         (or/c list? #f)]{

Like @racket[memq], but for @tech[#:doc reference]{mutable lists}.}


@defproc[(massoc [v any/c] [mlst (mlistof mpair?)])
         (or/c mpair? #f)]{

Like @racket[assoc], but for @tech[#:doc reference]{mutable lists} of
@tech[#:doc reference]{mutable pairs}.}


@defproc[(massv [v any/c] [mlst (mlistof mpair?)])
         (or/c mpair? #f)]{

Like @racket[assv], but for @tech[#:doc reference]{mutable lists} of
@tech[#:doc reference]{mutable pairs}.}


@defproc[(massq [v any/c] [mlst (mlistof mpair?)])
         (or/c mpair? #f)]{

Like @racket[assq], but for @tech[#:doc reference]{mutable lists} of
@tech[#:doc reference]{mutable pairs}.}


@defproc[(mlistof [pred (any/c . -> . any/c)])
         (any/c . -> . boolean?)]{

Returns a procedure that returns @racket[#t] when given a
@tech[#:doc reference]{mutable list} for which @racket[pred] returns a true
value for all elements.}

@; ----------------------------------------------------------------------

@section[#:style '(hidden)]{Legacy Racket Mutable List Library}

@defmodule[racket/mpair]{The @racket[racket/mpair] library
re-exports @racketmodname[compatibility/mlist] for backward
compatibility.}
