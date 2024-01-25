#lang scribble/manual
@(require "mz.rkt"
          (for-syntax racket/base)
          (for-label racket/treelist
                     racket/mutable-treelist))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/treelist racket/mutable-treelist))

@title[#:tag "treelist"]{Treelists}

A @deftech{treelist} is represents a sequence of list elements in a
way that supports many operations in @math{O(log N)} time: accessing
an element of the list by index, adding to the front of the list,
adding to the end of the list, removing an element by index, replacing
an element by index, appending lists, dropping elements from the start
or end of the list, and extracting a sublist. The base for the
@math{log} in @math{O(log N)} is large enough that it's effectively
constant-time for many purposes. Treelists are currently implemented
as RRB trees @cite["Stucki15"].

Treelists are primarily intended to be used in immutable form via
@racketmodname[racket/treelist], where an operation such as adding to
the treelist produces a new treelist while the old one remains intact.
A mutable variant of treelists is provided by
@racketmodname[racket/mutable-treelist], where an mutable treelist can
be a convenient alternative to putting an immutable treelist into a
@tech{box}. Mutable treelist operations take the same time as
immutable treelist operations, unless otherwise specified. Where the
term ``treelist'' is used by itself, it refers to an immutable
treelist.

An immutable or mutable treelist can be used as a single-valued
sequence (see @secref["sequences"]). The elements of the list serve as
elements of the sequence. See also @racket[in-treelist] and
@racket[in-mutable-treelist]. An immutable treelist can also be used
as a @tech{stream}.

@section{Immutable Treelists}

@note-lib-only[racket/treelist]

@history[#:added "8.12.0.7"]


@defproc[(treelist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{treelist}, @racket[#f]
otherwise.}

@defproc[(treelist [v any/c] ...) treelist?]{

Returns a @tech{treelist} with @racket[v]s as its elements in order.

This operation takes @math{O(N log N)} time to construct a treelist of
@math{N} elements. Unless otherwise specified, other operations on a
treelist of length @math{N} also take @math{O(log N)} time.

@examples[
#:eval the-eval
(treelist 1 "a" 'apple)
]}


@deftogether[(
@defproc[(treelist-empty? [tl treelist?]) boolean?]
@defthing[empty-treelist (and/c treelist? treelist-empty?)]
)]{

A predicate and constant for a @tech{treelist} of length 0.

Although every empty treelist is @racket[equal?] to
@racket[empty-treelist], since a treelist can be chaperoned via
@racket[chaperone-treelist], not every empty treelist is @racket[eq?]
to @racket[empty-treelist].}


@defproc[(treelist-length [tl treelist?]) exact-nonnegative-integer?]{

Returns the number of elements in @racket[tl]. This operation takes
@math{O(1)} time.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-length items)
]}

@defproc[(treelist-ref [tl treelist?] [pos exact-nonnegative-integer?]) any/c]{

Returns the @racket[i]th element of @racket[tl]. The first element is
position @racket[0], and the last position is one less than
@racket[(treelist-length tl)].

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-ref items 0)
(treelist-ref items 2)
(eval:error (treelist-ref items 3))
]}


@deftogether[(
@defproc[(treelist-first [tl treelist?]) any/c]
@defproc[(treelist-last [tl treelist?]) any/c]
)]{

Shorthands for using @racket[treelist-ref] to access the first or last
element of a @tech{treelist}.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-first items)
(treelist-last items)
]}


@defproc[(treelist-insert [tl treelist?] [pos exact-nonnegative-integer?] [v any/c]) treelist?]{

Produces a treelist like @racket[tl], except that @racket[v] is
inserted as an element before the element at @racket[pos]. If
@racket[pos] is @racket[(treelist-length tl)], then @racket[v] is
added to the end of the treelist.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-insert items 1 "alpha")
(treelist-insert items 3 "alpha")
]}


@deftogether[(
@defproc[(treelist-add [tl treelist?] [v any/c]) treelist?]
@defproc[(treelist-cons [tl treelist?] [v any/c]) treelist?]
)]{

Shorthands for using @racket[treelist-insert] to insert at the
end or beginning of a @tech{treelist}.

Although the main operation to extend a pair @tech{list} is
@racket[cons] to add to the front, treelists are intended to be
extended by adding to the end with @racket[treelist-add], and
@racket[treelist-add] tends to be faster than @racket[treelist-cons].

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-add items "alpha")
(treelist-cons items "alpha")
]}


@defproc[(treelist-delete [tl treelist?] [pos exact-nonnegative-integer?]) treelist?]{

Produces a treelist like @racket[tl], except that the element at
@racket[pos] is removed.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-delete items 1)
(eval:error (treelist-delete items 3))
]}


@defproc[(treelist-set [tl treelist?] [pos exact-nonnegative-integer?] [v any/c]) treelist?]{

Produces a treelist like @racket[tl], except that is the element at
@racket[pos] is replaced with @racket[v]. The result is equivalent to
@racket[(treelist-insert (treelist-delete tl pos) pos v)].

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-set items 1 "b")
]}

@defproc[(treelist-append [tl treelist?] ...) treelist?]{

Appends the elements of the given @racket[tl]s into a single
@tech{treelist}. If @math{M} non-chaperoned treelists are given and
the resulting treelist's length is @math{N}, then appending takes
@math{O(M log N)} time.

If any other than the first @racket[tl] is chaperoned via
@racket[chaperone-treelist] with a procedure for it @racket[_ref-proc],
the time to produce the result of @math{O(N)} for @math{N} elements
added to the first @racket[tl].

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-append items items)
(treelist-append items (treelist "middle") items)
]}

@deftogether[(
@defproc[(treelist-take [tl treelist?] [n exact-nonnegative-integer?]) treelist?]
@defproc[(treelist-drop [tl treelist?] [n exact-nonnegative-integer?]) treelist?]
@defproc[(treelist-take-right [tl treelist?] [n exact-nonnegative-integer?]) treelist?]
@defproc[(treelist-drop-right [tl treelist?] [n exact-nonnegative-integer?]) treelist?]
)]{

Produces a @tech{treelist} like @racket[tl] but with only the first
@racket[n] elements, without the first @racket[n] elements, with only
the last @racket[n] elements, or without the last @racket[n] elements,
respectively.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-take items 2)
(treelist-drop items 2)
(treelist-take-right items 2)
(treelist-drop-right items 2)
]}

@defproc[(treelist-sublist [tl treelist?] [n exact-nonnegative-integer?] [m exact-nonnegative-integer?]) treelist?]{

Produces a @tech{treelist} like @racket[tl] but with only elements at
position @racket[n] (inclusive) through position @racket[m] (exclusive).

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-sublist items 1 3)
]}


@defproc[(treelist-reverse  [tl treelist?]) treelist?]{

Produces a @tech{treelist} like @racket[tl] but with its elements
reversed, equivalent to using @racket[treelist-take] to keep
@racket[0] elements (but also any chaperone on the treelist) and then
adding each element back in reverse order. Reversing takes
@math{O(N log N)} time.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-reverse items)
]}


@defproc[(treelist-rest [tl treelist?]) treelist?]{

A shorthand for using @racket[treelist-drop] to drop the first element
of a @tech{treelist}.

The @racket[treelist-rest] operation is efficient, but not as fast as
@racket[rest] or @racket[cdr]. For iterating through a treelist,
consider using @racket[treelist-ref] or a @racket[for] form with
@racket[in-treelist], instead.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-rest items)
]}


@deftogether[(
@defproc[(treelist->vector [tl treelist?]) vector?]
@defproc[(treelist->list [tl treelist?]) list?]
@defproc[(vector->treelist [vec vector?]) treelist?]
@defproc[(list->treelist [lst list?]) treelist?]
)]{

Convenience functions for converting between @tech{treelists},
@tech{lists}, and @tech{vectors}. Each conversion takes @math{O(N)}
time.

@examples[
#:eval the-eval
(define items (list->treelist '(1 "a" 'apple)))
(treelist->vector items)
]}


@defproc[(treelist-map [tl treelist?] [proc (any/c . -> . any/c)]) treelist?]{

Produces a @tech{treelist} by applying @racket[proc] to each element
of @racket[tl] and gathering the results into a new treelist. For a
constant-time @racket[proc], this operation takes @math{O(N)} time.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-map items box)
]}


@defproc[(treelist-for-each [tl treelist?] [proc (any/c . -> . any)]) void?]{

Applies @racket[proc] to each element of @racket[tl], ignoring the
results. For a constant-time @racket[proc], this operation takes
@math{O(N)} time.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-for-each items println)
]}

@defproc[(treelist-member? [tl treelist?] [v any/c] [eql? (any/c any/c . -> . any/c) equal?]) boolean?]{

Checks each element of @racket[tl] with @racket[eql?] and @racket[v]
(with @racket[v] the second argument) until the result is a true
value, and then returns @racket[#t]. If no such element is found, the
result is @racket[#false]. For a constant-time @racket[eql?], this
operation takes @math{O(N)} time.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-member? items "a")
(treelist-member? items 1.0 =)
(eval:error (treelist-member? items 2.0 =))
]}

@defproc[(treelist-find [tl treelist?] [pred (any/c . -> . any/c)]) any/c]{

Checks each element of @racket[tl] with @racket[pred] until the result
is a true value, and then returns that element. If no such element is
found, the result is @racket[#false]. For a constant-time
@racket[pred], this operation takes @math{O(N)} time.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-find items string?)
(treelist-find items symbol?)
]}

@defproc[(treelist-sort [tl treelist?]
                        [less-than? (any/c any/c . -> . any/c)]
                        [#:key key (or/c #f (any/c . -> . any/c)) #f]
                        [#:cache-keys? cache-keys? boolean? #f])
         treelist?]{

Like @racket[sort], but operates on a @tech{treelist} to
produce a sorted treelist. Sorting takes @math{O(N log N)} time.

@examples[
#:eval the-eval
(define items (treelist "x" "a" "q"))
(treelist-sort items string<?)
]}

@defproc[(in-treelist [tl treelist?]) sequence?]{

Returns a @tech{sequence} equivalent to @racket[tl].
@speed[in-treelist "treelist"]

@examples[
#:eval the-eval
(define items (treelist "x" "a" "q"))
(for/list ([e (in-treelist items)])
  (string-append e "!"))
]}

@deftogether[(
@defform[(for/treelist (for-clause ...) body-or-break ... body)]
@defform[(for*/treelist (for-clause ...) body-or-break ... body)]
)]{

Like @racket[for/list] and @racket[for*/list], but generating
@tech{treelists}.

@examples[
#:eval the-eval
(for/treelist ([i (in-range 10)])
  i)
]}

@defproc[(chaperone-treelist [tl treelist?]
                             [ref-proc (or/c #f (treelist? exact-nonnegative-integer? any/c . -> . any/c))]
                             [set-proc (treelist? exact-nonnegative-integer? any/c . -> . any/c)]
                             [insert-proc (treelist? exact-nonnegative-integer? any/c . -> . any/c)]
                             [append-proc (treelist? treelist? . -> . treelist?)]
                             [prop impersonator-property?]
                             [prop-val any/c] ... ...)
          (and/c treelist? chaperone?)]{

Analogous to @racket[chaperone-vector], returns a @tech{chaperone} of
@racket[tl], which redirects the @racket[treelist-ref],
@racket[treelist-set], @racket[treelist-insert], and
@racket[treelist-append] operations, as well as operations derived
from those.

If @racket[ref-proc] is a procedure, it must accept @racket[tl], an index
passed to @racket[treelist-ref], and the value that
@racket[treelist-ref] on @racket[tl] produces for the given index; it
must produce a chaperone replacement for the value, which is the
result of @racket[treelist-ref] on the chaperone. A @racket[ref-proc] as
@racket[#false] is equivalent to @racket[(lambda (tl _i _v) _v)], except
that it does not disable efficient @racket[treelist-append].

The @racket[set-proc] procedure must accept @racket[tl], an index
passed to @racket[treelist-set], and the value provided to
@racket[treelist-set]; it must produce a chaperone replacement for the
value, which is used in the result of @racket[treelist-set] on the
chaperone. The result of @racket[treelist-set] is chaperoned with the
same procedures as @racket[tl].

The @racket[insert-proc] procedure is like @racket[set-proc], but for
inserting via @racket[treelist-insert].

The @racket[append-proc] procedure must accept @racket[tl] and a
treelist to append into @racket[tl]; it must produce a chaperone
replacement for the second treelist, which is appended for result of
@racket[treelist-append] on the chaperone. The result of
@racket[treelist-append] is chaperoned with the same procedures as
@racket[tl].}


@section{Mutable Treelists}

@note-lib-only[racket/mutable-treelist]

A @deftech{mutable treelist} is like an immutable @tech{treelist} in a
box, where operations that change the mutable treelist replace the
treelist in the box. As a special case, @racket[mutable-treelist-set!]
modifies the treelist representation within the boxed value. This
model of a mutable treelist explains its behavior in the case of
concurrent modification: concurrent @racket[mutable-treelist-set!]
operations for different positions will not interefere, but races with
other operations will sometimes negate one of the modifications.
Concurrent modification is thus somewhat unpredictable but still safe,
and it is not managed by a lock.

A mutable treelist is not a treelist in the sense of
@racket[treelist?], which recognizes only immutable treelists.
Operations on a mutable treelist have the same time complexity as
corresponding operations on an immutable treelist.

@defproc[(mutable-treelist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{mutable treelist},
@racket[#f] otherwise.}

@defproc[(mutable-treelist [v any/c] ...) treelist?]{

Returns a @tech{mutable treelist} with @racket[v]s as its elements in order.

@examples[
#:eval the-eval
(mutable-treelist 1 "a" 'apple)
]}

@defproc[(make-mutable-treelist [n nonnegative-exact-integer?] [v any/c #f]) mutable-treelist?]{

Creates a @tech{mutable treelist} that contains @racket[n] elements,
each initialized as @racket[v]. Creating the mutable treelist takes @math{O(N)}
time for @math{N} elements.

@examples[
#:eval the-eval
(make-mutable-treelist 3 "a")
]}


@defproc[(treelist-copy [tl treelist?]) mutable-treelist?]{

Creates a @tech{mutable treelist} that contains the same elements as
@racket[tl]. Creating the mutable treelist takes @math{O(N)} time for
@math{N} elements.

@examples[
#:eval the-eval
(treelist-copy (treelist 3 "a"))
]}

@defproc[(mutable-treelist-snapshot [tl mutable-treelist?]) treelist?]{

Produces an immutable @tech{treelist} that has the same elements as
@racket[tl]. Creating the immutable treelist takes @math{O(N)} time for
@math{N} elements.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(define snap (mutable-treelist-snapshot items))
(mutable-treelist-drop! items 2)
items
snap
]}


@defproc[(mutable-treelist-empty? [tl mutable-treelist?]) boolean?]{

Returns @racket[#t] for @tech{mutable treelist} that is currently of
length 0, @racket[#f] otherwise.}


@defproc[(mutable-treelist-length [tl mutable-treelist?]) exact-nonnegative-integer?]{

Returns the number of elements currently in @racket[tl].

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-length items)
(mutable-treelist-add! items 'extra)
(mutable-treelist-length items)
]}

@defproc[(mutable-treelist-ref [tl mutable-treelist?] [pos exact-nonnegative-integer?]) any/c]{

Returns the @racket[i]th element of @racket[tl]. The first element is
position @racket[0], and the last position is one less than
@racket[(mutable-treelist-length tl)].

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-ref items 0)
(mutable-treelist-ref items 2)
(eval:error (mutable-treelist-ref items 3))
]}


@deftogether[(
@defproc[(mutable-treelist-first [tl mutable-treelist?]) any/c]
@defproc[(mutable-treelist-last [tl mutable-treelist?]) any/c]
)]{

Shorthands for using @racket[mutable-treelist-ref] to access the first or last
element of a @tech{treelist}.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-first items)
(mutable-treelist-last items)
]}


@defproc[(mutable-treelist-insert! [tl mutable-treelist?] [pos exact-nonnegative-integer?] [v any/c]) void?]{

Modifies @racket[tl] to insert @racket[v] into the list before
position @racket[pos]. If @racket[pos] is
@racket[(mutable-treelist-length tl)], then @racket[v] is added to the
end of the treelist.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-insert! items 1 "alpha")
items
]}


@deftogether[(
@defproc[(mutable-treelist-cons! [tl mutable-treelist?] [v any/c]) void?]
@defproc[(mutable-treelist-add! [tl mutable-treelist?] [v any/c]) void?]
)]{

Shorthands for using @racket[mutable-treelist-insert!] to insert at the
beginning or end of a @tech{treelist}.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-cons! items "before")
(mutable-treelist-add! items "after")
items
]}


@defproc[(mutable-treelist-delete! [tl mutable-treelist?] [pos exact-nonnegative-integer?]) void?]{

Modifies @racket[tl] to remove the element at @racket[pos].

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-delete! items 1)
items
]}


@defproc[(mutable-treelist-set! [tl mutable-treelist?] [pos exact-nonnegative-integer?] [v any/c]) void?]{

Modifies @racket[tl] to change the element at @racket[pos] to
@racket[v].

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-set! items 1 "b")
items
]}

@defproc[(mutable-treelist-append! [tl mutable-treelist?] [other-tl (or/c treelist? mutable-treelist?)]) void?]{

Modifies @racket[tl]s by appending all of the elements of
@racket[other-tl]. If @racket[other-tl] is a @tech{mutable treelist},
it is first converted to an immutable @tech{treelist} with
@racket[mutable-treelist-snapshot].

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-append! items (treelist 'more 'things))
items
(mutable-treelist-append! items items)
items
]}

@deftogether[(
@defproc[(mutable-treelist-take! [tl mutable-treelist?] [n exact-nonnegative-integer?]) void?]
@defproc[(mutable-treelist-drop! [tl mutable-treelist?] [n exact-nonnegative-integer?]) void?]
@defproc[(mutable-treelist-take-right! [tl mutable-treelist?] [n exact-nonnegative-integer?]) void?]
@defproc[(mutable-treelist-drop-right! [tl mutable-treelist?] [n exact-nonnegative-integer?]) void?]
)]{

Modifies @racket[tl] to remove all but the first @racket[n] elements,
to remove the first @racket[n] elements, to remove all but the last
@racket[n] elements, or to remove the last @racket[n] elements,
respectively.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-take! items 2)
items
(mutable-treelist-drop-right! items 1)
items
]}

@defproc[(mutable-treelist-sublist! [tl mutable-treelist?] [n exact-nonnegative-integer?] [m exact-nonnegative-integer?]) void?]{

Modifies @racket[tl] to remove elements other than elements at
position @racket[n] (inclusive) through position @racket[m]
(exclusive).

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple 'pie))
(mutable-treelist-sublist! items 1 3)
items
]}

@defproc[(mutable-treelist-reverse! [tl mutable-treelist?]) void?]{

Modifies @racket[tl] to reverse all of its elements.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple 'pie))
(mutable-treelist-reverse! items)
items
]}

@deftogether[(
@defproc[(mutable-treelist->vector [tl mutable-treelist?]) vector?]
@defproc[(mutable-treelist->list [tl mutable-treelist?]) list?]
@defproc[(vector->mutable-treelist [vec vector?]) mutable-treelist?]
@defproc[(list->mutable-treelist [lst list?]) mutable-treelist?]
)]{

Convenience functions for converting between @tech{mutable treelists},
@tech{lists}, and @tech{vectors}. Each conversion takes @math{O(N)}
time.

@examples[
#:eval the-eval
(define items (list->mutable-treelist '(1 "a" 'apple)))
(mutable-treelist->vector items)
]}


@defproc[(mutable-treelist-map! [tl mutable-treelist?] [proc (any/c . -> . any/c)]) void?]{

Modifies @racket[tl] by applying @racket[proc] to each element
of @racket[tl] and installed the result in place of the element.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-map! items box)
items
]}


@defproc[(mutable-treelist-for-each [tl mutable-treelist?] [proc (any/c . -> . any)]) void?]{

Like @racket[treelist-for-each], but for a @tech{mutable treelist}.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-for-each items println)
]}

@defproc[(mutable-treelist-member? [tl mutable-treelist?] [v any/c] [eql? (any/c any/c . -> . any/c) equal?]) boolean?]{

Like @racket[treelist-member?], but for a @tech{mutable treelist}.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-member? items "a")
(mutable-treelist-member? items 1.0 =)
]}

@defproc[(mutable-treelist-find [tl mutable-treelist?] [pred (any/c . -> . any/c)]) any/c]{

Like @racket[treelist-find], but for a @tech{mutable treelist}.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-find items string?)
(mutable-treelist-find items symbol?)
]}

@defproc[(mutable-treelist-sort! [tl mutable-treelist?]
                                 [less-than? (any/c any/c . -> . any/c)]
                                 [#:key key (or/c #f (any/c . -> . any/c)) #f]
                                 [#:cache-keys? cache-keys? boolean? #f])
         void?]{

Like @racket[vector-sort!], but operates on a @tech{mutable treelist}.

@examples[
#:eval the-eval
(define items (mutable-treelist "x" "a" "q"))
(mutable-treelist-sort! items string<?)
items
]}

@defproc[(in-mutable-treelist [tl mutable-treelist?]) sequence?]{

Returns a @tech{sequence} equivalent to @racket[tl].
@speed[in-mutable-treelist "mutable treelist"]

@examples[
#:eval the-eval
(define items (mutable-treelist "x" "a" "q"))
(for/list ([e (in-mutable-treelist items)])
  (string-append e "!"))
]}

@deftogether[(
@defform[(for/mutable-treelist (for-clause ...) body-or-break ... body)]
@defform[(for*/mutable-treelist (for-clause ...) body-or-break ... body)]
)]{

Like @racket[for/list] and @racket[for*/list], but generating
@tech{mutable treelists}.

@examples[
#:eval the-eval
(for/mutable-treelist ([i (in-range 10)])
  i)
]}

@defproc[(chaperone-mutable-treelist [tl mutable-treelist?]
                                     [ref-proc (or/c #f (mutable-treelist? exact-nonnegative-integer? any/c . -> . any/c))]
                                     [set-proc (mutable-treelist? exact-nonnegative-integer? any/c . -> . any/c)]
                                     [insert-proc (mutable-treelist? exact-nonnegative-integer? any/c . -> . any/c)]
                                     [append-proc (mutable-treelist? treelist? . -> . treelist?)]
                                     [prop impersonator-property?]
                                     [prop-val any/c] ... ...)
          (and/c mutable-treelist? chaperone?)]{

Like @racket[chaperone-treelist], but for @tech{mutable treelists}.
For example, the given @racket[set-proc] is used for
@racket[mutable-treelist-set!], and the resulting value is installed
into the mutable treelist instead of the one provided to @racket[set-proc].}

@defproc[(impersonate-mutable-treelist [tl mutable-treelist?]
                                       [ref-proc (mutable-treelist? exact-nonnegative-integer? any/c . -> . any/c)]
                                       [set-proc (mutable-treelist? exact-nonnegative-integer? any/c . -> . any/c)]
                                       [insert-proc (mutable-treelist? exact-nonnegative-integer? any/c . -> . any/c)]
                                       [append-proc (mutable-treelist? treelist? . -> . treelist?)]
                                       [prop impersonator-property?]
                                       [prop-val any/c] ... ...)
          (and/c mutable-treelist? chaperone?)]{

Like @racket[chaperone-mutable-treelist], but @racket[ref-proc],
@racket[set-proc], @racket[insert-proc], and @racket[append-proc]
are not obligated to produce chaperones.}


@(close-eval the-eval)
