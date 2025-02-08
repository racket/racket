#lang scribble/manual
@(require "mz.rkt"
          (for-syntax racket/base)
          (for-label racket/treelist
                     racket/mutable-treelist))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/treelist racket/mutable-treelist racket/stream))

@title[#:tag "treelist"]{Treelists}

A @deftech{treelist} represents a sequence of elements in a
way that supports many operations in @math{O(log N)} time: accessing
an element of the list by index, adding to the front of the list,
adding to the end of the list, removing an element by index, replacing
an element by index, appending lists, dropping elements from the start
or end of the list, and extracting a sublist. More generally,
unless otherwise specified, operations on a
treelist of length @math{N} take @math{O(log N)} time. The base for the
@math{log} in @math{O(log N)} is large enough that it's effectively
constant-time for many purposes. Treelists are currently implemented
as RRB trees @cite["Stucki15"].

Treelists are primarily intended to be used in immutable form via
@racketmodname[racket/treelist], where an operation such as adding to
the treelist produces a new treelist while the old one remains intact.
A mutable variant of treelists is provided by
@racketmodname[racket/mutable-treelist], where a mutable treelist can
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

@history[#:changed "8.15.0.3" @elem{Made treelists serializable.}]

@section{Immutable Treelists}

@note-lib-only[racket/treelist]

@history[#:added "8.12.0.7"]


@defproc[(treelist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{treelist}, @racket[#f]
otherwise.}

@defproc[(treelist [v any/c] ...) treelist?]{

Returns a @tech{treelist} with @racket[v]s as its elements in order.

This operation takes @math{O(N log N)} time to construct a treelist of
@math{N} elements.

@examples[
#:eval the-eval
(treelist 1 "a" 'apple)
]}

@defproc[(make-treelist [size exact-nonnegative-integer?] [v any/c]) treelist?]{

 Returns a @tech{treelist} with size @racket[size], where
 every element is @racket[v].
 This operation takes @math{O(log N)} time to construct a
 treelist of @math{N} elements.

 @examples[
 #:eval the-eval
 (make-treelist 0 'pear)
 (make-treelist 3 'pear)
 ]

@history[#:added "8.12.0.11"]}

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

Returns the @racket[pos]th element of @racket[tl]. The first element is
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

Produces a treelist like @racket[tl], except that the element at
@racket[pos] is replaced with @racket[v]. The result is equivalent to
@racket[(treelist-insert (treelist-delete tl pos) pos v)].

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-set items 1 "b")
]}

@defproc[(treelist-append [tl treelist?] ...) treelist?]{

Appends the elements of the given @racket[tl]s into a single
@tech{treelist}. If @math{M} treelists are given and the resulting
treelist's length is @math{N}, then appending takes @math{O(M log N)}
time.

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

@defproc[(treelist-filter [keep (any/c . -> . any/c)] [tl treelist?])
         treelist?]{

Produces a treelist with only members of @racket[tl] that satisfy
@racket[keep].

@examples[
#:eval the-eval
(treelist-filter even? (treelist 1 2 3 2 4 5 2))
(treelist-filter odd? (treelist 1 2 3 2 4 5 2))
(treelist-filter (λ (x) (not (even? x))) (treelist 1 2 3 2 4 5 2))
(treelist-filter (λ (x) (not (odd? x))) (treelist 1 2 3 2 4 5 2))
]

@history[#:added "8.15.0.6"]}

@defproc[(treelist-member? [tl treelist?] [v any/c] [eql? (any/c any/c . -> . any/c) equal?]) boolean?]{

Checks each element of @racket[tl] with @racket[eql?] and @racket[v]
(with @racket[v] the second argument) until the result is a true
value, and then returns @racket[#t]. If no such element is found, the
result is @racket[#f]. For a constant-time @racket[eql?], this
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
found, the result is @racket[#f]. For a constant-time
@racket[pred], this operation takes @math{O(N)} time.

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-find items string?)
(treelist-find items symbol?)
(treelist-find items number->string)
]}

@defproc[(treelist-index-of [tl treelist?]
                            [v any/c]
                            [eql? (any/c any/c . -> . any/c) equal?])
         (or/c exact-nonnegative-integer? #f)]{

Returns the index of the first element in @racket[tl] that is
@racket[eql?] to @racket[v].
If no such element is found, the result is @racket[#f].

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-index-of items 1)
(treelist-index-of items "a")
(treelist-index-of items 'apple)
(treelist-index-of items 'unicorn)
]

@history[#:added "8.15.0.6"]}

@defproc[(treelist-flatten [v any/c]) treelist?]{

Flattens a tree of nested treelists into a single treelist.

@examples[
#:eval the-eval
(treelist-flatten
 (treelist (treelist "a") "b" (treelist "c" (treelist "d") "e") (treelist)))
(treelist-flatten "a")
]

@history[#:added "8.15.0.6"]}

@defproc[(treelist-append* [tlotl (treelist/c treelist?)]) treelist?]{

Appends elements of a treelist of treelists together into one treelist,
leaving any further nested treelists alone.

@examples[
#:eval the-eval
(treelist-append*
 (treelist (treelist "a" "b") (treelist "c" (treelist "d") "e") (treelist)))
]

@history[#:added "8.15.0.6"]}

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

@defproc[(sequence->treelist [s sequence?]) treelist?]{

Returns a treelist whose elements are the elements of @racket[s],
each of which must be a single value.
If @racket[s] is infinite, this function does not terminate.

@examples[
#:eval the-eval
(sequence->treelist (list 1 "a" 'apple))
(sequence->treelist (vector 1 "a" 'apple))
(sequence->treelist (stream 1 "a" 'apple))
(sequence->treelist (open-input-bytes (bytes 1 2 3 4 5)))
(sequence->treelist (in-range 0 10))
]

@history[#:added "8.15.0.6"]}

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
                             [#:state state any/c]
                             [#:state-key state-key any/c (list 'fresh)]
                             [#:ref ref-proc (treelist? exact-nonnegative-integer? any/c any/c
                                              . -> . any/c)]
                             [#:set set-proc (treelist? exact-nonnegative-integer? any/c any/c
                                              . -> . (values any/c any/c))]
                             [#:insert insert-proc (treelist? exact-nonnegative-integer? any/c any/c
                                                    . -> . (values any/c any/c))]
                             [#:delete delete-proc (treelist? exact-nonnegative-integer? any/c
                                                    . -> . any/c)]
                             [#:take take-proc (treelist? exact-nonnegative-integer? any/c
                                                . -> . any/c)]
                             [#:drop drop-proc (treelist? exact-nonnegative-integer? any/c
                                                . -> . any/c)]
                             [#:append append-proc (treelist? treelist? any/c
                                                    . -> . (values treelist? any/c))]
                             [#:prepend prepend-proc (treelist? treelist? any/c
                                                      . -> . (values treelist? any/c))]
                             [#:append2 append2-proc (or/c #f (treelist? treelist? any/c any/c
                                                               . -> . (values treelist? any/c any/c))) #f]
                             [prop impersonator-property?]
                             [prop-val any/c] ... ...)
          (and/c treelist? chaperone?)]{

Analogous to @racket[chaperone-vector], returns a @tech{chaperone} of
@racket[tl], which redirects the @racket[treelist-ref],
@racket[treelist-set], @racket[treelist-insert],
@racket[treelist-append], @racket[treelist-delete],
@racket[treelist-take], and @racket[treelist-drop]
operations, as well as operations derived
from those. The @racket[state] argument is an initial state, where
a state value is passed to each procedure that redirects an operation,
and except for @racket[ref-proc] (which corresponds to the one
operation that does not update a treelist), a new state is returned to
be associated with the updated treelist. When @racket[state-key]
is provided, it can be used with @racket[treelist-chaperone-state]
to extract the state from the original treelist or an updated
treelist.

The @racket[ref-proc] procedure must accept @racket[tl], an index
passed to @racket[treelist-ref], the value that
@racket[treelist-ref] on @racket[tl] produces for the given index, and
the current chaperone state; it
must produce a chaperone replacement for the value, which is the
result of @racket[treelist-ref] on the chaperone.

The @racket[set-proc] procedure must accept @racket[tl], an index
passed to @racket[treelist-set], the value provided to
@racket[treelist-set], and the current chaperone state;
it must produce two values: a chaperone replacement for the
value, which is used in the result of @racket[treelist-set] on the
chaperone, and an updated state. The result of @racket[treelist-set] is chaperoned with the
same procedures and properties as @racket[tl], but with the updated state.

The @racket[insert-proc] procedure is like @racket[set-proc], but for
inserting via @racket[treelist-insert].

The @racket[delete-proc], @racket[take-proc], and @racket[drop-proc]
procedures must accept @racket[tl], the index or count for deleting,
taking or dropping, and the current chaperone state; they
must produce an updated state. The result of @racket[treelist-delete],
@racket[treelist-take], or @racket[treelist-drop] is chaperoned
with the same procedures and properties as @racket[tl], but with the
updated state.

The @racket[append-proc] procedure must accept @racket[tl], a treelist
to append onto @racket[tl], and the current chaperone state; it must
produce a chaperone replacement for the second treelist, which is
appended for the result of @racket[treelist-append] on the chaperone,
and an updated state. The result of @racket[treelist-append] is
chaperoned with the same procedures and properties as @racket[tl], but
with the updated state.

The @racket[prepend-proc] procedure must accept a treelist being
append with @racket[tl], @racket[tl], and the current chaperone
state; it must produce a chaperone replacement for the first
treelist, which is prepended for the result of @racket[treelist-append]
on the chaperone, and an updated state. The result of
@racket[treelist-append] is chaperoned with the same procedures and
properties as @racket[tl], but with the updated state.

The @racket[append2-proc] procedure is optional and similar to
@racket[append-proc], but when it is non-@racket[#f],
@racket[append2-proc] is used instead of @racket[append-proc] when a
second argument to @racket[treelist-append] is chaperoned with the
same @racket[state-key]. In that case, the second argument to
@racket[append2-proc] is the second argument with a @racket[state-key]
chaperone wrapper removed, and with that chaperone's state as the last
argument to @racket[append2-proc].

When two chaperoned treelists are given to @racket[treelist-append]
and @racket[append2-proc] is not used, then the @racket[append-proc]
of the first treelist is used, and the result of @racket[append-proc] will
still be a chaperone whose @racket[prepend-proc] is used. If the result
of @racket[prepend-proc] is a chaperone, then that chaperone's
@racket[append-proc] is used, and so on. If @racket[prepend-proc] and
@racket[append-proc] keep returning chaperones, it is possible that
no progress will be made.

@examples[
#:eval the-eval
(chaperone-treelist
 (treelist 1 "a" 'apple)
 #:state 'ignored-state
 #:ref (λ (tl _pos _v state)
         _v)
 #:set (λ (tl _pos _v state)
         (values _v state))
 #:insert (λ (tl _pos _v state)
            (values _v state))
 #:delete (λ (tl _pos state)
            state)
 #:take (λ (tl _pos state)
          state)
 #:drop (λ (tl _pos state)
          state)
 #:append2 (λ (tl _other state _other-state) (code:comment @#,elem{or @racket[#f]})
             (values _other state))
 #:append (λ (tl _other state)
            (values _other state))
 #:prepend (λ (_other tl state)
             (values _other state)))
 ]}

@defproc[(treelist-chaperone-state [tl treelist?]
                                   [state-key any/c]
                                   [fail-k (procedure-arity-includes/c 0) _key-error]) any/c]{

Extracts state associated with a treelist chaperone where
@racket[state-key] (compared using @racket[eq?])
was provided along with the initial state to
@racket[chaperone-treelist]. If @racket[tl] is not a chaperone with
state keyed by @racket[state-key], then @racket[fail-k] is called,
and the default @racket[fail-k] raises @racket[exn:fail:contract].

}



@section{Mutable Treelists}

@note-lib-only[racket/mutable-treelist]

A @deftech{mutable treelist} is like an immutable @tech{treelist} in a
box, where operations that change the mutable treelist replace the
treelist in the box. As a special case, @racket[mutable-treelist-set!]
on an unimpersonated mutable treelist modifies the treelist representation within the boxed value. This
model of a mutable treelist explains its behavior in the case of
concurrent modification: concurrent @racket[mutable-treelist-set!]
operations for different positions will not interefere, but races with
other operations or on impersonated mutable treelists will sometimes negate one of the modifications.
Concurrent modification is thus somewhat unpredictable but still safe,
and it is not managed by a lock.

A mutable treelist is not a treelist in the sense of
@racket[treelist?], which recognizes only immutable treelists.
Operations on a mutable treelist have the same time complexity as
corresponding operations on an immutable treelist unless otherwise
noted.

@history[#:added "8.12.0.7"]

@defproc[(mutable-treelist? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{mutable treelist},
@racket[#f] otherwise.}

@defproc[(mutable-treelist [v any/c] ...) mutable-treelist?]{

Returns a @tech{mutable treelist} with @racket[v]s as its elements in order.

@examples[
#:eval the-eval
(mutable-treelist 1 "a" 'apple)
]}

@defproc[(make-mutable-treelist [n exact-nonnegative-integer?] [v any/c #f]) mutable-treelist?]{

Creates a @tech{mutable treelist} that contains @racket[n] elements,
each initialized as @racket[v]. Creating the mutable treelist takes @math{O(N)}
time for @math{N} elements.

@examples[
#:eval the-eval
(make-mutable-treelist 3 "a")
]}


@deftogether[(
@defproc[(treelist-copy [tl treelist?]) mutable-treelist?]
@defproc[(mutable-treelist-copy [tl mutable-treelist?]) mutable-treelist?]
)]{

Creates a @tech{mutable treelist} that contains the same elements as
@racket[tl]. Creating the mutable treelist takes @math{O(N)} time for
@math{N} elements.

@examples[
#:eval the-eval
(treelist-copy (treelist 3 "a"))
(mutable-treelist-copy (mutable-treelist 3 "a"))
]}

@defproc[(mutable-treelist-snapshot [tl mutable-treelist?]
                                    [n exact-nonnegative-integer? 0]
                                    [m (or/c #f exact-nonnegative-integer?) #f])
         treelist?]{

Produces an immutable @tech{treelist} that has the same elements as
@racket[tl] at position @racket[n] (inclusive) through position
@racket[m] (exclusive). If @racket[m] is @racket[#f], then the length
of @racket[tl] is used, instead. Creating the immutable treelist takes
@math{O(N)} time for @math{N} elements of the resulting treelist, on
top of the cost of @racket[treelist-sublist] if the result is a
sublist.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(define snap (mutable-treelist-snapshot items))
snap
(mutable-treelist-snapshot items 1)
(mutable-treelist-snapshot items 1 2)
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

Returns the @racket[pos]th element of @racket[tl]. The first element is
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

@deftogether[(
@defproc[(mutable-treelist-append! [tl mutable-treelist?] [other-tl (or/c treelist? mutable-treelist?)]) void?]
@defproc[(mutable-treelist-prepend! [tl mutable-treelist?] [other-tl (or/c treelist? mutable-treelist?)]) void?]
)]{

Modifies @racket[tl] by appending or prepending all of the elements of
@racket[other-tl]. If @racket[other-tl] is a @tech{mutable treelist},
it is first converted to an immutable @tech{treelist} with
@racket[mutable-treelist-snapshot], which takes @math{O(N)} time
if @racket[other-tl] has @math{N} elements. If @racket[other-tl] is an
immutable treelist but chaperoned, then appending or prepending takes
@math{O(N)} time for @math{N} elements.

@examples[
#:eval the-eval
(define items (mutable-treelist 1 "a" 'apple))
(mutable-treelist-append! items (treelist 'more 'things))
items
(mutable-treelist-prepend! items (treelist 0 "b" 'banana))
items
(mutable-treelist-append! items items)
items
]

@history[#:changed "8.15.0.11" @elem{Added @racket[mutable-treelist-prepend!].}]}

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
of @racket[tl] and installing the result in place of the element.

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
@defform[(for/mutable-treelist maybe-length (for-clause ...) body-or-break ... body)]
@defform[(for*/mutable-treelist maybe-length (for-clause ...) body-or-break ... body)]
)]{

Like @racket[for/vector] and @racket[for*/vector], but generating
@tech{mutable treelists}.

@examples[
#:eval the-eval
(for/mutable-treelist ([i (in-range 10)]) i)
(for/mutable-treelist #:length 15 ([i (in-range 10)]) i)
(for/mutable-treelist #:length 15 #:fill 'a ([i (in-range 10)]) i)
]}

@defproc[(chaperone-mutable-treelist [tl mutable-treelist?]
                                     [#:ref ref-proc (mutable-treelist? exact-nonnegative-integer? any/c
                                                      . -> . any/c)]
                                     [#:set set-proc (mutable-treelist? exact-nonnegative-integer? any/c
                                                      . -> . any/c)]
                                     [#:insert insert-proc (mutable-treelist? exact-nonnegative-integer? any/c
                                                            . -> . any/c)]
                                     [#:append append-proc (mutable-treelist? treelist?
                                                            . -> . treelist?)]
                                     [#:prepend prepend-proc (treelist? mutable-treelist?
                                                              . -> . treelist?)
                                      (λ (o t) (append-proc t o))]
                                     [prop impersonator-property?]
                                     [prop-val any/c] ... ...)
          (and/c mutable-treelist? chaperone?)]{

Similar to @racket[chaperone-treelist], but for @tech{mutable treelists}.
For example, the given @racket[set-proc] is used for
@racket[mutable-treelist-set!], and the resulting value is installed
into the mutable treelist instead of the one provided to
@racket[set-proc]. Mutable treelist chaperones do not have state
separate from the treelist itself, and procedures like
@racket[set-proc] do not consume or return a state.}

@defproc[(impersonate-mutable-treelist [tl mutable-treelist?]
                                       [#:ref ref-proc (mutable-treelist? exact-nonnegative-integer? any/c
                                                        . -> . any/c)]
                                       [#:set set-proc (mutable-treelist? exact-nonnegative-integer? any/c
                                                        . -> . any/c)]
                                       [#:insert insert-proc (mutable-treelist? exact-nonnegative-integer? any/c
                                                              . -> . any/c)]
                                       [#:append append-proc (mutable-treelist? treelist?
                                                              . -> . treelist?)]
                                       [#:prepend prepend-proc (treelist? mutable-treelist?
                                                                . -> . treelist?)
                                        (λ (o t) (append-proc t o))]
                                       [prop impersonator-property?]
                                       [prop-val any/c] ... ...)
          (and/c mutable-treelist? impersonator?)]{

Like @racket[chaperone-mutable-treelist], but @racket[ref-proc],
@racket[set-proc], @racket[insert-proc], and @racket[append-proc]
are not obligated to produce chaperones.}


@(close-eval the-eval)
