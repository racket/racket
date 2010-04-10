#lang scribble/doc
@(require "mz.ss"
          scribble/scheme
          (for-label scheme/mpair))

@title[#:tag "mpairs"]{Mutable Pairs and Lists}

A @deftech{mutable pair} is like a pair created by @scheme[cons], but
it supports @scheme[set-mcar!] and @scheme[set-mcdr!] mutation
operations to change the parts of the mutable pair (like traditional Lisp and
Scheme pairs).

A @deftech{mutable list} is analogous to a list created with pairs, but
instead created with @tech{mutable pairs}.

A @tech{mutable pair} is not a @tech{pair}; they are completely
separate datatypes. Similarly, a @tech{mutable list} is not a
@tech{list}, except that the empty list is also the empty mutable
list. Instead of programming with mutable pairs and mutable lists,
data structures such as pairs, lists, and hash tables are practically
always better choices.

@; ----------------------------------------
@section{Mutable Pair Constructors and Selectors}

@defproc[(mpair? [v any/c]) boolean?]{Returns @scheme[#t] if @scheme[v] is
a @tech{mutable pair}, @scheme[#f] otherwise.}

@defproc[(mcons [a any/c] [d any/c]) pair?]{Returns a newly allocated 
@tech{mutable pair} whose first
element is @scheme[a] and second element is @scheme[d].}

@defproc[(mcar [p mpair?]) any/c]{Returns the first element of the
@tech{mutable pair} @scheme[p].}

@defproc[(mcdr [p mpair?]) any/c]{Returns the second element of the
@tech{mutable pair} @scheme[p].}


@defproc[(set-mcar! [p mpair?] [v any/v]) 
         void?]{

Changes the @tech{mutable pair} @scheme[p] so that its first element is
@scheme[v].}

@defproc[(set-mcdr! [p mpair?] [v any/v]) 
         void?]{

Changes the @tech{mutable pair} @scheme[p] so that its second element is
@scheme[v].}

@; ----------------------------------------
@section{Mutable List Functions}

@note-lib-only[scheme/mpair]

For functions described in this section, contracts are not directly
enforced. In particular, when a @tech{mutable list} is expected,
supplying any other kind of value (or mutating a value that starts as
a @tech{mutable list}) tends to produce an exception from
@scheme[mcar] or @scheme[mcdr].

@defproc[(mlist? [v any/c]) boolean?]{Returns @scheme[#t] if
 @scheme[v] is a @tech{mutable list}: either the empty list, or a 
 @tech{mutable pair} whose second element is a @tech{mutable list}.}


@defproc[(mlist [v any/c] ...) mlist?]{Returns a newly allocated
@tech{mutable list} containing the @scheme[v]s as its elements.}


@defproc[(list->mlist [lst list?]) mlist?]{

Returns a newly allocated @tech{mutable list} with the same elements as
@scheme[lst].}


@defproc[(mlist->list [mlst mlist?]) list?]{

Returns a newly allocated @tech{mutable list} with the same elements as
@scheme[nlst].}


@defproc[(mlength [mlst mlist?])
         exact-nonnegative-integer?]{

Returns the number of elements in @scheme[mlst].}


@defproc[(mlist-ref [mlst mlist?][pos exact-nonnegative-integer?])
         any/c]{

Like @scheme[list-ref], but for @tech{mutable lists}.}


@defproc[(mlist-tail [mlst mlist?][pos exact-nonnegative-integer?])
         any/c]{

Like @scheme[list-tail], but for @tech{mutable lists}.}


@defproc*[([(mappend [mlst mlist?] ...) mlist?]
           [(mappend [mlst mlist?] ... [v any/c]) any/c])]{

Like @scheme[append], but for @tech{mutable lists}.}


@defproc*[([(mappend! [mlst mlist?] ...) mlist?]
           [(mappend! [mlst mlist?] ... [v any/c]) any/c])]{

The @scheme[mappend!] procedure appends the given @tech{mutable lists} by mutating
the tail of each to refer to the next, using @scheme[set-mcdr!]. Empty
lists are dropped; in particular, the result of calling
@scheme[mappend!] with one or more empty lists is the same as the
result of the call with the empty lists removed from the set of
arguments.}


@defproc[(mreverse [mlst mlist?]) mlist?]{

Like @scheme[reverse], but for @tech{mutable lists}.}


@defproc[(mreverse! [mlst mlist?]) mlist?]{

Like @scheme[mreverse], but destructively reverses the 
@tech{mutable list} by using
all of the mutable pairs in @scheme[mlst] and changing them with
@scheme[set-mcdr!].}


@defproc[(mmap [proc procedure?] [mlst mlist?] ...+) 
         mlist?]{

Like @scheme[map], but for @tech{mutable lists}.}


@defproc[(mfor-each [proc procedure?] [mlst mlist?] ...+)
         void?]{

Like @scheme[for-each], but for @tech{mutable lists}.}


@defproc[(mmember [v any/c] [mlst mlist?])
         (or/c mlist? #f)]{

Like @scheme[member], but for @tech{mutable lists}.}


@defproc[(mmemv [v any/c] [mlst mlist?])
         (or/c mlist? #f)]{

Like @scheme[memv], but for @tech{mutable lists}.}


@defproc[(mmemq [v any/c] [mlst mlist?])
         (or/c list? #f)]{

Like @scheme[memq], but for @tech{mutable lists}.}


@defproc[(massoc [v any/c] [mlst (mlistof mpair?)])
         (or/c mpair? #f)]{

Like @scheme[assoc], but for @tech{mutable lists} of @tech{mutable pairs}.}


@defproc[(massv [v any/c] [mlst (mlistof mpair?)])
         (or/c mpair? #f)]{

Like @scheme[assv], but for @tech{mutable lists} of @tech{mutable pairs}.}


@defproc[(massq [v any/c] [mlst (mlistof mpair?)])
         (or/c mpair? #f)]{

Like @scheme[assq], but for @tech{mutable lists} of @tech{mutable pairs}.}


@defproc[(mlistof [pred (any/c . -> . any/c)])
         (any/c . -> . boolean?)]{

Returns a procedure that returns @scheme[#t] when given a @tech{mutable list}
for which @scheme[pred] returns a true value for all elements.}
