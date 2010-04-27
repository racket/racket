#lang scribble/doc
@(require "mz.ss")

@title[#:tag "memory" #:style 'toc]{Memory Management}

@local-table-of-contents[]

@section[#:tag "weakbox"]{Weak Boxes}

A @deftech{weak box} is similar to a normal box (see
@secref["boxes"]), but when the garbage collector (see
@secref["gc-model"]) can prove that the content value of a weak box is
only reachable via weak references, the content of the weak box is
replaced with @racket[#f]. A @defterm{@tech{weak reference}} is a
reference through a weak box, through a key reference in a weak hash
table (see @secref["hashtables"]), through a value in an ephemeron
where the value can be replaced by @racket[#f] (see
@secref["ephemerons"]), or through a custodian (see
@secref["custodians"]).

@defproc[(make-weak-box [v any/c]) weak-box?]{

Returns a new weak box that initially contains @racket[v].}


@defproc[(weak-box-value [weak-box weak-box?]) any]{

Returns the value contained in @racket[weak-box]. If the garbage
collector has proven that the previous content value of
@racket[weak-box] was reachable only through a weak reference, then
@racket[#f] is returned.}

@defproc[(weak-box? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a weak box, @racket[#f] otherwise.}

@;------------------------------------------------------------------------
@section[#:tag "ephemerons"]{Ephemerons}

An @deftech{ephemeron} is similar to a weak box (see
@secref["weakbox"]), except that

@itemize[

 @item{an ephemeron contains a key and a value; the value can be
 extracted from the ephemeron, but the value is replaced
 by @racket[#f] when the automatic memory manager can prove that
 either the ephemeron or the key is reachable only through weak
 references (see @secref["weakbox"]); and}

 @item{nothing reachable from the value in an ephemeron counts toward
 the reachability of an ephemeron key (whether for the same ephemeron
 or another), unless the same value is reachable through a non-weak
 reference, or unless the value's ephemeron key is reachable through a
 non-weak reference (see @secref["weakbox"] for information on weak
 references).}

]

In particular, an ephemeron can be combined with a weak hash table
(see @secref["hashtables"]) to produce a mapping where the memory
manager can reclaim key--value pairs even when the value refers to the
key.


@defproc[(make-ephemeron [key any/c][v any/c]) ephemeron?]{

Returns a new @tech{ephemeron} whose key is @racket[key] and whose
value is initially @racket[v].}


@defproc[(ephemeron-value [ephemeron ephemeron?]) any]{

Returns the value contained in @racket[ephemeron]. If the garbage
collector has proven that the key for @racket[ephemeron] is only
weakly reachable, then the result is @racket[#f].}


@defproc[(ephemeron? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{ephemeron}, @racket[#f]
otherwise.}

@;------------------------------------------------------------------------
@section[#:tag "willexecutor"]{Wills and Executors}

A @deftech{will executor} manages a collection of values and
associated @deftech{will} procedures
(a.k.a. @deftech{finalizers}). The @tech{will} procedure for each
value is ready to be executed when the value has been proven (by the
garbage collector) to be unreachable, except through weak references
(see @secref["weakbox"]) or as the registrant for other will
executors. A @tech{will} is useful for triggering clean-up actions on
data associated with an unreachable value, such as closing a port
embedded in an object when the object is no longer used.

Calling the @racket[will-execute] or @racket[will-try-execute]
procedure executes a will that is ready in the specified will
executor. Wills are not executed automatically, because certain
programs need control to avoid race conditions. However, a program can
create a thread whose sole job is to execute wills for a particular
executor.

If a value is registered with multiple wills (in one or multiple
executors), the wills are readied in the reverse order of
registration. Since readying a will procedure makes the value
reachable again, the will must be executed and the value must be
proven again unreachable through only weak references before another
of the wills is readied or executed.  However, wills for distinct
unreachable values are readied at the same time, regardless of whether
the values are reachable from each other.

A will executor's register is held non-weakly until after the
corresponding will procedure is executed. Thus, if the content value
of a weak box (see @secref["weakbox"]) is registered with a will
executor, the weak box's content is not changed to @racket[#f] until
all wills have been executed for the value and the value has been
proven again reachable through only weak references.


@defproc[(make-will-executor) will-executor?]{

Returns a new will executor with no managed values.}


@defproc[(will-executor? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a will executor, @racket[#f]
otherwise.}


@defproc[(will-register [executor will-executor?][v any/c][proc (any/c . -> . any)])
         void?]{

Registers the value @racket[v] with the will procedure @racket[proc]
in the will executor @racket[executor]. When @racket[v] is proven
unreachable, then the procedure @racket[proc] is ready to be called
with @racket[v] as its argument via @racket[will-execute] or
@racket[will-try-execute].  The @racket[proc] argument is strongly
referenced until the will procedure is executed.}


@defproc[(will-execute [executor will-executor?]) any]{

Invokes the will procedure for a single ``unreachable'' value
registered with the executor @racket[executable]. The values returned
by the will procedure are the result of the @racket[will-execute]
call.  If no will is ready for immediate execution,
@racket[will-execute] blocks until one is ready.}


@defproc[(will-try-execute [executor any/c]) any]{

Like @racket[will-execute] if a will is ready for immediate
execution. Otherwise, @racket[#f] is returned.}

@;------------------------------------------------------------------------
@section[#:tag "garbagecollection"]{Garbage Collection}

@defproc[(collect-garbage) void?]{

Forces an immediate garbage collection. Some effectively unreachable
data may remain uncollected, because the collector cannot prove that
it is unreachable.

The @racket[collect-garbage] procedure provides some control over the
timing of collections, but garbage will obviously be collected even if
this procedure is never called.}

@defproc[(current-memory-use [cust custodian? #f]) exact-nonnegative-integer?]{

Returns an estimate of the number of bytes of memory occupied by
reachable data from @racket[cust]. (The estimate is calculated
@italic{without} performing an immediate garbage collection;
performing a collection generally decreases the number returned by
@racket[current-memory-use].) If @racket[cust] is not provided, the
estimate is a total reachable from any custodians.

When Racket is compiled without support for memory accounting, the
estimate is the same (i.e., all memory) for any individual custodian;
see also @racket[custodian-memory-accounting-available?].}

@defproc[(dump-memory-stats) any]{

Dumps information about memory usage to the (low-level) standard
output port.}

