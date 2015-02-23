#lang scribble/manual
@(require "mz.rkt"
          scribble/bnf)

@title[#:tag "memory" #:style 'toc]{Memory Management}

@local-table-of-contents[]

@section[#:tag "weakbox"]{Weak Boxes}

A @deftech{weak box} is similar to a normal box (see
@secref["boxes"]), but when the garbage collector (see
@secref["gc-model"]) can prove that the content value of a weak box is
only reachable via weak references, the content of the weak box is
replaced with @racket[#f]. A @defterm{@tech{weak reference}} is a
reference through a weak box, through a key reference in a weak hash
table (see @secref["hashtables"]), through a value in an @tech{ephemeron}
where the value can be replaced by @racket[#f] (see
@secref["ephemerons"]), or through a custodian (see
@secref["custodians"]).

@defproc[(make-weak-box [v any/c]) weak-box?]{

Returns a new weak box that initially contains @racket[v].}


@defproc[(weak-box-value [weak-box weak-box?] [gced-v any/c #f]) any/c]{

Returns the value contained in @racket[weak-box]. If the garbage
collector has proven that the previous content value of
@racket[weak-box] was reachable only through a weak reference, then
@racket[gced-v] (which defaults to @racket[#f]) is returned.}

@defproc[(weak-box? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a weak box, @racket[#f] otherwise.}

@;------------------------------------------------------------------------
@section[#:tag "ephemerons"]{Ephemerons}

An @deftech{ephemeron} @cite{Hayes97} is a generalization of a
@tech{weak box} (see @secref["weakbox"]). Instead of just containing
one value, an ephemeron holds two values: one that is considered the
value of the ephemeron and another that is the ephemeron's key. Like
the value in a weak box, the value in an ephemeron may be replaced by
@racket[#f], but when the @emph{key} is no longer reachable (except
possibly via weak references) instead of when the value is no longer
reachable.

As long as an ephemeron's value is retained, the reference is
considered a non-weak reference. References to the key via the value
are treated specially, however, in that the reference does not
necessarily count toward the key's reachability. A @tech{weak box} can
be seen as a specialization of an ephemeron where the key and value
are the same.

One particularly common use of ephemerons is to combine them with a
weak hash table (see @secref["hashtables"]) to produce a mapping where
the memory manager can reclaim key--value pairs even when the value
refers to the key. A related use is to retain a reference to a value
as long as any value for which it is an @tech{impersonator} is
reachable; see @racket[impersonator-ephemeron].

More precisely,
@itemize[

 @item{the value in an ephemeron is replaced
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


@defproc[(make-ephemeron [key any/c] [v any/c]) ephemeron?]{

Returns a new @tech{ephemeron} whose key is @racket[key] and whose
value is initially @racket[v].}


@defproc[(ephemeron-value [ephemeron ephemeron?] [gced-v any/c #f]) any/c]{

Returns the value contained in @racket[ephemeron]. If the garbage
collector has proven that the key for @racket[ephemeron] is only
weakly reachable, then the result is @racket[gced-v] (which defaults to @racket[#f]).}


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
executor. A will executor is also a @tech{synchronizable event}, so @racket[sync]
or @racket[sync/timeout] can be used to detect when a will executor has
ready wills. Wills are not executed automatically, because certain
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

A will executor can be used as a @tech{synchronizable event} (see @secref["sync"]).
A will executor is @tech{ready for synchronization} when
@racket[will-execute] would not block; @resultItself{will executor}.


These examples show how to run cleanup actions when
no synchronization is necessary. It simply runs the registered
executors as they become ready in another thread.
@mz-examples[(define an-executor (make-will-executor))
             (eval:alts (void
                         (thread
                          (λ ()
                            (let loop ()
                              (will-execute an-executor)
                              (loop)))))
                        (void))
             (define (executor-proc v) (printf "a-box is now garbage\n"))
             (define a-box-to-track (box #f))
             (will-register an-executor a-box-to-track executor-proc)
             (eval:alts (collect-garbage) (void))
             (set! a-box-to-track #f)
             (eval:alts (collect-garbage) (executor-proc 'random-junk))]


@defproc[(make-will-executor) will-executor?]{

Returns a new will executor with no managed values.}


@defproc[(will-executor? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a will executor, @racket[#f]
otherwise.}


@defproc[(will-register [executor will-executor?] [v any/c] [proc (any/c . -> . any)])
         void?]{

Registers the value @racket[v] with the will procedure @racket[proc]
in the will executor @racket[executor]. When @racket[v] is proven
unreachable, then the procedure @racket[proc] is ready to be called
with @racket[v] as its argument via @racket[will-execute] or
@racket[will-try-execute].  The @racket[proc] argument is strongly
referenced until the will procedure is executed.}


@defproc[(will-execute [executor will-executor?]) any]{

Invokes the will procedure for a single ``unreachable'' value
registered with the executor @racket[executor]. The values returned
by the will procedure are the result of the @racket[will-execute]
call.  If no will is ready for immediate execution,
@racket[will-execute] blocks until one is ready.}


@defproc[(will-try-execute [executor any/c]) any]{

Like @racket[will-execute] if a will is ready for immediate
execution. Otherwise, @racket[#f] is returned.}

@;------------------------------------------------------------------------
@section[#:tag "garbagecollection"]{Garbage Collection}

Set the @as-index{@envvar{PLTDISABLEGC}} environment variable (to any
value) before Racket starts to disable @tech{garbage collection}.

In Racket 3m (the main variant of Racket), each garbage collection
logs a message (see @secref["logging"]) at the @racket['debug] level with topic @racket['GC].
The data portion of the message is an instance of a @indexed-racket[gc-info]
@tech{prefab} structure type with 10 fields as follows, but future
versions of Racket may use a @racket[gc-info] @tech{prefab} structure
with additional fields:

@racketblock[
(struct gc-info (major? pre-amount pre-admin-amount code-amount
                        post-amount post-admin-amount
                        start-process-time end-process-time
                        start-time end-time)
  #:prefab)
]

@itemlist[

 @item{The @racket[major?] field indicates whether the collection was
       a ``major'' collection that inspects all memory or a ``minor''
       collection that mostly inspects just recent allocations.}

 @item{The @racket[pre-amount] field reports place-local memory use
      (i.e., not counting the memory use of child places) in bytes at
      the time that the @tech{garbage collection} started.}

 @item{The @racket[pre-admin-amount] is a larger number that includes
       memory use for the garbage collector's overhead, such as space
       on memory pages that are mapped but not currently used.}

 @item{The @racket[code-amount] field reports additional memory use
       for generated native code (which is the same just before and
       after a garbage collection, since it is released via
       finalization).}

 @item{The @racket[post-amount] and @racket[post-admin-amount] fields
       correspond to @racket[pre-amount] and
       @racket[pre-admin-amount], but after garbage collection. In
       typical configurations, the difference between
       @racket[post-amount] and @racket[pre-amount] contributes to
       @racket[post-admin-amount], since reclaimed pages tend to stay
       in reserve with the expectation that they'll be needed again
       (but the pages are released if multiple collections pass
       without need for the pages).}

 @item{The @racket[start-process-time] and @racket[end-process-time]
       fields report processor time (in the sense of
       @racket[current-process-milliseconds]) at the start and end of
       garbage collection. The difference between the times is the
       processor time consumed by collection.}

 @item{The @racket[start-time] and @racket[end-time] fields report
       real time (in the sense of
       @racket[current-inexact-milliseconds]) at the start and end of
       garbage collection. The difference between the times is the
       real time consumed by garbage collection.}

]

The format of the logged message's text is subject to change.
Currently, after a prefix that indicates the @tech{place} and
collection mode, the text has the format

@nested[#:style 'inset]{
 @tt{@nonterm{used}(@nonterm{admin})[@nonterm{code}]; @;
     free @nonterm{reclaimed}(@nonterm{adjust}) @nonterm{elapsed} @"@" @nonterm{timestamp}}

@tabular[
  #:sep (hspace 1)
  (list (list @nonterm{used}
              @elem{Collectable memory in use just prior to garbage collection})
        (list @nonterm{admin}
              @elem{Additional memory used as to manage collectable memory})
        (list @nonterm{code}
              @elem{Additional memory used for generated machine code})
        (list @nonterm{reclaimed}
              @elem{Collectable memory reclaimed by garbage collection})
        (list @nonterm{adjust}
              @elem{Negation of change to administrative memory minus @nonterm{reclaimed}})
        (list @nonterm{elapsed}
              @elem{Processor time used to perform garbage collection})
        (list @nonterm{timestamp}
              @elem{Processor time since startup of garbage collection's start}))
]}


@defproc[(collect-garbage) void?]{

Forces an immediate @tech{garbage collection} (unless garbage
collection is disabled by setting @envvar{PLTDISABLEGC}). Some
effectively unreachable data may remain uncollected, because the
collector cannot prove that it is unreachable.

The @racket[collect-garbage] procedure provides some control over the
timing of collections, but garbage will obviously be collected even if
this procedure is never called (unless garbage collection is disabled).}

@defproc[(current-memory-use [cust custodian? #f]) exact-nonnegative-integer?]{

Returns an estimate of the number of bytes of memory occupied by
reachable data from @racket[cust].  This estimate is calculated by the
last garbage collection, and can be 0 if none occurred (or if none occurred
since the given custodian was created).  The @racket[current-memory-use]
function does @italic{not} perform a collection by itself; doing one
before the call will generally decrease the result (or increase it from
0 if no collections happened yet).

If @racket[cust] is not provided, the estimate is a total reachable from
any custodians.

When Racket is compiled without support for memory accounting, the
estimate is the same (i.e., all memory) for any individual custodian;
see also @racket[custodian-memory-accounting-available?].

See also @racket[vector-set-performance-stats!].}

@defproc[(dump-memory-stats [v any/c] ...) any]{

Dumps information about memory usage to the low-level error port
 or console.

Various combinations of @racket[v] arguments can control the
information in a dump. The information that is available depends on
your Racket build; check the end of a dump from a particular build to
see if it offers additional information; otherwise, all @racket[v]s are
ignored.}

@;------------------------------------------------------------------------
@section[#:tag "phantom-bytes"]{Phantom Byte Strings}

A @deftech{phantom byte string} is a small Racket value that is
treated by the Racket memory manager as having an arbitrary size,
which is specified when the @tech{phantom byte string} is created or
when it is changed via @racket[set-phantom-bytes!].

A @tech{phantom byte string} acts as a hint to Racket's memory
manager that memory is allocated within the process but through a
separate allocator, such as through a foreign library that is accessed
via @racketmodname[ffi/unsafe]. This hint is used to trigger
@tech{garbage collections} or to compute the result of
@racket[current-memory-use]. Currently, the hint is used only in
Racket 3m (the main variant of Racket).

@defproc[(phantom-bytes? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{phantom byte string},
@racket[#f] otherwise.}


@defproc[(make-phantom-bytes [k exact-nonnegative-integer?])
         phantom-bytes?]{

Creates a @tech{phantom byte string} that is treated by the Racket
memory manager as being @racket[k] bytes in size. For a large enough
@racket[k], the @exnraise[exn:fail:out-of-memory]---either because the
size is implausibly large, or because a memory limit has been
installed with @racket[custodian-limit-memory].}


@defproc[(set-phantom-bytes! [phantom-bstr phantom-bytes?]
                             [k exact-nonnegative-integer?])
         phantom-bytes?]{

Adjusts the size of a @tech{phantom byte string} as it is treated by
the Racket memory manager.

For example, if the memory that @racket[phantom-bstr] represents is
released through a foreign library, then @racket[(set-phantom-bytes!
phantom-bstr 0)] can reflect the change in memory use.

When @racket[k] is larger than the current size of
@racket[phantom-bstr], then this function can raise
@racket[exn:fail:out-of-memory], like @racket[make-phantom-bytes].}
