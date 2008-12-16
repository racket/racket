#lang scribble/doc
@(require "mz.ss")

@(define eventspaces @tech[#:doc '(lib "scribblings/gui/gui.scrbl")]{eventspaces})

@title[#:tag "custodians"]{Custodians}

See @secref["custodian-model"] for basic information on the PLT
Scheme custodian model.

@defproc[(custodian? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{custodian} value,
@scheme[#f] otherwise.}


@defproc[(make-custodian [cust custodian? (current-custodian)]) custodian?]{

Creates a new custodian that is subordinate to @scheme[cust]. When
@scheme[cust] is directed (via @scheme[custodian-shutdown-all]) to
shut down all of its managed values, the new subordinate custodian is
automatically directed to shut down its managed values as well.}


@defproc[(custodian-shutdown-all [cust custodian?]) void?]{

@margin-note{In MrEd, @|eventspaces| managed by @scheme[cust] are also
             shut down.}

Closes all @tech{file-stream ports}, @tech{TCP ports}, @tech{TCP
listeners}, and @tech{UDP sockets} that are managed by @scheme[cust]
(and its subordinates), and empties all @tech{custodian box}es
associated with @scheme[cust] (and its subordinates). It also removes
@scheme[cust] (and its subordinates) as managers of all threads; when
a thread has no managers, it is killed (or suspended; see
@scheme[thread/suspend-to-kill]) If the current thread is to be
killed, all other shut-down actions take place before killing the
thread.}


@defparam[current-custodian cust custodian?]{

@margin-note{In MrEd, custodians also manage @|eventspaces|.}

A parameter that determines a custodian that assumes responsibility
for newly created threads, @tech{file-stream ports}, TCP ports,
@tech{TCP listeners}, @tech{UDP sockets}, and @tech{byte converters}.}


@defproc[(custodian-managed-list [cust custodian?][super custodian?]) list?]{

Returns a list of immediately managed objects (not including
@tech{custodian box}es) and subordinate custodians for @scheme[cust],
where @scheme[cust] is itself subordinate to @scheme[super] (directly
or indirectly). If @scheme[cust] is not strictly subordinate to
@scheme[super], the @exnraise[exn:fail:contract].}

@defproc[(custodian-memory-accounting-available?) boolean?]{

@margin-note{Memory accounting is normally available in PLT Scheme 3m,
which is the main variant of PLT Scheme, and not normally available in
PLT Scheme CGC.}

Returns @scheme[#t] if PLT Scheme is compiled with support for
per-custodian memory accounting, @scheme[#f] otherwise.}

@defproc[(custodian-require-memory [limit-cust custodian?]
                                   [need-amt exact-nonnegative-integer?]
                                   [stop-cust custodian?]) void?]{

Registers a required-memory check if PLT Scheme is compiled with
support for per-custodian memory accounting, otherwise the
@exnraise[exn:fail:unsupported].

If a check is registered, and if PLT Scheme later reaches a state after
garbage collection (see @secref["gc-model"]) where allocating
@scheme[need-amt] bytes charged to @scheme[limit-cust] would fail or
trigger some shutdown, then @scheme[stop-cust] is shut down.}

@defproc[(custodian-limit-memory [limit-cust custodian?]
                                 [limit-amt exact-nonnegative-integer?]
                                 [stop-cust custodian? limit-cust]) void?]{

Registers a limited-memory check if PLT Scheme is compiled with
support for per-custodian memory accounting, otherwise the
@exnraise[exn:fail:unsupported].

If a check is registered, and if PLT Scheme later reaches a state
after garbage collection (see @secref["gc-model"]) where
@scheme[limit-cust] owns more than @scheme[limit-amt] bytes, then
@scheme[stop-cust] is shut down.

@margin-note{A custodian's limit is checked only after a garbage
             collection, except that it may also be checked during
             certain large allocations that are individually larger
             than the custodian's limit. A single garbage collection
             may shut down multiple custodians, even if shutting down
             only one of the custodians would have reduced memory use
             for other custodians.}

For reliable shutdown, @scheme[limit-amt] for
@scheme[custodian-limit-memory] must be much lower than the total
amount of memory available (minus the size of memory that is
potentially used and not charged to @scheme[limit-cust]). Moreover, if
individual allocations that are initially charged to
@scheme[limit-cust] can be arbitrarily large, then @scheme[stop-cust]
must be the same as @scheme[limit-cust], so that excessively large
immediate allocations can be rejected with an
@scheme[exn:fail:out-of-memory] exception.}

@defproc[(make-custodian-box [cust custodian?][v any/c]) custodian-box?]{

Returns a @tech{custodian box} that contains @scheme[v] as long as
@scheme[cust] has not been shut down.}

@defproc[(custodian-box? [v any/c]) boolean?]{Returns @scheme[#t] if
 @scheme[v] is a @tech{custodian box} produced by
 @scheme[make-custodian-box], @scheme[#f] otherwise.}

@defproc[(custodian-box-value [cb custodian-box?]) any]{Returns the
 value in the given @tech{custodian box}, or @scheme[#f] if the value
 has been removed.}
