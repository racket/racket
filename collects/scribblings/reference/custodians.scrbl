#reader(lib "docreader.ss" "scribble")
@require[(lib "bnf.ss" "scribble")]
@require["mz.ss"]

@title[#:tag "mz:custodians"]{Custodians}

See @secref["mz:custodian-model"] for basic information on the PLT
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

Closes all open ports and closes all active TCP listeners and UDP
sockets that are managed by @scheme[cust]. It also removes
@scheme[cust] (and its subordinates) as managers of all threads; when
a thread has no managers, it is killed (or suspended; see
@scheme[thread/suspend-to-kill]) If the current thread is to be
killed, all other shut-down actions take place before killing the
thread.}


@defparam[current-custodian cust custodian?]{

A parameter that determines a custodian that assumes responsibility
for newly created threads, ports, TCP listeners, UDP sockets, and
byte converters.}


@defproc[(custodian-managed-list [cust custodian?][super custodian?]) list?]{

Returns a list of immediately managed objects and subordinate
custodians for @scheme[cust], where @scheme[cust] is itself
subordinate to @scheme[super] (directly or indirectly). If
@scheme[cust] is not strictly subordinate to @scheme[super], the
@exnraise[exn:fail:contract].}

@defproc[(custodian-memory-accounting-available?) boolean?]{

Returns @scheme[#t] if MzScheme is compiled with support for
per-custodian memory accounting, @scheme[#f] otherwise.

@margin-note{Memory accounting is normally available in PLT Scheme 3m,
which is the main variant of PLT Scheme, and not normally available in
PLT Scheme CGC.}}

@defproc[(custodian-require-memory [limit-cust custodian?]
                                   [need-amt nonnegative-exact-integer?]
                                   [stop-cust custodian?]) void?]{

Registers a require check if PLT Scheme is compiled with support for
per-custodian memory accounting, otherwise the
@exnraise[exn:fail:unsupported].

If a check is registered, and if PLT Scheme later reaches a state after
garbage collection (see @secref["mz:gc-model"]) where allocating
@scheme[need-amt] bytes charged to @scheme[limit-cust] would fail or
tigger some shutdown, then @scheme[stop-cust] is shut down.}

@defproc[(custodian-limit-memory [limit-cust custodian?]
                                 [limit-amt nonnegative-exact-integer?]
                                 [stop-cust custodian? limit-cust]) void?]{

Registers a limit check if PLT Scheme is compiled with support for
per-custodian memory accounting, otherwise the
@exnraise[exn:fail:unsupported].

If a check is registered, and if PLT Scheme later reaches a state
after garbage collection (see @secref["mz:gc-model"]) where
@scheme[limit-cust] owns more than @scheme[limit-amt] bytes, then
@scheme[stop-cust] is shut down.

For reliable shutdown, @scheme[limit-amt] for
@scheme[custodian-limit-memory] must be much lower than the total
amount of memory available (minus the size of memory that is
potentially used and not charged to @scheme[limit-cust]). Moreover, if
indvidual allocations that are initially charged to
@scheme[limit-cust] can be arbitrarily large, then @scheme[stop-cust]
must be the same as @scheme[limit-cust], so that excessively large
immediate allocations can be rejected with an
@scheme[exn:fail:out-of-memory] exception.}

@defproc[(make-custodian-box [cust custodian?][v any/c]) custodian-box?]{

Returns a @deftech{custodian box} that contains @scheme[v] as long as
@scheme[cust] has not been shut down.}

@defproc[(custodian-box? [v any/c]) boolean?]{Returns @scheme[#t] if
 @scheme[v] is a @tech{custodian box} produced by
 @scheme[make-custodian-box], @scheme[#f] otherwise.}

@defproc[(custodian-box-value [cb custodian-box?]) any]{Rturns the
 value in the given @tech{custodian box}, or @scheme[#f] if the value
 has been removed.}
