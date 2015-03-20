#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "breakhandler"]{Breaks}

@section-index["threads" "breaking"]

A @deftech{break} is an asynchronous exception, usually triggered
through an external source controlled by the user, or through the
@racket[break-thread] procedure. For example, the user may type Ctl-C
in a terminal to trigger a break. On some platforms, the Racket
process may receive @as-index{@tt{SIGINT}}, @as-index{@tt{SIGHUP}},
or @as-index{@tt{SIGTERM}}; the latter two correspond to hang-up and
terminate breaks as reflected by @racket[exn:break:hang-up] and
@racket[exn:break:terminate], respectively. Multiple breaks may be
collapsed into a single exception, and multiple breaks of different
kinds may be collapsed to a single ``strongest'' break, where a
hang-up break is stronger than an interrupt break, and a terminate
break is stronger than a hang-up break.

A break exception can only occur in a
thread while breaks are enabled. When a break is detected and enabled,
the @racket[exn:break] (or @racket[exn:break:hang-up] or 
@racket[exn:break:terminate]) exception is raised
in the thread sometime afterward; if breaking
is disabled when @racket[break-thread] is called, the break is
suspended until breaking is again enabled for the thread. While a
thread has a suspended break, additional breaks are ignored.

Breaks are enabled through the @racket[break-enabled] parameter-like
procedure and through the @racket[parameterize-break] form, which is
analogous to @racket[parameterize]. The @racket[break-enabled]
procedure does not represent a parameter to be used with
@racket[parameterize], because changing the break-enabled state of a
thread requires an explicit check for breaks, and this check is
incompatible with the tail evaluation of a @racket[parameterize]
expression's body.

Certain procedures, such as @racket[semaphore-wait/enable-break],
enable breaks temporarily while performing a blocking action. If
breaks are enabled for a thread, and if a break is triggered for the
thread but not yet delivered as an @racket[exn:break] exception, then
the break is guaranteed to be delivered before breaks can be disabled
in the thread. The timing of @racket[exn:break] exceptions is not
guaranteed in any other way.

Before calling a @racket[with-handlers] predicate or handler, an
exception handler, an error display handler, an error escape handler,
an error value conversion handler, or a @racket[pre-thunk] or
@racket[post-thunk] for a @racket[dynamic-wind], the call is
@racket[parameterize-break]ed to disable breaks. Furthermore, breaks
are disabled during the transitions among handlers related to
exceptions, during the transitions between @racket[pre-thunk]s and
@racket[post-thunk]s for @racket[dynamic-wind], and during other
transitions for a continuation jump. For example, if breaks are
disabled when a continuation is invoked, and if breaks are also
disabled in the target continuation, then breaks will remain disabled
from the time of the invocation until the target continuation
executes unless a relevant @racket[dynamic-wind] @racket[pre-thunk] or
@racket[post-thunk] explicitly enables breaks.

If a break is triggered for a thread that is blocked on a nested
thread (see @racket[call-in-nested-thread]), and if breaks are enabled
in the blocked thread, the break is implicitly handled by transferring
it to the nested thread.

When breaks are enabled, they can occur at any point within execution,
which makes certain implementation tasks subtle. For example, assuming
breaks are enabled when the following code is executed,

@racketblock[
(with-handlers ([exn:break? (lambda (x) (void))])
  (semaphore-wait s))
]

then it is @italic{not} the case that a @|void-const| result means the
semaphore was decremented or a break was received, exclusively. It is
possible that @italic{both} occur: the break may occur after the
semaphore is successfully decremented but before a @|void-const|
result is returned by @racket[semaphore-wait]. A break exception will
never damage a semaphore, or any other built-in construct, but many
built-in procedures (including @racket[semaphore-wait]) contain
internal sub-expressions that can be interrupted by a break.

In general, it is impossible using only @racket[semaphore-wait] to
implement the guarantee that either the semaphore is decremented or an
exception is raised, but not both.  Racket therefore supplies
@racket[semaphore-wait/enable-break] (see @secref["semaphore"]),
which does permit the implementation of such an exclusive guarantee:

@racketblock[
(parameterize-break #f
  (with-handlers ([exn:break? (lambda (x) (void))])
    (semaphore-wait/enable-break s)))
]

In the above expression, a break can occur at any point until breaks
are disabled, in which case a break exception is propagated to the
enclosing exception handler. Otherwise, the break can only occur
within @racket[semaphore-wait/enable-break], which guarantees that if
a break exception is raised, the semaphore will not have been
decremented.

To allow similar implementation patterns over blocking port
operations, Racket provides @racket[read-bytes-avail!/enable-break],
@racket[write-bytes-avail/enable-break], and other procedures.


@;------------------------------------------------------------------------

@defproc*[([(break-enabled) boolean?]
           [(break-enabled [on? any/c]) void?])]{

Gets or sets the break enabled state of the current thread. If
@racket[on?] is not supplied, the result is @racket[#t] if breaks are
currently enabled, @racket[#f] otherwise.  If @racket[on?] is supplied
as @racket[#f], breaks are disabled, and if @racket[on?] is a true
value, breaks are enabled.}

@defform[(parameterize-break boolean-expr body ...+)]{Evaluates
@racket[boolean-expr] to determine whether breaks are initially
enabled while evaluating the @racket[body]s in sequence. The result
of the @racket[parameterize-break] expression is the result of the last
@racket[expr].

Like @racket[parameterize] (see @secref["parameters"]), a fresh
@tech{thread cell} (see @secref["threadcells"]) is allocated to
hold the break-enabled state of the continuation, and calls to
@racket[break-enabled] within the continuation access or modify the
new cell. Unlike parameters, the break setting is not inherited by new
threads.}
 
@defproc[(current-break-parameterization) break-parameterization?]{
Analogous to @racket[(current-parameterization)] (see
@secref["parameters"]); it returns a break parameterization
(effectively, a thread cell) that holds the current continuation's
break-enable state.}

@defproc[(call-with-break-parameterization 
                [break-param break-parameterization?]
                [thunk (-> any)]) 
               any]{
Analogous to @racket[(call-with-parameterization parameterization
thunk)] (see @secref["parameters"]), calls @racket[thunk] in a
continuation whose break-enabled state is in @racket[break-param]. The
@racket[thunk] is @italic{not} called in tail position with respect to
the @racket[call-with-break-parameterization] call.}

@defproc[(break-parameterization? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a break parameterization as produced by
@racket[current-break-parameterization], @racket[#f] otherwise.

@history[#:added "6.1.1.8"]}
