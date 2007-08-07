#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:breakhandler"]{Breaks}

@section-index["threads" "breaking"]

A @deftech{break} is an asynchronous exception, usually triggered
through an external source controlled by the user, or through the
@scheme[break-thread] procedure. A break exception can only occur in a
thread while breaks are enabled. When a break is detected and enabled,
the @exnraise[exn:break] in the thread sometime afterward; if breaking
is disabled when @scheme[break-thread] is called, the break is
suspended until breaking is again enabled for the thread. While a
thread has a suspended break, additional breaks are ignored.

Breaks are enabled through the @scheme[break-enabled] parameter-like
procedure, and through the @scheme[parameterize-break] form, which is
analogous to @scheme[parameterize]. The @scheme[break-enabled]
procedure does not represent a parameter to be used with
@scheme[parameterize], because changing the break-enabled state of a
thread requires an explicit check for breaks, and this check is
incompatible with the tail evaluation of a @scheme[parameterize]
expression's body.

Certain procedures, such as @scheme[semaphore-wait/enable-break],
enable breaks temporarily while performing a blocking action. If
breaks are enabled for a thread, and if a break is triggered for the
thread but not yet delivered as an @scheme[exn:break] exception, then
the break is guaranteed to be delivered before breaks can be disabled
in the thread. The timing of @scheme[exn:break] exceptions is not
guaranteed in any other way.

Before calling a @scheme[with-handlers] predicate or handler, an
exception handler, an error display handler, an error escape handler,
an error value conversion handler, or a @scheme[pre-thunk] or
@scheme[post-thunk] for a @scheme[dynamic-wind], the call is
@scheme[parameterize-break]ed to disable breaks. Furthermore, breaks
are disabled during the transitions among handlers related to
exceptions, during the transitions between @scheme[pre-thunk]s and
@scheme[post-thunk]s for @scheme[dynamic-wind], and during other
transitions for a continuation jump. For example, if breaks are
disabled when a continuation is invoked, and if breaks are also
disabled in the target continuation, then breaks will remain disabled
until from the time of the invocation until the target continuation
executes unless a relevant @scheme[dynamic-wind] @scheme[pre-thunk] or
@scheme[post-thunk] explicitly enables breaks.

If a break is triggered for a thread that is blocked on a nested
thread (see @scheme[call-in-nested-thread]), and if breaks are enabled
in the blocked thread, the break is implicitly handled by transferring
it to the nested thread.

When breaks are enabled, they can occur at any point within execution,
which makes certain implementation tasks subtle. For example, assuming
breaks are enabled when the following code is executed,

@schemeblock[
(with-handlers ([exn:break? (lambda (x) (void))])
  (semaphore-wait s))
]

then it is @italic{not} the case that a @|void-const| result means the
semaphore was decremented or a break was received, exclusively. It is
possible that @italic{both} occur: the break may occur after the
semaphore is successfully decremented but before a @|void-const|
result is returned by @scheme[semaphore-wait]. A break exception will
never damage a semaphore, or any other built-in construct, but many
built-in procedures (including @scheme[semaphore-wait]) contain
internal sub-expressions that can be interrupted by a break.

In general, it is impossible using only @scheme[semaphore-wait] to
implement the guarantee that either the semaphore is decremented or an
exception is raised, but not both.  Scheme therefore supplies
@scheme[semaphore-wait/enable-break] (see @secref["mz:semaphore"]),
which does permit the implementation of such an exclusive guarantee:

@schemeblock[
(parameterize ([break-enabled #f])
  (with-handlers ([exn:break? (lambda (x) (void))])
    (semaphore-wait/enable-break s)))
]

In the above expression, a break can occur at any point until breaks
are disabled, in which case a break exception is propagated to the
enclosing exception handler. Otherwise, the break can only occur
within @scheme[semaphore-wait/enable-break], which guarantees that if
a break exception is raised, the semaphore will not have been
decremented.

To allow similar implementation patterns over blocking port
operations, MzScheme provides @scheme[read-bytes-avail!/enable-break],
@scheme[write-bytes-avail/enable-break], and other procedures.


@;------------------------------------------------------------------------

@defproc*[([(break-enabled) boolean?]
           [(break-enabled [on? any/c]) void?])]{

Gets or sets the break enabled state of the current thread. If
@scheme[on?] is not supplied, the result is @scheme[#t] if breaks are
currently enabled, @scheme[#f] otherwise.  If @scheme[on?] is supplied
as @scheme[#f], breaks are disabled, and if @scheme[on?] is a true
value, breaks are enabled.}

@defform[(parameterize-break boolean-expr body ...+)]{Evaluates
@scheme[boolean-expr] to determine whether breaks are initially
enabled in while evaluating the @scheme[body]s in sequence. The result
of the @scheme[parameter-break] expression is the result of the last
@scheme[expr].

Like @scheme[parameterize] (see @secref["mz:parameters"]), a fresh
@tech{thread cell} (see @secref["mz:threadcells"]) is allocated to
hold the break-enabled state of the continuation, and calls to
@scheme[break-enabled] within the continuation access or modify the
new cell. Unlike parameters, the break setting is not inherited by new
threads.}
 
@defproc[(current-break-parameterization) break-parameterization?]{
Analogous to @scheme[(current-parameterization)] (see
@secref["mz:parameters"]); it returns a break-parameterization
(effectively a thread cell) that holds the current continuation's
break-enable state.}

@defproc[(call-with-break-parameterization 
                [break-param break-parameterization?]
                [thunk (-> any)]) 
               any]{
Analogous to @scheme[(call-with-parameterization parameterization
thunk)] (see @secref["mz:parameters"]), calls @scheme[thunk] in a
continuation whose break-enabled state is in @scheme[break-param]. The
@scheme[thunk] is @italic{not} called in tail position with respect to
the @scheme[call-with-break-parameterization] call.}
