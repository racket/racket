#lang scribble/doc
@(require "mz.ss")

@title[#:tag "semaphore"]{Semaphores}

A @deftech{semaphore} has an internal counter; when this counter is
zero, the semaphore can block a thread's execution (through
@scheme[semaphore-wait]) until another thread increments the counter
(using @scheme[semaphore-post]). The maximum value for a semaphore's
internal counter is platform-specific, but always at least
@scheme[10000].

A semaphore's counter is updated in a single-threaded manner, so that
semaphores can be used for reliable synchronization. Semaphore waiting
is @defterm{fair}: if a thread is blocked on a semaphore and the
semaphore's internal value is non-zero infinitely often, then the
thread is eventually unblocked.

In addition to its use with semaphore-specific procedures, semaphores
can be used as events; see @secref["sync"].

@defproc[(make-semaphore [init nonnegative-exact-integer? 0]) semaphore?]{

Creates and returns a new semaphore with the counter initially set to
@scheme[init]. If @scheme[init-k] is larger than a semaphore's maximum
internal counter value, the @exnraise[exn:fail].}


@defproc[(semaphore? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a semaphore created by
@scheme[make-semaphore], @scheme[#f] otherwise.}


@defproc[(semaphore-post [sema semaphore?]) void?]{Increments the
semaphore's internal counter and returns @|void-const|. If the
semaphore's internal counter has already reached its maximum value,
the @exnraise[exn:fail].}

@defproc[(semaphore-wait [sema semaphore?]) void?]{Blocks until the
internal counter for semaphore @scheme[sema] is non-zero. When the
counter is non-zero, it is decremented and @scheme[semaphore-wait]
returns @|void-const|.}

@defproc[(semaphore-try-wait? [sema semaphore?]) boolean?]{Like
@scheme[semaphore-wait], but @scheme[semaphore-try-wait?] never blocks
execution.  If @scheme[sema]'s internal counter is zero,
@scheme[semaphore-try-wait?] returns @scheme[#f] immediately without
decrementing the counter. If @scheme[sema]'s counter is positive, it
is decremented and @scheme[#t] is returned.}

@defproc[(semaphore-wait/enable-break [sema semaphore?]) void?]{Like
@scheme[semaphore-wait], but breaking is enabled (see
@secref["breakhandler"]) while waiting on @scheme[sema]. If
breaking is disabled when @scheme[semaphore-wait/enable-break] is
called, then either the semaphore's counter is decremented or the
@scheme[exn:break] exception is raised, but not both.}

@defproc[(semaphore-peek-evt [sema semaphore?]) evt?]{Creates and
returns a new synchronizable event (for use with @scheme[sync], for
example) that is ready when @scheme[sema] is ready, but synchronizing
the event does not decrement @scheme[sema]'s internal count.}

@defproc[(call-with-semaphore [sema semaphore?]
                              [proc procedure?]
                              [try-fail-thunk (or/c (-> any) false/c) #f]
                              [arg any/c] ...) any]{

Waits on @scheme[sema] using @scheme[semaphore-wait], calls
@scheme[proc] with all @scheme[arg]s, and then posts to
@scheme[sema]. A @tech{continuation barrier} blocks full continuation jumps
into or out of @scheme[proc] (see @secref["prompt-model"]), but
escape jumps are allowed, and @scheme[sema] is posted on escape. If
@scheme[try-fail-thunk] is provided and is not @scheme[#f], then
@scheme[semaphore-try-wait?] is called on @scheme[sema] instead of
@scheme[semaphore-wait], and @scheme[try-fail-thunk] is called if the
wait fails.}

@defproc[(call-with-semaphore/enable-break [sema semaphore?]
                              [proc procedure?]
                              [try-fail-thunk (or/c (-> any) false/c) #f]
                              [arg any/c] ...) any]{
Like @scheme[call-with-semaphore], except that
@scheme[semaphore-wait/enable-break] is used with @scheme[sema] in
non-try mode. When @scheme[try-fail-thunk] is provided and not
@scheme[#f], then breaks are enabled around the use of
@scheme[semaphore-try-wait?] on @scheme[sema].}
