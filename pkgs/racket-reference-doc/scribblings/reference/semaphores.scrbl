#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "semaphore"]{Semaphores}

A @deftech{semaphore} has an internal counter; when this counter is
zero, the semaphore can block a thread's execution (through
@racket[semaphore-wait]) until another thread increments the counter
(using @racket[semaphore-post]). The maximum value for a semaphore's
internal counter is platform-specific, but always at least
@racket[10000].

A semaphore's counter is updated in a single-threaded manner, so that
semaphores can be used for reliable synchronization. Semaphore waiting
is @defterm{fair}: if a thread is blocked on a semaphore and the
semaphore's internal value is non-zero infinitely often, then the
thread is eventually unblocked.

In addition to its use with semaphore-specific procedures, a semaphore
can be used as a @tech{synchronizable event} (see @secref["sync"]).
A semaphore is @tech{ready for synchronization} when
@racket[semaphore-wait] would not block; @resultItself{semaphore}.

@defproc[(semaphore? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{semaphore}, 
@racket[#f] otherwise.}


@defproc[(make-semaphore [init exact-nonnegative-integer? 0]) semaphore?]{

Creates and returns a new semaphore with the counter initially set to
@racket[init]. If @racket[init] is larger than a semaphore's maximum
internal counter value, the @exnraise[exn:fail].}


@defproc[(semaphore-post [sema semaphore?]) void?]{Increments the
semaphore's internal counter and returns @|void-const|. If the
semaphore's internal counter has already reached its maximum value,
the @exnraise[exn:fail].}

@defproc[(semaphore-wait [sema semaphore?]) void?]{Blocks until the
internal counter for semaphore @racket[sema] is non-zero. When the
counter is non-zero, it is decremented and @racket[semaphore-wait]
returns @|void-const|.}

@defproc[(semaphore-try-wait? [sema semaphore?]) boolean?]{Like
@racket[semaphore-wait], but @racket[semaphore-try-wait?] never blocks
execution.  If @racket[sema]'s internal counter is zero,
@racket[semaphore-try-wait?] returns @racket[#f] immediately without
decrementing the counter. If @racket[sema]'s counter is positive, it
is decremented and @racket[#t] is returned.}

@defproc[(semaphore-wait/enable-break [sema semaphore?]) void?]{Like
@racket[semaphore-wait], but breaking is enabled (see
@secref["breakhandler"]) while waiting on @racket[sema]. If
breaking is disabled when @racket[semaphore-wait/enable-break] is
called, then either the semaphore's counter is decremented or the
@racket[exn:break] exception is raised, but not both.}

@defproc[(semaphore-peek-evt [sema semaphore?]) semaphore-peek-evt?]{Creates and
returns a new @tech{synchronizable event} (for use with @racket[sync], for
example) that is @tech{ready for synchronization} when @racket[sema] is ready,
but synchronizing
the event does not decrement @racket[sema]'s internal count.
@ResultItself{semaphore-peek event}.}

@defproc[(semaphore-peek-evt? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a semaphore wrapper produced by
@racket[semaphore-peek-evt], @racket[#f] otherwise.}

@defproc[(call-with-semaphore [sema semaphore?]
                              [proc procedure?]
                              [try-fail-thunk (or/c (-> any) #f) #f]
                              [arg any/c] ...) any]{

Waits on @racket[sema] using @racket[semaphore-wait], calls
@racket[proc] with all @racket[arg]s, and then posts to
@racket[sema]. A @tech{continuation barrier} blocks full continuation jumps
into or out of @racket[proc] (see @secref["prompt-model"]), but
escape jumps are allowed, and @racket[sema] is posted on escape. If
@racket[try-fail-thunk] is provided and is not @racket[#f], then
@racket[semaphore-try-wait?] is called on @racket[sema] instead of
@racket[semaphore-wait], and @racket[try-fail-thunk] is called if the
wait fails.}

@defproc[(call-with-semaphore/enable-break [sema semaphore?]
                              [proc procedure?]
                              [try-fail-thunk (or/c (-> any) #f) #f]
                              [arg any/c] ...) any]{
Like @racket[call-with-semaphore], except that
@racket[semaphore-wait/enable-break] is used with @racket[sema] in
non-try mode. When @racket[try-fail-thunk] is provided and not
@racket[#f], then breaks are enabled around the use of
@racket[semaphore-try-wait?] on @racket[sema].}
