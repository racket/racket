#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "threads"]{Threads}

@guideintro["concurrency"]{threads}

See @secref["thread-model"] for basic information on the Racket
thread model. See also @secref["futures"] and @secref["places"].

When a thread is created, it is placed into the management of the
@tech{current custodian} and added to the current @tech{thread
group}. A thread can have any number of custodian managers added
through @racket[thread-resume].
The allocation made by a thread is accounted to the thread's custodian managers.
See @racket[custodian-limit-memory] for examples.

A thread that has not terminated can be garbage collected (see
@secref["gc-model"]) if it is unreachable and suspended or if it is
unreachable and blocked on only unreachable events through functions
such as @racket[semaphore-wait], @racket[semaphore-wait/enable-break],
@racket[channel-put], @racket[channel-get], @racket[sync],
@racket[sync/enable-break], or @racket[thread-wait]. Beware, however,
of a limitation on @tech{place-channel} blocking; see the
@elemref['(caveat "place-channel-gc")]{caveat} in @secref["places"].

@margin-note{In GRacket, a handler thread for an eventspace is blocked on
an internal semaphore when its event queue is empty. Thus, the handler
thread is collectible when the eventspace is unreachable and contains
no visible windows or running timers.}

A thread can be used as a @tech{synchronizable event} (see
@secref["sync"]).  A thread is @tech{ready for synchronization} when
@racket[thread-wait] would not block; @resultItself{thread}.

@;------------------------------------------------------------------------
@section{Creating Threads}

@defproc[(thread [thunk (-> any)]
                 [#:pool pool (or/c #f 'own parallel-thread-pool?) #f]
                 [#:keep keep (or/c #f 'results) #f])
         thread?]{

Calls @racket[thunk] with no arguments in a new thread of control. The
@racket[thread] procedure returns immediately with a @deftech{thread
descriptor} value. When the invocation of @racket[thunk] returns, the
thread created to invoke @racket[thunk] terminates.

The resulting thread is a @tech{coroutine thread} if @racket[pool] is
@racket[#f]. If @racket[pool] is @racket['own], then a new
@tech{parallel thread pool} is created, the thread is added to the
pool, and the pool is closed (in the sense of
@racket[parallel-thread-pool-close]) to additional threads. If
@racket[pool] is a parallel thread pool, then the new thread is
created in that pool, meaning that it shares processor resources with
other threads in the same pool.
@;
@margin-note*{See @guidesecref["parallel-threads"] for information about parallel threads and
performance. Parallel threads do not run in parallel on the @tech{BC}
variant of Racket or when Racket is built without support for
parallelism. For more information, see the description of
@tech{parallel thread pools}.}

If @racket[keep] is @racket['results], results are recorded with the
thread so that they can be reported by @racket[thread-wait].
Otherwise, then the results of @racket[thunk] are ignored.

@history[#:changed "8.18.0.2" @elem{Added the @racket[#:pool] and
                                    @racket[#:keep] arguments.}]}

@defproc[(thread? [v any/c]) thread?]{Returns @racket[#t] if
@racket[v] is a @tech{thread descriptor}, @racket[#f] otherwise.}

@defproc[(current-thread) thread?]{Returns the @tech{thread
 descriptor} for the currently executing thread.}

@defproc[(thread/suspend-to-kill [thunk (-> any)]) thread?]{

Like @racket[thread], except that ``killing'' the thread through
@racket[kill-thread] or @racket[custodian-shutdown-all] merely
suspends the thread instead of terminating it.  }

@defproc[(call-in-nested-thread [thunk (-> any)]
                                [cust custodian? (current-custodian)]) 
          any]{

Creates a nested thread managed by @racket[cust] to execute
@racket[thunk]. (The nested thread's current custodian is inherited
from the creating thread, independent of the @racket[cust] argument.)
The current thread blocks until @racket[thunk] returns, and the result
of the @racket[call-in-nested-thread] call is the result returned by
@racket[thunk].

The nested thread's exception handler is initialized to a procedure
that jumps to the beginning of the thread and transfers the exception
to the original thread. The handler thus terminates the nested thread
and re-raises the exception in the original thread.

If the thread created by @racket[call-in-nested-thread] dies before
@racket[thunk] returns, the @exnraise[exn:fail] in the original
thread. If the original thread is killed before @racket[thunk]
returns, a break is queued for the nested thread.

If a break is queued for the original thread (with
@racket[break-thread]) while the nested thread is running, the break
is redirected to the nested thread. If a break is already queued on
the original thread when the nested thread is created, the break is
moved to the nested thread. If a break remains queued on the nested
thread when it completes, the break is moved to the original thread.

If the thread created by @racket[call-in-nested-thread] dies while
itself in a call to @racket[call-in-nested-thread], the outer call to
@racket[call-in-nested-thread] waits for the innermost nested thread
to complete, and any breaks pending on the inner threads are moved to
the original thread.}

@;------------------------------------------------------------------------
@section[#:tag "threadkill"]{Suspending, Resuming, and Killing Threads}

@defproc[(thread-suspend [thd  thread?]) void?]{

Immediately suspends the execution of @racket[thd] if it is
running. If the thread has terminated or is already suspended,
@racket[thread-suspend] has no effect. The thread remains suspended
(i.e., it does not execute) until it is resumed with
@racket[thread-resume]. If the @tech{current custodian} does not
solely manage @racket[thd] (i.e., some custodian of @racket[thd]
is not the current custodian or a subordinate), the 
@exnraise[exn:fail:contract], and the thread is not suspended.}

@defproc[(thread-resume [thd thread?] [benefactor (or/c thread? custodian? #f) #f]) void?]{

Resumes the execution of @racket[thd] if it is suspended and has at
least one custodian (possibly added through @racket[benefactor], as
described below). If the thread has terminated, or if the thread is
already running and @racket[benefactor] is not supplied, or if the
thread has no custodian and @racket[benefactor] is not supplied, then
@racket[thread-resume] has no effect. Otherwise, if
@racket[benefactor] is supplied, it triggers up to three
additional actions:

@itemize[

   @item{If @racket[benefactor] is a thread, whenever it is resumed
   from a suspended state in the future, then @racket[thd] is also
   resumed. (Resuming @racket[thd] may trigger the resumption of other
   threads that were previously attached to @racket[thd] through
   @racket[thread-resume].)}

   @item{New custodians may be added to @racket[thd]'s set of
   managers.  If @racket[benefactor] is a thread, then all of the
   thread's custodians are added to @racket[thd]. Otherwise,
   @racket[benefactor] is a custodian, and it is added to @racket[thd]
   (unless the custodian is already shut down). If @racket[thd]
   becomes managed by both a custodian and one or more of its
   subordinates, the redundant subordinates are removed from
   @racket[thd].  If @racket[thd] is suspended and a custodian is
   added, then @racket[thd] is resumed only after the addition.}

   @item{If @racket[benefactor] is a thread, whenever it receives a
   new managing custodian in the future, then @racket[thd] also
   receives the custodian. (Adding custodians to @racket[thd] may
   trigger adding the custodians to other threads that were previously
   attached to @racket[thd] through @racket[thread-resume].)}

]}


@defproc[(kill-thread [thd thread?]) void?]{

Terminates the specified thread immediately, or suspends the thread if
@racket[thd] was created with
@racket[thread/suspend-to-kill]. Terminating the main thread exits the
application.  If @racket[thd] has already terminated,
@racket[kill-thread] does nothing.  If the @tech{current custodian} 
does not solely manage @racket[thd] (i.e., some custodian of @racket[thd]
is not the current custodian or a subordinate), the 
@exnraise[exn:fail:contract], and the thread is not killed or suspended.

Unless otherwise noted, procedures provided by Racket (and GRacket) are
kill-safe and suspend-safe; that is, killing or suspending a thread
never interferes with the application of procedures in other
threads. For example, if a thread is killed while extracting a
character from an input port, the character is either completely
consumed or not consumed, and other threads can safely use the port.}

@defproc[(break-thread [thd thread?]
                       [kind (or/c #f 'hang-up 'terminate) #f])
         void?]{

@index['("threads" "breaking")]{Registers} a break with the specified
thread. The optional @racket[kind] value indicates the kind of break to
register, where @racket[#f], @racket['hang-up], and @racket['terminate]
correspond to interrupt, hang-up, and terminate breaks respectively.
If breaking is disabled in @racket[thd], the break will be
ignored until breaks are re-enabled.
See @secref["breakhandler"] for details.}

@defproc[(sleep [secs (>=/c 0) 0]) void?]{

Causes the current thread to sleep until at least @racket[secs]
seconds have passed after it starts sleeping. A zero value for
@racket[secs] simply acts as a hint to allow other threads to
execute. The value of @racket[secs] can be a non-integer to request a
sleep duration to any precision; the precision of the actual sleep
time is unspecified.}

@defproc[(thread-running? [thd  thread?]) any]{

@index['("threads" "run state")]{Returns} @racket[#t] if @racket[thd]
has not terminated and is not suspended, @racket[#f] otherwise.}

@defproc[(thread-dead? [thd  thread?]) any]{

Returns @racket[#t] if @racket[thd] has terminated, @racket[#f]
otherwise.}

@;------------------------------------------------------------------------
@section[#:tag "threadsync"]{Synchronizing Thread State}

@defproc[(thread-wait [thd thread?]
                      [fail-k (procedure-arity-includes/c 0) void])
         any]{

Blocks execution of the current thread until @racket[thd] has
terminated. If the thread's procedure raised an exception or otherwise
aborted to the thread's initial @tech{prompt}, @racket[fail-k] is
called to produce the result of @racket[thread-wait]. Otherwise, if
the thread records its results (see @racket[#:keep] in
@racket[thread]), those results are returned, while @|void-const| is
return if the thread does not keep its results.

Note that @racket[(thread-wait (current-thread))]
deadlocks the current thread, but a break can end the deadlock if
breaking is enabled and if the thread is the main thread or otherwise
accessible; see @secref["breakhandler"].

Unless @racket[thd] was created with @racket[thread/suspend-to-kill],
a @racket[(thread-wait thd)] may potentially continue even if
@racket[thd] is otherwise inaccessible, because a @tech{custodian}
shut down could terminate the thread. As a result, a thread blocking
with @racket[thread-wait] normally cannot be garbage collected (see
@secref["gc-model"]). As a special case, however, @racket[(thread-wait
thd)] blocks without preventing garbage collection of the thread if
@racket[thd] is the current thread, since the thread could only
continue if a break escapes from the wait.

@history[#:changed "8.18.0.2" @elem{Added support for threads with
                                    values and the @racket[fail-k] argument.}]}

@defproc[(thread-dead-evt [thd thread?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that is
@tech{ready for synchronization} if and only if @racket[thd] has terminated. Unlike using
@racket[thd] directly, however, retaining a reference to the event does not
prevent @racket[thd] from being garbage collected (see
@secref["gc-model"]). @ResultItself{thread-dead event}.

A thread waiting on the result of @racket[(thread-dead-evt thd)]
normally cannot itself be garbage collected, unless @racket[thd] was
created with @racket[thread/suspend-to-kill], along the same lines as
waiting via @racket[thread-wait]. However, there is no special case
for waiting on the result of @racket[(thread-dead-evt thd)] where
@racket[thd] is the current thread.

For a given @racket[thd], @racket[thread-dead-evt] always returns the
same (i.e., @racket[eq?]) result.}


@defproc[(thread-resume-evt [thd thread?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that
becomes @tech{ready for synchronization} when @racket[thd] is running.  (If @racket[thd] has
terminated, the event never becomes ready.)  If @racket[thd] runs and
is then suspended after a call to @racket[thread-resume-evt], the
result event remains ready; after each suspend of @racket[thd] a fresh
event is generated to be returned by @racket[thread-resume-evt].  The
result of the event is @racket[thd], but if @racket[thd] is never
resumed, then reference to the event does not prevent @racket[thd]
from being garbage collected (see @secref["gc-model"]).}

@defproc[(thread-suspend-evt [thd thread?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that
becomes @tech{ready for synchronization} when @racket[thd] is suspended.  (If @racket[thd] has
terminated, the event will never unblock.)  If @racket[thd] is
suspended and then resumes after a call to
@racket[thread-suspend-evt], the result event remains ready;
each resume of @racket[thd] creates a fresh event to be returned by
@racket[thread-suspend-evt]. The
result of the event is @racket[thd], but if @racket[thd] was created
with @racket[thread] (as opposed to @racket[thread/suspend-to-kill]) and is never
resumed, then reference to the event does not prevent @racket[thd]
from being garbage collected (see @secref["gc-model"]).

If @racket[thd] was created with @racket[thread/suspend-to-kill], then
waiting on @racket[(thread-suspend-evt thd)] prevents garbage
collection of the waiting thread in the same way as
@racket[(thread-dead-evt _another-thd)] for a @racket[_another-thd]
created via @racket[thread]. Furthermore, since the event result is
@racket[thd], waiting on @racket[(thread-suspend-evt thd)] prevents
garbage collection of @racket[thd].

}

@;------------------------------------------------------------------------
@section[#:tag "threadmbox"]{Thread Mailboxes}

Each thread has a @defterm{mailbox} through which it can receive
arbitrary messages.  In other words, each thread has a built-in
asynchronous channel.

@margin-note/ref{See also @secref["async-channel"].}

@defproc[(thread-send [thd thread?] [v any/c] 
                      [fail-thunk (or/c (-> any) #f)
                                  (lambda () (raise-mismatch-error ....))]) 
         any]{

Queues @racket[v] as a message to @racket[thd] without blocking. If
the message is queued, the result is @|void-const|. If @racket[thd]
stops running---as in @racket[thread-running?]---before the message is
queued, then @racket[fail-thunk] is called (through a tail call) if it is
a procedure to produce the result, or @racket[#f] is returned if
@racket[fail-thunk] is @racket[#f].}

@defproc[(thread-receive) any/c]{

Receives and dequeues a message queued for the current thread, if
any. If no message is available, @racket[thread-receive] blocks until
one is available.}

@defproc[(thread-try-receive) any/c]{

Receives and dequeues a message queued for the current thread, if any,
or returns @racket[#f] immediately if no message is available.}

@defproc[(thread-receive-evt) evt?]{

Returns a constant @tech{synchronizable event} (see @secref["sync"])
that becomes @tech{ready for synchronization} when the synchronizing thread has a message to
receive. @ResultItself{thread-receive event}.}

@defproc[(thread-rewind-receive [lst list?]) void?]{

Pushes the elements of @racket[lst] back onto the front of the current
thread's queue. The elements are pushed one by one, so that the first
available message is the last element of @racket[lst].}

@;------------------------------------------------------------------------
@section[#:tag "threadpool"]{Parallel Thread Pools}

@defproc[(parallel-thread-pool? [v any/c]) thread?]{Returns @racket[#t] if
@racket[v] is a @tech{parallel thread pool}, @racket[#f] otherwise.

@history[#:added "8.18.0.2"]}

@defproc[(make-parallel-thread-pool [n exact-positive-integer? (processor-count)])
         parallel-thread-pool?]{

Creates a @deftech{parallel thread pool} that can be used to group a
@tech{parallel thread} with other parallel threads. The threads in a
pool can use up to @racket[n] processors to run. If more than
@racket[n] threads are in the pool, not all of them will run in
parallel, but they will still all run concurrently.

The new thread pool is placed into the management of the current
@tech{custodian}. If the custodian is shut down, then the pool is
closed in the same way as with @racket[parallel-thread-pool-close].
Any threads already in the pool can continue to run and use the pool's
resources (unless they are also shut down by the same custodian).

A parallel thread pool cannot run threads in parallel on the @tech{BC}
variant of Racket or when Racket is compiled without support for
parallelism. In that case, parallel threads behave the same as
@tech{coroutine threads}. For the @tech{CS} variant of Racket, the
@racket[futures-enabled?] predicate can be used to detect when
parallel threads behave differently from coroutine threads.

@history[#:added "8.18.0.2"]}

@defproc[(parallel-thread-pool-close [p parallel-thread-pool?])
         void?]{

Closes a @tech{parallel thread pool} so that no threads can be added
to the pool. Any existing threads in the pool are allowed to continue
running, and they continue to share the pool's processor resources.

When no more threads are running within a closed thread pool, or when
no threads are allowed to make progress because the pool's
@tech{custodian} has been shut down, then processor resources
allocated to the pool can be returned to the operating system (i.e.,
operating-system threads allocated to the pool are terminated).

@history[#:added "8.18.0.2"]}
