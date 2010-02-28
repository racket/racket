#lang scribble/doc
@(require "mz.ss")

@title[#:tag "threads"]{Threads}

See @secref["thread-model"] for basic information on the PLT Scheme
thread model. See also @secref["futures"].

When a thread is created, it is placed into the management of the
@tech{current custodian} and added to the current thread group (see
@secref["threadgroups"]). A thread can have any number of custodian
managers added through @scheme[thread-resume].

A thread that has not terminated can be garbage collected (see
@secref["gc-model"]) if it is unreachable and suspended or if it is
unreachable and blocked on only unreachable events through
@scheme[semaphore-wait], @scheme[semaphore-wait/enable-break],
@scheme[channel-put], @scheme[channel-get], @scheme[sync],
@scheme[sync/enable-break], or @scheme[thread-wait].

@margin-note{In MrEd, a handler thread for an eventspace is blocked on
an internal semaphore when its event queue is empty. Thus, the handler
thread is collectible when the eventspace is unreachable and contains
no visible windows or running timers.}

All constant-time procedures and operations provided by MzScheme are
thread-safe because they are @defterm{atomic}. For example,
@scheme[set!] assigns to a variable as an atomic action with respect
to all threads, so that no thread can see a ``half-assigned''
variable. Similarly, @scheme[vector-set!] assigns to a vector
atomically. The @scheme[hash-set!] procedure is not atomic, but
the table is protected by a lock; see @secref["hashtables"] for more
information. Port operations are generally not atomic, but they are
thread-safe in the sense that a byte consumed by one thread from an
input port will not be returned also to another thread, and procedures
like @scheme[port-commit-peeked] and @scheme[write-bytes-avail] offer
specific concurrency guarantees.

@;------------------------------------------------------------------------
@section{Creating Threads}

@defproc[(thread [thunk (-> any)]) thread?]{

Calls @scheme[thunk] with no arguments in a new thread of control. The
@scheme[thread] procedure returns immediately with a @deftech{thread
descriptor} value. When the invocation of @scheme[thunk] returns, the
thread created to invoke @scheme[thunk] terminates.

}

@defproc[(thread? [v any/c]) thread?]{Returns @scheme[#t] if
@scheme[v] is a @tech{thread descriptor}, @scheme[#f] otherwise.}

@defproc[(current-thread) thread?]{Returns the @tech{thread
 descriptor} for the currently executing thread.}

@defproc[(thread/suspend-to-kill [thunk (-> any)]) thread]{

Like @scheme[thread], except that ``killing'' the thread through
@scheme[kill-thread] or @scheme[custodian-shutdown-all] merely
suspends the thread instead of terminating it.  }

@defproc[(call-in-nested-thread [thunk (->any)]
                                [cust custodian? (current-custodian)]) 
          any]{

Creates a nested thread managed by @scheme[cust] to execute
@scheme[thunk]. (The nested thread's current custodian is inherited
from the creating thread, independent of the @scheme[cust] argument.)
The current thread blocks until @scheme[thunk] returns, and the result
of the @scheme[call-in-nested-thread] call is the result returned by
@scheme[thunk].

The nested thread's exception handler is initialized to a procedure
that jumps to the beginning of the thread and transfers the exception
to the original thread. The handler thus terminates the nested thread
and re-raises the exception in the original thread.

If the thread created by @scheme[call-in-nested-thread] dies before
@scheme[thunk] returns, the @exnraise[exn:fail] in the original
thread. If the original thread is killed before @scheme[thunk]
returns, a break is queued for the nested thread.

If a break is queued for the original thread (with
@scheme[break-thread]) while the nested thread is running, the break
is redirected to the nested thread. If a break is already queued on
the original thread when the nested thread is created, the break is
moved to the nested thread. If a break remains queued on the nested
thread when it completes, the break is moved to the original thread.}

@;------------------------------------------------------------------------
@section[#:tag "threadkill"]{Suspending, Resuming, and Killing Threads}

@defproc[(thread-suspend [thd  thread?]) void?]{

Immediately suspends the execution of @scheme[thd] if it is
running. If the thread has terminated or is already suspended,
@scheme[thread-suspend] has no effect. The thread remains suspended
(i.e., it does not execute) until it is resumed with
@scheme[thread-resume]. If the @tech{current custodian} does not
manage @scheme[thd] (and none of its subordinates manages
@scheme[thd]), the @exnraise[exn:fail:contract], and the thread is not
suspended.}

@defproc[(thread-resume [thd thread?][benefactor (or/c thread? custodian? #f) #f]) void?]{

Resumes the execution of @scheme[thd] if it is suspended and has at
least one custodian (possibly added through @scheme[benefactor], as
described below). If the thread has terminated, or if the thread is
already running and @scheme[benefactor] is not supplied, or if the
thread has no custodian and @scheme[benefactor] is not supplied, then
@scheme[thread-resume] has no effect. Otherwise, if
@scheme[benefactor] is supplied, it triggers up to three
additional actions:

@itemize[

   @item{If @scheme[benefactor] is a thread, whenever it is resumed
   from a suspended state in the future, then @scheme[thd] is also
   resumed. (Resuming @scheme[thd] may trigger the resumption of other
   threads that were previously attached to @scheme[thd] through
   @scheme[thread-resume].)}

   @item{New custodians may be added to @scheme[thd]'s set of
   managers.  If @scheme[benefactor] is a thread, then all of the
   thread's custodians are added to @scheme[thd]. Otherwise,
   @scheme[benefactor] is a custodian, and it is added to @scheme[thd]
   (unless the custodian is already shut down). If @scheme[thd]
   becomes managed by both a custodian and one or more of its
   subordinates, the redundant subordinates are removed from
   @scheme[thd].  If @scheme[thd] is suspended and a custodian is
   added, then @scheme[thd] is resumed only after the addition.}

   @item{If @scheme[benefactor] is a thread, whenever it receives a
   new managing custodian in the future, then @scheme[thd] also
   receives the custodian. (Adding custodians to @scheme[thd] may
   trigger adding the custodians to other threads that were previously
   attached to @scheme[thd] through @scheme[thread-resume].)}

]}


@defproc[(kill-thread [thd thread?]) void?]{

Terminates the specified thread immediately, or suspends the thread if
@scheme[thd] was created with
@scheme[thread/suspend-to-kill]. Terminating the main thread exits the
application.  If @scheme[thd] has already terminated,
@scheme[kill-thread] does nothing.  If the @tech{current custodian}
does not manage @scheme[thd] (and none of its subordinates manages
@scheme[thd]), the @exnraise[exn:fail:contract], and the thread is not
killed or suspended.

Unless otherwise noted, procedures provided by MzScheme (and MrEd) are
kill-safe and suspend-safe; that is, killing or suspending a thread
never interferes with the application of procedures in other
threads. For example, if a thread is killed while extracting a
character from an input port, the character is either completely
consumed or not consumed, and other threads can safely use the port.}

@defproc[(break-thread [thd thread?]) void?]{

@index['("threads" "breaking")]{Registers} a break with the specified
thread. If breaking is disabled in @scheme[thd], the break will be
ignored until breaks are re-enabled (see @secref["breakhandler"]).}

@defproc[(sleep [secs nonnegative-number? 0]) void?]{

Causes the current thread to sleep until at least @scheme[secs]
seconds have passed after it starts sleeping. A zero value for
@scheme[secs] simply acts as a hint to allow other threads to
execute. The value of @scheme[secs] can be non-integral to request a
sleep duration to any precision; the precision of the actual sleep
time is unspecified.}

@defproc[(thread-running? [thd  thread?]) any]{

@index['("threads" "run state")]{Returns} @scheme[#t] if @scheme[thd]
has not terminated and is not suspended, @scheme[#f] otherwise.}

@defproc[(thread-dead? [thd  thread?]) any]{

Returns @scheme[#t] if @scheme[thd] has terminated, @scheme[#f]
otherwise.}

@;------------------------------------------------------------------------
@section[#:tag "threadsync"]{Synchronizing Thread State}

@defproc[(thread-wait [thd thread?]) void?]{

Blocks execution of the current thread until @scheme[thd] has
terminated. Note that @scheme[(thread-wait (current-thread))]
deadlocks the current thread, but a break can end the deadlock (if
breaking is enabled; see @secref["breakhandler"]).}

@defproc[(thread-dead-evt [thd thread?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that is
ready if and only if @scheme[thd] has terminated.  Unlike using
@scheme[thd] directly, however, a reference to the event does not
prevent @scheme[thd] from being garbage collected (see
@secref["gc-model"]). For a given @scheme[thd],
@scheme[thread-dead-evt] always returns the same (i.e., @scheme[eq?])
result.}

@defproc[(thread-resume-evt [thd thread?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that
becomes ready when @scheme[thd] is running.  (If @scheme[thd] has
terminated, the event never becomes ready.)  If @scheme[thd] runs and
is then suspended after a call to @scheme[thread-resume-evt], the
result event remains ready; after each suspend of @scheme[thd] a fresh
event is generated to be returned by @scheme[thread-resume-evt].  The
result of the event is @scheme[thd], but if @scheme[thd] is never
resumed, then reference to the event does not prevent @scheme[thd]
from being garbage collected (see @secref["gc-model"]).}

@defproc[(thread-suspend-evt [thd thread?]) evt?]{

Returns a @tech{synchronizable event} (see @secref["sync"]) that
becomes ready when @scheme[thd] is suspended.  (If @scheme[thd] has
terminated, the event will never unblock.)  If @scheme[thd] is
suspended and then resumes after a call to
@scheme[thread-suspend-evt], the result event remains ready; after
each resume of @scheme[thd] created a fresh event to be returned by
@scheme[thread-suspend-evt].}

@;------------------------------------------------------------------------
@section[#:tag "threadmbox"]{Thread Mailboxes}

Each thread has a @defterm{mailbox} through which it can receive
arbitrary message. In other words, each thread has a built-in
asynchronous channel.

@margin-note/ref{See also @secref["async-channel"].}

@defproc[(thread-send [thd thread?] [v any/c] 
                      [fail-thunk (or/c (-> any) #f)
                                  (lambda () (raise-mismatch-error ....))]) 
         any]{

Queues @scheme[v] as a message to @scheme[thd] without blocking. If
the message is queued, the result is @|void-const|. If @scheme[thd]
stops running---as in @scheme[thread-running?]---before the message is
queued, then @scheme[fail-thunk] is called (through a tail call) if is
a procedure to produce the result, or @scheme[#f] is returned if
@scheme[fail-thunk] is @scheme[#f].}

@defproc[(thread-receive) any/c]{

Receives and dequeues a message queued for the current thread, if
any. If no message is available, @scheme[thread-receive] blocks until
one is available.}

@defproc[(thread-try-receive) any/c]{

Receives and dequeues a message queued for the current thread, if any,
or returns @scheme[#f] immediately if no message is available.}

@defproc[(thread-receive-evt) evt?]{

Returns a constant @tech{synchronizable event} (see @secref["sync"])
that becomes ready when the synchronizing thread has a message to
receive. The event result is itself.}

@defproc[(thread-rewind-receive [lst list?]) void?]{

Pushes the elements of @scheme[lst] back onto the front of the current
thread's queue. The elements are pushed one by one, so that the first
available message is the last element of @scheme[lst].}
