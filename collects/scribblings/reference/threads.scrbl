#reader(lib "docreader.ss" "scribble")
@require[(lib "bnf.ss" "scribble")]
@require["mz.ss"]

@title[#:tag "mz:threads"]{Threads}

See @secref["mz:thread-model"] and @secref["mz:parameter-model"] for
basic information on the PLT Scheme thread and parameter model.

When a thread is created, it is placed into the management of the
@tech{current custodian} and added to the current thread group (see
@secref["mz:threadgroups"]). A thread can have any number of custodian
managers added through @scheme[thread-resume].

A thread that has not terminated can be garbage collected (see
@secref["mz:gc-model"]) if it is unreachable and suspended, or if it
is unreachable and blocked on a set of unreachable events through
@scheme[semaphore-wait] or @scheme[semaphore-wait/enable-break],
@scheme[channel-put] or @scheme[channel-get], @scheme[sync] or
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
atomically. The @scheme[hash-table-put!] procedure is not atomic, but
the table is protected by a lock; see @secref["mz:hashtable"] for more
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

@;------------------------------------------------------------------------
@section[#:tag "mz:threadkill"]{Suspending, Resuming, and Killing Threads}

@defproc[(thread-suspend [thd  thread?]) void?]{

Immediately suspends the execution of @scheme[thd] if it is
running. If the thread has terminated or is already suspended,
@scheme[thread-suspend] has no effect. The thread remains suspended
(i.e., it does not execute) until it is resumed with
@scheme[thread-resume]. If the @tech{current custodian} does not
manage @scheme[thd] (and none of its subordinates manages
@scheme[thd]), the @exnraise[exn:fail:contract], and the thread is not
suspended.}

@defproc[(thread-resume [thd thread?][benefactor (or/c thread? custodian? false/c) #f]) void?]{

Resumes the execution of @scheme[thd] if it is suspended and has at
least one custodian (possibly added through @scheme[benefactor], as
described below). If the thread has terminated, or if the thread is
already running and @scheme[benefactor] is not supplied, or if the
thread has no custodian and @scheme[benefactor] is not supplied, then
@scheme[thread-resume] has no effect. Otherwise, if
@scheme[benefactor] is supplied, it triggers up to three
additional actions:

@itemize{

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

}}


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
