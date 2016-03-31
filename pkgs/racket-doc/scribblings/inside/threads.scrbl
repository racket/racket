#lang scribble/doc
@(require "utils.rkt" (for-label scheme/tcp))

@title[#:tag "threads"]{Threads}

The initializer function @cppi{scheme_basic_env} creates the main
Racket thread; all other threads are created through calls to
@cppi{scheme_thread}.

Information about each internal Racket thread is kept in a
@cppi{Scheme_Thread} structure. A pointer to the current thread's
structure is available as @cppdef{scheme_current_thread} or
from @cppi{scheme_get_current_thread}.  A
@cpp{Scheme_Thread} structure includes the following fields:

@itemize[

 @item{@cppi{error_buf} --- the @cppi{mz_jmp_buf} value used to escape
 from errors. The @cpp{error_buf} value of the current thread is
 available as @cppi{scheme_error_buf}.}

 @item{@cppi{cjs.jumping_to_continuation} --- a flag that
 distinguishes escaping-continuation invocations from error
 escapes. The @cpp{cjs.jumping_to_continuation} value of the current
 thread is available as @cppi{scheme_jumping_to_continuation}.}

 @item{@cppi{init_config} ---
 the thread's initial parameterization. See also @secref["config"].}

 @item{@cppi{cell_values} --- The thread's values for thread cells
 (see also @secref["config"]).}

 @item{@cppi{next} --- The next thread in the linked list of threads;
 this is @cpp{NULL} for the main thread.}

]

The list of all scheduled threads is kept in a linked list;
@cppi{scheme_first_thread} points to the first thread in the list.
The last thread in the list is always the main thread.

@; ----------------------------------------------------------------------

@section{Integration with Threads}

Racket's threads can break external C code under two circumstances:

@itemize[

 @item{@italic{Pointers to stack-based values can be communicated
 between threads.}  For example, if thread A stores a pointer to a
 stack-based variable in a global variable, if thread B uses the
 pointer in the global variable, it may point to data that is not
 currently on the stack.}

 @item{@italic{C functions that can invoke Racket (and also be invoked
 by Racket) depend on strict function-call nesting.} For example,
 suppose a function F uses an internal stack, pushing items on to the
 stack on entry and popping the same items on exit. Suppose also that
 F invokes Racket to evaluate an expression.  If the evaluation of
 this expression invokes F again in a new thread, but then returns to
 the first thread before completing the second F, then F's internal
 stack will be corrupted.}

]

If either of these circumstances occurs, Racket will probably crash.


@; ----------------------------------------------------------------------

@section[#:tag "usefuel"]{Allowing Thread Switches}

C code that performs substantial or unbounded work should occasionally
call @cppi{SCHEME_USE_FUEL}---actually a macro---which allows Racket
to swap in another Racket thread to run, and to check for breaks on
the current thread.  In particular, if breaks are enabled, then
@cpp{SCHEME_USE_FUEL} may trigger an exception.

The macro consumes an integer argument. On most platforms, where
thread scheduling is based on timer interrupts, the argument is
ignored. On some platforms, however, the integer represents the amount
of ``fuel'' that has been consumed since the last call to
@cpp{SCHEME_USE_FUEL}. For example, the implementation of
@racket[vector->list] consumes a unit of fuel for each created cons
cell:

@verbatim[#:indent 2]{
  Scheme_Object *scheme_vector_to_list(Scheme_Object *vec)
  {
    int i;
    Scheme_Object *pair = scheme_null;

    i = SCHEME_VEC_SIZE(vec);

    for (; i--; ) {
      SCHEME_USE_FUEL(1);
      pair = scheme_make_pair(SCHEME_VEC_ELS(vec)[i], pair);
    }

    return pair;
  }
}

The @cpp{SCHEME_USE_FUEL} macro expands to a C block, not an
expression.

@; ----------------------------------------------------------------------

@section[#:tag "threadblock"]{Blocking the Current Thread}

Embedding or extension code sometimes needs to block, but blocking
should allow other Racket threads to execute. To allow other threads
to run, block using @cppi{scheme_block_until}.  This procedure takes
two functions: a polling function that tests whether the blocking
operation can be completed, and a prepare-to-sleep function that sets
bits in @cpp{fd_set}s when Racket decides to sleep (because all Racket
threads are blocked). On Windows, an ``@cpp{fd_set}'' can also
accommodate OS-level semaphores or other handles via
@cpp{scheme_add_fd_handle}.

Since the functions passed to @cppi{scheme_block_until} are called by
the Racket thread scheduler, they must never raise exceptions, call
@cpp{scheme_apply}, or trigger the evaluation of Racket code in any
way. The @cpp{scheme_block_until} function itself may call the current
exception handler, however, in reaction to a break (if breaks are
enabled).

When a blocking operation is associated with an object, then the
object might make sense as an argument to @indexed-racket[sync]. To
extend the set of objects accepted by @racket[sync], either register
polling and sleeping functions with @cppi{scheme_add_evt}, or register
a semaphore accessor with @cppi{scheme_add_evt_through_sema}.

The @cppi{scheme_signal_received} function can be called to wake up
Racket when it is sleeping. In particular, calling
@cppi{scheme_signal_received} ensures that Racket will poll all
blocking synchronizations soon afterward. Furthermore,
@cpp{scheme_signal_received} can be called from any OS-level thread.
Thus, when no adequate prepare-to-sleep function can be implemented
for @cpp{scheme_block_until} in terms of file descriptors or Windows
handles, calling @cpp{scheme_signal_received} when the poll result
changes will ensure that a poll is issued.

@; ----------------------------------------------------------------------

@section[#:tag "threadtime"]{Threads in Embedded Racket with Event Loops}

When Racket is embedded in an application with an event-based model
(i.e., the execution of Racket code in the main thread is repeatedly
triggered by external events until the application exits) special
hooks must be set to ensure that non-main threads execute
correctly. For example, during the execution in the main thread, a new
thread may be created; the new thread may still be running when the
main thread returns to the event loop, and it may be arbitrarily long
before the main thread continues from the event loop. Under such
circumstances, the embedding program must explicitly allow Racket to
execute the non-main threads; this can be done by periodically calling
the function @cppi{scheme_check_threads}.

Thread-checking only needs to be performed when non-main threads exist
(or when there are active callback triggers). The embedding
application can set the global function pointer
@cppi{scheme_notify_multithread} to a function that takes an integer
parameter and returns @cpp{void}.  This function is be called with 1
when thread-checking becomes necessary, and then with 0 when thread
checking is no longer necessary. An embedding program can use this
information to prevent unnecessary @cpp{scheme_check_threads} polling.

The below code illustrates how GRacket formerly set up
@cpp{scheme_check_threads} polling using the wxWindows @cpp{wxTimer}
class. (Any regular event-loop-based callback is appropriate.) The
@cpp{scheme_notify_multithread} pointer is set to
@cpp{MrEdInstallThreadTimer}. (GRacket no longer work this way, however.)

@verbatim[#:indent 2]{
  class MrEdThreadTimer : public wxTimer
  {
   public:
    void Notify(void); /* callback when timer expires */
  };

  static int threads_go;
  static MrEdThreadTimer *theThreadTimer;
  #define THREAD_WAIT_TIME 40

  void MrEdThreadTimer::Notify()
  {
    if (threads_go)
      Start(THREAD_WAIT_TIME, TRUE);

    scheme_check_threads();
  }

  static void MrEdInstallThreadTimer(int on)
  {
    if (!theThreadTimer)
      theThreadTimer = new MrEdThreadTimer;

    if (on)
      theThreadTimer->Start(THREAD_WAIT_TIME, TRUE);
    else
      theThreadTimer->Stop();

    threads_go = on;
    if (on)
      do_this_time = 1;
  }
}

An alternate architecture, which GRacket now uses, is to send the main
thread into a loop, which blocks until an event is ready to handle.
Racket automatically takes care of running all threads, and it does so
efficiently because the main thread blocks on a file descriptor, as
explained in @secref["threadblock"].

@subsection[#:tag "blockednonmainel"]{Callbacks for Blocked Threads}

Racket threads are sometimes blocked on file descriptors, such as an
input file or the X event socket. Blocked non-main threads do not
block the main thread, and therefore do not affect the event loop, so
@cppi{scheme_check_threads} is sufficient to implement this case
correctly. However, it is wasteful to poll these descriptors with
@cpp{scheme_check_threads} when nothing else is happening in the
application and when a lower-level poll on the file descriptors can be
installed. If the global function pointer
@cppi{scheme_wakeup_on_input} is set, then this case is handled more
efficiently by turning off thread checking and issuing a ``wakeup''
request on the blocking file descriptors through
@cpp{scheme_wakeup_on_input}.

A @cpp{scheme_wakeup_on_input} procedure takes a pointer to an array
of three @cpp{fd_set}s (use @cpp{MZ_FD_SET} instead of @cpp{FD_SET}, etc.)
and returns @cpp{void}. The @cpp{scheme_wakeup_on_input}
function does not sleep immediately; it just
sets up callbacks on the specified file descriptors.  When input is
ready on any of those file descriptors, the callbacks are removed and
@cpp{scheme_wake_up} is called.

For example, the X Windows version of GRacket formerly set
@cpp{scheme_wakeup_on_input} to this @cpp{MrEdNeedWakeup}:

@verbatim[#:indent 2]{
  static XtInputId *scheme_cb_ids = NULL;
  static int num_cbs;

  static void MrEdNeedWakeup(void *fds)
  {
    int limit, count, i, p;
    fd_set *rd, *wr, *ex;

    rd = (fd_set *)fds;
    wr = ((fd_set *)fds) + 1;
    ex = ((fd_set *)fds) + 2;

    limit = getdtablesize();

    /* See if we need to do any work, really: */
    count = 0;
    for (i = 0; i < limit; i++) {
      if (MZ_FD_ISSET(i, rd))
        count++;
      if (MZ_FD_ISSET(i, wr))
        count++;
      if (MZ_FD_ISSET(i, ex))
        count++;
    }

    if (!count)
      return;

    /* Remove old callbacks: */
    if (scheme_cb_ids)
      for (i = 0; i < num_cbs; i++)
        notify_set_input_func((Notify_client)NULL, (Notify_func)NULL,
                              scheme_cb_ids[i]);

    num_cbs = count;
    scheme_cb_ids = new int[num_cbs];

    /* Install callbacks */
    p = 0;
    for (i = 0; i < limit; i++) {
      if (MZ_FD_ISSET(i, rd))
        scheme_cb_ids[p++] = XtAppAddInput(wxAPP_CONTEXT, i,
                                           (XtPointer *)XtInputReadMask,
                                           (XtInputCallbackProc)MrEdWakeUp, NULL);
      if (MZ_FD_ISSET(i, wr))
        scheme_cb_ids[p++] = XtAppAddInput(wxAPP_CONTEXT, i,
                                           (XtPointer *)XtInputWriteMask,
                                           (XtInputCallbackProc)MrEdWakeUp, NULL);
      if (MZ_FD_ISSET(i, ex))
        scheme_cb_ids[p++] = XtAppAddInput(wxAPP_CONTEXT, i,
                                           (XtPointer *)XtInputExceptMask,
                                           (XtInputCallbackProc)MrEdWakeUp,
                                           NULL);
    }
  }

  /* callback function when input/exception is detected: */
  Bool MrEdWakeUp(XtPointer, int *, XtInputId *)
  {
    int i;

    if (scheme_cb_ids) {
      /* Remove all callbacks: */
      for (i = 0; i < num_cbs; i++)
       XtRemoveInput(scheme_cb_ids[i]);

      scheme_cb_ids = NULL;

      /* ``wake up'' */
      scheme_wake_up();
    }

    return FALSE;
  }
}

@; ----------------------------------------------------------------------

@section[#:tag "sleeping"]{Sleeping by Embedded Racket}

When all Racket threads are blocked, Racket must ``sleep'' for a
certain number of seconds or until external input appears on some file
descriptor. Generally, sleeping should block the main event loop of
the entire application. However, the way in which sleeping is
performed may depend on the embedding application. The global function
pointer @cppi{scheme_sleep} can be set by an embedding application to
implement a blocking sleep, although Racket implements this function
for you.

A @cpp{scheme_sleep} function takes two arguments: a @cpp{float} and a
@cpp{void*}. The latter is really points to an array of three
``@cpp{fd_set}'' records (one for read, one for write, and one for
exceptions); these records are described further below. If the
@cpp{float} argument is non-zero, then the @cpp{scheme_sleep} function
blocks for the specified number of seconds, at most. The
@cpp{scheme_sleep} function should block until there is input one of
the file descriptors specified in the ``@cpp{fd_set},'' indefinitely
if the @cpp{float} argument is zero.

The second argument to @cpp{scheme_sleep} is conceptually an array of
three @cpp{fd_set} records, but always use @cpp{scheme_get_fdset} to
get anything other than the zeroth element of this array, and
manipulate each ``@cpp{fd_set}'' with @cpp{MZ_FD_SET},
@cpp{MZ_FD_CLR}, @|etc| instead of @cpp{FD_SET}, @cpp{FD_CLR}, etc.

The following function @cpp{mzsleep} is an appropriate
@cpp{scheme_sleep} function for most any Unix or Windows application.
(This is approximately the built-in sleep used by Racket.)

@verbatim[#:indent 2]{
  void mzsleep(float v, void *fds)
  {
    if (v) {
      sleep(v);
    } else {
      int limit;
      fd_set *rd, *wr, *ex;

  # ifdef WIN32
      limit = 0;
  # else
      limit = getdtablesize();
  # endif

      rd = (fd_set *)fds;
      wr = (fd_set *)scheme_get_fdset(fds, 1);
      ex = (fd_set *)scheme_get_fdset(fds, 2);

      select(limit, rd, wr, ex, NULL);
    }
  }
}


@; ----------------------------------------------------------------------

@section{Thread Functions}

@function[(Scheme_Thread* scheme_get_current_thread)]{

Returns the currently executing thread. The result is equivalent to
@cppi{scheme_current_thread}, but the function form must be used in
some embedding contexts.}

@function[(Scheme_Object* scheme_thread
           [Scheme_Object* thunk])]{

Creates a new thread, just like @racket[thread].}

@function[(Scheme_Object* scheme_thread_w_details
           [Scheme_Object* thunk]
           [Scheme_Config* config]
           [Scheme_Thread_Cell_Table* cells]
           [Scheme_Custodian* cust]
           [int suspend_to_kill])]{

Like @cpp{scheme_thread}, except that the created thread belongs to
@var{cust} instead of the current custodian, it uses the given
@var{config} for its initial configuration, it uses @var{cells} for
its thread-cell table, and if @var{suspend_to_kill} is non-zero, then
the thread is merely suspended when it would otherwise be killed
(through either @racket[kill-thread] or
@racket[custodian-shutdown-all]).

The @var{config} argument is typically obtained through
@cpp{scheme_current_config} or @cpp{scheme_extend_config}. A
@var{config} is immutable, so different threads can safely use the
same value. The @var{cells} argument should be obtained from
@cpp{scheme_inherit_cells}; it is mutable, and a particular cell table
should be used by only one thread.}

@function[(Scheme_Object* scheme_make_sema
           [intptr_t v])]{

Creates a new semaphore.}

@function[(void scheme_post_sema
           [Scheme_Object* sema])]{

Posts to @var{sema}.}

@function[(int scheme_wait_sema
           [Scheme_Object* sema]
           [int try])]{

Waits on @var{sema}. If @var{try} is not 0, the wait can fail and 0 is
returned for failure, otherwise 1 is returned.}

@function[(void scheme_thread_block
           [float sleep_time])]{

Allows the current thread to be swapped out in favor of other
threads. If @var{sleep_time} positive, then the current thread will
sleep for at least @var{sleep_time} seconds.

After calling this function, a program should almost always call
@cppi{scheme_making_progress} next. The exception is when
@cpp{scheme_thread_block} is called in a polling loop that performs no
work that affects the progress of other threads. In that case,
@cpp{scheme_making_progress} should be called immediately after
exiting the loop.

See also @cpp{scheme_block_until}, and see also the
@cpp{SCHEME_USE_FUEL} macro in @secref["usefuel"].}

@function[(void scheme_thread_block_enable_break
           [float sleep_time]
           [int break_on])]{

Like @cpp{scheme_thread_block}, but breaks are enabled while blocking if
 @var{break_on} is true.}

@function[(void scheme_swap_thread
           [Scheme_Thread* thread])]{

Swaps out the current thread in favor of @var{thread}.}

@function[(void scheme_break_thread
           [Scheme_Thread* thread])]{

Sends a break signal to the given thread.}

@function[(int scheme_break_waiting
           [Scheme_Thread* thread])]{

Returns @cpp{1} if a break from @racket[break-thread] or @cpp{scheme_break_thread}
 has occurred in the specified thread but has not yet been handled.}

@function[(int scheme_block_until
           [Scheme_Ready_Fun f]
           [Scheme_Needs_Wakeup_Fun fdf]
           [Scheme_Object* data]
           [float sleep])]{

The @cpp{Scheme_Ready_Fun} and @cpp{Scheme_Needs_Wakeup_Fun}
 types are defined as follows:

@verbatim[#:indent 2]{
   typedef int (*Scheme_Ready_Fun)(Scheme_Object *data);
   typedef void (*Scheme_Needs_Wakeup_Fun)(Scheme_Object *data,
                                           void *fds);
}

Blocks the current thread until @var{f} with @var{data} returns a true
 value.  The @var{f} function is called periodically---at least once
 per potential swap-in of the blocked thread---and it may be called
 multiple times even after it returns a true value. If @var{f}
 with @var{data} ever returns a true value, it must continue to return
 a true value until @cpp{scheme_block_until} returns. The argument
 to @var{f} is the same @var{data} as provided
 to @cpp{scheme_block_until}, and @var{data} is ignored
 otherwise. (The @var{data} argument is not actually required to be
 a @cpp{Scheme_Object*} value, because it is only used by @var{f}
 and @var{fdf}.)

If Racket decides to sleep, then the @var{fdf} function is called to
 sets bits in @var{fds}, conceptually an array of three
 @cpp{fd_set}s: one or reading, one for writing, and one for
 exceptions. Use @cpp{scheme_get_fdset} to get elements of this
 array, and manipulate an ``@cpp{fd_set}'' with @cpp{MZ_FD_SET}
 instead of @cpp{FD_SET}, etc. On Windows, an ``@cpp{fd_set}'' can
 also accommodate OS-level semaphores or other handles via
 @cpp{scheme_add_fd_handle}.

The @var{fdf} argument can be @cpp{NULL}, which implies that the thread
 becomes unblocked (i.e., @var{ready} changes its result to true) only
 through Racket actions, and never through external processes (e.g.,
 through a socket or OS-level semaphore)---with the exception that
 @cpp{scheme_signal_received} may be called to indicate an external
 change.

If @var{sleep} is a positive number, then @cpp{scheme_block_until}
 polls @var{f} at least every @var{sleep} seconds, but
 @cpp{scheme_block_until} does not return until @var{f} returns a
 true value. The call to @cpp{scheme_block_until} can return before
 @var{sleep} seconds if @var{f} returns a true value.

The return value from @cpp{scheme_block_until} is the return value
 of its most recent call to @var{f}, which enables @var{f} to return
 some information to the @cpp{scheme_block_until} caller.

See @secref["threadblock"] for information about restrictions on the
 @var{f} and @var{fdf} functions.}

@function[(int scheme_block_until_enable_break
           [Scheme_Ready_Fun f]
           [Scheme_Needs_Wakeup_Fun fdf]
           [Scheme_Object* data]
           [float sleep]
           [int break_on])]{

Like @cpp{scheme_block_until}, but breaks are enabled while blocking
 if @var{break_on} is true.}

@function[(int scheme_block_until_unless
           [Scheme_Ready_Fun f]
           [Scheme_Needs_Wakeup_Fun fdf]
           [Scheme_Object* data]
           [float sleep]
           [Scheme_Object* unless_evt]
           [int break_on])]{

Like @cpp{scheme_block_until_enable_break}, but the function
 returns if @var{unless_evt} becomes ready, where @var{unless_evt}
 is a port progress event implemented by
 @cpp{scheme_progress_evt_via_get}. See
 @cpp{scheme_make_input_port} for more information.}


@function[(void scheme_signal_received)]{

Indicates that an external event may have caused the result of a
synchronization poll to have a different result. Unlike most other
Racket functions, this one can be called from any OS-level thread, and
it wakes up if the Racket thread if it is sleeping.}

@function[(void scheme_check_threads)]{

This function is periodically called by the embedding program to give
background processes time to execute. See @secref["threadtime"]
for more information.

As long as some threads are ready, this functions returns only after
one thread quantum, at least.}

@function[(void scheme_wake_up)]{

This function is called by the embedding program
when there is input on an external file descriptor. See
@secref["sleeping"] for more information.}

@function[(void* scheme_get_fdset
           [void* fds]
           [int pos])]{

Extracts an ``@cpp{fd_set}'' from an array passed to
 @cpp{scheme_sleep}, a callback for @cpp{scheme_block_until}, or an
 input port callback for @cpp{scheme_make_input_port}.}

@function[(void scheme_add_fd_handle
           [void* h]
           [void* fds]
           [int repost])]{

Adds an OS-level semaphore (Windows) or other waitable handle
 (Windows) to the ``@cpp{fd_set}'' @var{fds}. When Racket performs
 a ``@cpp{select}'' to sleep on @var{fds}, it also waits on the given
 semaphore or handle. This feature makes it possible for Racket to
 sleep until it is awakened by an external process.

Racket does not attempt to deallocate the given semaphore or handle,
 and the ``@cpp{select}'' call using @var{fds} may be unblocked due to
 some other file descriptor or handle in @var{fds}. If @var{repost} is
 a true value, then @var{h} must be an OS-level semaphore, and if the
 ``@cpp{select}'' unblocks due to a post on @var{h}, then @var{h} is
 reposted; this allows clients to treat @var{fds}-installed semaphores
 uniformly, whether or not a post on the semaphore was consumed by
 ``@cpp{select}''.

The @cpp{scheme_add_fd_handle} function is useful for implementing
 the second procedure passed to @cpp{scheme_wait_until}, or for
 implementing a custom input port.

On Unix and Mac OS X, this function has no effect.}


@function[(void scheme_add_fd_eventmask
           [void* fds]
           [int mask])]{

Adds an OS-level event type (Windows) to the set of types in the
 ``@cpp{fd_set}'' @var{fds}. When Racket performs a
 ``@cpp{select}'' to sleep on @var{fds}, it also waits on events of
 them specified type. This feature makes it possible for Racket to
 sleep until it is awakened by an external process.

The event mask is only used when some handle is installed with
 @cpp{scheme_add_fd_handle}. This awkward restriction may force you
 to create a dummy semaphore that is never posted.

On Unix, and Mac OS X, this function has no effect.}

@function[(void scheme_add_evt
           [Scheme_Type type]
           [Scheme_Ready_Fun ready]
           [Scheme_Needs_Wakeup_Fun wakeup]
           [Scheme_Wait_Filter_Fun filter]
           [int can_redirect])]{

The argument types are defined as follows:

@verbatim[#:indent 2]{
   typedef int (*Scheme_Ready_Fun)(Scheme_Object *data);
   typedef void (*Scheme_Needs_Wakeup_Fun)(Scheme_Object *data,
                                           void *fds);
   typedef int (*Scheme_Wait_Filter_Fun)(Scheme_Object *data);
}

Extends the set of waitable objects for @racket[sync]
 to those with the type tag @var{type}. If @var{filter} is
 non-@cpp{NULL}, it constrains the new waitable set to those objects
 for which @var{filter} returns a non-zero value.

The @var{ready} and @var{wakeup} functions are used in the same way
 was the arguments to @cpp{scheme_block_until}.

The @var{can_redirect} argument should be @cpp{0}.}

@function[(void scheme_add_evt_through_sema
           [Scheme_Type type]
           [Scheme_Wait_Sema_Fun getsema]
           [Scheme_Wait_Filter_Fun filter])]{

Like @cpp{scheme_add_evt}, but for objects where waiting is based
 on a semaphore. Instead of @var{ready} and @var{wakeup} functions,
 the @var{getsema} function extracts a semaphore for a given object:

@verbatim[#:indent 2]{
   typedef
   Scheme_Object *(*Scheme_Wait_Sema_Fun)(Scheme_Object *data,
                                          int *repost);
}

If a successful wait should leave the semaphore waited, then
 @var{getsema} should set @var{*repost} to @cpp{0}. Otherwise, the
 given semaphore will be re-posted after a successful wait. A
 @var{getsema} function should almost always set @var{*repost} to
 @cpp{1}.}


@function[(void scheme_making_progress)]{

Notifies the scheduler that the current thread is not simply calling
 @cppi{scheme_thread_block} in a loop, but that it is actually
 making progress.}

@function[(int scheme_tls_allocate)]{

Allocates a thread local storage index to be used with
 @cpp{scheme_tls_set} and @cpp{scheme_tls_get}.}

@function[(void scheme_tls_set
           [int index]
           [void* v])]{

Stores a thread-specific value using an index allocated with
@cpp{scheme_tls_allocate}.}

@function[(void* scheme_tls_get
           [int index])]{

Retrieves a thread-specific value installed with @cpp{scheme_tls_set}.
If no thread-specific value is available for the given index, @cpp{NULL} is
returned.}

@function[(Scheme_Object* scheme_call_enable_break
           [Scheme_Prim* prim]
           [int argc]
           [Scheme_Object** argv])]{

Calls @var{prim} with the given @var{argc} and @var{argv} with breaks
 enabled. The @var{prim} function can block, in which case it might be
 interrupted by a break. The @var{prim} function should not block,
 yield, or check for breaks after it succeeds, where ``succeeds''
 depends on the operation. For example,
 @racket[tcp-accept/enable-break] is implemented by wrapping this
 function around the implementation of @racket[tcp-accept]; the
 @racket[tcp-accept] implementation does not block or yield after it
 accepts a connection.}

@function[(Scheme_Object* scheme_make_thread_cell
           [Scheme_Object* def_val]
           [int preserved]
           [Scheme_Object* cell]
           [Scheme_Thread_Cell_Table* cells]
           [Scheme_Object* cell]
           [Scheme_Thread_Cell_Table* cells]
           [Scheme_Object* v])]{

Prevents Racket thread swaps until @cpp{scheme_end_atomic} or
 @cpp{scheme_end_atomic_no_swap} is called. Start-atomic and
 end-atomic pairs can be nested.}

@function[(void scheme_end_atomic)]{

Ends an atomic region with respect to Racket threads. The current
 thread may be swapped out immediately (i.e., the call to
 @cpp{scheme_end_atomic} is assumed to be a safe point for thread
 swaps).}

@function[(void scheme_end_atomic_no_swap)]{

Ends an atomic region with respect to Racket threads, and also
 prevents an immediate thread swap. (In other words, no Racket
 thread swaps will occur until a future safe point.)}

@function[(void scheme_add_swap_callback
                [Scheme_Closure_Func f]
                [Scheme_Object* data])]{

Registers a callback to be invoked just after a Racket thread is
swapped in. The @var{data} is provided back to @var{f} when it is
called, where @cpp{Closure_Func} is defined as follows:

@verbatim[#:indent 2]{
  typedef Scheme_Object *(*Scheme_Closure_Func)(Scheme_Object *);
}}

@function[(void scheme_add_swap_out_callback
                [Scheme_Closure_Func f]
                [Scheme_Object* data])]{

Like @cpp{scheme_add_swap_callback}, but registers a callback to be
invoked just before a Racket thread is swapped out.}
