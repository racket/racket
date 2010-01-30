#lang scribble/doc
@(require scribble/struct
          "mz.ss")

@(define (ResultItself x)
   (make-element #f (list "The "
                          (tech "synchronization result")
                          " of " x " is " x " itself")))

@title[#:tag "sync"]{Events}

@section-index["select"]
@section-index["poll"]

A @deftech{synchronizable event} (or just @defterm{event} for short)
works with the @scheme[sync] procedure to coordinate synchronization
among threads. Certain kinds of objects double as events, including
ports and threads. Other kinds of objects exist only for their use as
events.

At an point in time, an event is either @defterm{ready} for
synchronization, or it is not; depending on the kind of event and how
it is used by other threads, an event can switch from not ready to
ready (or back), at any time.  If a thread synchronizes on an event
when it is ready, then the event produces a particular
@deftech{synchronization result}.

Synchronizing an event may affect the state of the event. For example,
when synchronizing a semaphore, then the semaphore's internal count is
decremented, just as with @scheme[semaphore-wait]. For most kinds of
events, however (such as a port), synchronizing does not modify the
event's state.

The following act as events in stand-alone MzScheme.  An extension or
embedding application can extend the set of primitive events --- in
particular, an eventspace in MrEd is an event --- and new structure
types can generate events (see @scheme[prop:evt]).

@itemize[

 @item{@scheme[_semaphore] --- a semaphore is ready when
 @scheme[semaphore-wait] would not block.  @ResultItself{semaphore}.}

 @item{@scheme[_semaphore-peek] --- a semaphore-peek event returned by
 @scheme[semaphore-peek-evt] applied to @scheme[_semaphore] is ready
 exactly when @scheme[_semaphore] is
 ready. @ResultItself{semaphore-peek}.}

 @item{@scheme[_channel] --- a channel returned by
 @scheme[make-channel] is ready when @scheme[channel-get] would not
 block. The channel's result as an event is the same as the
 @scheme[channel-get] result.}

 @item{@scheme[_channel-put] --- an event returned by
 @scheme[channel-put-evt] applied to @scheme[_channel] is ready when
 @scheme[channel-put] would not block on
 @scheme[_channel]. @ResultItself{channel-put}.}

 @item{@scheme[_input-port] --- an input port is ready as an event when
 @scheme[read-byte] would not block. @ResultItself{input-port}.}

 @item{@scheme[_output-port] --- an output port is ready when
 @scheme[write-bytes-avail] would not block or
 when the port contains buffered characters and
 @scheme[write-bytes-avail*] can flush part of the buffer (although
 @scheme[write-bytes-avail] might block). @ResultItself{output-port}.}

 @item{@scheme[_progress] --- an event produced by
 @scheme[port-progress-evt] applied to @scheme[_input-port] is ready after
 any subsequent read from @scheme[_input-port]. @ResultItself{progress}.}

 @item{@scheme[_tcp-listener] --- a TCP listener is ready when
 @scheme[tcp-accept] would not block.  @ResultItself{listener}.}

 @item{@scheme[_thd] --- a thread is ready when @scheme[thread-wait]
 would not block. @ResultItself{thread}.}

 @item{@scheme[_thread-dead] --- an event returned by
 @scheme[thread-dead-evt] applied to @scheme[thd] is ready when
 @scheme[thd] has terminated.  @ResultItself{thread-dead}.}

 @item{@scheme[_thread-resume] --- an event returned by
 @scheme[thread-resume-evt] applied to @scheme[thd] is ready when
 @scheme[thd] subsequently resumes execution (if it was not already
 running). The event's result is @scheme[thd].}

 @item{@scheme[_thread-suspend] --- an event returned by
 @scheme[thread-suspend-evt] applied to @scheme[thd] is ready when
 @scheme[thd] subsequently suspends execution (if it was not already
 suspended).  The event's result is @scheme[thd].}

 @item{@scheme[_alarm] --- an event returned by @scheme[alarm-evt] is
 ready after a particular date and time.  @ResultItself{alarm}.}

 @item{@scheme[_subprocess] --- a subprocess is ready when
 @scheme[subprocess-wait] would not block.
 @ResultItself{subprocess}.}

 @item{@scheme[_will-executor] --- a will executor is ready when
 @scheme[will-execute] would not block.
 @ResultItself{will-executor}.}

 @item{@scheme[_udp] --- an event returned by @scheme[udp-send-evt] or
 @scheme[udp-receive!-evt] is ready when a send or receive on the
 original socket would block, respectively. @ResultItself{udp}.}

 @item{@scheme[_log-receiver] --- a @tech{log receiver} as produced by
 @scheme[make-log-receiver] is ready when a logged message is
 available. The event's result is a vector, as described with
 @scheme[make-log-receiver].}

 @item{@scheme[_choice] --- an event returned by @scheme[choice-evt] is
 ready when one or more of the @scheme[_evt]s supplied to
 @scheme[chocie-evt] are ready. If the choice event is chosen, one of
 its ready @scheme[_evt]s is chosen pseudo-randomly, and the result is
 the chosen @scheme[_evt]'s result.}

 @item{@scheme[_wrap] --- an event returned by @scheme[wrap-evt]
 applied to @scheme[_evt] and @scheme[_proc] is ready when @scheme[_evt] is
 ready. The event's result is obtained by a call to @scheme[_proc] (with
 breaks disabled) on the result of @scheme[evt].}

 @item{@scheme[_handle] --- an event returned by @scheme[handle-evt]
 applied to @scheme[_evt] and @scheme[_proc] is ready when @scheme[_evt] is
 ready. The event's result is obtained by a tail call to @scheme[_proc] on
 the result of @scheme[_evt].}

 @item{@elemtag["guard-evt"]{@scheme[_guard]} --- an event returned by @scheme[guard-evt] applied
 to @scheme[_thunk] generates a new event every time that @scheme[_guard] is
 used with @scheme[sync] (or whenever it is part of a choice event
 used with @scheme[sync], etc.); the generated event is the result of
 calling @scheme[_thunk] when the synchronization begins; if @scheme[_thunk]
 returns a non-event, then @scheme[_thunk]'s result is replaced with an
 event that is ready and whose result is @scheme[_guard].}

 @item{@elemtag["nack-guard-evt"]{@scheme[_nack-guard]} --- an event
 returned by @scheme[nack-guard-evt] applied to @scheme[_proc]
 generates a new event every time that @scheme[_nack-guard] is used
 with @scheme[sync] (or whenever it is part of a choice event used
 with @scheme[sync], etc.); the generated event is the result of
 calling @scheme[_proc] with a NACK (``negative acknowledgment'') event
 when the synchronization begins; if @scheme[_proc] returns a
 non-event, then @scheme[_proc]'s result is replaced with an event that
 is ready and whose result is @scheme[_nack-guard].

 If the event from @scheme[_proc] is not ultimately chosen as the
 unblocked event, then the NACK event supplied to @scheme[_proc]
 becomes ready with a @|void-const| value.  This NACK event becomes ready
 when the event is abandoned because some other event is chosen,
 because the synchronizing thread is dead, or because control escaped
 from the call to @scheme[sync] (even if @scheme[_nack-guard]'s @scheme[_proc]
 has not yet returned a value). If the event returned by @scheme[_proc] is
 chosen, then the NACK event never becomes ready.}

 @item{@elemtag["poll-guard-evt"]{@scheme[_poll-guard]} --- an event
 returned by @scheme[poll-guard-evt] applied to @scheme[_proc]
 generates a new event every time that @scheme[poll-guard] is used
 with @scheme[sync] (or whenever it is part of a choice event used
 with @scheme[sync], etc.); the generated event is the result of
 calling @scheme[_proc] with a boolean: @scheme[#t] if the event will
 be used for a poll, @scheme[#f] for a blocking synchronization.

 If @scheme[#t] is supplied to @scheme[_proc], if breaks are disabled, if
 the polling thread is not terminated, and if polling the resulting
 event produces a result, the event will certainly be chosen for its
 result.}

 @item{@scheme[_struct] --- a structure whose type has the
 @scheme[prop:evt] property identifies/generates an event through the
 property.}

 @item{@scheme[always-evt] --- a constant event that is always
 ready. @ResultItself{@scheme[always-evt]}.}

 @item{@scheme[never-evt] --- a constant event that is never ready.}

 @item{@elemtag["system-idle-evt"]{@scheme[_idle]} --- an event
   produced by @scheme[system-idle-evt] is ready when, if this event
   were replaced by @scheme[never-evt], no thread in the system would
   be available to run.  In other words, all threads must be suspended
   or blocked on events with timeouts that have not yet expired. The
   event's result is @|void-const|.}

 ]

@;------------------------------------------------------------------------

@defproc[(evt? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{synchronizable event},
@scheme[#f] otherwise.}


@defproc[(sync [evt evt?] ...+) any]{

Blocks as long as none of the @tech{synchronizable events}
@scheme[evt]s are ready, as defined above.

When at least one @scheme[evt] is ready, its @tech{synchronization
result} (often @scheme[evt] itself) is returned.  If multiple
@scheme[evt]s are ready, one of the @scheme[evt]s is chosen
pseudo-randomly for the result; the
@scheme[current-evt-pseudo-random-generator] parameter sets the
random-number generator that controls this choice.}


@defproc[(sync/timeout [timeout-secs (or/c nonnegative-number? #f)]
                       [evt evt?] ...+) 
          any]{

Like @scheme[sync], but returns @scheme[#f] if @scheme[timeout-secs]
is not @scheme[#f] and if @scheme[timeout-secs] seconds pass without a
successful synchronization.

If @scheme[timeout-secs] is @scheme[0], each @scheme[evt] is checked
at least once, so a @scheme[timeout-secs] value of @scheme[0] can be
used for polling.

See also @scheme[alarm-evt] for an alternative timeout mechanism.}


@defproc[(sync/enable-break [evt evt?] ...+) any]{

Like @scheme[sync], but breaking is enabled (see
@secref["breakhandler"]) while waiting on the @scheme[evt]s. If
breaking is disabled when @scheme[sync/enable-break] is called, then
either all @scheme[evt]s remain unchosen or the @scheme[exn:break]
exception is raised, but not both.}


@defproc[(sync/timeout/enable-break [timeout-secs (or/c nonnegative-number? #f)]
                                    [evt evt?] ...+) 
         any]{

Like @scheme[sync/enable-break], but with a timeout in seconds (or
@scheme[#f]), as for @scheme[sync/timeout].}


@defproc[(choice-evt [evt evt?] ...) evt?]{

Creates and returns a single event that combines the
@scheme[evt]s. Supplying the result to @scheme[sync] is the same as
supplying each @scheme[evt] to the same call.}


@defproc[(wrap-evt [evt (and/c evt? (not/c handle-evt?))]
                   [wrap (any/c . -> . any)]) 
         evt?]{

Creates an event that is in a ready when @scheme[evt] is ready, but
whose result is determined by applying @scheme[wrap] to the result of
@scheme[evt]. The call to @scheme[wrap] is
@scheme[parameterize-break]ed to disable breaks initially. The
@scheme[evt] cannot be an event created by @scheme[handle-evt] or any
combination of @scheme[choice-evt] involving an event from
@scheme[handle-evt].}

@defproc[(handle-evt [evt (and/c evt? (not/c handle-evt?))]
                     [handle (any/c . -> . any)]) 
         evt?]{

Like @scheme[wrap], except that @scheme[handle] is called in tail
position with respect to the synchronization request, and without
breaks explicitly disabled.}

@defproc[(guard-evt [generator (-> evt?)]) evt?]{

Creates a value that behaves as an event, but that is actually an
event generator. For details, see @elemref["guard-evt"]{the
overview}.}

@defproc[(nack-guard-evt [generator (evt? . -> . evt?)]) evt?]{

Creates a value that behaves as an event, but that is actually an
event generator; the generator procedure receives an event that
becomes ready with a @|void-const| value if the generated event was
not ultimately chosen. For details, see
@elemref["nack-guard-evt"]{the overview}.}

@defproc[(poll-guard-evt [generator (boolean? . -> . evt?)]) evt?]{

Creates a value that behaves as an event, but that is actually an
event generator; the generator procedure receives a boolean indicating
whether the event is used for polling. For details, see
@elemref["poll-guard-evt"]{the overview}.}

@defthing[always-evt evt?]{A constant event that is always ready, with
itself as its result.}

@defthing[never-evt evt?]{A constant event that is never ready.}


@defproc[(system-idle-evt) evt?]{Returns an event that is ready when
the system is otherwise idle; see @elemref["system-idle-evt"]{the
overview} for more information. The result of the
@scheme[system-idle-evt] procedure is always the same event.}


@defproc[(alarm-evt [msecs nonnegative-number?]) evt]{

Returns a synchronizable event that is not ready when
@scheme[(current-inexact-milliseconds)] would return a value that is
less than @scheme[msecs], and it is ready when
@scheme[(current-inexact-milliseconds)] would return a value that is
more than @scheme[msecs].}


@defproc[(handle-evt? [evt evt?]) boolean?]{

Returns @scheme[#t] if @scheme[evt] was created by @scheme[handle-evt]
or by @scheme[choice-evt] applied to another event for which
@scheme[handle-evt?] produces @scheme[#t]. Such events are illegal as
an argument to @scheme[handle-evt] or @scheme[wrap-evt], because they
cannot be wrapped further. For any other event, @scheme[handle-evt?]
produces @scheme[#f], and the event is a legal argument to
@scheme[handle-evt] or @scheme[wrap-evt] for further wrapping.}

@;------------------------------------------------------------------------
@defthing[prop:evt struct-type-property?]{

A @tech{structure type property} that identifies structure types whose
 instances can serve as synchronizable events. The property value can
 be any of the following:

@itemize[
 
 @item{An event @scheme[_evt]: In this case, using the structure as an
 event is equivalent to using @scheme[_evt].}

 @item{A procedure @scheme[_proc] of one argument: In this case, the
 structure is similar to an event generated
 by @scheme[guard-evt], except that the would-be guard
 procedure @scheme[_proc] receives the structure as an argument, instead
 of no arguments.}

 @item{An exact, non-negative integer between @scheme[0] (inclusive)
 and the number of non-automatic fields in the structure type
 (exclusive, not counting supertype fields): The integer identifies a
 field in the structure, and the field must be designated as
 immutable. If the field contains an object or an event-generating
 procedure of one argument, the event or procedure is used as
 above. Otherwise, the structure acts as an event that is never
 ready.}

]

Instances of a structure type with the @scheme[prop:input-port] or
@scheme[prop:output-port] property are also synchronizable by virtue
of being a port. If the structure type has more than one of
@scheme[prop:evt], @scheme[prop:input-port], and
@scheme[prop:output-port], then the @scheme[prop:evt] value (if any)
takes precedence for determing the instance's behavior as an event,
and the @scheme[prop:input-port] property takes precedence over
@scheme[prop:output-port] for synchronization.

@examples[
(define-struct wt (base val)
               #:property prop:evt (struct-field-index base))

(define sema (make-semaphore))
(sync/timeout 0 (make-wt sema #f))
(semaphore-post sema)
(sync/timeout 0 (make-wt sema #f))
(semaphore-post sema)
(sync/timeout 0 (make-wt (lambda (self) (wt-val self)) sema))
(semaphore-post sema)
(define my-wt (make-wt (lambda (self) (wrap-evt
                                       (wt-val self)
                                       (lambda (x) self)))
                       sema))
(sync/timeout 0 my-wt)
(sync/timeout 0 my-wt)
]}


@defparam[current-evt-pseudo-random-generator generator pseudo-random-generator?]{

A parameter that determines the pseudo-random number generator used by
@scheme[sync] for events created by @scheme[choice-evt].}
