#lang scribble/doc
@(require scribble/struct "mz.rkt" (for-label racket/async-channel))

@(define-syntax-rule (ResultItself x)
   (make-element #f (list "The "
                          (tech "synchronization result")
                          " of " @racket[x] " is " @racket[x] " itself")))

@title[#:tag "sync"]{Events}

@section-index["select"]
@section-index["poll"]

A @deftech{synchronizable event} (or just @defterm{event} for short)
works with the @racket[sync] procedure to coordinate synchronization
among threads. Certain kinds of objects double as events, including
ports and threads. Other kinds of objects exist only for their use as
events.

At any point in time, an event is either @defterm{ready} for
synchronization, or it is not; depending on the kind of event and how
it is used by other threads, an event can switch from not ready to
ready (or back), at any time.  If a thread synchronizes on an event
when it is ready, then the event produces a particular
@deftech{synchronization result}.

Synchronizing an event may affect the state of the event. For example,
when synchronizing a semaphore, then the semaphore's internal count is
decremented, just as with @racket[semaphore-wait]. For most kinds of
events, however (such as a port), synchronizing does not modify the
event's state.

The following act as events in Racket.  An extension or embedding
application can extend the set of primitive events --- in particular,
an eventspace in GRacket is an event --- and new structure types can
generate events (see @racket[prop:evt]).

@itemize[

 @item{@racket[_semaphore] --- a semaphore is ready when
 @racket[semaphore-wait] would not block.  @ResultItself[_semaphore].}

 @item{@racket[_semaphore-peek] --- a semaphore-peek event returned by
 @racket[semaphore-peek-evt] applied to @racket[_semaphore] is ready
 exactly when @racket[_semaphore] is
 ready. @ResultItself[_semaphore-peek].}

 @item{@racket[_channel] --- a channel returned by
 @racket[make-channel] is ready when @racket[channel-get] would not
 block. The channel's result as an event is the same as the
 @racket[channel-get] result.}

 @item{@racket[_channel-put] --- an event returned by
 @racket[channel-put-evt] applied to @racket[_channel] is ready when
 @racket[channel-put] would not block on
 @racket[_channel]. @ResultItself[_channel-put].}

 @item{@racket[_async-channel] --- a channel returned by
 @racket[make-async-channel] is ready when @racket[async-channel-get] would not
 block. The channel's result as an event is the same as the
 @racket[async-channel-get] result.}

 @item{@racket[_async-channel-put] --- an event returned by
 @racket[async-channel-put-evt] applied to @racket[_async-channel] is ready when
 @racket[async-channel-put] would not block on
 @racket[_async-channel]. @ResultItself[_async-channel-put].}

 @item{@racket[_input-port] --- an input port is ready as an event when
 @racket[read-byte] would not block. @ResultItself[_input-port].}

 @item{@racket[_output-port] --- an output port is ready when
 @racket[write-bytes-avail] would not block or
 when the port contains buffered characters and
 @racket[write-bytes-avail*] can flush part of the buffer (although
 @racket[write-bytes-avail] might block). @ResultItself[_output-port].}

 @item{@racket[_closed] --- an event produced by
 @racket[port-closed-evt] applied to @racket[_port] is ready after
 @racket[_port] is closed. @ResultItself[_closed].}

 @item{@racket[_progress] --- an event produced by
 @racket[port-progress-evt] applied to @racket[_input-port] is ready after
 any subsequent read from @racket[_input-port]. @ResultItself[_progress].}

 @item{@racket[_tcp-listener] --- a TCP listener is ready when
 @racket[tcp-accept] would not block.  @ResultItself[_listener].}

 @item{@racket[_thd] --- a thread is ready when @racket[thread-wait]
 would not block. @ResultItself[_thread].}

 @item{@racket[_thread-dead] --- an event returned by
 @racket[thread-dead-evt] applied to @racket[thd] is ready when
 @racket[thd] has terminated.  @ResultItself[_thread-dead].}

 @item{@racket[_thread-resume] --- an event returned by
 @racket[thread-resume-evt] applied to @racket[thd] is ready when
 @racket[thd] subsequently resumes execution (if it was not already
 running). The event's result is @racket[thd].}

 @item{@racket[_thread-suspend] --- an event returned by
 @racket[thread-suspend-evt] applied to @racket[thd] is ready when
 @racket[thd] subsequently suspends execution (if it was not already
 suspended).  The event's result is @racket[thd].}

 @item{@racket[_thread-receive] --- an event returned by
 @racket[thread-receive-evt] is ready
 when the synchronizing thread has a message to
 receive. @ResultItself[_thread-receive].}

 @item{@racket[_alarm] --- an event returned by @racket[alarm-evt] is
 ready after a particular date and time.  @ResultItself[_alarm].}

 @item{@racket[_subprocess] --- a subprocess is ready when
 @racket[subprocess-wait] would not block.
 @ResultItself[_subprocess].}

 @item{@racket[_will-executor] --- a @tech{will executor} is ready when
 @racket[will-execute] would not block.
 @ResultItself[_will-executor].}

 @item{@racket[_custodian-box] --- a @tech{custodian box} is ready when
 its custodian is shut down. @ResultItself[_custodian-box].}

 @item{@racket[_udp] --- an event returned by @racket[udp-send-evt] or
 @racket[udp-receive!-evt] is ready when a send or receive on the
 original socket would block, respectively. @ResultItself[_udp].}

 @item{@racket[_log-receiver] --- a @tech{log receiver} as produced by
 @racket[make-log-receiver] is ready when a logged message is
 available. The event's result is a vector, as described with
 @racket[make-log-receiver].}

 @item{@racket[_choice] --- an event returned by @racket[choice-evt] is
 ready when one or more of the @racket[_evt]s supplied to
 @racket[choice-evt] are ready. If the choice event is chosen, one of
 its ready @racket[_evt]s is chosen pseudo-randomly, and the result is
 the chosen @racket[_evt]'s result.}

 @item{@racket[_wrap] --- an event returned by @racket[wrap-evt]
 applied to @racket[_evt] and @racket[_proc] is ready when @racket[_evt] is
 ready. The event's result is obtained by a call to @racket[_proc] (with
 breaks disabled) on the result of @racket[evt].}

 @item{@racket[_handle] --- an event returned by @racket[handle-evt]
 applied to @racket[_evt] and @racket[_proc] is ready when @racket[_evt] is
 ready. The event's result is obtained by a tail call to @racket[_proc] on
 the result of @racket[_evt].}

 @item{@elemtag["guard-evt"]{@racket[_guard]} --- an event returned by @racket[guard-evt] applied
 to @racket[_thunk] generates a new event every time that @racket[_guard] is
 used with @racket[sync] (or whenever it is part of a choice event
 used with @racket[sync], etc.); the generated event is the result of
 calling @racket[_thunk] when the synchronization begins; if @racket[_thunk]
 returns a non-event, then @racket[_thunk]'s result is replaced with an
 event that is ready and whose result is @racket[_guard].}

 @item{@elemtag["nack-guard-evt"]{@racket[_nack-guard]} --- an event
 returned by @racket[nack-guard-evt] applied to @racket[_proc]
 generates a new event every time that @racket[_nack-guard] is used
 with @racket[sync] (or whenever it is part of a choice event used
 with @racket[sync], etc.); the generated event is the result of
 calling @racket[_proc] with a NACK (``negative acknowledgment'') event
 when the synchronization begins; if @racket[_proc] returns a
 non-event, then @racket[_proc]'s result is replaced with an event that
 is ready and whose result is @racket[_nack-guard].

 If the event from @racket[_proc] is not ultimately chosen as the
 unblocked event, then the NACK event supplied to @racket[_proc]
 becomes ready with a @|void-const| value.  This NACK event becomes ready
 when the event is abandoned because some other event is chosen,
 because the synchronizing thread is dead, or because control escaped
 from the call to @racket[sync] (even if @racket[_nack-guard]'s @racket[_proc]
 has not yet returned a value). If the event returned by @racket[_proc] is
 chosen, then the NACK event never becomes ready.}

 @item{@elemtag["poll-guard-evt"]{@racket[_poll-guard]} --- an event
 returned by @racket[poll-guard-evt] applied to @racket[_proc]
 generates a new event every time that @racket[poll-guard] is used
 with @racket[sync] (or whenever it is part of a choice event used
 with @racket[sync], etc.); the generated event is the result of
 calling @racket[_proc] with a boolean: @racket[#t] if the event will
 be used for a poll, @racket[#f] for a blocking synchronization.

 If @racket[#t] is supplied to @racket[_proc], if breaks are disabled, if
 the polling thread is not terminated, and if polling the resulting
 event produces a result, the event will certainly be chosen for its
 result.}

 @item{@racket[_struct] --- a structure whose type has the
 @racket[prop:evt] property identifies/generates an event through the
 property.}

 @item{@racket[always-evt] --- a constant event that is always
 ready. @ResultItself[always-evt].}

 @item{@racket[never-evt] --- a constant event that is never ready.}

 @item{@elemtag["system-idle-evt"]{@racket[_idle]} --- an event
   produced by @racket[system-idle-evt] is ready when, if this event
   were replaced by @racket[never-evt], no thread in the system would
   be available to run.  In other words, all threads must be suspended
   or blocked on events with timeouts that have not yet expired. The
   event's result is @|void-const|.}

 @item{@racket[_place-channel] --- a @tech{place channel} is ready when
 @racket[place-channel-get] would not block. The channel's result as an
 event is the same as the @racket[place-channel-get] result.}

 @item{@racket[_place-dead] --- an event returned by
 @racket[(place-dead-evt _p)] is ready when @racket[_p] has
 terminated.  @ResultItself[_place-dead].}

 ]

@;------------------------------------------------------------------------

@defproc[(evt? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{synchronizable event},
@racket[#f] otherwise.}


@defproc[(sync [evt evt?] ...+) any]{

Blocks as long as none of the @tech{synchronizable events}
@racket[evt]s are ready, as defined above.

When at least one @racket[evt] is ready, its @tech{synchronization
result} (often @racket[evt] itself) is returned.  If multiple
@racket[evt]s are ready, one of the @racket[evt]s is chosen
pseudo-randomly for the result; the
@racket[current-evt-pseudo-random-generator] parameter sets the
random-number generator that controls this choice.}


@defproc[(sync/timeout [timeout (or/c #f (and/c real? (not/c negative?)) (-> any))]
                       [evt evt?] ...+) 
          any]{

Like @racket[sync] if @racket[timeout] is @racket[#f]. If
@racket[timeout] is a real number, then the result is @racket[#f]
if @racket[timeout] seconds pass without a
successful synchronization. If @racket[timeout] is a procedure, then
it is called in tail position if polling the @racket[evt]s discovers
no ready events.

A zero value for @racket[timeout] is equivalent to @racket[(lambda ()
#f)]. In either case, each @racket[evt] is checked at least once
before returning @racket[#f] or calling @racket[timeout].

See also @racket[alarm-evt] for an alternative timeout mechanism.}


@defproc[(sync/enable-break [evt evt?] ...+) any]{

Like @racket[sync], but breaking is enabled (see
@secref["breakhandler"]) while waiting on the @racket[evt]s. If
breaking is disabled when @racket[sync/enable-break] is called, then
either all @racket[evt]s remain unchosen or the @racket[exn:break]
exception is raised, but not both.}


@defproc[(sync/timeout/enable-break [timeout (or/c #f (and/c real? (not/c negative?)) (-> any))]
                                    [evt evt?] ...+) 
         any]{

Like @racket[sync/enable-break], but with a timeout as for @racket[sync/timeout].}


@defproc[(choice-evt [evt evt?] ...) evt?]{

Creates and returns a single event that combines the
@racket[evt]s. Supplying the result to @racket[sync] is the same as
supplying each @racket[evt] to the same call.}


@defproc[(wrap-evt [evt (and/c evt? (not/c handle-evt?))]
                   [wrap (any/c . -> . any)]) 
         evt?]{

Creates an event that is in a ready when @racket[evt] is ready, but
whose result is determined by applying @racket[wrap] to the result of
@racket[evt]. The call to @racket[wrap] is
@racket[parameterize-break]ed to disable breaks initially. The
@racket[evt] cannot be an event created by @racket[handle-evt] or any
combination of @racket[choice-evt] involving an event from
@racket[handle-evt].}

@defproc[(handle-evt [evt (and/c evt? (not/c handle-evt?))]
                     [handle (any/c . -> . any)]) 
         handle-evt?]{

Like @racket[wrap], except that @racket[handle] is called in tail
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
@racket[system-idle-evt] procedure is always the same event.}


@defproc[(alarm-evt [msecs nonnegative-number?]) evt]{

Returns a synchronizable event that is not ready when
@racket[(current-inexact-milliseconds)] would return a value that is
less than @racket[msecs], and it is ready when
@racket[(current-inexact-milliseconds)] would return a value that is
more than @racket[msecs].}


@defproc[(handle-evt? [evt evt?]) boolean?]{

Returns @racket[#t] if @racket[evt] was created by @racket[handle-evt]
or by @racket[choice-evt] applied to another event for which
@racket[handle-evt?] produces @racket[#t]. Such events are illegal as
an argument to @racket[handle-evt] or @racket[wrap-evt], because they
cannot be wrapped further. For any other event, @racket[handle-evt?]
produces @racket[#f], and the event is a legal argument to
@racket[handle-evt] or @racket[wrap-evt] for further wrapping.}

@;------------------------------------------------------------------------
@defthing[prop:evt struct-type-property?]{

A @tech{structure type property} that identifies structure types whose
 instances can serve as synchronizable events. The property value can
 be any of the following:

@itemize[
 
 @item{An event @racket[_evt]: In this case, using the structure as an
 event is equivalent to using @racket[_evt].}

 @item{A procedure @racket[_proc] of one argument: In this case, the
 structure is similar to an event generated
 by @racket[guard-evt], except that the would-be guard
 procedure @racket[_proc] receives the structure as an argument, instead
 of no arguments.}

 @item{An exact, non-negative integer between @racket[0] (inclusive)
 and the number of non-automatic fields in the structure type
 (exclusive, not counting supertype fields): The integer identifies a
 field in the structure, and the field must be designated as
 immutable. If the field contains an object or an event-generating
 procedure of one argument, the event or procedure is used as
 above. Otherwise, the structure acts as an event that is never
 ready.}

]

Instances of a structure type with the @racket[prop:input-port] or
@racket[prop:output-port] property are also synchronizable by virtue
of being a port. If the structure type has more than one of
@racket[prop:evt], @racket[prop:input-port], and
@racket[prop:output-port], then the @racket[prop:evt] value (if any)
takes precedence for determing the instance's behavior as an event,
and the @racket[prop:input-port] property takes precedence over
@racket[prop:output-port] for synchronization.

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
@racket[sync] for events created by @racket[choice-evt].}
