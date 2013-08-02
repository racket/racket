#lang scribble/doc
@(require "common.rkt")

@title[#:tag "eventspace-funcs"]{Eventspaces}

@defproc[(make-eventspace)
         eventspace?]{
Creates and returns a new @tech{eventspace} value. The new eventspace is
 created as a child of the current eventspace. The eventspace is used
 by making it the current eventspace with the
 @racket[current-eventspace] parameter.

See @|eventspacediscuss| for more information about eventspaces.

}

@defparam[current-eventspace e eventspace?]{

A parameter @|SeeMzParam| that determines the current eventspace.

See @|eventspacediscuss| for more information about eventspaces.

}


@defproc[(eventspace? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is an eventspace value or @racket[#f]
 otherwise.

See @|eventspacediscuss| for more information about eventspaces.
}


@defparam[event-dispatch-handler handler (eventspace? . -> . any)]{

A parameter @|SeeMzParam| that determines the current event
 dispatch handler. The event dispatch handler is called by an
 eventspace's handler thread for every queue-based event to be
 processed in the eventspace. The only argument to the handler is the
 eventspace in which an event should be dispatched. The event dispatch
 handler gives the programmer control over the timing of event
 dispatching, but not the order in which events are dispatched within
 a single eventspace.

An event dispatch handler must ultimately call the primitive event
 dispatch handler. If an event dispatch handler returns without
 calling the primitive handler, then the primitive handler is called
 directly by the eventspace handler thread.
}


@defproc[(eventspace-event-evt [e eventspace? (current-eventspace)]) evt?]{

Produces a synchronizable event (see @racket[sync]) that is ready when
a GUI event (mouse or keyboard action, update event, timer, queued
callback, etc.) is ready for dispatch in @racket[e]. That is, the
result event is ready when @racket[(yield)] for the eventspace
@racket[e] would dispatch a GUI event. The synchronization result is
the eventspace @racket[e] itself.
}


@defproc[(check-for-break)
         boolean?]{
Inspects the event queue of the current eventspace, searching for a
 Shift-Ctl-C (Unix, Windows) or Cmd-. (Mac OS X) key combination. Returns
 @racket[#t] if such an event was found (and the event is dequeued) or
 @racket[#f] otherwise.

}

@defproc[(get-top-level-windows)
         (listof (or/c (is-a?/c frame%) (is-a?/c dialog%)))]{
Returns a list of visible top-level frames and dialogs in the current
 eventspace.

}

@defproc[(get-top-level-focus-window)
         (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]{
Returns the top level window in the current eventspace that has the
 keyboard focus (or contains the window with the keyboard focus), or
 @racket[#f] if no window in the current eventspace has the focus.

}

@defproc[(get-top-level-edit-target-window)
         (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]{
Returns the top level window in the current eventspace that is visible
 and most recently had the keyboard focus (or contains the window that
 had the keyboard focus), or @racket[#f] if there is no visible window
 in the current eventspace.

}

@defproc*[([(special-control-key [on? any/c])
            void?]
           [(special-control-key)
            boolean?])]{

Enables or disables special Control key handling (Mac OS X). When Control
 is treated as a special key, the system's key-mapper is called
 without Control for keyboard translations. For some languages,
 Control key presses must be seen by the system translation, so this
 mode should be turned off, but the default is on.

If @racket[on?] is provided and @racket[#f], Control is passed to the system
translation as normal. This setting affects all windows and
eventspaces.

If no argument is provided, the result is @racket[#t] if Control is
currently treated specially, @racket[#f] otherwise.

}

@defproc*[([(special-option-key [on? any/c])
            void?]
           [(special-option-key)
            boolean?])]{
Enables or disables special Option key handling (Mac OS X). When
 Option is treated as a special key, the system's key-mapper is called
 without Option for keyboard translations. By default, Option is not
 special.

If @racket[on?] is provided @racket[#f], Option is passed to the
 system translation as normal. This setting affects all windows and
 eventspaces.

If no argument is provided, the result is @racket[#t] if Option is
 currently treated specially, @racket[#f] otherwise.
}

@defproc[(queue-callback [callback (-> any)]
                         [high-priority? any/c #t])
         void?]{
Installs a procedure to be called via the current eventspace's event
 queue. The procedure is called once in the same way and under the
 same restrictions that a callback is invoked to handle a method.

A second (optional) boolean argument indicates whether the callback
 has a high or low priority in the event queue. See
 @|eventspacediscuss| for information about the priority of events.

}

@defproc*[([(yield)
            boolean?]
           [(yield [v (or/c 'wait evt?)])
            any/c])]{
@;@index{pause}@index{wait}
Yields control to event dispatching. See
 @secref["eventspaceinfo"] for details.

A handler procedure invoked by the system during a call to
 @racket[yield] can itself call @racket[yield], creating
 an additional level of nested (but single-threaded) event handling.

See also @racket[sleep/yield].

If no argument is provided, @racket[yield] dispatches an unspecified
 number of events, but only if the current thread is the current
 eventspace's handler thread (otherwise, there is no effect). The
 result is @racket[#t] if any events may have been handled,
 @racket[#f] otherwise.

If @racket[v] is @indexed-racket['wait], and @racket[yield] is called
 in the handler thread of an eventspace, then @racket[yield] starts
 processing events in that eventspace until

@itemize[

  @item{no top-level windows in the eventspace are visible;}

  @item{no timers in the eventspace are running;}

  @item{no callbacks are queued in the eventspace; and}

  @item{no @racket[menu-bar%] has been created for the eventspace
        with @racket['root] (i.e., creating a @racket['root] menu bar
        prevents an eventspace from ever unblocking).}

]

When called in a non-handler thread, @racket[yield] returns
 immediately. In either case, the result is @racket[#t].

Evaluating @racket[(yield 'wait)] is thus similar to
 @racket[(yield (current-eventspace))], except that it is
 sensitive to whether the current thread is a handler thread, instead
 of the value of the @racket[current-eventspace] parameter.

If @racket[v] is an event in Racket's sense (not to be confused with
 a GUI event), @racket[yield] blocks on @racket[v] in the same way as
 @racket[sync], except that it may start a @racket[sync] on @racket[v]
 multiple times (but it will complete a @racket[sync] on @racket[v] at
 most one time). If the current thread is the current eventspace's
 handler thread, events are dispatched until a @racket[v] sync
 succeeds on an event boundary. For other threads, calling
 @racket[yield] with a Racket event is equivalent to calling
 @racket[sync]. In either case, the result is the same that of
 @racket[sync]; however, if a wrapper procedure is associated with
 @racket[v] via @racket[handle-evt], it is not called in tail position
 with respect to the @racket[yield].

Always use @racket[(yield v)] instead of a busy-wait loop.
}

@defproc[(sleep/yield [secs (and/c real? (not/c negative?))])
         void?]{
Blocks for at least the specified number of seconds, handling events
 meanwhile if the current thread is the current eventspace's handler
 thread (otherwise, @racket[sleep/yield] is equivalent to
 @racket[sleep]).

}

@defproc[(eventspace-shutdown? [e eventspace?])
         boolean?]{
Returns @racket[#t] if the given eventspace has been shut down by its
 custodian, @racket[#f] otherwise. Attempting to create a new window,
 timer, or explicitly queued event in a shut-down eventspace raises
 the @racket[exn:fail] exception.

Attempting to use certain methods of windows and timers in a shut-down
 eventspace also raises the @racket[exn:fail] exception, but the
@xmethod[area<%> get-top-level-window] and
@xmethod[top-level-window<%> get-eventspace] methods work even after the area's eventspace is shut down.

}

@defproc[(eventspace-handler-thread [e eventspace?])
         (or/c thread? #f)]{
Returns the handler thread of the given eventspace. If the handler
 thread has terminated (e.g., because the eventspace was shut down), the
 result is @racket[#f].

}
