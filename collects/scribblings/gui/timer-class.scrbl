#lang scribble/doc
@(require "common.rkt")

@defclass/title[timer% object% ()]{

A @racket[timer%] object encapsulates an event-based alarm. To use a
 timer, either instantiate it with a @racket[timer-callback] thunk to
 perform the alarm-based action, or derive a new class and override
 the @method[timer% notify] method to perform the alarm-based
 action. Start a timer with @method[timer% start] and stop it with
 @method[timer% stop]. Supplying an initial @racket[interval] (in
 milliseconds) when creating a timer also starts the timer.

Timers have a relatively high priority in the event queue. Thus, if
 the timer delay is set low enough, repeated notification for a timer
 can preempt user activities (which might be directed at stopping the
 timer). For timers with relatively short delays, call @racket[yield]
 within the @method[timer% notify] procedure to allow guaranteed event
 processing.

See @secref["eventspaceinfo"] for more information about event
 priorities.


@defconstructor[([notify-callback (-> any) void]
                 [interval (or/c (integer-in 0 1000000000) #f) #f]
                 [just-once? any/c #f])]{

The @racket[notify-callback] thunk is called by the default
@method[timer% notify] method when the timer expires.

If @racket[interval] is @racket[#f] (the default), the timer is not
 started; in that case, @method[timer% start] must be called
 explicitly. If @racket[interval] is a number (in milliseconds), then
 @method[timer% start] is called with @racket[interval] and
 @racket[just-once?].

}


@defmethod[(interval)
           (integer-in 0 1000000000)]{

Returns the number of milliseconds between each timer expiration (when
 the timer is running).

}

@defmethod[(notify)
           void?]{

@methspec{

Called (on an event boundary) when the timer's alarm expires.

}
@methimpl{

Calls the @racket[notify-callback] procedure that was provided when the
 object was created.

}}

@defmethod[(start [msec (integer-in 0 1000000000)]
                  [just-once? any/c #f])
           void?]{

Starts (or restarts) the timer. If the timer is already running, its
 alarm time is not changed.

The timer's alarm expires after @racket[msec] milliseconds, at which
point @method[timer% notify] is called (on an event boundary). If
@racket[just-once?] is @racket[#f], the timer expires @italic{every}
@racket[msec] milliseconds until the timer is explicitly
stopped. (More precisely, the timer expires @racket[msec]
milliseconds after @method[timer% notify] returns each time.)
Otherwise, the timer expires only once.

}

@defmethod[(stop)
           void?]{

Stops the timer. A stopped timer never calls
@method[timer% notify]. If the timer has expired but the call to
@method[timer% notify] has not yet been dispatched, the call is removed from the event queue.

}}

