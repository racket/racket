#lang scribble/doc
@(require "common.rkt")

@defclass/title[event% object% ()]{

An @racket[event%] object contains information about a control,
keyboard, mouse, or scroll event. See also
@racket[control-event%], 
@racket[key-event%],
@racket[mouse-event%], and
@racket[scroll-event%].


@defconstructor[([time-stamp exact-integer? 0])]{

See @method[event% get-time-stamp] for information about
 @racket[time-stamp].

}

@defmethod[(get-time-stamp)
           exact-integer?]{

Returns the time, in milliseconds, when the event occurred. This time
 is compatible with times reported by Racket's
 @racket[current-milliseconds] procedure.

}

@defmethod[(set-time-stamp [time exact-integer?])
           void?]{

Set the time, in milliseconds, when the event occurred. See also
 Racket's @racket[current-milliseconds].

If the supplied value is outside the platform-specific range of time
 values, @|MismatchExn|.

}}

