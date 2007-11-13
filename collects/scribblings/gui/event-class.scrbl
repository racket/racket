#lang scribble/doc
@require["common.ss"]

@defclass/title[event% object% ()]{

An @scheme[event%] object contains information about a control,
keyboard, mouse, or scroll event. See also
@scheme[control-event%], 
@scheme[key-event%],
@scheme[mouse-event%], and
@scheme[scroll-event%].


@defconstructor[([time-stamp (and/c exact? integer?) 0])]{

See @method[event% get-time-stamp] for information about
 @scheme[time-stamp].

}

@defmethod[(get-time-stamp)
           (and/c exact? integer?)]{

Returns the time, in milliseconds, when the event occurred. This time
 is compatible with times reported by MzScheme's
 @scheme[current-milliseconds] procedure.

}

@defmethod[(set-time-stamp [time (and/c exact? integer?)])
           void?]{

Set the time, in milliseconds, when the event occurred. See also
 MzScheme's @scheme[current-milliseconds].

If the supplied value is outside the platform-specific range of time
 values, @|MismatchExn|.

}}

