#lang scribble/doc
@(require "common.rkt")

@defclass/title[mouse-event% event% ()]{

A @racket[mouse-event%] object encapsulates a mouse event.
 Mouse events are primarily processed by
@xmethod[window<%> on-subwindow-event] and
@xmethod[canvas<%> on-event].

See also @|mousekeydiscuss|.




@defconstructor[([event-type (or/c 'enter 'leave 'left-down 'left-up 
                                   'middle-down 'middle-up 
                                   'right-down 'right-up 'motion)]
                 [left-down any/c #f]
                 [middle-down any/c #f]
                 [right-down any/c #f]
                 [x exact-integer? 0]
                 [y exact-integer? 0]
                 [shift-down any/c #f]
                 [control-down any/c #f]
                 [meta-down any/c #f]
                 [alt-down any/c #f]
                 [time-stamp exact-integer? 0]
                 [caps-down any/c #f])]{

Creates a mouse event for a particular type of event. The event types
 are:

@itemize[
@item{@racket['enter] --- mouse pointer entered the window}
@item{@racket['leave] --- mouse pointer left the window}
@item{@racket['left-down] --- left mouse button pressed}
@item{@racket['left-up] --- left mouse button released}
@item{@racket['middle-down] --- middle mouse button pressed}
@item{@racket['middle-up] --- middle mouse button released}
@item{@racket['right-down] --- right mouse button pressed (Mac OS X: click with control key pressed)}
@item{@racket['right-up] --- right mouse button released (Mac OS X: release with control key pressed)}
@item{@racket['motion] --- mouse moved, with or without button(s) pressed}
]

See the corresponding @racketidfont{get-} and @racketidfont{set-}
 methods for information about @racket[left-down],
 @racket[middle-down], @racket[right-down], @racket[x], @racket[y],
 @racket[shift-down], @racket[control-down], @racket[meta-down],
 @racket[alt-down], @racket[time-stamp], and @racket[caps-down].

}

@defmethod[(button-changed? [button (or/c 'left 'middle 'right 'any) 'any])
           boolean?]{

Returns @racket[#t] if this was a mouse button press or release event,
 @racket[#f] otherwise. See also
@method[mouse-event% button-up?] and
@method[mouse-event% button-down?].

If @racket[button] is not @racket['any], then @racket[#t] is only returned
 if it is a release event for a specific button.

}

@defmethod[(button-down? [button (or/c 'left 'middle 'right 'any) 'any])
           boolean?]{

Returns @racket[#t] if the event is for a button press, @racket[#f]
 otherwise.

If @racket[button] is not @racket['any], then @racket[#t] is only returned
 if it is a press event for a specific button.

}

@defmethod[(button-up? [button (or/c 'left 'middle 'right 'any) 'any])
           boolean?]{

Returns @racket[#t] if the event is for a button release, @racket[#f]
 otherwise. (As noted in @|mousekeydiscuss|, button release events are
 sometimes dropped.)

If @racket[button] is not @racket['any], then @racket[#t] is only returned
 if it is a release event for a specific button.

}

@defmethod[(dragging?)
           boolean?]{

Returns @racket[#t] if this was a dragging event (motion while a button
 is pressed), @racket[#f] otherwise.

}

@defmethod[(entering?)
           boolean?]{

Returns @racket[#t] if this event is for the mouse entering a window,
 @racket[#f] otherwise.

When the mouse button is up, an enter/leave event notifies a window
 that it will start/stop receiving mouse events. When the mouse button
 is down, however, the window receiving the mouse-down event receives
 all mouse events until the button is released; enter/leave events are
 not sent to other windows, and are not reliably delivered to the
 click-handling window (since the window can detect movement out of
 its region via @method[mouse-event% get-x] and @method[mouse-event%
 get-y]). See also @|mousekeydiscuss|.

}


@defmethod[(get-alt-down)
           boolean?]{

Returns @racket[#t] if the Option (Mac OS X) key was down for the
 event. When the Alt key is pressed in Windows, it is reported as a
 Meta press (see @method[mouse-event% get-meta-down]).

}

@defmethod[(get-caps-down)
           boolean?]{

Returns @racket[#t] if the Caps Lock key was on for the event.

}

@defmethod[(get-control-down)
           boolean?]{

Returns @racket[#t] if the Control key was down for the event.

On Mac OS X, if a control-key press is combined with a mouse button
 click, the event is reported as a right-button click and
 @method[mouse-event% get-control-down] for the event reports
 @racket[#f].

}

@defmethod[(get-event-type)
           (or/c 'enter 'leave 'left-down 'left-up 
                 'middle-down 'middle-up 
                 'right-down 'right-up 'motion)]{

Returns the type of the event; see @racket[mouse-event%] for
information about each event type. See also @method[mouse-event%
set-event-type].

}

@defmethod[(get-left-down)
           boolean?]{
Returns @racket[#t] if the left mouse button was down (but not pressed) during the event.

}

@defmethod[(get-meta-down)
           boolean?]{

Returns @racket[#t] if the Meta (Unix), Alt (Windows), or Command (Mac OS
 X) key was down for the event.

}

@defmethod[(get-middle-down)
           boolean?]{

Returns @racket[#t] if the middle mouse button was down (but not
 pressed) for the event.  On Mac OS X, a middle-button click is
 impossible.

}

@defmethod[(get-right-down)
           boolean?]{

Returns @racket[#t] if the right mouse button was down (but not
 pressed) for the event. On Mac OS X, a control-click combination
 is treated as a right-button click.

}

@defmethod[(get-shift-down)
           boolean?]{

Returns @racket[#t] if the Shift key was down for the event.

}

@defmethod[(get-x)
           exact-integer?]{

Returns the x-position of the mouse at the time of the event, in the
 target's window's (client-area) coordinate system.

}

@defmethod[(get-y)
           exact-integer?]{

Returns the y-position of the mouse at the time of the event in the
 target's window's (client-area) coordinate system.

}

@defmethod[(leaving?)
           boolean?]{

Returns @racket[#t] if this event is for the mouse leaving a window,
 @racket[#f] otherwise.

See @method[mouse-event% entering?] for information about enter and
leave events while the mouse button is clicked.

}

@defmethod[(moving?)
           boolean?]{

Returns @racket[#t] if this was a moving event (whether a button is
 pressed is not), @racket[#f] otherwise.
}


@defmethod[(set-alt-down [down? any/c])
           void?]{

Sets whether the Option (Mac OS X) key was down for the event.  When
 the Alt key is pressed in Windows, it is reported as a Meta press
 (see @method[mouse-event% set-meta-down]).

}

@defmethod[(set-caps-down [down? any/c])
           void?]{

Sets whether the Caps Lock key was on for the event.

}

@defmethod[(set-control-down [down? any/c])
           void?]{

Sets whether the Control key was down for the event.

On Mac OS X, if a control-key press is combined with a mouse button
 click, the event is reported as a right-button click and
 @method[mouse-event% get-control-down] for the event reports
 @racket[#f].

}

@defmethod[(set-event-type [event-type (or/c 'enter 'leave 'left-down 'left-up 
                                             'middle-down 'middle-up 
                                             'right-down 'right-up 'motion)])
           void?]{

Sets the type of the event; see @racket[mouse-event%] for information
about each event type. See also @method[mouse-event% get-event-type].

}

@defmethod[(set-left-down [down? any/c])
           void?]{

Sets whether the left mouse button was down (but not pressed) during
the event.

}

@defmethod[(set-meta-down [down? any/c])
           void?]{

Sets whether the Meta (Unix), Alt (Windows), or Command (Mac OS X) key
 was down for the event.

}

@defmethod[(set-middle-down [down? any/c])
           void?]{

Sets whether the middle mouse button was down (but not pressed) for
 the event.  On Mac OS X, a middle-button click is impossible.

}

@defmethod[(set-right-down [down? any/c])
           void?]{

Sets whether the right mouse button was down (but not pressed) for the
 event. On Mac OS X, a control-click combination by the user is
 treated as a right-button click.

}

@defmethod[(set-shift-down [down? any/c])
           void?]{

Sets whether the Shift key was down for the event.

}

@defmethod[(set-x [pos exact-integer?])
           void?]{

Sets the x-position of the mouse at the time of the event in the
 target's window's (client-area) coordinate system.

}

@defmethod[(set-y [pos exact-integer?])
           void?]{

Sets the y-position of the mouse at the time of the event in the
 target's window's (client-area) coordinate system.

}}

