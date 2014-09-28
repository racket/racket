#lang scribble/doc
@(require "common.rkt")

@defclass/title[key-event% event% ()]{

A @racket[key-event%] object contains information about a key press
or release event. Key events are primarily processed by
@xmethod[window<%> on-subwindow-char] and
@xmethod[canvas<%> on-char].

For a key-press event, a virtual key code is provided by
@method[key-event% get-key-code]. For a key-release event, 
@method[key-event% get-key-code] reports @racket['release], and a virtual key code is provided by
@method[key-event% get-key-release-code].

See also @|mousekeydiscuss|.


@defconstructor[([key-code (or/c char? key-code-symbol?) #\nul]
                 [shift-down any/c #f]
                 [control-down any/c #f]
                 [meta-down any/c #f]
                 [alt-down any/c #f]
                 [x exact-integer? 0]
                 [y exact-integer? 0]
                 [time-stamp exact-integer? 0]
                 [caps-down any/c #f]
                 [mod3-down any/c #f]
                 [mod4-down any/c #f]
                 [mod5-down any/c #f]
                 [control+meta-is-altgr any/c #f])]{

See the corresponding @racketidfont{get-} and @racketidfont{set-}
 methods for information about @racket[key-code], @racket[shift-down],
 @racket[control-down], @racket[meta-down], @racket[mod3-down], @racket[mod4-down],
 @racket[mod5-down], @racket[alt-down], @racket[x], @racket[y], 
 @racket[time-stamp], @racket[caps-down], @racket[mod3-down],
 @racket[mod4-down], @racket[mod5-down], and @racket[control+meta-is-altgr].

The release key code, as returned by @method[key-event%
get-key-release-code], is initialized to @racket['press].

@history[#:changed "1.1" @elem{Added @racket[mod3-down], @racket[mod4-down], and @racket[mod5-down].}
         #:changed "1.2" @elem{Added @racket[control+meta-is-altgr].}]
}

@defmethod[(get-alt-down)
           boolean?]{
Returns @racket[#t] if the Option (Mac OS X) key was down for
 the event. When the Alt key is pressed in Windows, it is reported as
 a Meta press (see
@method[key-event% get-meta-down]).

}

@defmethod[(get-caps-down)
           boolean?]{
Returns @racket[#t] if the Caps Lock key was on for the event.

}

@defmethod[(get-control-down)
           boolean?]{
Returns @racket[#t] if the Control key was down for the event.

On Mac OS X, if a Control-key press is combined with a mouse button
 click, the event is reported as a right-button click and
@method[key-event% get-control-down] for the event reports @racket[#f].

}


@defmethod[(get-control+meta-is-altgr)
           boolean?]{

Returns @racket[#t] if a Control plus Meta event should be treated as
an AltGr event on Windows: the Control key was the left one and the
Alt key was the right one (typed that way on a keyboard with a right
Alt key, or produced by a single AltGr key).

@history[#:added "1.2"]}


@defmethod[(get-key-code)
           (or/c char? key-code-symbol?)]{

Gets the virtual key code for the key event. The virtual key code is
 either a character or a special key symbol, one of the following:

@itemize[
#:style 'compact
@item{@indexed-racket['start]}
@item{@indexed-racket['cancel]}
@item{@indexed-racket['clear]}
@item{@indexed-racket['shift] --- Shift key}
@item{@indexed-racket['rshift] --- right Shift key}
@item{@indexed-racket['control] --- Control key}
@item{@indexed-racket['rcontrol] --- right Control key}
@item{@indexed-racket['menu]}
@item{@indexed-racket['pause]}
@item{@indexed-racket['capital]}
@item{@indexed-racket['prior]}
@item{@indexed-racket['next]}
@item{@indexed-racket['end]}
@item{@indexed-racket['home]}
@item{@indexed-racket['left]}
@item{@indexed-racket['up]}
@item{@indexed-racket['right]}
@item{@indexed-racket['down]}
@item{@indexed-racket['escape]}
@item{@indexed-racket['select]}
@item{@indexed-racket['print]}
@item{@indexed-racket['execute]}
@item{@indexed-racket['snapshot]}
@item{@indexed-racket['insert]}
@item{@indexed-racket['help]}
@item{@indexed-racket['numpad0]}
@item{@indexed-racket['numpad1]}
@item{@indexed-racket['numpad2]}
@item{@indexed-racket['numpad3]}
@item{@indexed-racket['numpad4]}
@item{@indexed-racket['numpad5]}
@item{@indexed-racket['numpad6]}
@item{@indexed-racket['numpad7]}
@item{@indexed-racket['numpad8]}
@item{@indexed-racket['numpad9]}
@item{@indexed-racket['numpad-enter]}
@item{@indexed-racket['multiply]}
@item{@indexed-racket['add]}
@item{@indexed-racket['separator]}
@item{@indexed-racket['subtract]}
@item{@indexed-racket['decimal]}
@item{@indexed-racket['divide]}
@item{@indexed-racket['f1]}
@item{@indexed-racket['f2]}
@item{@indexed-racket['f3]}
@item{@indexed-racket['f4]}
@item{@indexed-racket['f5]}
@item{@indexed-racket['f6]}
@item{@indexed-racket['f7]}
@item{@indexed-racket['f8]}
@item{@indexed-racket['f9]}
@item{@indexed-racket['f10]}
@item{@indexed-racket['f11]}
@item{@indexed-racket['f12]}
@item{@indexed-racket['f13]}
@item{@indexed-racket['f14]}
@item{@indexed-racket['f15]}
@item{@indexed-racket['f16]}
@item{@indexed-racket['f17]}
@item{@indexed-racket['f18]}
@item{@indexed-racket['f19]}
@item{@indexed-racket['f20]}
@item{@indexed-racket['f21]}
@item{@indexed-racket['f22]}
@item{@indexed-racket['f23]}
@item{@indexed-racket['f24]}
@item{@indexed-racket['numlock]}
@item{@indexed-racket['scroll]}
@item{@indexed-racket['wheel-up] --- @index["wheel on mouse"]{mouse} wheel up one notch}
@item{@indexed-racket['wheel-down] --- mouse wheel down one notch}
@item{@indexed-racket['wheel-left] --- mouse wheel left one notch}
@item{@indexed-racket['wheel-right] --- mouse wheel right one notch}
@item{@indexed-racket['release] --- indicates a key-release event}
@item{@indexed-racket['press] --- indicates a key-press event; usually only from @racket[get-key-release-code]}
]

The special key symbols attempt to capture useful keys that have no
 standard ASCII representation. A few keys have standard
 representations that are not obvious:

@itemize[

 @item{@racket[#\space] --- the space bar}

 @item{@racket[#\return] --- the Enter or Return key (on all
      platforms), but not necessarily the Enter key near the numpad
      (which is reported as @racket['numpad-enter] Unix and Mac OS X)}

 @item{@racket[#\tab] --- the tab key}

 @item{@racket[#\backspace] --- the backspace key}

 @item{@racket[#\rubout] --- the delete key}

]

If a suitable special key symbol or ASCII representation is not
 available, @racket[#\nul] (the NUL character) is reported.

A @racket['wheel-up], @racket['wheel-down], @racket['wheel-left], or
 @racket['wheel-right] event may be sent to a window other than the
 one with the keyboard focus, because some platforms generate wheel
 events based on the location of the mouse pointer instead of the
 keyboard focus.

On Windows, when the Control key is pressed without Alt, the key
 code for ASCII characters is downcased, roughly cancelling the effect
 of the Shift key. On Mac OS X, the key code is computed without
 Caps Lock effects when the Control or Command key is pressed; in the
 case of Control, Caps Lock is used normally if special handling is
 disabled for the Control key via @racket[special-control-key]. On
 Unix, the key code is computed with Caps Lock effects when the Control
 key is pressed without Alt.

See also @method[key-event% get-other-shift-key-code].

@history[#:changed "6.1.0.8" @elem{Changed reporting of numpad Enter
                                   to @racket['numpad-enter] as
                                   documented, instead of
                                   @racket[#\u03].}]}


@defmethod[(get-key-release-code)
           (or/c char? key-code-symbol?)]{

Gets the virtual key code for a key-release event; the result is
 @racket['press] for a key-press event. See @method[key-event%
 get-key-code] for the list of virtual key codes.

}

@defmethod[(get-meta-down)
           boolean?]{

Returns @racket[#t] if the Meta (Unix), Alt (Windows), or Command (Mac OS
 X) key was down for the event.

}

@defmethod[(get-mod3-down)
           boolean?]{

Returns @racket[#t] if the Mod3 (Unix) key was down for the event.

@history[#:added "1.1"]}

@defmethod[(get-mod4-down)
           boolean?]{

Returns @racket[#t] if the Mod4 (Unix) key was down for the event.

@history[#:added "1.1"]}

@defmethod[(get-mod5-down)
           boolean?]{

Returns @racket[#t] if the Mod5 (Unix) key was down for the event.

@history[#:added "1.1"]}

@defmethod[(get-other-altgr-key-code)
           (or/c char? key-code-symbol? #f)]{

See @method[key-event% get-other-shift-key-code].

}

@defmethod[(get-other-caps-key-code)
           (or/c char? key-code-symbol? #f)]{

See @method[key-event% get-other-shift-key-code].

}

@defmethod[(get-other-shift-altgr-key-code)
           (or/c char? key-code-symbol? #f)]{

See @method[key-event% get-other-shift-key-code].

}

@defmethod[(get-other-shift-key-code)
           (or/c char? key-code-symbol? #f)]{

Since keyboard mappings vary, it is sometimes useful in key mappings
 for a program to know the result that the keyboard would have
 produced for an event if the Shift key had been toggled
 differently. The @method[key-event% get-other-shift-key-code]
 produces that other mapping, returning @racket[#f] if the alternate
 mapping is unavailable, otherwise returning the same kind of result
 as @method[key-event% get-key-code].

The @method[key-event% get-other-altgr-key-code] method provides the
same information with respect to the AltGr key (i.e., Alt combined
with Control) on Windows and Unix, or the Option key on Mac OS
X. The @method[key-event% get-other-shift-altgr-key-code] method
reports a mapping for in tha case that both Shift and AltGr/Option
were different from the actual event.

The @method[key-event% get-other-shift-key-code], @method[key-event%
get-other-altgr-key-code], and @method[key-event%
get-other-shift-altgr-key-code] results all report key mappings where
Caps Lock is off, independent of whether Caps Lock was on for the
actual event. The @method[key-event% get-other-caps-key-code] method
reports a mapping for in that case that the Caps Lock state was
treated opposite as for the @method[key-event% get-key-code]
result. (Caps Lock normally has either no effect or the same effect as
Shift, so further combinations involving Caps Lock and other modifier
keys would not normally produce further alternatives.)

Alternate mappings are not available for all events. On Windows,
 alternate mappings are reported when they produce ASCII letters,
 ASCII digits, and ASCII symbols. On Mac OS X, alternate mappings are
 available only when the Command key is pressed. On Unix, alternate
 mappings are usually available.

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

@defmethod[(set-alt-down [down? any/c])
           void?]{

Sets whether the Option (Mac OS X) key was down for the event.  When
 the Alt key is pressed in Windows, it is reported as a Meta press
 (see @method[key-event% set-meta-down]).

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
 @method[key-event% get-control-down] for the event reports
 @racket[#f].

}

@defmethod[(control+meta-is-altgr [down? any/c])
           void?]{

Sets whether a Control plus Meta combination on Windows should be
treated as an AltGr combinations. See @racket[get-control+meta-is-altgr].

@history[#:added "1.2"]}


@defmethod[(set-key-code [code (or/c char? key-code-symbol?)])
           void?]{

Sets the virtual key code for the event, either a character or one of
 the special symbols listed with @method[key-event% get-key-code].

}

@defmethod[(set-key-release-code [code (or/c char? key-code-symbol?)])
           void?]{

Sets the virtual key code for a release event, either a character or
 one of the special symbols listed with @method[key-event%
 get-key-code]. See also @method[key-event% get-key-release-code].

}

@defmethod[(set-meta-down [down? any/c])
           void?]{

Sets whether the Meta (Unix), Alt (Windows), or Command (Mac OS X) key
 was down for the event.

}

@defmethod[(set-mod3-down [down? any/c])
           void?]{

Sets whether the Mod3 (Unix) key was down for the event.

@history[#:added "1.1"]}

@defmethod[(set-mod4-down [down? any/c])
           void?]{

Sets whether the Mod4 (Unix) key was down for the event.

@history[#:added "1.1"]}

@defmethod[(set-mod5-down [down? any/c])
           void?]{

Sets whether the Mod5 (Unix) key was down for the event.

@history[#:added "1.1"]}

@defmethod[(set-other-altgr-key-code [code (or/c char? key-code-symbol? #f)])
           void?]{

Sets the key code produced by @method[key-event%
get-other-altgr-key-code].

}

@defmethod[(set-other-caps-key-code [code (or/c char? key-code-symbol? #f)])
           void?]{

Sets the key code produced by @method[key-event%
 get-other-caps-key-code].

}

@defmethod[(set-other-shift-altgr-key-code [code (or/c char? key-code-symbol? #f)])
           void?]{

Sets the key code produced by @method[key-event%
 get-other-shift-altgr-key-code].

}

@defmethod[(set-other-shift-key-code [code (or/c char? key-code-symbol? #f)])
           void?]{

Sets the key code produced by @method[key-event%
 get-other-shift-key-code].

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
 target's window's  (client-area) coordinate system.

}
}

