#reader(lib "defreader.ss" "scribble")
@require["common.ss"]
@require["event-class.scrbl"]

@defclass[key-event% event% ()]{

A @scheme[key-event%] object contains information about a key press
 or release event. Key events are primarily processed by
@xmethod[window<%> on-subwindow-char] and
@xmethod[canvas<%> on-char].

For a key-press event, a virtual key code is provided by
@method[key-event% get-key-code]. For a key-release event, 
@method[key-event% get-key-code] reports @scheme['release], and a virtual key code is provided by
@method[key-event% get-key-release-code].

See also @|mousekeydiscuss|.


@defconstructor[([key-code (or/c char? symbol?) #\nul]
                 [shift-down any/c #f]
                 [control-down any/c #f]
                 [meta-down any/c #f]
                 [alt-down any/c #f]
                 [x (and/c exact? integer?) 0]
                 [y (and/c exact? integer?) 0]
                 [time-stamp (and/c exact? integer?) 0]
                 [caps-down any/c #f])]{

See the corresponding @schemeidfont{get-} and @schemeidfont{set-}
 methods for information about @scheme[key-code], @scheme[shift-down],
 @scheme[control-down], @scheme[meta-down], @scheme[alt-down],
 @scheme[x], @scheme[y], @scheme[time-stamp], @scheme[caps-down].

The release key code, as returned by @method[key-event%
get-key-release-code], is initialized to @scheme['press].

}

@defmethod[(get-alt-down)
           boolean?]{
Returns @scheme[#t] if the Option (Mac OS X) key was down for
 the event. When the Alt key is pressed in Windows, it is reported as
 a Meta press (see
@method[key-event% get-meta-down]).

}

@defmethod[(get-caps-down)
           boolean?]{
Returns @scheme[#t] if the Caps Lock key was on for the event.

}

@defmethod[(get-control-down)
           boolean?]{
Returns @scheme[#t] if the Control key was down for the event.

Under Mac OS X, if a control-key press is combined with a mouse button
 click, the event is reported as a right-button click and
@method[key-event% get-control-down] for the event reports @scheme[#f].

}

@defmethod[(get-key-code)
           (or/c char? symbol?)]{

Gets the virtual key code for the key event. The virtual key code is
 either a character or a special key symbol, one of the following:

@itemize{
@item{@indexed-scheme['start]}
@item{@indexed-scheme['cancel]}
@item{@indexed-scheme['clear]}
@item{@indexed-scheme['shift]}
@item{@indexed-scheme['control]}
@item{@indexed-scheme['menu]}
@item{@indexed-scheme['pause]}
@item{@indexed-scheme['capital]}
@item{@indexed-scheme['prior]}
@item{@indexed-scheme['next]}
@item{@indexed-scheme['end]}
@item{@indexed-scheme['home]}
@item{@indexed-scheme['left]}
@item{@indexed-scheme['up]}
@item{@indexed-scheme['right]}
@item{@indexed-scheme['down]}
@item{@indexed-scheme['escape]}
@item{@indexed-scheme['select]}
@item{@indexed-scheme['print]}
@item{@indexed-scheme['execute]}
@item{@indexed-scheme['snapshot]}
@item{@indexed-scheme['insert]}
@item{@indexed-scheme['help]}
@item{@indexed-scheme['numpad0]}
@item{@indexed-scheme['numpad1]}
@item{@indexed-scheme['numpad2]}
@item{@indexed-scheme['numpad3]}
@item{@indexed-scheme['numpad4]}
@item{@indexed-scheme['numpad5]}
@item{@indexed-scheme['numpad6]}
@item{@indexed-scheme['numpad7]}
@item{@indexed-scheme['numpad8]}
@item{@indexed-scheme['numpad9]}
@item{@indexed-scheme['numpad-enter]}
@item{@indexed-scheme['multiply]}
@item{@indexed-scheme['add]}
@item{@indexed-scheme['separator]}
@item{@indexed-scheme['subtract]}
@item{@indexed-scheme['decimal]}
@item{@indexed-scheme['divide]}
@item{@indexed-scheme['f1]}
@item{@indexed-scheme['f2]}
@item{@indexed-scheme['f3]}
@item{@indexed-scheme['f4]}
@item{@indexed-scheme['f5]}
@item{@indexed-scheme['f6]}
@item{@indexed-scheme['f7]}
@item{@indexed-scheme['f8]}
@item{@indexed-scheme['f9]}
@item{@indexed-scheme['f10]}
@item{@indexed-scheme['f11]}
@item{@indexed-scheme['f12]}
@item{@indexed-scheme['f13]}
@item{@indexed-scheme['f14]}
@item{@indexed-scheme['f15]}
@item{@indexed-scheme['f16]}
@item{@indexed-scheme['f17]}
@item{@indexed-scheme['f18]}
@item{@indexed-scheme['f19]}
@item{@indexed-scheme['f20]}
@item{@indexed-scheme['f21]}
@item{@indexed-scheme['f22]}
@item{@indexed-scheme['f23]}
@item{@indexed-scheme['f24]}
@item{@indexed-scheme['numlock]}
@item{@indexed-scheme['scroll]}
@item{@indexed-scheme['wheel-up] --- \index["wheel on mouse"]{mouse} wheel up one notch}
@item{@indexed-scheme['wheel-down] --- mouse wheel down one notch}
@item{@indexed-scheme['release] --- indicates a key-release event}
@item{@indexed-scheme['press] --- indicates a key-press event; usually only from @scheme[get-key-release-code]}
}

The special key symbols attempt to capture useful keys that have no
 standard ASCII representation. A few keys have standard
 representations that are not obvious:

@itemize{

 @item{@scheme[#\space] --- the space bar}

 @item{@scheme[#\return] --- the Enter or Return key (on all
      platforms), but not necessarily the Enter key near the numpad
      (which is reported as @scheme['numpad-enter] if the platform
      distinguishes the two Enter keys)}

 @item{@scheme[#\tab] --- the tab key}

 @item{@scheme[#\backspace] --- the backspace key}

 @item{@scheme[#\rubout] --- the delete key}

}

If a suitable special key symbol or ASCII representation is not
 available, @scheme[#\nul] (the NUL character) is reported.

Under X, a @scheme['wheel-up] or @scheme['wheel-down] event may be sent
 to a window other than the one with the keyboard focus, because X
 generates wheel events based on the location of the mouse pointer.

Under Windows, when the Control key is pressed without Alt, the key
 code for ASCII characters is downcased, roughly cancelling the effect
 of the Shift key. Under Mac OS X, the key code is computed without
 Caps Lock effects when the Control or Command key is pressed; in the
 case of Control, Caps Lock is used normally if special handling is
 disabled for the Control key via @scheme[special-control-key]. Under
 X, the key code is computed with Caps Lock effects when the Control
 key is pressed without Alt.

See also @method[key-event% get-other-shift-key-code].

}

@defmethod[(get-key-release-code)
           (or/c char? symbol?)]{

Gets the virtual key code for a key-release event; the result is
 @scheme['press] for a key-press event. See @method[key-event%
 get-key-code] for the list of virtual key codes.

}

@defmethod[(get-meta-down)
           boolean?]{

Returns @scheme[#t] if the Meta (X), Alt (Windows), or Command (Mac OS
 X) key was down for the event.

}

@defmethod[(get-other-altgr-key-code)
           (or/c char? symbol? false/c)]{

See @method[key-event% get-other-shift-key-code].

}

@defmethod[(get-other-caps-key-code)
           (or/c char? symbol? false/c)]{

See @method[key-event% get-other-shift-key-code].

}

@defmethod[(get-other-shift-altgr-key-code)
           (or/c char? symbol? false/c)]{

See @method[key-event% get-other-shift-key-code].

}

@defmethod[(get-other-shift-key-code)
           (or/c char? symbol? false/c)]{

Since keyboard mappings vary, it is sometimes useful in key mappings
 for a program to know the result that the keyboard would have
 produced for an event if the Shift key had been toggled
 differently. The @method[key-event% get-other-shift-key-code]
 produces that other mapping, returning @scheme[#f] if the alternate
 mapping is unavailable, otherwise returning the same kind of result
 as @method[key-event% get-key-code].

The @method[key-event% get-other-altgr-key-code] method provides the
same information with respect to the AltGr key (i.e., Alt combined
with Control) under Windows and X, or the Option key under Mac OS
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

Alternate mappings are not available for all events. Under Windows,
 alternate mappings are reported when they produce ASCII letters,
 ASCII digits, and ASCII symbols. Under Mac OS X, alternate mappings are
 available only when the Command key is pressed. Under X, alternate
 mappings are usually available.

}

@defmethod[(get-shift-down)
           boolean?]{

Returns @scheme[#t] if the Shift key was down for the event.

}

@defmethod[(get-x)
           (and/c exact? integer?)]{

Returns the x-position of the mouse at the time of the event, in the
 target's window's (client-area) coordinate system.

}

@defmethod[(get-y)
           (and/c exact? integer?)]{

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

Under Mac OS X, if a control-key press is combined with a mouse button
 click, the event is reported as a right-button click and
 @method[key-event% get-control-down] for the event reports
 @scheme[#f].

}

@defmethod[(set-key-code [code (or/c char? symbol?)])
           void?]{

Sets the virtual key code for the event, either a character or one of
 the special symbols listed with @method[key-event% get-key-code].

}

@defmethod[(set-key-release-code [code (or/c char? symbol?)])
           void?]{

Sets the virtual key code for a release event, either a character or
 one of the special symbols listed with @method[key-event%
 get-key-code]. See also @method[key-event% get-key-release-code].

}

@defmethod[(set-meta-down [down? any/c])
           void?]{

Sets whether the Meta (X), Alt (Windows), or Command (Mac OS X) key
 was down for the event.

}

@defmethod[(set-other-altgr-key-code [code (or/c char? symbol? false/c)])
           void?]{

Sets the key code produced by @method[key-event%
get-other-altgr-key-code].

}

@defmethod[(set-other-caps-key-code [code (or/c char? symbol? false/c)])
           void?]{

Sets the key code produced by @method[key-event%
 get-other-caps-key-code].

}

@defmethod[(set-other-shift-altgr-key-code [code (or/c char? symbol? false/c)])
           void?]{

Sets the key code produced by @method[key-event%
 get-other-shift-altgr-key-code].

}

@defmethod[(set-other-shift-key-code [code (or/c char? symbol? false/c)])
           void?]{

Sets the key code produced by @method[key-event%
 get-other-shift-key-code].

}

@defmethod[(set-shift-down [down? any/c])
           void?]{

Sets whether the Shift key was down for the event.

}

@defmethod[(set-x [pos (and/c exact? integer?)])
           void?]{

Sets the x-position of the mouse at the time of the event in the
 target's window's (client-area) coordinate system.

}

@defmethod[(set-y [pos (and/c exact? integer?)])
           void?]{

Sets the y-position of the mouse at the time of the event in the
 target's window's  (client-area) coordinate system.

}
}

