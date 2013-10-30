#lang scribble/doc
@(require "common.rkt")

@definterface/title[top-level-window<%> (area-container-window<%>)]{

A top-level window is either a @racket[frame%] or @racket[dialog%]
 object.

@defmethod[#:mode pubment 
           (can-close?)
           boolean?]{

Called just before the window might be closed (e.g., by the window
 manager). If @racket[#f] is returned, the window is not closed,
 otherwise @method[top-level-window<%> on-close] is called and the
 window is closed (i.e., the window is hidden, like calling
 @method[window<%> show] with @racket[#f]).

This method is @italic{not} called by @method[window<%> show].
}

@defmethod[(can-exit?)
           boolean?]{
@methspec{

Called before @method[top-level-window<%> on-exit] to check whether an
exit is allowed. See @method[top-level-window<%> on-exit] for more
information.

}
@methimpl{

Calls @method[top-level-window<%> can-close?] and returns the result.

}}

@defmethod[(center [direction (or/c 'horizontal 'vertical 'both) 'both])
           void?]{

Centers the window on the screen if it has no parent. If it has a
 parent, the window is centered with respect to its parent's location.

If @racket[direction] is @racket['horizontal], the window is centered
 horizontally.  If @racket[direction] is @racket['vertical], the
 window is centered vertically.  If @racket[direction] is
 @racket['both], the window is centered in both directions.

}

@defmethod[(get-edit-target-object)
           (or/c (or/c (is-a?/c window<%>) (is-a?/c editor<%>)) #f)]{

@index['("keyboard focus" "last active")]{Like}
 @method[top-level-window<%> get-edit-target-window], but if an editor
 canvas had the focus and it also displays an editor, the editor is
 returned instead of the canvas. Further, if the editor's focus is
 delegated to an embedded editor, the embedded editor is returned.

See also @method[top-level-window<%> get-focus-object].

}

@defmethod[(get-edit-target-window)
           (or/c (is-a?/c window<%>) #f)]{

@index['("keyboard focus" "last active")]{Returns} the window that
most recently had the keyboard focus, either the top-level window or
one of its currently-shown children. If neither the window nor any of
its currently-shown children has even owned the keyboard focus,
@racket[#f] is returned.

See also @method[top-level-window<%> get-focus-window] and
@method[top-level-window<%> get-edit-target-object].

}

@defmethod[(get-eventspace)
           eventspace?]{
Returns the window's eventspace.

}

@defmethod[(get-focus-object)
           (or/c (or/c (is-a?/c window<%>) (is-a?/c editor<%>)) #f)]{

@index["keyboard focus"]{Like} @method[top-level-window<%>
get-focus-window], but if an editor canvas has the focus and it also
displays an editor, the editor is returned instead of the
canvas. Further, if the editor's focus is delegated to an embedded
editor, the embedded editor is returned.

See also @method[top-level-window<%> get-edit-target-object].

}

@defmethod[(get-focus-window)
           (or/c (is-a?/c window<%>) #f)]{

@index["keyboard focus"]{Returns} the window that has the keyboard
 focus, either the top-level window or one of its children. If neither
 the window nor any of its children has the focus, @racket[#f] is
 returned.

See also @method[top-level-window<%> get-edit-target-window] and
@method[top-level-window<%> get-focus-object].

}


@defmethod[(move [x position-integer?]
                 [y position-integer?])
           void?]{

Moves the window to the given position on the screen.

@MonitorMethod[@elem{A window's position} @elem{the user dragging the window} @elem{@method[window<%> on-move]} @elem{position}]

}


@defmethod[(on-activate [active? any/c])
           void?]{

Called when a window is @defterm{activated} or
 @defterm{deactivated}. A top-level window is activated when the
 keyboard focus moves from outside the window to the window or one of
 its children. It is deactivated when the focus moves back out of the
 window. On Mac OS X, a child of a floating frames can have the
 focus instead of a child of the active non-floating frame; in other
 words, floating frames act as an extension of the active non-frame
 for keyboard focus.

The method's argument is @racket[#t] when the window is activated,
 @racket[#f] when it is deactivated.

}

@defmethod[#:mode pubment 
           (on-close)
           void?]{

Called just before the window is closed (e.g., by the window manager).
 This method is @italic{not} called by @method[window<%> show].

See also
@method[top-level-window<%> can-close?].

}

@defmethod[(on-exit)
           void?]{

@methspec{

Called by the default application quit handler (as determined by the
 @racket[application-quit-handler] parameter) when the operating
 system requests that the application shut down (e.g., when the
 @onscreen{Quit} menu item is selected in the main application menu
 on Mac OS X). In that case, this method is called for the most
 recently active top-level window in the initial eventspace, but only
 if the window's @method[top-level-window<%> can-exit?]  method first
 returns true.

}
@methimpl{

Calls
@method[top-level-window<%> on-close] and then
@method[top-level-window<%> show] to hide the window.



}}

@defmethod[(on-message [message any/c])
           any/c]{
@methspec{

@index["drag-and-drop"]{A} generic message method, usually called by
@racket[send-message-to-window].

If the method is invoked by @racket[send-message-to-window], then it
is invoked in the thread where @racket[send-message-to-window] was
called (which is possibly @italic{not} the handler thread of the
window's eventspace).

}
@methimpl{

Returns @|void-const|.

}}


@defmethod[(display-changed) any/c]{
  @methspec{
    Called when the displays configuration changes.

    To determine the new monitor configuration, use
    @racket[get-display-count], @racket[get-display-size],
    @racket[get-display-left-top-inset], and
    @racket[get-display-backing-scale].

    Note that this method may be invoked multiple times for a single
    logical change to the monitors.
    
  }
  @methimpl{
    Returns @|void-const|.
  }
}


@defmethod[(on-traverse-char [event (is-a?/c key-event%)])
           boolean?]{

@methspec{

@index['("keyboard focus" "navigation")]{Attempts} to handle the given
 keyboard event as a navigation event, such as a Tab key event that
 moves the keyboard focus. If the event is handled, @racket[#t] is
 returned, otherwise @racket[#f] is returned.

}
@methimpl{

The following rules determine, in order, whether and how @racket[event]
is handled:

@itemize[

@item{
If the window that currently owns the focus specifically handles the
 event, then @racket[#f] is returned. The following describes window
 types and the keyboard events they specifically handle:
@itemize[

 @item{@racket[editor-canvas%] --- tab-exit is disabled (see
@method[editor-canvas% allow-tab-exit]): all keyboard events, except alphanumeric key events when the Meta
       (Unix) or Alt (Windows) key is pressed; when tab-exit is enabled:
       all keyboard events except Tab, Enter, Escape, and alphanumeric
       Meta/Alt events.}

 @item{@racket[canvas%] --- when tab-focus is disabled (see
@method[canvas<%> accept-tab-focus]): all keyboard events, except alphanumeric key events when the Meta
       (Unix) or Alt (Windows) key is pressed; when tab-focus is enabled:
       no key events}

 @item{@racket[text-field%], @racket['single] style --- arrow key
 events and alphanumeric key events when the Meta (Unix) or Alt
 (Windows) key is not pressed (and all alphanumeric events on
 Mac OS X)}

 @item{@racket[text-field%], @racket['multiple] style --- all
 keyboard events, except alphanumeric key events when the Meta (Unix) or
 Alt (Windows) key is pressed}

 @item{@racket[choice%] --- arrow key events and alphanumeric key
 events when the Meta (Unix) or Alt (Windows) key is not pressed}

 @item{@racket[list-box%] --- arrow key events and alphanumeric key
 events when the Meta (Unix) or Alt (Windows) key is not pressed}

]}

@item{
If @racket[event] is a Tab or arrow key event, the keyboard focus is
 moved within the window and @racket[#t] is returned. Across platforms,
 the types of windows that accept the keyboard focus via navigation
 may vary, but @racket[text-field%] windows always accept the focus,
 and @racket[message%], @racket[gauge%], and @racket[panel%]
 windows never accept the focus.}

@item{
If @racket[event] is a Space key event and the window that currently
 owns the focus is a @racket[button%], @racket[check-box%], or
 @racket[radio-box%] object, the event is handled in the same way as
 a click on the control and @racket[#t] is returned.}

@item{
If @racket[event] is an Enter key event and the current top-level window
 contains a border button, the button's callback is invoked and
 @racket[#t] is returned. (The @racket['border] style for a
 @racket[button%] object indicates to the user that pressing Enter
 is the same as clicking the button.) If the window does not contain a
 border button, @racket[#t] is returned if the window with the current
 focus is not a text field or editor canvas.}

@item{
In a dialog, if @racket[event] is an Escape key event, the event is
 handled the same as a click on the dialog's close box (i.e., the
 dialog's
@method[top-level-window<%> can-close?] and
@method[top-level-window<%> on-close] methods are called, and the dialog is hidden) and @racket[#t] is
 returned.}

@item{
If @racket[event] is an alphanumeric key event and the current top-level
 window contains a control with a mnemonic matching the key (which is
 installed via a label that contains @litchar{&}; see
 @method[window<%> get-label] for more information), then the
 keyboard focus is moved to the matching control. Furthermore, if the
 matching control is a @racket[button%], @racket[check-box%], or
 @racket[radio-box%] button, the keyboard event is handled in the
 same way as a click on the control.}

@item{
Otherwise, @racket[#f] is returned.}

]
}}

@defmethod[(on-system-menu-char [event (is-a?/c key-event%)])
           boolean?]{

Checks whether the given event pops open the system menu in the
 top-left corner of the window (Windows only). If the window's system
 menu is opened, @racket[#t] is returned, otherwise @racket[#f] is
 returned.

}

@defmethod[(resize [width dimension-integer?]
                   [height dimension-integer?])
           void?]{

Sets the size of the window (in pixels), but only if the given size is
 larger than the window's minimum size.

@MonitorMethod[@elem{A window's size} @elem{the user} @elem{@method[window<%> on-size]} @elem{size}]

}


@defmethod[(set-icon [icon (is-a?/c bitmap%)]
                     [mask (is-a?/c bitmap%) #f]
                     [which (or/c 'small 'large 'both) 'both])
           void?]{

Sets the large or small icon bitmap for the window.  Future changes to
 the bitmap do not affect the window's icon.

The icon is used in a platform-specific way:

@itemize[

 @item{Windows --- the small icon is used for the window's icon (in the
       top-left) and in the task bar, and the large icon is used for
       the Alt-Tab task switcher.}

 @item{Mac OS X --- both icons are ignored.}

 @item{Unix --- many window managers use the small icon in the same way
       as Windows, and others use the small icon when iconifying the
       frame; the large icon is ignored.}

]

The bitmap for either icon can be any size, but most platforms scale
 the small bitmap to 16 by 16 pixels and the large bitmap to 32 by 32
 pixels.

If a mask bitmap is not provided, then the entire (rectangular) bitmap
 is used as an icon.

If a mask bitmap is provided, the mask must be monochrome. In the mask
 bitmap, use black pixels to indicate the icon's region and use white
 pixels outside the icon's region. In the icon bitmap, use black
 pixels for the region outside the icon.}


@defmethod[(show [show any/c])
           void?]{

If the window is already shown, it is moved front of other top-level
 windows. If the window is iconized (frames only), it is deiconized.

See also @xmethod[window<%> show].

}}

