#lang scribble/doc
@(require "common.rkt"
          (for-label (only-in ffi/unsafe cpointer?)))

@definterface/title[window<%> (area<%>)]{

A @racket[window<%>] object is an @racket[area<%>] with a graphical
 representation that can respond to events.

All @racket[window<%>] classes accept the following named instantiation
 arguments:
@itemize[

 @item{@indexed-racket[enabled] --- default is @racket[#t]; passed to
@method[window<%> enable] if @racket[#f]}

]




@defmethod*[([(accept-drop-files)
              boolean?]
             [(accept-drop-files [accept-files? any/c])
              void?])]{

@index["drag-and-drop"]{Enables} or disables drag-and-drop dropping
 for the window, or gets the enable state. Dropping is initially
 disabled. See also @method[window<%> on-drop-file].
}

@defmethod[(client->screen [x (integer-in -10000 10000)]
                           [y (integer-in -10000 10000)])
           (values (integer-in -10000 10000)
                   (integer-in -10000 10000))]{

@index["global coordinates"]{Converts} local window coordinates to
screen coordinates.

On Mac OS X, the screen coordinates start with @math{(0, 0)} at the
upper left of the menu bar. In contrast, @xmethod[top-level-window<%>
move] considers @math{(0, 0)} to be below the menu bar. See also
@racket[get-display-left-top-inset].

}

@defmethod[(enable [enable? any/c])
           void?]{

Enables or disables a window so that input events are ignored. (Input
 events include mouse events, keyboard events, and close-box clicks,
 but not focus or update events.) When a window is disabled, input
 events to its children are also ignored.

@MonitorMethod[@elem{The enable state of a window} @elem{enabling a parent window} @elem{@method[window<%> on-superwindow-enable]} @elem{enable state}]

If @racket[enable?] is true, the window is enabled, otherwise it is
 disabled.

}

@defmethod[(focus)
           void?]{

@index['("keyboard focus" "setting")]{Moves} the keyboard focus to the
 window, relative to its top-level window, if the window ever accepts
 the keyboard focus.  If the focus is in the window's top-level
 window, then the focus is immediately moved to this
 window. Otherwise, the focus is not immediately moved, but when the
 window's top-level window gets the keyboard focus, the focus is
 delegated to this window.

See also
@method[window<%> on-focus].

Note that on Unix, keyboard focus can move to the menu bar
 when the user is selecting a menu item.

@MonitorMethod[@elem{The current keyboard focus window} @elem{the user} @elem{@method[window<%> on-focus]} @elem{focus}]

}


@defmethod[(get-client-handle) cpointer?]{

Returns a handle to the ``inside'' of the window for the current
platform's GUI toolbox. The value that the pointer represents depends
on the platform:

@itemize[

 @item{Windows: @tt{HWND}}

 @item{Mac OS X: @tt{NSView}}

 @item{Unix: @tt{GtkWidget}}

]

See also @method[window<%> get-handle].}


@defmethod[(get-client-size)
           (values (integer-in 0 10000)
                   (integer-in 0 10000))]{

Gets the interior size of the window in pixels. For a container, the
 interior size is the size available for placing subwindows (including
 the border margin). For a canvas, this is the visible drawing
 area.

The client size is returned as two values: width and height (in pixels).

See also
@method[area-container<%> reflow-container].

}


@defmethod[(get-cursor)
           (or/c (is-a?/c cursor%) #f)]{

Returns the window's cursor, or @racket[#f] if this window's cursor
 defaults to the parent's cursor.  See
@method[window<%> set-cursor] for more information.

}


@defmethod[(get-handle) cpointer?]{

Returns a handle to the ``outside'' of the window for the current platform's GUI
toolbox. The value that the pointer represents depends on the
platform:

@itemize[

 @item{Windows: @tt{HWND}}

 @item{Mac OS X: @tt{NSWindow} for a @racket[top-level-window<%>] object,
       @tt{NSView} for other windows}

 @item{Unix: @tt{GtkWidget}}

]

See also @method[window<%> get-client-handle].}


@defmethod[(get-height)
           (integer-in 0 10000)]{

Returns the window's total height (in pixels).

See also
@method[area-container<%> reflow-container].

}

@defmethod[(get-label)
           (or/c label-string? 
                 (is-a?/c bitmap%)
                 (or/c 'app 'caution 'stop) 
                 (list/c (is-a?/c bitmap%)
                         label-string?
                         (or/c 'left 'top 'right 'bottom))
                 #f)]{

Gets a window's label, if any. Control windows generally display their
 label in some way. Frames and dialogs display their label as a window
 title. Panels do not display their label, but the label can be used
 for identification purposes. Messages, buttons, and check boxes can
 have bitmap labels (only when they are created with bitmap labels),
 but all other windows have string labels. In addition, a message
 label can be an icon symbol @racket['app], @racket['caution], or
 @racket['stop], and a button can have both a bitmap label and a
 string label (along with a position for the bitmap).

A label string may contain @litchar{&}s, which serve as
 keyboard navigation annotations for controls on Windows and Unix. The
 ampersands are not part of the displayed label of a control; instead,
 ampersands are removed in the displayed label (on all platforms),
 and any character preceding an ampersand is underlined (Windows and
 Unix) indicating that the character is a mnemonic for the
 control. Double ampersands are converted into a single ampersand
 (with no displayed underline). See also
 @method[top-level-window<%> on-traverse-char].

If the window does not have a label, @racket[#f] is returned.

}


@defmethod[(get-plain-label)
           (or/c string? #f)]{

Like
@method[window<%> get-label], except that ampersands in the label are removed. If the window has
 no label or the window's
 label is not a string, @racket[#f] is returned.

}


@defmethod[(get-size)
           (values (integer-in 0 10000)
                   (integer-in 0 10000))]{

Gets the current size of the entire window in pixels, not counting
 horizontal and vertical margins. (On Unix, this size does not include
 a title bar or borders for a frame/dialog.) See also
@method[window<%> get-client-size].

The geometry is returned as two values: width and height (in pixels).

See also
@method[area-container<%> reflow-container].

}


@defmethod[(get-width)
           (integer-in 0 10000)]{

Returns the window's current total width (in pixels).

See also
@method[area-container<%> reflow-container].

}

@defmethod[(get-x)
           (integer-in -10000 10000)]{

Returns the position of the window's left edge in its
 parent's coordinate system.

See also
@method[area-container<%> reflow-container].

}

@defmethod[(get-y)
           (integer-in -10000 10000)]{

Returns the position of the window's top edge in its
 parent's coordinate system.

See also
@method[area-container<%> reflow-container].

}


@defmethod[(has-focus?)
           boolean?]{

Indicates whether the window currently has the keyboard focus. See
 also
@method[window<%> on-focus].

}


@defmethod[(is-enabled?)
           boolean?]{

Indicates whether the window is currently enabled or not. The result is
 @racket[#t] if this window is enabled when its ancestors are enabled, or
 @racket[#f] if this window remains disable when its ancestors are
 enabled. (That is, the result of this method is affected only by calls
 to @method[window<%> enable] for @this-obj[], not by the enable state of
 parent windows.)}


@defmethod[(is-shown?)
           boolean?]{

Indicates whether the window is currently shown or not. The result is
 @racket[#t] if this window is shown when its ancestors are shown, or
 @racket[#f] if this window remains hidden when its ancestors are
 shown. (That is, the result of this method is affected only by calls
 to @method[window<%> show] for @this-obj[], not by the visibility of
 parent windows.)}


@defmethod[(on-drop-file [pathname path?])
           void?]{

@index["drag-and-drop"]{Called} when the user drags a file onto the
 window. (On Unix, drag-and-drop is supported via the XDND
 protocol.) Drag-and-drop must first be enabled for the window with
 @method[window<%> accept-drop-files].

On Mac OS X, when the application is running and user
 double-clicks an application-handled file or drags a file onto the
 application's icon, the main thread's application file handler is
 called (see
@racket[application-file-handler]). The default handler calls the
@method[window<%> on-drop-file] method of the most-recently activated frame if drag-and-drop is
 enabled for that frame, independent of the frame's eventspace (but
 the method is called in the frame's eventspace's handler
 thread). When the application is not running, the filenames are
 provided as command-line arguments.

}

@defmethod[(on-focus [on? any/c])
           void?]{
@methspec{

@index['("keyboard focus" "notification")]{Called} when a window
 receives or loses the keyboard focus. If the argument is @racket[#t],
 the keyboard focus was received, otherwise it was lost.

Note that on Unix, keyboard focus can move to the menu bar
 when the user is selecting a menu item.

}
@methimpl{

Does nothing.



}}

@defmethod[(on-move [x (integer-in -10000 10000)]
                    [y (integer-in -10000 10000)])
           void?]{
@methspec{

Called when the window is moved. (For windows that are not top-level
 windows, ``moved'' means moved relative to the parent's top-left
 corner.) The new position is provided to the method.

}
@methimpl{

Does nothing.

}}


@defmethod[(on-size [width (integer-in 0 10000)]
                    [height (integer-in 0 10000)])
           void?]{
@methspec{

Called when the window is resized. The window's new size (in pixels)
 is provided to the method. The size values are for the entire window,
 not just the client area.

}
@methimpl{

Does nothing.

}}

@defmethod[(on-subwindow-char [receiver (is-a?/c window<%>)]
                              [event (is-a?/c key-event%)])
           boolean?]{
@methspec{

Called when this window or a child window receives a keyboard event.
 The
@method[window<%> on-subwindow-char] method of the receiver's top-level window is called first (see
@method[area<%> get-top-level-window]); if the return value is @racket[#f], then the
@method[window<%> on-subwindow-char] method is called for the next child in the path to the receiver, and
 so on. Finally, if the receiver's
@method[window<%> on-subwindow-char] method returns @racket[#f], the event is passed on to the receiver's
 normal key-handling mechanism.

The @racket[event] argument is the event that was generated for the
 @racket[receiver] window.

The atomicity limitation @method[window<%> on-subwindow-event] applies
 to @method[window<%> on-subwindow-char] as well. That is, an insufficiently cooperative
 @method[window<%> on-subwindow-char] method can effectively disable
 a control's handling of key events, even when it returns @racket[#f]

BEWARE: The default
@xmethod[frame% on-subwindow-char] and
@xmethod[dialog% on-subwindow-char] methods consume certain keyboard events (e.g., arrow keys, Enter) used
 for navigating within the window. Because the top-level window gets
 the first chance to handle the keyboard event, some events never
 reach the ``receiver'' child unless the default frame or dialog
 method is overridden.

}
@methimpl{

Returns @racket[#f].

}}

@defmethod[(on-subwindow-event [receiver (is-a?/c window<%>)]
                               [event (is-a?/c mouse-event%)])
           boolean?]{
@methspec{

Called when this window or a child window receives a mouse event.
 The
@method[window<%> on-subwindow-event] method of the receiver's top-level window is called first (see
@method[area<%> get-top-level-window]); if the return value is @racket[#f], the
@method[window<%> on-subwindow-event] method is called for the next child in the path to the receiver, and
 so on. Finally, if the receiver's
@method[window<%> on-subwindow-event] method returns @racket[#f],  the event is passed on to the
 receiver's normal mouse-handling mechanism. 

The @racket[event] argument is the event that was generated for the
 @racket[receiver] window.

If the @method[window<%> on-subwindow-event] method chain does not complete
 atomically (i.e., without requiring other threads to run) or does not complete
 fast enough, then the corresponding event may not be delivered to a target
 control, such as a button. In other words, an insufficiently cooperative
 @method[window<%> on-subwindow-event] method can effectively disable a
 control's handling of mouse events, even when it returns @racket[#f].

}
@methimpl{

Returns @racket[#f].

}}


@defmethod[(on-subwindow-focus [receiver (is-a?/c window<%>)]
                               [on? boolean?])
           void?]{

@methspec{

Called when this window or a child window receives or loses the keyboard focus.
 This method is called after the @method[window<%> on-focus] method of @racket[receiver].
 The
@method[window<%> on-subwindow-focus] method of the receiver's top-level window is called first (see
@method[area<%> get-top-level-window]), then the
@method[window<%> on-subwindow-focus] method is called for the next child in the path to the receiver, and
 so on.

}
@methimpl{

Does nothing.

}}

@defmethod[(on-superwindow-enable [enabled? any/c])
           void?]{

@methspec{

Called via the event queue whenever the enable state of a window has
 changed, either through a call to the window's
@method[window<%> enable] method, or through the enabling/disabling of one of the window's
 ancestors. The method's argument indicates whether the window is now
 enabled or not.

This method is not called when the window is initially created; it is
 called only after a change from the window's initial enable
 state. Furthermore, if an enable notification event is queued for the
 window and it reverts its enabled state before the event is
 dispatched, then the dispatch is canceled.

If the enable state of a window's ancestor changes while the window is
 deleted (e.g., because it was removed with
@method[area-container<%> delete-child]), then no enable events are queued for the deleted window. But if
 the window is later re-activated into an enable state that is
 different from the window's state when it was de-activated, then an
 enable event is immediately queued.

}
@methimpl{

Does nothing.

}}
@defmethod[(on-superwindow-show [shown? any/c])
           void?]{

@methspec{

Called via the event queue whenever the visibility of a window has
 changed, either through a call to the window's
@method[window<%> show], through the showing/hiding of one of the window's ancestors, or
 through the activating or deactivating of the window or its ancestor
 in a container (e.g., via
@method[area-container<%> delete-child]). The method's argument indicates whether the window is now
 visible or not.

This method is not called when the window is initially created; it is
 called only after a change from the window's initial
 visibility. Furthermore, if a show notification event is queued for
 the window and it reverts its visibility before the event is
 dispatched, then the dispatch is canceled.

}
@methimpl{

Does nothing.

}}

@defmethod[(popup-menu [menu (is-a?/c popup-menu%)]
                       [x (integer-in 0 10000)]
                       [y (integer-in 0 10000)])
           void?]{

@popupmenuinfo["window" "window" ""]

The @racket[menu] is popped up within the window at position
 (@racket[x], @racket[y]).

}

@defmethod[(refresh)
           void?]{

Enqueues an event to repaint the window.

}

@defmethod[(screen->client [x (integer-in -10000 10000)]
                           [y (integer-in -10000 10000)])
           (values (integer-in -10000 10000)
                   (integer-in -10000 10000))]{

@index["global coordinates"]{Converts} global coordinates to window
 local coordinates. See also @racket[client->screen] for information
 on screen coordinates.

}


@defmethod[(set-cursor [cursor (or/c (is-a?/c cursor%) #f)])
           void?]{

Sets the window's cursor. Providing @racket[#f] instead of a cursor
 value removes the window's cursor.

If a window does not have a cursor, it uses the cursor of its parent.
 Frames and dialogs start with the standard arrow cursor, and text
 fields start with an I-beam cursor. All other windows are created
 without a cursor.

}


@defmethod[(set-label [l label-string?])
           void?]{

Sets a window's label. The window's natural minimum size might be
 different after the label is changed, but the window's minimum size
 is not recomputed.

If the window was not created with a label, or if the window was
 created with a non-string label, @racket[l] is ignored.

See
@method[window<%> get-label] for more information.

}

@defmethod[(show [show? any/c])
           void?]{

Shows or hides a window.

@MonitorMethod[@elem{The visibility of a window} @elem{the user clicking the window's close box, for example} @elem{@method[window<%> on-superwindow-show] or @method[top-level-window<%> on-close]} @elem{visibility}]

If @racket[show?] is @racket[#f], the window is hidden. Otherwise, the
window is shown.

}

}

