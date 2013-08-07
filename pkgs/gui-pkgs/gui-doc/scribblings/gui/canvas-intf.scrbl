#lang scribble/doc
@(require "common.rkt")

@definterface/title[canvas<%> (subwindow<%>)]{

A canvas is a subwindow onto which graphics and text can be drawn. Canvases also
 receive mouse and keyboard events.

The @racket[canvas<%>] interface is implemented by two classes:
@itemize[

 @item{@racket[canvas%] --- a canvas for arbitrary drawing and
  event handling; and}

 @item{@racket[editor-canvas%] --- a canvas for displaying
  @racket[editor<%>] objects.}

]

To draw onto a canvas, get its device context via @method[canvas<%>
 get-dc]. There are two basic approaches to updating a canvas:

@itemlist[

 @item{Drawing normally occurs during the canvas's @method[canvas<%>
       on-paint] callback.  The @racket[canvas%] class supports a
       @racket[paint-callback] initialization argument to be called
       from the default @method[canvas<%> on-paint] method.

       A canvas's @method[canvas<%> on-paint] method is called
       automatically as an event when the windowing system determines
       that the canvas must be updated, such as when the canvas is
       first shown or when it is resized. Use the @method[window<%>
       refresh] method to explicitly trigger an @method[canvas<%>
       on-paint] call from the windowing system. (Multiple refresh
       requests before @method[canvas<%> on-paint] can be called are
       coaleced into a single @method[canvas<%> on-paint] call.)

       Before the windowing system calls @method[canvas<%> on-paint],
       it may erase the canvas's background (see @method[dc<%>
       erase]), depending on the style of the canvas (e.g., as
       determined by the @racket[style] initialization argument for
       @racket[canvas%]). Even when the canvas's style suppresses
       explicit clearing of the canvas, a canvas may be erased by the
       windowing system due to window-moving and -resizing
       operations. For a transparent canvas, ``erased'' means that the
       canvas's parent window shows through.}

 @item{Drawing can also occur at any time outside an @method[canvas<%>
       on-paint] call form the windowing system, including from
       threads other than the @tech{handler thread} of the canvas's
       eventspace. Drawing outside an @method[canvas<%> on-paint]
       callback from the system is transient in the sense that
       windowing activity can erase the canvas, but the drawing is
       persistent as long as no windowing refresh is needed.

       Calling an @method[canvas<%> on-paint] method directly is the
       same as drawing outside an @method[canvas<%> on-paint] callback
       from the windowing system. For a @racket[canvas%], use
       @method[canvas% refresh-now] to force an immediate update of
       the canvas's content that is otherwise analogous to queueing an
       update with @method[window<%> refresh].}

]

Drawing to a canvas's drawing context actually renders into an
offscreen buffer. The buffer is automatically flushed to the screen
asynchronously, explicitly via the @method[canvas<%> flush] method, or
explicitly via @racket[flush-display]---unless flushing has been
disabled for the canvas.  The @method[canvas<%> suspend-flush] method
suspends flushing for a canvas until a matching @method[canvas<%>
resume-flush] calls; calls to @method[canvas<%> suspend-flush] and
@method[canvas<%> resume-flush] can be nested, in which case flushing
is suspended until the outermost @method[canvas<%> suspend-flush] is
balanced by a @method[canvas<%> resume-flush]. An @method[canvas<%>
on-paint] call from the windowing system is implicitly wrapped with
@method[canvas<%> suspend-flush] and @method[canvas<%> resume-flush]
calls, as is a call to a paint procedure by @method[canvas% refresh-now].

In the case of a transparent canvas, line and text smoothing can
depend on the window that serves as the canvas's background. For
example, smoothing may color pixels differently depending on whether
the target context is white or gray.  Background-sensitive smoothing
is supported only if a relatively small number of drawing commands are
recorded in the canvas's offscreen buffer, however.


@defmethod*[([(accept-tab-focus)
              boolean?]
             [(accept-tab-focus [on? any/c])
              void?])]{

@index['("keyboard focus" "navigation")]{Gets} or sets whether
tab-focus is enabled for the canvas (assuming that the canvas is
not created with the @racket['no-focus] style for @racket[canvas%]). When tab-focus is
enabled, the canvas can receive the keyboard focus when the user
navigates among a frame or dialog's controls with the Tab and
arrow keys. By default, tab-focus is disabled.

When tab-focus is enabled for a @racket[canvas%] object, Tab, arrow,
 Enter, and Escape keyboard events are consumed by a frame's default
 @method[top-level-window<%> on-traverse-char] method. (In addition, a
 dialog's default method consumes Escape key events.) Otherwise,
 @method[top-level-window<%> on-traverse-char] allows the keyboard
 events to be propagated to the canvas.

For an @racket[editor-canvas%] object, handling of Tab, arrow, Enter,
 and Escape keyboard events is determined by the
 @method[editor-canvas% allow-tab-exit] method.


}


@defmethod[(flush) void?]{

Like @racket[flush-display], but constrained if possible to the canvas.}


@defmethod[(get-canvas-background)
           (or/c (is-a?/c color%) #f)]{
Returns the color currently used to ``erase'' the canvas content before
@method[canvas<%> on-paint] is called. See also
@method[canvas<%> set-canvas-background].

The result is @racket[#f] if the canvas was created with the
 @indexed-racket['transparent] style, otherwise it is always a
 @racket[color%] object.

}


@defmethod[(get-dc)
           (is-a?/c dc<%>)]{
Gets the canvas's device context. See @racket[dc<%>] for more information about
drawing.

}



@defmethod*[([(min-client-height)
              dimension-integer?]
             [(min-client-height [h dimension-integer?])
              void?])]{

Gets or sets the canvas's minimum height for geometry management,
 based on the client size rather than the full size. The client height
 is obtained or changed via
@xmethod[area<%> min-height], adding or subtracting border and scrollbar sizes as appropriate.

The minimum height is ignored when it is smaller than the canvas's
 @tech{graphical minimum height}. See @|geomdiscuss| for
 more information.
}


@defmethod*[([(min-client-width)
              dimension-integer?]
             [(min-client-width [w dimension-integer?])
              void?])]{

Gets or sets the canvas's minimum width for geometry management, based
 on the canvas's client size rather than its full size. The client
 width is obtained or changed via
@xmethod[area<%> min-width], adding or subtracting border and scrollbar sizes as appropriate.

The minimum width is ignored when it is smaller than the canvas's
 @tech{graphical minimum width}. See @|geomdiscuss| for
 more information.

}


@defmethod[(on-char [ch (is-a?/c key-event%)])
           void?]{
@methspec{

Called when the canvas receives a keyboard event.  See also
 @|mousekeydiscuss|.

}
@methimpl{

Does nothing.



}}


@defmethod[(on-event [event (is-a?/c mouse-event%)])
           void?]{
@methspec{

Called when the canvas receives a mouse event. See also
 @|mousekeydiscuss|, noting in particular that certain mouse events
 can get dropped.

}
@methimpl{

Does nothing.



}}

@defmethod[(on-paint)
           void?]{
@methspec{

Called when the canvas is exposed or resized so that the image in the
 canvas can be repainted.

When
@method[canvas<%> on-paint] is called in response to a system expose event and only a portion of
 the canvas is newly exposed, any drawing operations performed by
@method[canvas<%> on-paint] are clipped to the newly-exposed region; however, the clipping region
 as reported by
@method[dc<%> get-clipping-region] does not change.

}
@methimpl{

Does nothing.



}}

@defmethod[(on-tab-in)
           void?]{
@methspec{

Called when the keyboard focus enters the canvas via keyboard
 navigation events. The
@method[window<%> on-focus] method is also called, as usual for a focus change. When the keyboard
 focus leaves a canvas due to a navigation event, only
@method[window<%> on-focus] is called.

See also
@method[canvas<%> accept-tab-focus] and
@xmethod[top-level-window<%> on-traverse-char] .

}
@methimpl{

Does nothing.



}}


@defmethod[(resume-flush) void?]{

See @racket[canvas<%>] for information on canvas flushing.}



@defmethod[(set-canvas-background [color (is-a?/c color%)])
           void?]{

Sets the color used to ``erase'' the canvas content before
@method[canvas<%> on-paint] is called. (This color is typically associated with the canvas at a
 low level, so that it is used even when a complete refresh of the
 canvas is delayed by other activity.)

If the canvas was created with the @indexed-racket['transparent] style,
 @|MismatchExn|.

}

@defmethod[(set-resize-corner [on? any/c])
           void?]{

On Mac OS X, enables or disables space for a resize tab at the
 canvas's lower-right corner when only one scrollbar is visible. This
 method has no effect on Windows or Unix, and it has no effect when
 both or no scrollbars are visible. The resize corner is disabled by
 default, but it can be enabled when a canvas is created with the
 @racket['resize-corner] style.

}


@defmethod[(suspend-flush) void?]{

See @racket[canvas<%>] for information on canvas flushing.

Beware that suspending flushing for a canvas can discourage refreshes
for other windows in the same frame on some platforms.}

}

