#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@definterface[canvas<%> (subwindow<%>)]{

A canvas is a subwindow onto which graphics and text can be drawn. Canvases also
 receive mouse and keyboard events.

To draw onto a canvas, get its device context (see
@method[canvas<%> get-dc]).

The @scheme[canvas<%>] interface is implemented by two classes:
@itemize{

 @item{@scheme[canvas%] --- a canvas for arbitrary drawing and
  event handling}

 @item{@scheme[editor-canvas%] --- a canvas for displaying
  @scheme[editor<%>] objects}

}




@defmethod[(on-char [ch (is-a/c key-event%)])
           void?]{
@spec{

Called when the canvas receives a keyboard event.  See also
 @|mousekeydiscuss|.

}
@impl{

Does nothing.



}}

@defmethod[(on-event [event (is-a/c mouse-event%)])
           void?]{
@spec{

Called when the canvas receives a mouse event. See also
 @|mousekeydiscuss|, noting in particular that certain mouse events
 can get dropped.

}
@impl{

Does nothing.



}}

@defmethod[(on-paint)
           void?]{
@spec{

Called when the canvas is exposed or resized so that the image in the
 canvas can be repainted.

When
@method[canvas<%> on-paint] is called in response to a system expose event and only a portion of
 the canvas is newly exposed, any drawing operations performed by
@method[canvas<%> on-paint] are clipped to the newly-exposed region; however, the clipping region
 as reported by
@method[dc<%> get-clipping-region] does not change.

}
@impl{

Does nothing.



}}

@defmethod[(on-tab-in)
           void?]{
@spec{

Called when the keyboard focus enters the canvas via keyboard
 navigation events. The
@method[window<%> on-focus] method is also called, as usual for a focus change. When the keyboard
 focus leaves a canvas due to a navigation event, only
@method[window<%> on-focus] is called.

See also
@xmethod[canvas% accept-tab-focus] and
@xmethod[top-level-window<%> on-traverse-char] .

}
@impl{

Does nothing.



}}

@defmethod[(get-dc)
           (is-a/c dc<%>)]{
@spec{

Gets the canvas's device context. See
@scheme[dc<%>] for more information about drawing.

}}

@defmethod*[([(min-client-width)
              (integer-in 0 10000)]
             [(min-client-width [w (integer-in 0 10000)])
              void?])]{
@spec{

Gets or sets the canvas's minimum width for geometry management, based
 on the canvas's client size rather than its full size. The client
 width is obtained or changed via
@xmethod[area<%> min-width], adding or subtracting border and scrollbar sizes as appropriate.

The minimum width is ignored when it is smaller than the canvas's
 \DefinedElsewhere{minimum graphical width}. See @|geomdiscuss| for
 more information.

}
@impl{
First case:


Returns the current minimum client width (in pixels).



Second case:


Sets the minimum client width (in pixels).



}}

@defmethod*[([(min-client-height)
              (integer-in 0 10000)]
             [(min-client-height [h (integer-in 0 10000)])
              void?])]{
@spec{

Gets or sets the canvas's minimum height for geometry management,
 based on the client size rather than the full size. The client height
 is obtained or changed via
@xmethod[area<%> min-height], adding or subtracting border and scrollbar sizes as appropriate.

The minimum height is ignored when it is smaller than the canvas's
 \DefinedElsewhere{minimum graphical height}. See @|geomdiscuss| for
 more information.

}
@impl{
First case:


Returns the current minimum client height (in pixels).



Second case:


Sets the minimum client height (in pixels).



}}

@defmethod[(warp-pointer [x (integer-in 0 10000)]
                         [y (integer-in 0 10000)])
           void?]{
@spec{

Moves the cursor to the given location on the canvas.

}}

@defmethod[(set-canvas-background [color (is-a/c color%)])
           void?]{
@spec{

Sets the color used to ``erase'' the canvas content before
@method[canvas<%> on-paint] is called. (This color is typically associated with the canvas at a
 low level, so that it is used even when a complete refresh of the
 canvas is delayed by other activity.)

If the canvas was created with the @indexed-scheme['transparent] style,
 @|MismatchExn|}.

}
@impl{

Sets the canvas's background.



}}

@defmethod[(get-canvas-background)
           (or/c (is-a/c color%) false/c)]{
@spec{

Returns the color currently used to ``erase'' the canvas content before
@method[canvas<%> on-paint] is called. See also
@method[canvas<%> set-canvas-background].

The result is @scheme[#f] if the canvas was created with the
 @indexed-scheme['transparent] style, otherwise it is always a
 @scheme[color%] object.

}}

@defmethod[(set-resize-corner [on? any/c])
           void?]{
@spec{

Under Mac OS X, enables or disables space for a resize tab at the
 canvas's lower-right corner when only one scrollbar is visible. This
 method has no effect under Windows or X, and it has no effect when
 both or no scrollbars are visible. The resize corner is disabled by
 default, but it can be enabled when a canvas is created with the
 @scheme['resize-corner] style.

}
@impl{



}}}

