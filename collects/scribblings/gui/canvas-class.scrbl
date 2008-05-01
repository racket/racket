#lang scribble/doc
@(require "common.ss")

@defclass/title[canvas% object% (canvas<%>)]{

A @scheme[canvas%] object is a general-purpose window for drawing
 and handling events.


@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'border 'control-border 'combo 
                                          'vscroll 'hscroll 'resize-corner
                                          'gl 'no-autoclear 'transparent
                                          'no-focus 'deleted)) null]
                 [paint-callback ((is-a?/c button%) (is-a?/c dc<%>) . -> . any) void]
                 [label (or/c label-string? false/c) #f]
                 [gl-config (or/c (is-a?/c gl-config%) false/c) #f]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 0]
                 [horiz-margin (integer-in 0 1000) 0]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

The @scheme[style] argument indicates one or more of the following styles:

@itemize{

 @item{@scheme['border] --- gives the canvas a thin border}

 @item{@scheme['control-border] --- gives the canvas a border that is
 like a @scheme[text-field%] control}

 @item{@scheme['combo] --- gives the canvas a combo button that is like
 a @scheme[combo-field%] control; this style is intended for use
 with @scheme['control-border] and not with @scheme['hscroll] or
 @scheme['vscroll]}

 @item{@scheme['hscroll] --- enables horizontal scrolling (initially visible but inactive)}

 @item{@scheme['vscroll] --- enables vertical scrolling (initially visible but inactive)}

 @item{@scheme['resize-corner] --- leaves room for a resize control at the canvas's
                                  bottom right when only one scrollbar is visible}

 @item{@scheme['gl] --- @italic{obsolete} (every canvas is an OpenGL context where supported)}

 @item{@scheme['no-autoclear] --- prevents automatic erasing of the
 canvas before calls to
@method[canvas% on-paint]} 
 @item{@scheme['transparent] --- the canvas is automatically ``erased''
 before an update using it's parent window's background; the result is
 undefined if this flag is combined with @scheme['no-autoclear]}
 
 @item{@scheme['no-focus] --- prevents the canvas from accepting the
 keyboard focus when the canvas is clicked, or when the
@method[window<%> focus]   method is called}

 @item{@scheme['deleted] --- creates the canvas as initially hidden and without affecting
                             @scheme[parent]'s geometry; the canvas can be made active
                             later by calling @scheme[parent]'s @method[area-container<%> add-child]
                             method}
 
}

The @scheme['hscroll] and @scheme['vscroll] styles create a
 canvas with an initially inactive scrollbar. The scrollbars are
 activated with either
@method[canvas% init-manual-scrollbars] or
@method[canvas% init-auto-scrollbars], and they can be hidden and re-shown with
@method[canvas% show-scrollbars].

The @scheme[paint-callback] argument is called by the default
@method[canvas% on-paint] method, using the canvas and the DC returned by
@method[canvas<%> get-dc] as the argument.

The @scheme[label] argument names the canvas for
@method[window<%> get-label], but it is not displayed with the canvas.

The @scheme[gl-config] argument determines properties of an OpenGL
 context for this canvas, as obtained through the canvas's drawing
 context. See also
@method[canvas<%> get-dc] and
@xmethod[dc<%> get-gl-context].


@WindowKWs[] @SubareaKWs[] @AreaKWs[]

}

@defmethod*[([(accept-tab-focus)
              boolean?]
             [(accept-tab-focus [on? any/c])
              void?])]{

@index['("keyboard focus" "navigation")]{Gets} or sets whether
tab-focus is enabled for the canvas (assuming that the canvas is
not created with the @scheme['no-focus] style). When tab-focus is
enabled, the canvas can receive the keyboard focus when the user
navigates among a frame or dialog's controls with the Tab and
arrow keys. By default, tab-focus is disabled.

When tab-focus is enabled for a canvas, Tab, arrow, and Enter keyboard
 events are consumed by a frame's default
@method[top-level-window<%> on-traverse-char] method. (In addition, a dialog's default method consumes Escape key
 events.) Otherwise,
@method[top-level-window<%> on-traverse-char] allows the keyboard events to be propagated to the canvas.
}


@defmethod[(get-scroll-page [which (one-of/c 'horizontal 'vertical)])
           (integer-in 1 10000)]{

Get the current page step size of a manual scrollbar. The result is
 @scheme[0] if the scrollbar is not active or it is automatic.

The @scheme[which] argument is either @scheme['horizontal] or
 @scheme['vertical], indicating whether to get the page step size of
 the horizontal or vertical scrollbar, respectively.

See also
@method[canvas% init-manual-scrollbars].
}


@defmethod[(get-scroll-pos [which (one-of/c 'horizontal 'vertical)])
           (integer-in 0 10000)]{

Gets the current value of a manual scrollbar. The result is always
 @scheme[0] if the scrollbar is not active or it is automatic.

The @scheme[which] argument is either @scheme['horizontal] or
 @scheme['vertical], indicating that the value of the horizontal or
 vertical scrollbar should be returned, respectively.

See also
@method[canvas% init-manual-scrollbars].
}


@defmethod[(get-scroll-range [which (one-of/c 'horizontal 'vertical)])
           (integer-in 0 10000)]{

Gets the current maximum value of a manual scrollbar. The result is
 always @scheme[0] if the scrollbar is not active or it is automatic.

The @scheme[which] argument is either @scheme['horizontal] or
 @scheme['vertical], indicating whether to get the maximum value of the
 horizontal or vertical scrollbar, respectively.

See also
@method[canvas% init-manual-scrollbars].
}


@defmethod[(get-view-start)
           (values (integer-in 0 10000) (integer-in 0 10000))]{

Get the location at which the visible portion of the canvas
starts, based on the current values of the horizontal and
vertical scrollbars if they are initialized as automatic (see
@method[canvas% init-auto-scrollbars]). Combined with
@method[window<%> get-client-size], an application can
efficiently redraw only the visible portion of the canvas.  The
values are in pixels.

If the scrollbars are disabled or initialized as manual (see
@method[canvas% init-manual-scrollbars]), the result is @scheme[(values 0 0)].

}


@defmethod[(get-virtual-size)
           (value (integer-in 0 10000) (integer-in 0 10000))]{
Gets the size in device units of the scrollable canvas area (as
 opposed to the client size, which is the area of the canvas currently
 visible). This is the same size as the client size (as returned by
@method[window<%> get-client-size]) unless scrollbars are initialized as automatic (see
@method[canvas% init-auto-scrollbars]).

}


@defmethod[(init-auto-scrollbars [horiz-pixels (or/c (integer-in 1 10000) false/c)]
                                 [vert-pixels (or/c (integer-in 1 10000) false/c)]
                                 [h-value (real-in 0.0 1.0)]
                                 [v-value (real-in 0.0 1.0)])
           void?]{

Enables and initializes automatic scrollbars for the canvas.  A
 horizontal or vertical scrollbar can be activated only in a canvas
 that was created with the @indexed-scheme['hscroll] or
 @indexed-scheme['vscroll] style flag, respectively.

With automatic scrollbars, the programmer specifies the desired
 virtual size of the canvas, and the scrollbars are automatically
 handled to allow the user to scroll around the virtual area. The
 scrollbars are not automatically hidden if they are unneeded; see
@method[canvas% show-scrollbars].

See also
@method[canvas% init-manual-scrollbars] for information about manual scrollbars. The horizontal and vertical
 scrollbars are always either both manual or both automatic, but they
 are independently enabled. Automatic scrollbars can be
 re-initialized as manual, and vice-versa.

If either @scheme[horiz-pixels] or @scheme[vert-pixels] is
 @scheme[#f], the scrollbar is not enabled in the corresponding
 direction, and the canvas's virtual size in that direction is the
 same as its client size.

The @scheme[h-value] and @scheme[v-value] arguments specify the initial
 values of the scrollbars as a fraction of the scrollbar's range.  A
 @scheme[0.0] value initializes the scrollbar to its left/top, while a
 @scheme[1.0] value initializes the scrollbar to its right/bottom.

See also
@method[canvas% on-scroll] and
@method[canvas% get-virtual-size].

}

@defmethod[(init-manual-scrollbars [h-length (or/c (integer-in 0 10000) false/c)]
                                   [v-length (or/c (integer-in 0 10000) false/c)]
                                   [h-page (integer-in 1 10000)]
                                   [v-page (integer-in 1 10000)]
                                   [h-value (integer-in 0 10000)]
                                   [v-value (integer-in 0 10000)])
           void?]{

Enables and initializes manual scrollbars for the canvas.  A
 horizontal or vertical scrollbar can be activated only in a canvas
 that was created with the @indexed-scheme['hscroll] or
 @indexed-scheme['vscroll] style flag, respectively.

With manual scrollbars, the programmer is responsible for managing all
 details of the scrollbars, and the scrollbar state has no effect on
 the canvas's virtual size. Instead, the canvas's virtual size is the
 same as its client size.

See also
@method[canvas% init-auto-scrollbars] for information about automatic scrollbars. The horizontal and vertical
 scrollbars are always either both manual or both automatic, but they
 are independently enabled. Automatic scrollbars can be re-initialized
 as manual, and vice-versa.

The @scheme[h-length] and @scheme[v-length] arguments specify the length of
 each scrollbar in scroll steps (i.e., the maximum value of each
 scrollbar). If either is @scheme[#f], the scrollbar is disabled in the
corresponding direction.

The @scheme[h-page] and @scheme[v-page] arguments set the number of
 scrollbar steps in a page, i.e., the amount moved when pressing above
 or below the value indicator in the scrollbar control.

The @scheme[h-value] and @scheme[v-value] arguments specify the initial
 values of the scrollbars.

If @scheme[h-value] is greater than @scheme[h-length] or @scheme[v-value] is
 greater than @scheme[v-length], @|MismatchExn|. (The page step may be
 larger than the total size of a scrollbar.)

See also
@method[canvas% on-scroll] and
@method[canvas% get-virtual-size].

}


@defmethod[#:mode override 
           (on-paint)
           void?]{

Calls the procedure supplied as the @scheme[paint-callback] argument when
 the @scheme[canvas%] was created.

}


@defmethod[(on-scroll [event (is-a?/c scroll-event%)])
           void?]{
Called when the user changes one of the canvas's scrollbars. A
 @scheme[scroll-event%] argument provides information about the
 scroll action.

This method is called only when manual
 scrollbars are changed, not automatic scrollbars; for automatic scrollbars,
 the
@method[canvas<%> on-paint] method is called, instead.

}


@defmethod[(scroll [h-value (or/c (real-in 0.0 1.0) false/c)]
                   [v-value (or/c (real-in 0.0 1.0) false/c)])
           void?]{

Sets the values of automatic scrollbars. (This method has no effect on
 manual scrollbars.)

If either argument is @scheme[#f], the scrollbar value is not changed in
 the corresponding direction.

The @scheme[h-value] and @scheme[v-value] arguments each specify a fraction
 of the scrollbar's movement.  A @scheme[0.0] value sets the scrollbar to
 its left/top, while a @scheme[1.0] value sets the scrollbar to its
 right/bottom. A @scheme[0.5] value sets the scrollbar to its middle. In
 general, if the canvas's virtual size is @scheme[v], its client size is
 @scheme[c], and @scheme[(> v c)], then scrolling to @scheme[p]
 sets the view start to @scheme[(floor (* p (- v c)))].

See also
@method[canvas% init-auto-scrollbars] and
@method[canvas% get-view-start].

}


@defmethod[(set-scroll-page [which (one-of/c 'horizontal 'vertical)]
                            [value (integer-in 1 10000)])
           void?]{

Set the current page step size of a manual scrollbar. (This method has
 no effect on automatic scrollbars.)

The @scheme[which] argument is either @scheme['horizontal] or
 @scheme['vertical], indicating whether to set the page step size of
 the horizontal or vertical scrollbar, respectively.

See also
@method[canvas% init-manual-scrollbars].

}


@defmethod[(set-scroll-pos [which (one-of/c 'horizontal 'vertical)]
                           [value (integer-in 0 10000)])
           void?]{

Sets the current value of a manual scrollbar. (This method has no
 effect on automatic scrollbars.)

The @scheme[which] argument is either @scheme['horizontal] or
 @scheme['vertical], indicating whether to set the value of the
 horizontal or vertical scrollbar set, respectively.

@MonitorMethod[@elem{The value of the canvas's scrollbar} @elem{the user scrolling} @elem{@method[canvas% on-scroll]} @elem{scrollbar value}]

See also
@method[canvas% init-manual-scrollbars] and
@method[canvas% scroll].

}


@defmethod[(set-scroll-range [which (one-of/c 'horizontal 'vertical)]
                             [value (integer-in 0 10000)])
           void?]{

Sets the current maximum value of a manual scrollbar. (This method has
 no effect on automatic scrollbars.)

The @scheme[which] argument is either @scheme['horizontal] or
 @scheme['vertical], indicating whether to set the maximum value of the
 horizontal or vertical scrollbar, respectively.

See also
@method[canvas% init-manual-scrollbars].
}


@defmethod[(show-scrollbars [show-horiz? any/c]
                            [show-vert? any/c])
           void?]{

Shows or hides the scrollbars as indicated by
@scheme[show-horiz?] and @scheme[show-vert?]. If
@scheme[show-horiz?] is true and the canvas was not created with
the @scheme['hscroll] style, @|MismatchExn|.  Similarly, if
@scheme[show-vert?] is true and the canvas was not created with
the @scheme['vscroll] style, @|MismatchExn|.


The horizontal scrollbar can be shown only if the canvas was
created with the @scheme['hscroll] style, and the vertical
scrollbar can be shown only if the canvas was created with the
@scheme['vscroll] style. See also @method[canvas%
init-auto-scrollbars] and @method[canvas%
init-manual-scrollbars].

}


@defmethod[(swap-gl-buffers)
           void?]{
Calls
@method[gl-context<%> swap-buffers]
on the result of 
@method[dc<%> get-gl-context]
for this canvas's DC as returned by
@method[canvas<%> get-dc].

The 
@method[gl-context<%> swap-buffers]
method acquires a re-entrant lock, so nested calls to
@method[canvas% with-gl-context] on different threads or OpenGL contexts can block or deadlock.


}


@defmethod[(with-gl-context [thunk (-> any)])
           any]{
Passes the given thunk to
@method[gl-context<%> call-as-current]
of the result of 
@method[dc<%> get-gl-context]
for this canvas's DC as returned by
@method[canvas<%> get-dc].

The 
@method[gl-context<%> call-as-current]
method acquires a re-entrant lock, so nested calls to
@method[canvas% with-gl-context] on different threads or OpenGL contexts can block or deadlock.


}
}

