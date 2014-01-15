#lang scribble/doc
@(require "common.rkt")

@defclass/title[canvas% object% (canvas<%>)]{

A @racket[canvas%] object is a general-purpose window for drawing and
 handling events. See @racket[canvas<%>] for information about drawing
 onto a canvas.


@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (or/c 'border 'control-border 'combo 
                                      'vscroll 'hscroll 'resize-corner
                                      'gl 'no-autoclear 'transparent
                                      'no-focus 'deleted)) null]
                 [paint-callback ((is-a?/c canvas%) (is-a?/c dc<%>) . -> . any) void]
                 [label (or/c label-string? #f) #f]
                 [gl-config (or/c (is-a?/c gl-config%) #f) #f]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 0]
                 [horiz-margin spacing-integer? 0]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

The @racket[style] argument indicates one or more of the following styles:

@itemize[

 @item{@racket['border] --- gives the canvas a thin border}

 @item{@racket['control-border] --- gives the canvas a border that is
 like a @racket[text-field%] control}

 @item{@racket['combo] --- gives the canvas a combo button that is like
 a @racket[combo-field%] control; this style is intended for use
 with @racket['control-border] and not with @racket['hscroll] or
 @racket['vscroll]}

 @item{@racket['hscroll] --- enables horizontal scrolling (initially visible but inactive)}

 @item{@racket['vscroll] --- enables vertical scrolling (initially visible but inactive)}

 @item{@racket['resize-corner] --- leaves room for a resize control at the canvas's
                                  bottom right when only one scrollbar is visible}

 @item{@racket['gl] --- creates a canvas for OpenGL drawing instead of
       normal @racket[dc<%>] drawing; call the @method[dc<%>
       get-gl-context] method on the result of @method[canvas<%>
       get-dc]; this style is usually combined with
       @racket['no-autoclear]}

 @item{@racket['no-autoclear] --- prevents automatic erasing of the
       canvas by the windowing system; see @racket[canvas<%>] for
       information on canvas refresh}

 @item{@racket['transparent] --- the canvas is ``erased'' by the
 windowing system by letting its parent show through; see
 @racket[canvas<%>] for information on window refresh and on the
 interaction of @racket['transparent] and offscreen buffering; the
 result is undefined if this flag is combined with
 @racket['no-autoclear]}
 
 @item{@racket['no-focus] --- prevents the canvas from accepting the
 keyboard focus when the canvas is clicked or when the
@method[window<%> focus]   method is called}

 @item{@racket['deleted] --- creates the canvas as initially hidden and without affecting
                             @racket[parent]'s geometry; the canvas can be made active
                             later by calling @racket[parent]'s @method[area-container<%> add-child]
                             method}
 
]

The @racket['hscroll] and @racket['vscroll] styles create a
 canvas with an initially inactive scrollbar. The scrollbars are
 activated with either
@method[canvas% init-manual-scrollbars] or
@method[canvas% init-auto-scrollbars], and they can be hidden and re-shown with
@method[canvas% show-scrollbars].

The @racket[paint-callback] argument is called by the default
@method[canvas% on-paint] method, using the canvas and the DC returned by
@method[canvas<%> get-dc] as the argument.

The @racket[label] argument names the canvas for
@method[window<%> get-label], but it is not displayed with the canvas.

The @racket[gl-config] argument determines properties of an OpenGL
 context for this canvas, as obtained through the canvas's drawing
 context. See also
@method[canvas<%> get-dc] and
@xmethod[dc<%> get-gl-context].


@WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]

}


@defmethod[(get-scroll-page [which (or/c 'horizontal 'vertical)])
           positive-dimension-integer?]{

Get the current page step size of a manual scrollbar. The result is
 @racket[0] if the scrollbar is not active or it is automatic.

The @racket[which] argument is either @racket['horizontal] or
 @racket['vertical], indicating whether to get the page step size of
 the horizontal or vertical scrollbar, respectively.

See also
@method[canvas% init-manual-scrollbars].
}


@defmethod[(get-scroll-pos [which (or/c 'horizontal 'vertical)])
           dimension-integer?]{

Gets the current value of a manual scrollbar. The result is always
 @racket[0] if the scrollbar is not active or it is automatic.

The @racket[which] argument is either @racket['horizontal] or
 @racket['vertical], indicating that the value of the horizontal or
 vertical scrollbar should be returned, respectively.

See also
@method[canvas% init-manual-scrollbars].
}


@defmethod[(get-scroll-range [which (or/c 'horizontal 'vertical)])
           dimension-integer?]{

Gets the current maximum value of a manual scrollbar. The result is
 always @racket[0] if the scrollbar is not active or it is automatic.

The @racket[which] argument is either @racket['horizontal] or
 @racket['vertical], indicating whether to get the maximum value of the
 horizontal or vertical scrollbar, respectively.

See also
@method[canvas% init-manual-scrollbars].
}


@defmethod[(get-view-start)
           (values dimension-integer? dimension-integer?)]{

Get the location at which the visible portion of the canvas
starts, based on the current values of the horizontal and
vertical scrollbars if they are initialized as automatic (see
@method[canvas% init-auto-scrollbars]). Combined with
@method[window<%> get-client-size], an application can
efficiently redraw only the visible portion of the canvas.  The
values are in pixels.

If the scrollbars are disabled or initialized as manual (see
@method[canvas% init-manual-scrollbars]), the result is @racket[(values 0 0)].

}


@defmethod[(get-virtual-size)
           (value dimension-integer? dimension-integer?)]{
Gets the size in device units of the scrollable canvas area (as
 opposed to the client size, which is the area of the canvas currently
 visible). This is the same size as the client size (as returned by
@method[window<%> get-client-size]) unless scrollbars are initialized as automatic (see
@method[canvas% init-auto-scrollbars]).

}


@defmethod[(init-auto-scrollbars [horiz-pixels (or/c positive-dimension-integer? #f)]
                                 [vert-pixels (or/c positive-dimension-integer? #f)]
                                 [h-value (real-in 0.0 1.0)]
                                 [v-value (real-in 0.0 1.0)])
           void?]{

Enables and initializes automatic scrollbars for the canvas.  A
 horizontal or vertical scrollbar can be activated only in a canvas
 that was created with the @indexed-racket['hscroll] or
 @indexed-racket['vscroll] style flag, respectively.

With automatic scrollbars, the programmer specifies the desired
 virtual size of the canvas, and the scrollbars are automatically
 handled to allow the user to scroll around the virtual area. The
 scrollbars are not automatically hidden if they are unneeded; see
@method[canvas% show-scrollbars]. 

The coordinates for mouse
events (passed to @method[canvas<%> on-event]) are not adjusted to
account for the position of the scrollbar; 
use the @method[canvas% get-view-start] method to find suitable
offsets.

See also
@method[canvas% init-manual-scrollbars] for information about manual scrollbars. The horizontal and vertical
 scrollbars are always either both manual or both automatic, but they
 are independently enabled. Automatic scrollbars can be
 re-initialized as manual, and vice versa.

If either @racket[horiz-pixels] or @racket[vert-pixels] is
 @racket[#f], the scrollbar is not enabled in the corresponding
 direction, and the canvas's virtual size in that direction is the
 same as its client size.

The @racket[h-value] and @racket[v-value] arguments specify the initial
 values of the scrollbars as a fraction of the scrollbar's range.  A
 @racket[0.0] value initializes the scrollbar to its left/top, while a
 @racket[1.0] value initializes the scrollbar to its right/bottom.

It is possible to adjust the virtual sizes by calling this function again.


See also
@method[canvas% on-scroll] and
@method[canvas% get-virtual-size].

}

@defmethod[(init-manual-scrollbars [h-length (or/c dimension-integer? #f)]
                                   [v-length (or/c dimension-integer? #f)]
                                   [h-page positive-dimension-integer?]
                                   [v-page positive-dimension-integer?]
                                   [h-value dimension-integer?]
                                   [v-value dimension-integer?])
           void?]{

Enables and initializes manual scrollbars for the canvas.  A
 horizontal or vertical scrollbar can be activated only in a canvas
 that was created with the @indexed-racket['hscroll] or
 @indexed-racket['vscroll] style flag, respectively.

With manual scrollbars, the programmer is responsible for managing all
 details of the scrollbars, and the scrollbar state has no effect on
 the canvas's virtual size. Instead, the canvas's virtual size is the
 same as its client size.

See also
@method[canvas% init-auto-scrollbars] for information about automatic scrollbars. The horizontal and vertical
 scrollbars are always either both manual or both automatic, but they
 are independently enabled. Automatic scrollbars can be re-initialized
 as manual, and vice versa.

The @racket[h-length] and @racket[v-length] arguments specify the length of
 each scrollbar in scroll steps (i.e., the maximum value of each
 scrollbar). If either is @racket[#f], the scrollbar is disabled in the
corresponding direction.

The @racket[h-page] and @racket[v-page] arguments set the number of
 scrollbar steps in a page, i.e., the amount moved when pressing above
 or below the value indicator in the scrollbar control.

The @racket[h-value] and @racket[v-value] arguments specify the initial
 values of the scrollbars.

If @racket[h-value] is greater than @racket[h-length] or @racket[v-value] is
 greater than @racket[v-length], @|MismatchExn|. (The page step may be
 larger than the total size of a scrollbar.)

See also
@method[canvas% on-scroll] and
@method[canvas% get-virtual-size].

}

@defmethod[(make-bitmap [width exact-positive-integer?]
                        [height exact-positive-integer?]) 
           (is-a/c? bitmap%)]{

Creates a bitmap that draws in a way that is the same as drawing to the
canvas. See also @racket[make-screen-bitmap]
and @secref[#:doc '(lib "scribblings/draw/draw.scrbl") "Portability"].}


@defmethod[#:mode override 
           (on-paint)
           void?]{

Calls the procedure supplied as the @racket[paint-callback] argument when
 the @racket[canvas%] was created.
}


@defmethod[(on-scroll [event (is-a?/c scroll-event%)])
           void?]{
Called when the user changes one of the canvas's scrollbars. A
 @racket[scroll-event%] argument provides information about the
 scroll action.

This method is called only when manual
 scrollbars are changed (see @method[canvas% init-manual-scrollbars]), 
 not automatic scrollbars; for automatic scrollbars,
 the
@method[canvas<%> on-paint] method is called, instead.

}


@defmethod[(refresh-now [paint-proc ((is-a?/c dc<%>) . -> . any)
                                    (lambda (dc) (send @#,this-obj[] on-paint))]
                        [#:flush? flush? any/c #t])
           void?]{

Calls @racket[paint-proc] with the canvas's drawing context to immediately
update the canvas (in contrast to @method[window<%> refresh], which merely
queues an update request to be handled at the windowing system's discretion).

Before @racket[paint-proc] is called, flushing is disabled for the
canvas. Also, the canvas is erased, unless the canvas has the
@racket['no-autoclear] style. After @racket[paint-proc] returns,
flushing is enabled, and if @racket[flush?] is true, then
@method[canvas<%> flush] is called immediately.}


@defmethod[(scroll [h-value (or/c (real-in 0.0 1.0) #f)]
                   [v-value (or/c (real-in 0.0 1.0) #f)])
           void?]{

Sets the values of automatic scrollbars. (This method has no effect on
 manual scrollbars.)

If either argument is @racket[#f], the scrollbar value is not changed in
 the corresponding direction.

The @racket[h-value] and @racket[v-value] arguments each specify a fraction
 of the scrollbar's movement.  A @racket[0.0] value sets the scrollbar to
 its left/top, while a @racket[1.0] value sets the scrollbar to its
 right/bottom. A @racket[0.5] value sets the scrollbar to its middle. In
 general, if the canvas's virtual size is @racket[_v], its client size is
 @racket[_c], and @racket[(> _v _c)], then scrolling to @racket[_p]
 sets the view start to @racket[(floor (* _p (- _v _c)))].

See also
@method[canvas% init-auto-scrollbars] and
@method[canvas% get-view-start].

}


@defmethod[(set-scroll-page [which (or/c 'horizontal 'vertical)]
                            [value positive-dimension-integer?])
           void?]{

Set the current page step size of a manual scrollbar. (This method has
 no effect on automatic scrollbars.)

The @racket[which] argument is either @racket['horizontal] or
 @racket['vertical], indicating whether to set the page step size of
 the horizontal or vertical scrollbar, respectively.

See also
@method[canvas% init-manual-scrollbars].

}


@defmethod[(set-scroll-pos [which (or/c 'horizontal 'vertical)]
                           [value dimension-integer?])
           void?]{

Sets the current value of a manual scrollbar. (This method has no
 effect on automatic scrollbars.)

The @racket[which] argument is either @racket['horizontal] or
 @racket['vertical], indicating whether to set the value of the
 horizontal or vertical scrollbar set, respectively.

@MonitorMethod[@elem{The value of the canvas's scrollbar} @elem{the user scrolling} @elem{@method[canvas% on-scroll]} @elem{scrollbar value}]

See also
@method[canvas% init-manual-scrollbars] and
@method[canvas% scroll].

}


@defmethod[(set-scroll-range [which (or/c 'horizontal 'vertical)]
                             [value dimension-integer?])
           void?]{

Sets the current maximum value of a manual scrollbar. (This method has
 no effect on automatic scrollbars.)

The @racket[which] argument is either @racket['horizontal] or
 @racket['vertical], indicating whether to set the maximum value of the
 horizontal or vertical scrollbar, respectively.

See also
@method[canvas% init-manual-scrollbars].
}


@defmethod[(show-scrollbars [show-horiz? any/c]
                            [show-vert? any/c])
           void?]{

Shows or hides the scrollbars as indicated by
@racket[show-horiz?] and @racket[show-vert?]. If
@racket[show-horiz?] is true and the canvas was not created with
the @racket['hscroll] style, @|MismatchExn|.  Similarly, if
@racket[show-vert?] is true and the canvas was not created with
the @racket['vscroll] style, @|MismatchExn|.


The horizontal scrollbar can be shown only if the canvas was
created with the @racket['hscroll] style, and the vertical
scrollbar can be shown only if the canvas was created with the
@racket['vscroll] style. See also @method[canvas%
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
@xmethod[gl-context<%> swap-buffers]
method acquires a re-entrant lock, so nested calls to
@method[canvas% swap-gl-buffers] or @method[canvas% with-gl-context]
on different threads or OpenGL contexts can block or deadlock.


}


@defmethod[(with-gl-context [thunk (-> any)]
			    [#:fail fail (-> any) (lambda () (error ....))])
           any]{
Passes the given thunk to
@method[gl-context<%> call-as-current]
of the result of 
@method[dc<%> get-gl-context]
for this canvas's DC as returned by
@method[canvas<%> get-dc]. If @method[dc<%> get-gl-context]
returns @racket[#f], then @racket[fail] is called,
instead.

The 
@xmethod[gl-context<%> call-as-current]
method acquires a re-entrant lock, so nested calls to
@method[canvas% with-gl-context] or @method[canvas% swap-gl-buffers]
on different threads or OpenGL contexts can block or deadlock.


}
}

