

@class XfwfSlider2 (XfwfLabel) @file=xwSlider2

@ A Slider2 widget consiabout. A Slider2 is typically used to pan or scroll another window; as
such is can replace two scrollbars. The thumb can be dragged with the mouse,
or the mouse can be clicked next to the thumb, to move it in the direction of
the mouse. The thumb may contain one or more lines of text, although there
is usually no room for more than one or two words.

The widget has three callbacks. The thumb position and size are not
controled by resources, but by calling a function.


@public

@ The color of the thumb is by default set to the default background
color of the display, but it can be changed with the |thumbColor|
resource. It is also possible to tile the thumb with a pixmap, see
below.

	@var Pixel thumbColor = <String> XtDefaultBackground

@ Instead of a color, the thumb can also be tiled with a pixmap.
However, there is currently no converter from string to pixmap, so
this resource can only be set from the application, not from resource
files.

If both |thumbColor| and |thumbPixmap| are set, the pixmap takes
precedence.

	@var Pixmap thumbPixmap = NULL

@ The minimum size of the thumb is by default 20 pixels. It can be set
with the |minsize| resource.

	@var Dimension minsize = 20

@ The width of the frame around the thumb is independent of the frame
around the whole widget. It can be set with |thumbFrameWidth|.

	@var Dimension thumbFrameWidth = 2

@ The style of the frame around the thumb is set with
|thumbFrameType|. By default, it is |XfwfRaised|. Note that there are no
resources to control the shadow scheme of the thumb independently from
that of the outer frame. That means that the resources |shadowScheme|,
|topShadowColor|, |topShadowStipple|, etc, also influence the frame of the
thumb.

	@var FrameType thumbFrameType = XfwfRaised

@ The routines on the callback list are called whenever the user
manipulates the slider and also when the Slider2 receives a call on
its |scrollResponse| function with a reason other than |XfwfSNotify|.

The |call_data| parameter of the callback routines is a pointer
to an |XfwfScrollInfo| structure, which looks like this: |typedef
struct _XfwfScrollInfo { XfwfSReason reason; XfwfSFlags flags; float
vpos, vsize, hpos, hsize;} XfwfScrollInfo|.

	@var <Callback> XtCallbackList scrollCallback = NULL

@ The Slider2 widget provides has a method for dealing with scroll
requests from the application or from other widgets. A pointer to that
function can be retrieved with |XtGetValues| as the resource
|XtNscrollResponse|. This resource can only be queried, not set.

	@var XtCallbackProc scrollResponse = scroll_response

@ The default frame width is changed from 0 to 2.

	@var frameWidth = 2

@ The default frame type is now |XfwfSunken|.

	@var frameType = XfwfSunken


@classvars

@ The Core variable |compress_exposure| is OR'ed with
|XtExposeGraphicsExpose|, in order to get graphics expose events delivered
to the |expose| method.

@var compress_exposure = XtExposeCompressMultiple | XtExposeGraphicsExpose




@private

@ The position and size of the thumb are controlled by four variables
that can assume values between 0 and 1. If |thumb_x| is 0, the thumb
is located against the left side, if it is 1, the thumb is put against
the right side.

If |thumb_wd| is 1, the thumb is as large as possible, if it is 0, it
will have its minimum width |minsize|.

@var float thumb_x
@var float thumb_y
@var float thumb_wd
@var float thumb_ht

@ A boolean variable is set to when a draggin action has started, but
not yet finished.

@var Boolean drag_in_progress

@ During a drag operation, the thumb is kept at a fixed offset from
the moving mouse. The offset is stored in two local variables.

@var int m_delta_x
@var int m_delta_y

@ We also need three more GC's for the thumb and the light and dark
parts of the thumb's frame.

@var GC thumbgc
@var GC thumblightgc
@var GC thumbdarkgc
@var GC fggc



@exports

@ The |scroll.h| header file is needed for the |XfwfScrollInfo|
structure.

@incl "xwscroll.h"

@ The current size and position of the thumb can be queried with the
function |getThumb|. It fills the |info| argument with the current values.

@proc XfwfGetThumb($, XfwfScrollInfo *info)
{
    if (! XtIsSubclass($, xfwfSlider2WidgetClass))
	XtError("XfwfGetThumb called with incorrect widget type");
    info->reason = XfwfSNotify;
    info->flags = XFWF_VPOS | XFWF_VSIZE | XFWF_HPOS | XFWF_HSIZE;
    info->vpos = $thumb_y;
    info->vsize = $thumb_ht;
    info->hpos = $thumb_x;
    info->hsize = $thumb_wd;
}

@ To change the position of the thumb, a call to |moveThumb| can be
used.  The arguments must be two numbers between 0 and 1.  The thumb
is moved with |XCopyArea|.

This is a convenience function. The standard interface is through the
|scrollResponse| resource. In the Slider2 widget, that resource is
connected to the |scroll_response| method.

@proc XfwfMoveThumb($, double x, double y)
{
    XfwfScrollInfo info;

    if (! XtIsSubclass($, xfwfSlider2WidgetClass))
	XtError("XfwfMoveThumb called with incorrect widget type");
    if (x < 0.0 || x > 1.0 || y < 0.0 || y > 1.0)
	XtError("XfwfMoveThumb called with incorrect arguments");
    info.flags = XFWF_VPOS | XFWF_HPOS;
    info.reason = XfwfSNotify;
    info.vpos = y;
    info.hpos = x;
    $scroll_response(NULL, $, &info);
}

@ Resizing the thumb is done with |resizeThumb|. The two arguments
must be between 0 and 1.

This is a convenience function. The standard interface is through the
|scrollResponse| resource. In the Slider2 widget, that resource is
connected to the |scroll_response| method.

@proc XfwfResizeThumb($, double wd, double ht)
{
    XfwfScrollInfo info;

    if (! XtIsSubclass($, xfwfSlider2WidgetClass))
	XtError("XfwfResizeThumb called with incorrect widget type");
    if (wd < 0.0 || wd > 1.0 || ht < 0.0 || ht > 1.0)
	XtError("XfwfResizeThumb called with incorrect arguments");

    info.reason = XfwfSNotify;
    info.flags = XFWF_VSIZE | XFWF_HSIZE;
    info.vsize = ht;
    info.hsize = wd;
    $scroll_response(NULL, $, &info);
}



@methods

@ The |compute_thumb| method returns the position and size of the
thumb in pixels.

@proc compute_thumb($, Position *x, Position *y,
		    Dimension *width, Dimension *height)
{
    Position fx, fy;
    int fw, fh;

    #compute_inside($, &fx, &fy, &fw, &fh);
    fw = max(0, fw);
    fh = max(0, fh);
    *width = $thumb_wd * fw + 0.5;
    *height = $thumb_ht * fh + 0.5;
    if (*width < $minsize) *width = min(fw, $minsize);
    if (*height < $minsize) *height = min(fh, $minsize);
    *x = fx + $thumb_x * (fw - *width) + 0.5;
    *y = fy + $thumb_y * (fh - *height) + 0.5;
}

@ The |compute_inside| method of the Label class returns the area inside the
frame, but the label of Slider2 widget should appear in the thumb. Therefore
the |compute_inside| method is redefined. This means that the |expose|
method of the Label class can still be used, because it calls this function
to establish the position of the text.

@proc compute_inside
{
    Dimension ww, hh;

    $compute_thumb($, x, y, &ww, &hh);
    *x += $thumbFrameWidth;
    *y += $thumbFrameWidth;
    *w = (int)ww - 2 * $thumbFrameWidth;
    *h = (int)hh - 2 * $thumbFrameWidth;
}

@ The |expose| method of the superclass is called to draw the outer frame
and the text inside the thumb. Only the frame of the thumb is drawn here.

@proc _expose
{
    Position x, y;
    Dimension wd, ht, d;

    if (! XtIsRealized($)) return;
    if (region != NULL) {
	XSetRegion(XtDisplay($), $fggc, region);
	XSetRegion(XtDisplay($), $thumbgc, region);
	XSetRegion(XtDisplay($), $thumbdarkgc, region);
	XSetRegion(XtDisplay($), $thumblightgc, region);
    }
    $compute_thumb($, &x, &y, &wd, &ht);
    d = $thumbFrameWidth;
    if ((wd > 2 * d) && (ht > 2 * d)) {
      XFillRectangle(XtDisplay($), XtWindow($), $thumbgc, x + d, y + d, wd - 2 * d, ht - 2 * d);
      XfwfDrawFrame($, x, y, wd, ht, $thumbFrameType, d,
	            $thumblightgc, $thumbdarkgc, NULL);
    }
    if (region != NULL) {
	XSetClipMask(XtDisplay($), $fggc, None);
	XSetClipMask(XtDisplay($), $thumbgc, None);
	XSetClipMask(XtDisplay($), $thumbdarkgc, None);
	XSetClipMask(XtDisplay($), $thumblightgc, None);
    }
    #_expose($, event, region);
}

@ The |initialize| method only needs to set the local variables. The
|graygc| that is inherited from Label has to be defined differently,
because it now should use the thumb's background, instead of the
widget's.  (It still doesn't work right when the thumb is tiled with a
pixmap, however.)  Likewise, |gc| and |rv_gc| must be defined
differently.  The two new GC's are also initialized.

@proc initialize
{
    $text_bg = $thumbColor;
    $thumb_x = $thumb_y = 0.0;
    $thumb_wd = $thumb_ht = 1.0;
    $drag_in_progress = False;
    create_fggc($);
    create_thumbgc($);
    create_gc($);
    $thumblightgc = NULL; create_thumblightgc($);
    $thumbdarkgc = NULL; create_thumbdarkgc($);
}

@proc destroy
{
  if ($gc) XtReleaseGC($, $gc); $gc = NULL;
  if ($thumblightgc) XtReleaseGC($, $thumblightgc); $thumblightgc = NULL;
  if ($thumbdarkgc) XtReleaseGC($, $thumbdarkgc); $thumbdarkgc = NULL;
  if ($thumbgc) XtReleaseGC($, $thumbdarkgc); $thumbgc = NULL;
  if ($fggc) XtReleaseGC($, $fggc); $fggc = NULL;
}

@ The following routine's name, |move_thumb|, indicates what it is
used for, but not what it really does. It doesn't depend on the thumb
at all, it simply copies a rectangle to another position in the window
and clears the old rectangle to the background color.

@proc move_thumb($, int oldx, int oldy, int wd, int ht, int newx, int newy)
{
    int h, ah;

    XCopyArea(XtDisplay($), XtWindow($), XtWindow($),
	      $gc,
	      oldx, oldy, wd, ht, newx, newy);
    /* First check if the old and new areas do not overlap */
    if (newx + wd <= oldx || oldx + wd <= newx
	|| newy + ht <= oldy || oldy + ht <= newy) {
	XClearArea(XtDisplay($), XtWindow($), oldx, oldy, wd, ht, False);
	return;
    } else {					/* They do overlap */
	h = oldy - newy;
	if (h > 0)
	    XClearArea(XtDisplay($), XtWindow($), oldx, newy + ht, wd,h,False);
	else if (h < 0)
	    XClearArea(XtDisplay($), XtWindow($), oldx, oldy, wd, -h, False);
	ah = (h < 0) ? -h : h;
	if (newx < oldx)
	    XClearArea(XtDisplay($), XtWindow($), newx + wd,
		       max(oldy, newy), oldx - newx, ht - ah, False);
	else if (oldx < newx)
	    XClearArea(XtDisplay($), XtWindow($), oldx, max(oldy, newy),
		       newx - oldx, ht - ah, False);
    }
}


@ The |compute_info| method computes the relative position and size of the
thumb, given its geometry in pixels. Before that, it makes sure the pixel
values are within the frame and it adapts the values if needed.

@proc compute_info($, Position *x, Position *y, Dimension *w, Dimension *h,
    float *thumb_x, float *thumb_y, float *thumb_wd, float *thumb_ht)
{
    int fw, fh;
    Position fx, fy;

    #compute_inside($, &fx, &fy, &fw, &fh);
    fw = max(0, fw);
    fh = max(0, fh);
    *w = min(fw, max($minsize, *w));
    *h = min(fh, max($minsize, *h));
    *x = min(fx + fw - *w, max(fx, *x));
    *y = min(fy + fh - *h, max(fy, *y));
    *thumb_wd = ((float) *w)/fw;
    *thumb_ht = ((float) *h)/fh;
    *thumb_x = (*w == fw) ? 0.0 : ((float) (*x - fx))/(fw - *w);
    *thumb_y = (*h == fh) ? 0.0 : ((float) (*y - fy))/(fh - *h);
}

@ The |set_values| method changes the GC's when needed.
A change in |minsize| doesn't necessarily cause a redraw; only if
the current thumb size is less than the new minimum does the widget
needs to be redrawn.

@proc set_values
{
    Boolean need_redisplay = False;
    Position x, y;
    Dimension w, h;

    if ($thumbPixmap != $old$thumbPixmap) {
	create_fggc($);
	create_thumbgc($);
	create_thumblightgc($);
	create_thumbdarkgc($);
	need_redisplay = True;
    } else if ($thumbColor != $old$thumbColor) {
	$thumbPixmap = 0;
	create_fggc($);
	create_thumbgc($);
	create_thumblightgc($);
	create_thumbdarkgc($);
	need_redisplay = True;
    }
    if ($thumbFrameWidth != $old$thumbFrameWidth)
	need_redisplay = True;
    if ($thumbFrameType != $old$thumbFrameType)
	need_redisplay = True;
    if ($minsize != $old$minsize) {
	compute_thumb(old, &x, &y, &w, &h);
	if (w < $minsize || h < $minsize) need_redisplay = True;
    }
    if ($scrollResponse != $old$scrollResponse) {
	$scrollResponse = $old$scrollResponse;
	XtWarning("scrollResponse resource may only be queried, not set");
    }

    return need_redisplay;
}

@ The method |scroll_response| is exported via the |scrollResponse|
resource. It has the format of a callback function, so that it can be
registered as a callback in the |scrollCallback| list of some other
widget. The |client_data| must be a pointer to the Slider2 widget
itself, the |call_data| is a pointer to an |XfwfScrollInfo| structure.
The widget |wdg| is the widget from whose callback list the function
is called.

If the size of the thumb changed, the area must be cleared and redrawn
with |expose|, but if only the position changed, the thumb can be
moved with the |move_thumb| method, which is much faster.

@def range(x) = (0.0 <= (x) && (x) <= 1.0)

@proc scroll_response(Widget wdg, XtPointer client_data, XtPointer call_data)
{
    Widget self = (Widget) client_data;
    XfwfScrollInfo *inf = (XfwfScrollInfo *)call_data;
    XfwfScrollInfo new_info;
    float x, y, w, h;
    Position newx, newy, oldx, oldy;
    Dimension newwd, newht, oldwd, oldht, wd, ht;
    XEvent event;
    XRectangle rect;
    Region clip;
    Display *dpy = XtDisplay($);

    x = (inf->flags&XFWF_HPOS) && range(inf->hpos) ? inf->hpos : $thumb_x;
    y = (inf->flags&XFWF_VPOS) && range(inf->vpos) ? inf->vpos : $thumb_y;
    w = (inf->flags&XFWF_HSIZE) && range(inf->hsize) ? inf->hsize : $thumb_wd;
    h = (inf->flags&XFWF_VSIZE) && range(inf->vsize) ? inf->vsize : $thumb_ht;

    if ($thumb_wd != w || $thumb_ht != h) {	/* Size changed */
	if (XtIsRealized($))
	    $compute_thumb($, &oldx, &oldy, &oldwd, &oldht);
	$thumb_wd = w;
	$thumb_ht = h;
	$thumb_x = x;
	$thumb_y = y;
	if (XtIsRealized($)) {
	    $compute_thumb($, &newx, &newy, &newwd, &newht);
	    if ((oldwd > newwd) || (oldht > newht))
	      XClearArea(dpy, XtWindow($), oldx, oldy, oldwd, oldht, False);
	    event.xexpose.x = rect.x = newx;
	    event.xexpose.y = rect.y = newy;
	    event.xexpose.width = rect.width = newwd;
	    event.xexpose.height = rect.height = newht;
	    clip = XCreateRegion();
	    XUnionRectWithRegion(&rect, clip, clip);
	    $_expose($, &event, clip);
	    XDestroyRegion(clip);
	}
    } else if ($thumb_x != x || $thumb_y != y) { /* Only position changed */
	if (XtIsRealized($))
	    $compute_thumb($, &oldx, &oldy, &wd, &ht);
	$thumb_x = x;
	$thumb_y = y;
	if (XtIsRealized($)) {
	    $compute_thumb($, &newx, &newy, &wd, &ht);
	    $move_thumb($, oldx, oldy, wd, ht, newx, newy);
	}
    }

    if (inf->reason != XfwfSNotify) {
	new_info = *inf;
	new_info.reason = XfwfSNotify;
	XtCallCallbackList($, $scrollCallback, &new_info);
    }
}

@translations

@ The |start| action should be bound to a mouse button press, because it
needs the coordinates of the mouse. The |drag| action is bound to mouse
movement and the |finish| action is normally bound to a release  of the
mouse button.

@trans <Btn1Down>: start()
@trans <Btn1Motion>: drag()
@trans <Btn1Up>: finish()
@trans <Btn2Down>: start()
@trans <Btn2Motion>: drag()
@trans <Btn2Up>: finish()


@actions

@ The |start| action checks the position of the mouse and if it was
outside the thumb, it calls the |scrollCallback|. Otherwise, it only
records the position. Note that the mouse may have been to the left as
well as below the thumb, causing the callbacks to be called twice.

@proc start
{
    Dimension w, h;
    Position x, y;
    XfwfScrollInfo info;
    Boolean outside = False;

    if (event->type != ButtonPress && event->type != ButtonRelease
	&& event->type != MotionNotify)
	XtError("The start action must be bound to a mouse event");
    $compute_thumb($, &x, &y, &w, &h);
    if (event->xbutton.button == Button2) {
      /* Pretend mouse was clicked on the middle of the thumb... */
      $drag_in_progress = True;
      $m_delta_x = - (w / 2);
      $m_delta_y = - (h / 2);
      /* and dragged to here: */
      drag(self,event,params,num_params);
    } else {
      if (event->xbutton.x < x) {			/* Left of thumb */
	info.reason = XfwfSPageLeft;
	info.flags = XFWF_HPOS;			/* Suggest a value: */
	info.hpos = max(0.0, $thumb_x - $thumb_wd);
	outside = True;
	XtCallCallbackList($, $scrollCallback, &info);
      }
      if (event->xbutton.x >= x + w) {		/* Right of thumb */
	info.reason = XfwfSPageRight;
	info.flags = XFWF_HPOS;			/* Suggest a value: */
	info.hpos = min(1.0, $thumb_x + $thumb_wd);
	outside = True;
	XtCallCallbackList($, $scrollCallback, &info);
      }
      if (event->xbutton.y < y) {			/* Above thumb */
	info.reason = XfwfSPageUp;
	info.flags = XFWF_VPOS;			/* Suggest a value: */
	info.vpos = max(0.0, $thumb_y - $thumb_ht);
	outside = True;
	XtCallCallbackList($, $scrollCallback, &info);
      }
      if (event->xbutton.y >= y + h) {		/* Below thumb */
	info.reason = XfwfSPageDown;
	info.flags = XFWF_VPOS;			/* Suggest a value: */
	info.vpos = min(1.0, $thumb_y + $thumb_ht);
	outside = True;
	XtCallCallbackList($, $scrollCallback, &info);
      }

      if (! outside) {				/* Inside the thumb */
	$drag_in_progress = True;
	$m_delta_x = x - event->xbutton.x;
	$m_delta_y = y - event->xbutton.y;
      }
    }
}

@ The |finish| action does nothing if this is the end of a
click outside the thumb. The callbacks for this event have already
been called.

If this is the end of a drag action, we reset the flag
|drag_in_progress| to False and call the drop callbacks.

@proc finish
{
    XfwfScrollInfo info;

    if ($drag_in_progress) {
	$drag_in_progress = False;
	info.reason = XfwfSMove;
	info.flags = XFWF_VPOS | XFWF_HPOS;
	info.hpos = $thumb_x;
	info.vpos = $thumb_y;
	XtCallCallbackList($, $scrollCallback, &info);
    }
}

@ An application that can draw fast enough, may wish to redraw with
every movement of the thumb, instead of only at the end of the drag
action. The drag callback is provided for this purpose. It is called
in the same way as the drop callback, with the current relative
position of the thumb.

@proc drag
{
    XfwfScrollInfo info;
    Dimension wd, ht;
    Position oldx, oldy, newx, newy;
    float dum1, dum2;

    if (! $drag_in_progress) return;
    if (event->type != ButtonPress && event->type != ButtonRelease
	&& event->type != MotionNotify)
	XtError("The drag action must be bound to a mouse event");
    $compute_thumb($, &oldx, &oldy, &wd, &ht);
    newx = event->xbutton.x + $m_delta_x;
    newy = event->xbutton.y + $m_delta_y;
    $compute_info($, &newx, &newy, &wd, &ht, &$thumb_x, &$thumb_y,&dum1,&dum2);
    $move_thumb($, oldx, oldy, wd, ht, newx, newy);
    info.reason = XfwfSDrag;
    info.flags = XFWF_VPOS | XFWF_HPOS;
    info.hpos = $thumb_x;
    info.vpos = $thumb_y;
    XtCallCallbackList($, $scrollCallback, &info);
}


@utilities

@ The |create_gc| routine creates the GCs for the text.

@proc create_gc($)
{
    XtGCMask mask;
    XGCValues values;

    if ($gc != NULL) XtReleaseGC($, $gc);
    values.background = $thumbColor;
    values.foreground = $foreground;
    values.font = $font->fid;
    mask = GCFont | GCBackground | GCForeground;
    $gc = XtGetGC($, mask, &values);
}

@ |create_thumbgc| creates the GC that draw the background in the
thumb.

@proc create_thumbgc($)
{
    XtGCMask mask;
    XGCValues values;

    if ($thumbgc != NULL) XtReleaseGC($, $thumbgc);
    if ($thumbPixmap != 0) {
	mask = GCTile | GCFillStyle;
	values.tile = $thumbPixmap;
	values.fill_style = FillTiled;
    } else {
	mask = GCForeground;
	values.foreground = $thumbColor;
    }
    $thumbgc = XtGetGC($, mask, &values);
}

@proc create_fggc($)
{
    XtGCMask mask;
    XGCValues values;

    if ($fggc != NULL) XtReleaseGC($, $fggc);
    mask = GCForeground;
    $set_color($, $background_pixel, &values.foreground);
    $fggc = XtGetGC($, mask, &values);
}

@ The |create_thumblightgc| functions makes the GC for drawing the light
parts of the thumb's frame.

@proc create_thumblightgc($)
{
    XtGCMask mask=0;
    XGCValues values;

    if ($thumblightgc != NULL) XtReleaseGC($, $thumblightgc);
    switch ($shadowScheme) {
    case XfwfColor:
	mask = GCForeground;
	values.foreground = $topShadowColor;
	break;
    case XfwfStipple:
	mask = GCFillStyle | GCStipple | GCForeground | GCBackground;
	values.fill_style = FillOpaqueStippled;
	values.background = $thumbColor;
	values.stipple = $topShadowStipple;
	values.foreground = WhitePixelOfScreen(XtScreen($));
	break;
    case XfwfBlack:
    case XfwfAuto:
	if (DefaultDepthOfScreen(XtScreen($)) > 4
	    && $lighter_color($, $thumbColor, &values.foreground)) {
	    mask = GCForeground;
	} else {
	    mask = GCFillStyle | GCBackground | GCForeground | GCStipple;
	    values.fill_style = FillOpaqueStippled;
	    /* values.background = $thumbColor; */
	    values.background = BlackPixelOfScreen(XtScreen($));
	    values.foreground = WhitePixelOfScreen(XtScreen($));
	    values.stipple =
		XCreateBitmapFromData(XtDisplay($),
				      RootWindowOfScreen(XtScreen($)),
				      gray_bits, gray_width, gray_height);
	}
	break;
    }
    $thumblightgc = XtGetGC($, mask, &values);
}

@ The |create_thumbdarkgc| routines does the same for the dark parts of the
thumb's frame.

@proc create_thumbdarkgc($)
{
    XtGCMask mask=0;
    XGCValues values;

    if ($thumbdarkgc != NULL) XtReleaseGC($, $thumbdarkgc);
    switch ($shadowScheme) {
    case XfwfColor:
	mask = GCForeground;
	values.foreground = $bottomShadowColor;
	break;
    case XfwfStipple:
	mask = GCFillStyle | GCStipple | GCForeground | GCBackground;
	values.fill_style = FillOpaqueStippled;
	values.stipple = $bottomShadowStipple;
	values.foreground = BlackPixelOfScreen(XtScreen($));
	values.background = $thumbColor;
	break;
    case XfwfBlack:
    case XfwfAuto:
	if (DefaultDepthOfScreen(XtScreen($)) > 4
	    && $darker_color($, $thumbColor, &values.foreground)) {
	    mask = GCForeground;
	} else {
	    mask = GCFillStyle | GCBackground | GCForeground | GCStipple;
	    values.fill_style = FillOpaqueStippled;
	    /* values.background = $thumbColor; */
	    values.background = BlackPixelOfScreen(XtScreen($));
	    values.foreground = WhitePixelOfScreen(XtScreen($));
	    values.stipple =
		XCreateBitmapFromData(XtDisplay($),
				      RootWindowOfScreen(XtScreen($)),
				      gray_bits, gray_width, gray_height);
	}
	break;
    }
    $thumbdarkgc = XtGetGC($, mask, &values);
}

@imports

@incl <X11/bitmaps/gray>
@incl <stdio.h>
