

@CLASS XfwfScrollbar (XfwfBoard)  @file = xwScrollbar

@ The scrollbar widget helps the user to view data thatsed through a
ScrolledWindow (see there), where they are put next to and/or below
the widget that contains the data. The scrollbar controls which part
is visible. By manipulating the scrollbar the user can move (`scroll')
the data within the other widget.

A scrollbar consists of two arrows placed at each end of a rectangle,
either at the top and bottom or at the left and right. The former is
called a vertical scrollbar, the latter a horizontal one.  A smaller
rectangle, called the thumb or slider, is placed within the larger
one. It can move up and down (vertical scrollbar) or left/right
(horizontal scrollbar). Clicking an arrow moves the data in that
direction. Pressing the mouse button on an arrow and holding it, moves
the data by small increments as long as the mouse button is down.
Dragging the slider moves the data proportionally with the slider,
either along with the movement of the mouse, or all at once when the
mouse button is released. Pressing the mouse button onthe rectangle
outside the slider moves the data in larger increments.

The ratio of the slider size to the scroll region size typically
corresponds to the relationship between the size of the visible data
and the total size of the data.  For example, if 10 percent of the
data is visible, the slider typically occupies 10 percent of the
scroll region.  This provides the user with a visual clue to the size
of the invisible data.

The scrollbar widget can be configured as either horizontal or
vertical. It is made up of two XfwfArrow widgets and an XfwfSlider2
widget. Therefore, many of the scrollbar's resources are actually
resources for these, nameless, widgets. The scrollbar has the
same callback list as the slider.

@PUBLIC

@ The orientation of the scrollbar is set with the |vertical|
resource. |True| means vertical, |False| horizontal. This resource may
only be set during initialization. Any changes via |XtVaSetValues|
result in a warning and are then ignored.

	@var Boolean vertical = True

@ The user can interact with the scrollbar in many ways, but there is
only a single callback list: |scrollCallback|. The callback uses the
|call_data| parameter to pass a pointer to an |XfwfScrollInfo|
structure: |struct {XfwfSReason reason; XfwfSFlags flags; float vpos,
vsize, hpos, hsize;}| The |flags| field is a bitwise combination of
|XFWF_VPOS|, |XFWF_HPOS|, |XFWF_VSIZE| and |XFWF_HSIZE|.  The
structure contains a field |reason|, which can have the following
values (there exist other values, but they are not used by the
scrollbar):

\item{-} |XfwfSUp|: if the user clicked (or holds) the up arrow.
\item{-} |XfwfSLeft|: ditto left arrow.
\item{-} |XfwfSDown|: ditto down arrow.
\item{-} |XfwfSRight|: ditto right arrow.
\item{-} |XfwfSPageUp|: ditto area above thumb.
\item{-} |XfwfSPageDown|: ditto area below thumb.
\item{-} |XfwfSPageLeft|: ditto area left of thumb.
\item{-} |XfwfSPageRight|: ditto area right of thumb.
\item{-} |XfwfSDrag|: if the user drags the thumb.
\item{-} |XfwfSMove|: if the user stops dragging the thumb.

The other four fields contain the new position of the thumb, as
numbers between 0.0 and 1.0 inclusive.  In the case of drag and drop
actions, the position and size reflect the thumb's {\em actual}
position at the end of the dragging.  In the other cases, the position
is the {\em suggested} new position. In these latter cases, the
callback routine should compute the new position and use a call to the
function in the |ScrollResponse| resource (or, more conveniently, to
|XfwfSetScrollbar|) to update the scrollbar.

	@var <Callback> XtCallbackList scrollCallback = NULL

@ The standard way to update the scrollbar is with a call to the
function that is knwon as the |scrollResponse| function. To get a
pointer to that function, you should use |XtGetValues| or
|XtVaGetValues| on the |XtNscrollResponse| resource. But from an
application it might be easier to use the |XfwfSetScrollbar|
convenience function instead.

The |scrollResponse| routine has the same format as a callback
procedure. The first argument is a widget, this argument is ignored.
The second argument is |XtPointer client_data|, it must point to the
scrollbar that is to be updated. The third argument is |XtPointer
call_data|, it must be a pointer to an |XfwfScrollInfo| structure (see
above).

	@var XtCallbackProc scrollResponse = scroll_response

@ When the user presses and then holds the mouse button on an arrow or
on an area outside the thumb, the scrollbar waits some milliseconds
before it starts repeating the callbacks.

	@var Cardinal initialDelay = 500

@ Between repeated calls to the callback routines, the scrollbar will
wait a few milliseconds.

	@var Cardinal repeatDelay = 50

@ Some widgets may be simple enough that they can scroll a fixed
amount whenever the user clicks on one of the arrow buttons. If that
is the case, they can let the scrollbar compute the new position. It
will be passed in the |XfwfScrollInfo| structure as the suggested new
position. Any receiver is free to ignore the suggestion, however. The
default is to add or subtract 0.05.

	@var float increment = <String> "0.05"

@ The color of the arrow and the thumb also determines the color of
the 3D shadow, at least if |shadowScheme| is set to |XfwfAuto|, as it
is by default. The default value is determined dynamically, it is
copied from the |background| resource.

	@var Pixel scrollbarForeground = <CallProc> copy_background

@ The width of the arrow's and thumb's shadow is by default 2 pixels.

	@var Dimension shadow = 2

@ The minimum size of the thumb is by default 20 pixels. It can be set
with the |minsize| resource.

	@var Dimension minsize = 20

@ The slider and the two arrows frame will be forced to 0 pixels. The
only frame is that of the whole scrollbar.  The default frame width is
changed from 0 to 2.

	@var frameWidth = 2

@ The default frame type is now |XfwfSunken|.

	@var frameType = XfwfSunken

        @var Boolean drawgrayScrollbar = FALSE

	@var Boolean egdeBar = FALSE

@PRIVATE

@ The three children of the scrollbar are stored in private variables.

	@var Widget arrow1
	@var Widget arrow2
	@var Widget slider

@ During the |initialize| method, the variable |initializing| will be
|True|, so that |insert_child| can check whether a child should be
inserted or not.

	@var Boolean initializing

@ The |scrollResponse| function of the Slider2 that implements the
thumb is stored in a private variable.

	@var XtCallbackProc slider_scroll

@EXPORTS

@ The |XfwfSetScrollbar| convenience function can be used to set the
position and size of a scrollbar. The two arguments must be between
0.0 and 1.0 (inclusive).

@proc XfwfSetScrollbar($, double pos, double size)
{
    if (! XtIsSubclass($, xfwfScrollbarWidgetClass))
	XtError("XfwfSetScrollbar called with incorrect widget type");
    if (pos < 0.0 || pos > 1.0 || size < 0.0 || size > 1.0)
	XtError("XfwfSetScrollbar called with incorrect arguments");
    if ($vertical) {
	XfwfResizeThumb($slider, 1.0, size);
	XfwfMoveThumb($slider, 0.0, pos);
    } else {
	XfwfResizeThumb($slider, size, 1.0);
	XfwfMoveThumb($slider, pos, 0.0);
    }
}

@proc XfwfGetScrollbar($, double *hpos, double *vpos)
{
    XfwfScrollInfo info;

    XfwfGetThumb($slider, &info);
    if (hpos) *hpos = info.hpos;
    if (vpos) *vpos = info.vpos;
}

@METHODS

@ The |initialize| method creates the three widgets that make up the
scrollbar: two arrows and a slider. It sets the resources of these
widgets and redirects the callbacks.

@proc initialize
{
    Position x, y, xa2, xslider, ya2, yslider;
    int w, h, wa, ha, wslider, hslider;
    Pixel bg, thumb_bg;

    $initializing = True;
    $compute_inside($, &x, &y, &w, &h);
    w = max(1, w);
    h = max(1, h);
    if ($vertical) {
	ha = wa = wslider = w;
	xa2 = xslider = x;
	hslider = ((int)h - 2*ha > 0) ? h - 2*ha : 10;
	yslider = y + ha;
	ya2 = yslider + hslider;
    } else {
 	wa = ha = hslider = h;
	ya2 = yslider = y;
	wslider = ((int)w - 2*wa > 0) ? w - 2*wa : 10;
	xslider = x + wa;
	xa2 = xslider + wslider;
    }
    XtVaGetValues($, XtNbackground, &bg, NULL),
    $set_color($, bg, &thumb_bg);
    $arrow1 = XtVaCreateManagedWidget
	("_arrow1", xfwfArrowWidgetClass, $,
	 XtNx, x,
	 XtNy, y,
	 XtNwidth, max(1, wa),
	 XtNheight, max(1, ha),
	 XtNframeWidth, 0,
	 XtNforeground, BlackPixelOfScreen(XtScreen($)),
	 XtNinitialDelay, $initialDelay,
	 XtNrepeatDelay, $repeatDelay,
	 XtNtraversalOn, False,
	 XtNhighlightThickness, 0,
	 XtNdirection, $vertical?XfwfTop:XfwfLeft,
	 XtNouterOffset, 0,
	 XtNbackground, bg,
	 XtNarrowShadow, 2,
         XtNhighlightColor, $highlightColor,
	 NULL);
    XtAddCallback($arrow1, XtNcallback, up, $);
    $arrow2 = XtVaCreateManagedWidget
	("_arrow2", xfwfArrowWidgetClass, $,
	 XtNx, xa2,
	 XtNy, ya2,
	 XtNwidth, max(1, wa),
	 XtNheight, max(1, ha),
	 XtNframeWidth, 0,
	 XtNforeground, BlackPixelOfScreen(XtScreen($)),
	 XtNinitialDelay, $initialDelay,
	 XtNrepeatDelay, $repeatDelay,
	 XtNtraversalOn, False,
	 XtNhighlightThickness, 0,
	 XtNdirection, $vertical?XfwfBottom:XfwfRight,
	 XtNouterOffset, 0,
	 XtNbackground, bg,
	 XtNarrowShadow, 2,
         XtNhighlightColor, $highlightColor,
	 NULL);
    XtAddCallback($arrow2, XtNcallback, down, $);
    $slider = XtVaCreateManagedWidget
	("_slider", xfwfSlider2WidgetClass, $,
	 XtNx, xslider,
	 XtNy, yslider,
	 XtNwidth, max(1, wslider),
	 XtNheight, max(1, hslider),
	 XtNthumbColor, $scrollbarForeground,
	 XtNframeWidth, 0,
	 XtNinitialDelay, $initialDelay,
	 XtNrepeatDelay, $repeatDelay,
	 XtNtraversalOn, False,
	 XtNhighlightThickness, 0,
	 XtNouterOffset, 0,
	 XtNborderWidth, 0,
	 XtNbackground, thumb_bg,
         XtNthumbFrameWidth, 1,
	 NULL);
    XtAddCallback($slider, XtNscrollCallback, thumbscroll, $);
    XtVaGetValues($slider, XtNscrollResponse, &$slider_scroll, NULL);
    $initializing = False;
}

@ When the scrollbar is resized, the children must be resized also.

@proc resize
{
    Position x, y, xa2, xslider, ya2, yslider;
    int w, h, wa, ha, wslider, hslider;

    $compute_inside($, &x, &y, &w, &h);
    w = max(1, w);
    h = max(1, h);
    if ($vertical) {
	wa = wslider = w;
	xa2 = xslider = x;
	ha = wa;
	hslider = ((int)h - 2*ha > 0) ? h - 2*ha : 10;
	yslider = y + ha;
	ya2 = yslider + hslider;
    } else {
	ha = hslider = h;
	ya2 = yslider = y;
	wa = ha;
	wslider = ((int)w - 2*wa > 0) ? w - 2*wa : 10;
	xslider = x + wa;
	xa2 = xslider + wslider;
    }
    XtConfigureWidget($arrow1, x, y, max(1, wa), max(1, ha), 0);
    XtConfigureWidget($arrow2, xa2, ya2, max(1, wa), max(1, ha), 0);
    XtConfigureWidget($slider, xslider, yslider, max(1, wslider), max(1, hslider), 0);
}

@ |insert_child| is redefined here only to be able to give a warning
when a child is inserted beyond the three that are created in the
|initialize| method.

@proc insert_child
{
    if ($initializing)
	#insert_child(child);
    else {
	char s[500];
	(void)sprintf(s, "Cannot add children to a scrollbar (\"%s\"->\"%s\")",
		      XtName(child), XtName($));
	XtWarning(s);
    }
}

@ The |set_values| method passes resource values on to the three
children.

@proc set_values
{
    if ($old$vertical != $vertical) {
	XtWarning("Cannot change the \"vertical\" resource of a scrollbar\n");
	$vertical = $old$vertical;
    }
    if ($old$scrollbarForeground != $scrollbarForeground) {
	XtVaSetValues($slider, XtNthumbColor, $scrollbarForeground, NULL);
	XtVaSetValues($arrow1, XtNforeground, $scrollbarForeground, NULL);
	XtVaSetValues($arrow2, XtNforeground, $scrollbarForeground, NULL);
    }
    if ($old$shadow != $shadow) {
	XtVaSetValues($slider, XtNthumbFrameWidth, $shadow, NULL);
	XtVaSetValues($arrow1, XtNarrowShadow, $shadow, NULL);
	XtVaSetValues($arrow2, XtNarrowShadow, $shadow, NULL);
    }
    if ($old$minsize != $minsize) {
	XtVaSetValues($slider, XtNminsize, $minsize, NULL);
    }
    if ($old$drawgrayScrollbar != $drawgrayScrollbar) {
      XtVaSetValues($arrow1, XtNdrawgrayArrow, $drawgrayScrollbar, NULL);
      XtVaSetValues($arrow2, XtNdrawgrayArrow, $drawgrayScrollbar, NULL);
    }
    return False;
}

@ The |scroll_response| method is accessed via the |scrollResponse|
resource. It is passed the scrollbar itself as the |client_data|
argument and a pointer to an |XfwfScrollInfo| record as |call_data|.

The scrollbar passes on the call to the Slider2 widget that is
managing the thumb.

@proc scroll_response(Widget wdg, XtPointer client_data, XtPointer call_data)
{
    Widget $ = (Widget) client_data;

    $slider_scroll(wdg, $slider, call_data);
}

@UTILITIES

@ The |up| routine is a callback for the first arrow. It invokes the
scrollbar's callback.

@proc up(Widget arrow, XtPointer client_data, XtPointer call_data)
{
    Widget $ = (Widget) client_data;
    XfwfScrollInfo info;

    XfwfGetThumb($slider, &info);
    if ($vertical) {
	info.reason = XfwfSUp;
	info.flags = XFWF_VPOS;
	info.vpos = max(0.0, info.vpos - $increment);
    } else {
	info.reason = XfwfSLeft;
	info.flags = XFWF_HPOS;
	info.hpos = max(0.0, info.hpos - $increment);
    }
    XtCallCallbackList($, $scrollCallback, &info);
}

@ The |down| routine is the callback for the second arrow. It invokes the
scrollbar's callback.

@proc down(Widget arrow, XtPointer client_data, XtPointer call_data)
{
    Widget $ = (Widget) client_data;
    XfwfScrollInfo info;

    XfwfGetThumb($slider, &info);
    if ($vertical) {
	info.reason = XfwfSDown;
	info.flags = XFWF_VPOS;
	info.vpos = min(1.0, info.vpos + $increment);
    } else {
	info.reason = XfwfSRight;
	info.flags = XFWF_HPOS;
	info.hpos = min(1.0, info.hpos + $increment);
    }
    XtCallCallbackList($, $scrollCallback, &info);
}

@ The |thumbscroll| routine is the callback for the scroll callback of
the slider. It invokes the scrollbar's |scrollCallback|, after making
sure that only appropriate position information is passed. (The
Slider2 might pass horizontal position where only vertical position is
relevant, and vice versa.)

@proc thumbscroll(Widget w, XtPointer client_data, XtPointer call_data)
{
    Widget $ = (Widget) client_data;
    XfwfScrollInfo *info = (XfwfScrollInfo*) call_data;

    if ($vertical)
	info->flags &= XFWF_VPOS;
    else
	info->flags &= XFWF_HPOS;
    XtCallCallbackList($, $scrollCallback, info);
}

@ The |copy_background| routine is resource default procedure. It is
called to initialize the default value of the |foreground| resource.

@proc copy_background($, int offset, XrmValue* value)
{
    value->addr = (XtPointer) &$background_pixel;
}


@ACTIONS

@ The following action is not used by default, but it is defined here,
because someone might want to bind it to keys. For example, the
subclasses |XfwfVScrollbar| and |XfwfHScrollbar| do that.

@proc Scroll
{
    XfwfScrollInfo info;

    XfwfGetThumb($slider, &info);
    info.reason = XfwfCvtStringToScrollReason(params[0]);
    switch (info.reason) {
    case XfwfSUp: 
	info.flags = XFWF_VPOS;
	info.vpos = max(0.0, info.vpos - $increment);
	break;
    case XfwfSDown:
	info.flags = XFWF_VPOS;
	info.vpos = min(1.0, info.vpos + $increment);
	break;
    case XfwfSLeft:
	info.flags = XFWF_HPOS;
	info.hpos = max(0.0, info.hpos - $increment);
	break;
    case XfwfSRight:
	info.flags = XFWF_HPOS;
	info.hpos = min(1.0, info.hpos + $increment);
	break;
    case XfwfSPageUp:
	info.flags = XFWF_VPOS;
	info.vpos = max(0.0, info.vpos - info.vsize);
	break;
    case XfwfSPageDown:
	info.flags = XFWF_VPOS;
	info.vpos = min(1.0, info.vpos + info.vsize);
	break;
    case XfwfSPageLeft:
	info.flags = XFWF_HPOS;
	info.hpos = max(0.0, info.hpos - info.hsize);
	break;
    case XfwfSPageRight:
	info.flags = XFWF_HPOS;
	info.hpos = min(1.0, info.hpos + info.hsize);
	break;
    case XfwfSTop:
	info.flags = XFWF_VPOS;
	info.vpos = 0.0;
	break;
    case XfwfSBottom:
	info.flags = XFWF_VPOS;
	info.vpos = 1.0;
	break;
    case XfwfSLeftSide:
	info.flags = XFWF_HPOS;
	info.hpos = 0.0;
	break;
    case XfwfSRightSide:
	info.flags = XFWF_HPOS;
	info.hpos = 1.0;
	break;
    default: break;				/* Not understood */
    }

    XtCallCallbackList($, $scrollCallback, &info);
}


@IMPORTS

@incl <xwArrow.h>
@incl <xwSlider2.h>
@incl <stdio.h>
