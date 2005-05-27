# Toggle widget
# Bert Bos <bert@let.rug.nl>
# Version 2.1 for FWF V4.0

@class XfwfToggle(XfwfButton)  @file=xwToggle

@ The |XfwfToggle| is a button that switches states with every activation
(which is by default with every mouse click). The states are named `on' and
`off'. The states can be indicated with a $\surd$ before the label Two
callbacks report the changed state to the application: |onCallback| is called
when the button switches to `on', |offCallback| is called when the button
switches back to `off'.


@EXPORTS

@ The symbolic constants are used to distinguish different
indicator types.

	@def XfwfSquareIndicator = 0

	@def XfwfDiamondIndicator = 1


@public

@ The |onCallback| is called by the |toggle| action or by the |switch_on|
action, if the previous state was `off'. The |call_data| argument will contain
the |XEvent| pointer that trigerred the action function.

	@var <Callback> XtCallbackList onCallback = NULL

@ The |offCallback| is called from the |toggle| or |switch_off| action, if the
previous state was `on'. The |call_data| argument will be a pointer to the
|XEvent| that caused the action.

	@var <Callback> XtCallbackList offCallback = NULL

@ The variable |on| records the state of the widget: |True| means `on' and
|False| means `off'.

	@var Boolean on = False

@ The state of the |XfwfToggle| is shown by an indicator of
|indicatorSize| and |indicatorType|. The suggested size for the
indicator is about $10\times10$. The default indicator is an
empty or a filled square.

	@var Dimension indicatorSize = 0

	@var <IndicatorType> int indicatorType = XfwfSquareIndicator

        @var Pixel indicatorColor = <CallProc> compute_indicatorcolor

@ The default |frameWidth| is set to 0 pixels.

	@var Dimension frameWidth = 0

@translations

@ The |toggle| action toggles the widget between `on' and `off'. By
default it is bound to a click of the left mouse button as well as to
the Return key.

	@trans <Btn1Up>: toggle()
	@trans <Key>Return: toggle()

@actions

@ The |toggle| action switches the state. Depending on the resources
it might change the tickmark. The |onCallback| or |offCallback|
functions are called, with the event as |call_data| argument.

@proc toggle
{
    XtVaSetValues($, "on", !$on, NULL);
    XtCallCallbackList($, $on ? $onCallback : $offCallback, event);
}

@ The |switch_on| action switches the button to `on' if it is `off',
otherwise it does nothing. By default it isn't bound to any event. If
the widget is changed, the |onCallback| is called with the event as
|call_data|.

@proc switch_on
{
    if (! $on) {
        XtVaSetValues($, "on", True, NULL);
        XtCallCallbackList($, $onCallback, event);
    }
}

@ The |switch_off| action switches the widget to `off' if the state is
`on', otherwise it does nothing. When the widget changes states, the
|offCallback| is called, with a pointer to the |XEvent| structure as
|call_data| argument.

@proc switch_off
{
    if ($on) {
        XtVaSetValues($, "on", False, NULL);
        XtCallCallbackList($, $offCallback, event);
    }
}


@private

@ The |indicator_gc| GC holds the color for the `on' state.

	@var GC indicator_gc
	@var GC center_gc
	@var GC ex_gc

@ The previous value of |leftMargin| is stored in a private variable.
This value is added to the width of the widest pixmap to give the new
value of |leftMargin|.

	@var Dimension saveLeftMargin

@methods

@ The GC's are created for the first time and the left margin is
increased to make room for the indicators.

@proc initialize
{
    $saveLeftMargin = $leftMargin;

    if ($xfont) {
      if (!$indicatorSize || $indicatorSize > wx_ASCENT($font, ((wxExtFont)$xfont)))
	$indicatorSize = wx_ASCENT($font, ((wxExtFont)$xfont));
    } else {
      if (!$indicatorSize || $indicatorSize > $font->ascent)
	$indicatorSize = $font->ascent + 2;
    }

    $indicator_gc = NULL;
    $center_gc = NULL;
    $ex_gc = NULL;

    XtVaSetValues($, XtNleftMargin, 2 * $leftMargin + $indicatorSize, NULL);
}

@proc destroy
{
   if ($center_gc) XtReleaseGC($, $center_gc); $center_gc = NULL;
   if ($indicator_gc) XtReleaseGC($, $indicator_gc); $indicator_gc = NULL;
   if ($ex_gc) XtReleaseGC($, $ex_gc); $ex_gc = NULL;
}

@proc realize
{
    #realize($, mask, attributes);
    create_indicator_gc($);
    create_center_gc($);
    create_ex_gc($);
}

@ Question: Does the computation of |leftMargin| have the desired
effect? Since |set_values| is downward chained, the Label widget has
already processed it; changing |leftMargin| doesn't cause Label to
recompute the preferred size\dots

@proc set_values
{
    Boolean redraw = False;

    if (!XtIsRealized($))
	return False;

    if ($on != $old$on
    ||  $indicatorType != $old$indicatorType) {
        redraw = True;
    }
    if ($shadowScheme != $old$shadowScheme) {
        create_indicator_gc($);
        redraw = True;
    } else if ($shadowScheme == XfwfColor
           &&  $indicatorColor != $old$indicatorColor) {
	create_indicator_gc($);
        redraw = True;
    }
    if ($indicatorSize != $old$indicatorSize) {
	/* Compute new left margin */
	Dimension w = $leftMargin + 2 * $saveLeftMargin;
	/* change left margin */
	XtVaSetValues($, XtNleftMargin, w, NULL);
	redraw = True;
    }
    return redraw;
}

@ The |expose| method uses the |expose| method of the superclass to draw the
button and then possibly adds a tick mark.

@proc _expose
{
    Position x, y;
    int w, h;

    if (! XtIsRealized($)) return;
    #_expose($, event, region);
    $compute_inside($, &x, &y, &w, &h);
    x += $saveLeftMargin;
    y += (h - $indicatorSize) / 2;
    switch ($indicatorType) {
    default:
	XtWarning("XfwfToggle has wrong indicatorType, using square!");
    case XfwfSquareIndicator:
	Xaw3dDrawToggle(XtDisplay($), XtWindow($),
	   	        $lightgc, $darkgc, $indicator_gc, NULL, $ex_gc,
		        x, y, $indicatorSize, 2, $on);
	break;
    case XfwfDiamondIndicator:
	Xaw3dDrawRadio(XtDisplay($), XtWindow($),
	   	       $lightgc, $darkgc, $indicator_gc, $center_gc, $ex_gc,
		       x, y, $indicatorSize, 2, $on);
	break;
    }
}

@utilities

@ The |create_indicator_gc| function creates the color for the 'on' state.

@proc create_ex_gc($)
{
    XtGCMask mask = 0;
    XGCValues values;

    if ($ex_gc != NULL) XtReleaseGC($, $ex_gc);
    mask = GCForeground;
    values.foreground = $highlightColor;
    $ex_gc = XtGetGC($, mask, &values);
}

@proc create_indicator_gc($)
{
    XtGCMask mask = 0;
    XGCValues values;

    if ($indicator_gc != NULL) XtReleaseGC($, $indicator_gc);
    switch ($shadowScheme) {
    case XfwfColor:
        mask = GCForeground;
        values.foreground = $topShadowColor;
        break;
    case XfwfStipple:
        mask = GCFillStyle | GCStipple | GCForeground | GCBackground;
        values.fill_style = FillOpaqueStippled;
        values.background = BlackPixelOfScreen(XtScreen($));
        values.foreground = WhitePixelOfScreen(XtScreen($));
        values.stipple    = GetGray($);
        break;
    case XfwfBlack:
    case XfwfAuto:
        if (DefaultDepthOfScreen(XtScreen($)) > 4
            && $set_color($, $background_pixel, &values.foreground)) {
            mask = GCForeground;
        } else {
            mask = GCFillStyle | GCBackground | GCForeground | GCStipple;
            values.fill_style = FillOpaqueStippled;
            values.background = BlackPixelOfScreen(XtScreen($));
            values.foreground = WhitePixelOfScreen(XtScreen($));
            values.stipple    = GetGray($);
        }
        break;
    }
    $indicator_gc = XtGetGC($, mask, &values);
}

@proc create_center_gc($)
{
  XtGCMask mask = 0;
  XGCValues values;

  if ($center_gc != NULL) XtReleaseGC($, $center_gc);
  if ($indicatorType == XfwfDiamondIndicator) {
    mask = GCForeground;
    values.foreground = $background_pixel;
    $center_gc = XtGetGC($, mask, &values);
  } else
    $center_gc = NULL;
}

@proc compute_indicatorcolor($, int offset, XrmValue* value)
{
    static Pixel color;
#if 1
    $set_color($, $background_pixel, &color);
#else
    (void) choose_color($, 0.85, $background_pixel, &color);
#endif
    value->addr = (XtPointer) &color;
}

@imports

@incl <xwTools3d.h>
@incl <xwTabString.h>
