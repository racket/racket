#! /users1/bert/bin/wbuild
#
# Contains the source for the Frame widget
# Version 2.2.1 for FWF V4.0

@class XfwfFrame (XfwfCommon) @file=xwFrame

@ The Frame widget is a composite widget that accepts just one child.
Its only purpose is to draw a frame around widgets that do not have a
frame of their own. It always uses the size of its child, with a
little extra for the frame. There are several types of frames
available, selectable with a resource.

Widget writers can also use the Frame class as a superclass for new
widgets.  The frame is drawn by the |expose| method (which must
therefore be called by subclasses). Its width is given by
|XtNframeWidth|, the appearance by |XtNframeType|. The possible types
are:

\item{|XfwfRaised|} Gives a beveled look. The top and left borders will
be lighter, the bottom and right sides darker.

\item{|XfwfSunken|} Just the opposite.

\item{|XfwfChiseled|} The border will look as if it was made with a
chisel.

\item{|XfwfLedged|} The border will be a ledge that juts out of the
background.



@public

@ The cursor (when not |None|) is the mouse cursor that is displayed
when the mouse is over the Board widget. The default value |None|
causes the parent's cursor to be used.

        @var Cursor cursor = None

@ The |XtNframeType| determines how the border looks.

        @var FrameType frameType = XfwfRaised

@ |XtNframeWidth| gives the width of the border. The default value
of 0 shows no border at all. The border is drawn {\it inside\/} the
widget. (See also |XtNouterOffset|.)

        @var Dimension frameWidth = 0

@ Normally, the border is draw along the outer edge of the widget, but
it can be moved inward. |XtNouterOffset| is the number of pixels
between the edge and the frame.

        @var Dimension outerOffset = 0

@ Between the frame and whatever is inside the widget, there is also
margin. By default, however, it is 0.

        @var Dimension innerOffset = 0

@ The colors of the top and bottom shadows can be set with the
resources |topShadowColor| and |bottomShadowColor|, but it is also
possible to use a stiple of foreground and background colors. This may
be preferable on workstations with limited or no color capabilities.
However, the easiest way (which is also the default) is to let the
widget determine its own shadow colors or stipples, based on the
widget's background color and the color capabilities of the screen.

The resource |shadowScheme| can be set to |XfwfColor|, |XfwfStipple|
or |XfwfAuto|. The converter for the shadow pixmap accepts the strings
|"stipple0"| through |"stipple8"|, which create pixmaps of the current
background and foreground colors, with |"stipple0"| entirely
background and |"stipple8"| entirely foreground. Setting pixmaps or
colors is only useful when |shadowScheme| is set to |XfwfStipple| or
|XfwfColor| respectively.

The values of |topShadowColor| and |bottomShadowColor| are ignored by
the Frame widget as long as |shadowScheme| is not |XfwfColor|, but the
default values are computed nevertheless, since they are useful, e.g.,
when an icon uses `topShadowColor' and `bottomShadowColor' as dynamic
colors.

        @var ShadowScheme shadowScheme = XfwfAuto
        @var Pixel topShadowColor = <CallProc> compute_topcolor
        @var Pixel bottomShadowColor = <CallProc> compute_bottomcolor
        @var Bitmap topShadowStipple = NULL
        @var Bitmap bottomShadowStipple = NULL

@ The inherited resource |borderWidth| is given a default value of 0,
instead of 1.

        @var border_width = 0

@exports

@ A number of new types is introduced by the Common class.  The
possible types of borders are enumerated in |Frame3dType| (see the
introduction).

        @type FrameType = enum {
            XfwfRaised, XfwfSunken, XfwfChiseled, XfwfLedged, XfwfPlain, XfwfNothing }

@ The shadow scheme can be used to choose colors, pixmaps or automatic
shadows.

        @type ShadowScheme = enum {XfwfAuto, XfwfColor, XfwfStipple, XfwfBlack }

@ The type |Bitmap| is an alias for |Pixmap|, but it is meant to
contain only bitmaps, i.e., pixmaps of depth one.

        @type Bitmap = Pixmap

@ The routine that draws the border is generally useful, so it is
exported. |t| is the thickness of the frame. The frame is drawn inside
the rectangle |(x, y, x+w-1, y+h-1)|.

@proc XfwfDrawFrame($, int x, int y, int w, int h, FrameType tp, int t,
        GC lightgc, GC darkgc, GC fggc)
{
    XPoint tlPoints[7], brPoints[7];

    if (t == 0) return;
    switch (tp) {
    case XfwfPlain:
	XDrawRectangle(XtDisplay($), XtWindow($), darkgc, x+1, y+1, w-1, h-1);
	break;
    case XfwfRaised:
    case XfwfSunken:
        tlPoints[0].x = x;              tlPoints[0].y = y;
        tlPoints[1].x = x + w;          tlPoints[1].y = y;
        tlPoints[2].x = x + w - t;      tlPoints[2].y = y + t;
        tlPoints[3].x = x + t;          tlPoints[3].y = y + t;
        tlPoints[4].x = x + t;          tlPoints[4].y = y + h - t;
        tlPoints[5].x = x;              tlPoints[5].y = y + h;
        tlPoints[6].x = x;              tlPoints[6].y = y;
        brPoints[0].x = x + w;          brPoints[0].y = y + h;
        brPoints[1].x = x;              brPoints[1].y = y + h;
        brPoints[2].x = x + t;          brPoints[2].y = y + h - t;
        brPoints[3].x = x + w - t;      brPoints[3].y = y + h - t;
        brPoints[4].x = x + w - t;      brPoints[4].y = y + t;
        brPoints[5].x = x + w;          brPoints[5].y = y;
        brPoints[6].x = x + w;          brPoints[6].y = y + h;
        if (tp == XfwfSunken) {
            XFillPolygon(XtDisplay($), XtWindow($),
                         darkgc, tlPoints, 7, Nonconvex, CoordModeOrigin);
            XFillPolygon(XtDisplay($), XtWindow($),
                         lightgc, brPoints, 7, Nonconvex, CoordModeOrigin);
        } else {
            XFillPolygon(XtDisplay($), XtWindow($),
                         lightgc, tlPoints, 7, Nonconvex, CoordModeOrigin);
            XFillPolygon(XtDisplay($), XtWindow($),
                         darkgc, brPoints, 7, Nonconvex, CoordModeOrigin);
        }
	if (fggc)
	  XDrawRectangle(XtDisplay($), XtWindow($), fggc, x, y, w-1, h-1);
        break;
    case XfwfLedged:
        XfwfDrawFrame($, x, y, w, h, XfwfRaised, t/2, lightgc, darkgc, NULL);
        XfwfDrawFrame($, x+t/2, y+t/2, w-2*(int)(t/2), h-2*(int)(t/2),
                  XfwfSunken, t/2, lightgc, darkgc, NULL);
        break;
    case XfwfChiseled:
        XfwfDrawFrame($, x, y, w, h, XfwfSunken, t/2, lightgc, darkgc, NULL);
        XfwfDrawFrame($, x+t/2, y+t/2, w-2*(int)(t/2), h-2*(int)(t/2),
                  XfwfRaised, t/2, lightgc, darkgc, NULL);
        break;
    case XfwfNothing:
        break;
    }

}



@private

@ The GC for drawing the light parts of the frame:

        @var GC lightgc

@ The GC for drawing the dark parts of the frame:

        @var GC darkgc

@ The GC fro drawing the border for sunken variants:

        @var GC fggc

@ The |gray| bitmap is used on screens with insufficient colors to
simulate light and dark shadows. It will be created by the
|initialize| method, whether or not it is needed. Since it is but a
small bitmap, this can't hurt much.

        @var Pixmap gray
        @var Pixmap darkGray
        @var Pixmap lightGray

@ The |old_frame_type| variable is used by the |set_shadow| action
function to store the original frame type, when it is temporarily
changed.

        @var FrameType old_frame_type


@methods

@ |class_initialize| installs the type converters. The type converters
back to String are installed as a convenience, so resources can be
retrieved in readable form with |XtVaGetValues|.

@proc class_initialize
{
    static XtConvertArgRec screenArg[] = {
    {XtBaseOffset, (XtPointer)XtOffset(Widget, core.screen), sizeof(Screen*)}};

    XtSetTypeConverter(XtRString, XtRFrameType, cvtStringToFrameType,
                       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRFrameType, XtRString, cvtFrameTypeToString,
                       NULL, 0, XtCacheNone, NULL);

    XtAddConverter(XtRString, XtRBitmap, XmuCvtStringToBitmap,
                       screenArg, XtNumber(screenArg));

    XtSetTypeConverter(XtRString, XtRShadowScheme, cvtStringToShadowScheme,
                       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRShadowScheme, XtRString, cvtShadowSchemeToString,
                       NULL, 0, XtCacheNone, NULL);
}

@ Much of the initialization that one would expect in the |initialize|
method is actually delegated to the |realize| method, since a window
ID is needed for most of the initializations.

@proc initialize
{
    Dimension frame;

    $lightgc = NULL;
    $darkgc = NULL;
    $fggc = NULL;
    $old_frame_type = $frameType;
    /* Make sure the width and height are at least as large as the frame */
    frame = $total_frame_width($);
    if ($width < 2 * frame) $width = 2 * frame;
    if ($height < 2 * frame) $height = 2 * frame;
}

@ The |realize| method uses the inherited method, but adds the cursor
attribute.

This is also the place to create the |gray| bitmap, that is used for
stippled shadows. It could not be created in |initialize|, since
creating a bitmap requires a window ID.

The GC's must be created after the |gray| bitmaps, since they might
have to use it as a stipple.

@proc realize
{
    *mask |= CWCursor;
    attributes->cursor = $cursor;
    #realize($, mask, attributes);

    $gray = (Pixmap)NULL;
    $lightGray = (Pixmap)NULL;
    $darkGray = (Pixmap)NULL;

    create_lightgc($);
    create_darkgc($);
    create_fggc($);
}

@proc destroy
{
  if ($darkgc) XtReleaseGC($, $darkgc); $darkgc = NULL;
  if ($lightgc) XtReleaseGC($, $lightgc); $lightgc = NULL;
  if ($fggc) XtReleaseGC($, $fggc); $fggc = NULL;
}

@ The |set_values| method has to create new GC's if the resources
change. It also makes sure that |frameWidth| is even if the frame type
is chiseled or ledged.

If the frame width was and is zero, nothing needs to be drawn,
regardless of the changes in other resources. Therefore, at the end
|need_redisplay| is set to False.

When the cursor changes, the |set_values| method uses the
|XDefineCursor| routine to set the attribute on the widget's window,
provided the widget is realized.

@proc set_values
{
    Boolean need_redisplay = False;

    if ($cursor != $old$cursor && XtIsRealized($))
        XDefineCursor(XtDisplay($), XtWindow($), $cursor);

    if ($frameType == XfwfChiseled || $frameType == XfwfLedged)
        $frameWidth = 2 * ((int) ($frameWidth / 2));

    if ($shadowScheme     != $old$shadowScheme
    ||  $background_pixel != $old$background_pixel) {
        create_darkgc($);
        create_lightgc($);
        create_fggc($);
        need_redisplay = True;
    } else if ($shadowScheme == XfwfColor) {
        if ($topShadowColor != $old$topShadowColor) {
            create_lightgc($);
            need_redisplay = True;
        }
        if ($bottomShadowColor != $old$bottomShadowColor) {
            create_darkgc($);
            need_redisplay = True;
        }
    } else if ($shadowScheme == XfwfStipple) {
        if ($topShadowStipple != $old$topShadowStipple) {
            create_lightgc($);
            need_redisplay = True;
        }
        if ($bottomShadowStipple != $old$bottomShadowStipple) {
            create_darkgc($);
            need_redisplay = True;
        }
    }

    if ($outerOffset != $old$outerOffset)
        need_redisplay = True;

    if ($innerOffset != $old$innerOffset)
        need_redisplay = True;

    if ($frameType != $old$frameType) {
        $old_frame_type = $frameType;
        need_redisplay = True;
    }

    if ($frameWidth != $old$frameWidth)
        need_redisplay = True;
    else if ($frameWidth == 0)
        need_redisplay = False;

    return need_redisplay;
}

@ The |expose| method draws the frame, for which it uses the
|XfwfDrawFrame| routine. Before it calls the routine, it sets the clip
region. Afterwards, the clip region is reset, because we don't know
which other widgets share the same GC's. As explained in {\em X
Toolkit Intrinsics Programming Manual} (Nye \& O'Reilly, Motif
Edition, 1990, p~223), the test for |XtIsRealized| is there for the
unlikely case when an expose event arrives after the widget has been
destroyed or unrealized.

@proc _expose
{
    Position x, y;
    int w, h;

    if (! XtIsRealized($)) return;
    if (region != NULL) {
        XSetRegion(XtDisplay($), $lightgc, region);
        XSetRegion(XtDisplay($), $darkgc, region);
        XSetRegion(XtDisplay($), $fggc, region);
    }
    $compute_inside($, &x, &y, &w, &h);
    w += 2*$innerOffset + 2*$frameWidth;
    h += 2*$innerOffset + 2*$frameWidth;
    XfwfDrawFrame($, 
		  x - $frameWidth - $innerOffset,
		  y - $frameWidth - $innerOffset,
		  max(w, 0),
		  max(h, 0),
		  $frameType, $frameWidth, $lightgc, $darkgc, $fggc);
    if (region != NULL) {
        XSetClipMask(XtDisplay($), $lightgc, None);
        XSetClipMask(XtDisplay($), $darkgc, None);
        XSetClipMask(XtDisplay($), $fggc, None);
    }
    #_expose($, event, region);
}

@ The method |compute_inside| is re-defined. The method now returns
the area inside the frame. It calls the superclass's method and then
decreases the area by the width of the frame.

@proc compute_inside
{
    #compute_inside($, x, y, w, h);
    *x += $outerOffset + $frameWidth + $innerOffset;
    *y += $outerOffset + $frameWidth + $innerOffset;
    *w -= 2 * ($outerOffset + $frameWidth + $innerOffset);
    *h -= 2 * ($outerOffset + $frameWidth + $innerOffset);
}

@ The method |total_frame_width| is overridden, because the Frame
widget draws an additional frame. The frame that the Common widget
draws is added to the shadow frame of the Frame widget.

@proc Dimension total_frame_width
{
    return #total_frame_width($) + $outerOffset + $frameWidth + $innerOffset ;
}

@ A Frame widget passes its parent's inquiry on to its (presumably)
single child. If there is no child, the proposal is accepted.
The border and position proposals are always accepted, the stacking
order and size are left to the child to decide.

@proc query_geometry
{
    XtWidgetGeometry request2, reply2;
    XtGeometryResult result;
    Dimension h;

    if ($num_children == 0) return XtGeometryYes;

    /* We're only interested in size and stacking order */
    reply->request_mode =
        (CWWidth | CWHeight | CWStackMode) & request->request_mode;

    /* If nothing of interest is left, we can return immediately */
    if (reply->request_mode == 0)
        return XtGeometryYes;

    /* Prepare a request to the child */
    h = 2 * ($outerOffset + $frameWidth + $innerOffset);
    request2.request_mode = reply->request_mode;
    request2.width = request->width - h;
    request2.height = request->height - h;
    request2.sibling = request->sibling;
    request2.stack_mode = request->stack_mode;

    result = XtQueryGeometry($children[0], &request2, &reply2);

    /* If the child accepted its proposal, we accept ours */
    if (result == XtGeometryYes) return XtGeometryYes;

    /* If the child doesn't want any change, we don't want any, either */
    if (result == XtGeometryNo) return XtGeometryNo;

    /* Otherwise, ignore everything but size and stacking order */
    reply->request_mode &= reply2.request_mode;
    if (reply->request_mode == 0) return XtGeometryYes;

    reply->width = reply2.width + h;
    reply->height = reply2.height + h;
    reply->sibling = reply2.sibling;
    reply->stack_mode = reply2.stack_mode;
    return XtGeometryAlmost;
}

@ Requests by the child to be resized are passed on to the parent. If
the parent replies with |XtGeometryYes|, the change is accepted and
(if not |XtCWQueryOnly|) already done. In that case the Frame widget
accepts its child's request. If the parent replies with
|XtGeometryNo|, the change is denied and the denial is passed on. If
the parent replies with a different geometry, the geometry is passed
on, after compensating for the frame width.

Requests for anything other than width or height are always granted.

@proc geometry_manager
{
    XtWidgetGeometry request2, reply2;
    XtGeometryResult result;
    Position x, y;
    int w, h, extraw, extrah;

    $compute_inside($, &x, &y, &w, &h);
    if (! (request->request_mode & (CWWidth|CWHeight))) return XtGeometryYes;
    extraw = $width - w;
    extrah = $height - h;
    request2.request_mode = request->request_mode & (CWWidth|CWHeight);
    request2.width = request->width + extraw;
    request2.height = request->height + extrah;
    result = XtMakeGeometryRequest($, &request2, &reply2);
    if (result == XtGeometryNo) return XtGeometryNo;
    if (result == XtGeometryYes) return XtGeometryYes;
    reply->request_mode = reply2.request_mode & (CWWidth|CWHeight);
    reply->width = reply2.width - extraw;
    reply->height = reply2.height - extrah;
    return XtGeometryAlmost;
}

@ The |resize| method doesn't have to recompute any private variables,
but it passes on the resize message to its child, after decreasing the
area by the amount needed for the frame.

@proc resize
{
    Position x, y;
    int w, h;
    Widget child;

    if ($num_children == 0) return;
    $compute_inside($, &x, &y, &w, &h);
    child = $children[0];
    w -= 2 * $child$border_width;
    h -= 2 * $child$border_width;
    XtConfigureWidget(child, x, y, max(1, w), max(1, h), $child$border_width);
}

@ The |change_managed| method is called when a child becomes managed
or unmanaged. The task of the routine is enforcing the layout policy,
which in this case consists of trying to take on the size of the child
or otherwise resize the child to fit inside the frame.
If the parent of the Frame widget doesn't allow the Frame widget to be
resized, the child of the Frame widget will be resized instead.

@proc change_managed
{
    XtWidgetGeometry request2, reply2;
    XtGeometryResult result;
    Widget child;
    Position x, y;
    int w, h;

    if ($num_children == 0) return;
    $compute_inside($, &x, &y, &w, &h);
    child = $children[0];
    request2.request_mode = CWWidth | CWHeight;
    request2.width = $child$width + $width - w;
    request2.height = $child$height + $height - h;
    result = XtMakeGeometryRequest($, &request2, &reply2);
    $compute_inside($, &x, &y, &w, &h);
    w -= 2 * $child$border_width;
    h -= 2 * $child$border_width;
    XtConfigureWidget(child, x, y, max(1, w), max(1, h), $child$border_width);
}

@actions

@ Although the Frame widget has no translations, one action is
defined, that may be of use to subclasses. The action function
|set_shadow| can be used to change the shadow frame. It has zero or
one argument. Without an argument, it resets the shadow to its
original type; with an argument, it sets the shadow to the type given
in the argument.

Warning: the function uses the |XfwfDrawFrame| routine to draw the
frames directly, instead of calling the |expose| or even |set_values|
methods.  Any subclass that defines behaviour that depends on knowing
the frame type, will have to redefine the |set_shadow| action.

@proc set_shadow
{
    Position x, y;
    int w, h;
    FrameType f = XfwfSunken;

    if (*num_params == 0) f = $old_frame_type;  /* Reset to old style */
    else if (strcmp("raised", params[0]) == 0) f = XfwfRaised;
    else if (strcmp("sunken", params[0]) == 0) f = XfwfSunken;
    else if (strcmp("chiseled", params[0]) == 0) f = XfwfChiseled;
    else if (strcmp("ledged", params[0]) == 0) f = XfwfLedged;
    else XtWarning("Unknown frame type in set_shadow action");

    if ($frameType != f) {
        $frameType = f;
        #compute_inside($, &x, &y, &w, &h);
	w -= 2*$outerOffset;
	h -= 2*$outerOffset;
        XfwfDrawFrame($, x + $outerOffset, y + $outerOffset,
                      max(w, 0), max(h, 0),
                      $frameType, $frameWidth, $lightgc, $darkgc, $fggc);
    }
}

@utilities

@ The converters use the following macro.

@def done(type, value) =
  do {
      if (to->addr != NULL) {
          if (to->size < sizeof(type)) {
              to->size = sizeof(type);
              return False;
          }
          *(type*)(to->addr) = (value);
      } else {
          static type static_val;
          static_val = (value);
          to->addr = (XtPointer)&static_val;
      }
      to->size = sizeof(type);
      return True;
  } while (0)

@exports

@ |cvtStringToFrameType| converts the strings `raised', `sunken',
`chiseled' and `ledged'. Case doesn't matter.

@proc Boolean cvtStringToFrameType(Display *display, XrmValuePtr args,
  Cardinal *num_args, XrmValuePtr from, XrmValuePtr to,
  XtPointer *converter_data)
{
    String s = (String) from->addr;

    if (*num_args != 0)
        XtAppErrorMsg(XtDisplayToApplicationContext(display),
                      "cvtStringToFrameType", "wrongParameters",
                      "XtToolkitError",
                      "String to frame type conversion needs no arguments",
                      (String*) NULL, (Cardinal*) NULL);

    if (XmuCompareISOLatin1(s, "raised") == 0) done(FrameType, XfwfRaised);
    if (XmuCompareISOLatin1(s, "sunken") == 0) done(FrameType, XfwfSunken);
    if (XmuCompareISOLatin1(s, "chiseled") == 0) done(FrameType, XfwfChiseled);
    if (XmuCompareISOLatin1(s, "ledged") == 0) done(FrameType, XfwfLedged);
    XtDisplayStringConversionWarning(display, s, XtRFrameType);
    done(FrameType, XfwfRaised);
}

@proc Boolean cvtFrameTypeToString(Display *display, XrmValuePtr args,
  Cardinal *num_args, XrmValuePtr from, XrmValuePtr to,
  XtPointer *converter_data)
{
    if (*num_args != 0)
        XtAppErrorMsg(XtDisplayToApplicationContext(display),
                      "cvtFrameTypeToString", "wrongParameters",
                      "XtToolkitError",
                      "Fframe type to String conversion needs no arguments",
                      (String*) NULL, (Cardinal*) NULL);
    switch (*(FrameType*)from->addr) {
    case XfwfRaised: done(String, "raised");
    case XfwfSunken: done(String, "sunken");
    case XfwfChiseled: done(String, "chiseled");
    case XfwfLedged: done(String, "ledged");
    default: XtError("Illegal FrameType");
    }
    return FALSE;
}

@ The converter |cvtStringToShadowScheme| converts strings `color',
`auto' and `stipple' to |XfwfColor|, |XfwfAuto| and |XfwfStipple|.

@proc Boolean cvtStringToShadowScheme(Display *display, XrmValuePtr args,
  Cardinal *num_args, XrmValuePtr from, XrmValuePtr to,
  XtPointer *converter_data)
{
    String s = (String) from->addr;

    if (*num_args != 0)
        XtAppErrorMsg(XtDisplayToApplicationContext(display),
                      "cvtStringToShadowScheme", "wrongParameters",
                      "XtToolkitError",
                      "String to shadow scheme conversion needs no arguments",
                      (String*) NULL, (Cardinal*) NULL);

    if (XmuCompareISOLatin1(s, "auto")==0) done(ShadowScheme, XfwfAuto);
    if (XmuCompareISOLatin1(s, "color")==0) done(ShadowScheme, XfwfColor);
    if (XmuCompareISOLatin1(s, "stipple")==0) done(ShadowScheme, XfwfStipple);
    XtDisplayStringConversionWarning(display, s, XtRShadowScheme);
    done(ShadowScheme, XfwfAuto);
}

@proc Boolean cvtShadowSchemeToString(Display *display, XrmValuePtr args,
  Cardinal *num_args, XrmValuePtr from, XrmValuePtr to,
  XtPointer *converter_data)
{
    if (*num_args != 0)
        XtAppErrorMsg(XtDisplayToApplicationContext(display),
                      "cvtShadowSchemeToString", "wrongParameters",
                      "XtToolkitError",
                      "Shadow scheme to String conversion needs no arguments",
                      (String*) NULL, (Cardinal*) NULL);

    switch (*(ShadowScheme*)from->addr) {
    case XfwfAuto: done(String, "auto");
    case XfwfColor: done(String, "color");
    case XfwfStipple: done(String, "stipple");
    case XfwfPlain: done(String, "plain");
    default: XtError("Illegal ShadowScheme");
    }
    return FALSE;
}

@utilities

@ The |create_darkgc| function creates the GC for the dark parts of
the frame. The contents of the GC depend on the resources
|shadowScheme| and possibly |background_pixel|, |bottomShadowColor|,
|topShadowColor|, |bottomShadowStipple| and |topShadowStipple|.

@proc create_darkgc($)
{
    XtGCMask mask=0;
    XGCValues values;

    if ($darkgc != NULL) XtReleaseGC($, $darkgc);
    switch ($shadowScheme) {
    case XfwfColor:
        mask = GCForeground;
        values.foreground = $bottomShadowColor;
        break;
    case XfwfStipple:
        mask = GCFillStyle | GCStipple | GCForeground | GCBackground;
        values.fill_style = FillOpaqueStippled;
        values.stipple = $bottomShadowStipple ? $bottomShadowStipple : GetGray($);
        values.foreground = BlackPixelOfScreen(XtScreen($));
        values.background = $background_pixel;
        break;
    case XfwfBlack:
	mask = GCForeground;
	values.foreground = BlackPixelOfScreen(XtScreen($));
	break;
    case XfwfAuto:
        if (DefaultDepthOfScreen(XtScreen($)) > 4
            && $darker_color($, $background_pixel, &values.foreground)) {
            mask = GCForeground;
        } else {
            mask = GCFillStyle | GCBackground | GCForeground | GCStipple;
            values.fill_style = FillOpaqueStippled;
            /* values.background = $background_pixel; */
            values.background = WhitePixelOfScreen(XtScreen($));
            values.foreground = BlackPixelOfScreen(XtScreen($));
            values.stipple = GetDarkGray($);
        }
        break;
    }
    $darkgc = XtGetGC($, mask, &values);
}

@ |create_lightgc| does the same for the light parts of the frame.
When the |shadowScheme| resource is |XfwfAuto|, the depth of the screen
and the availability of colors determines whether colors or stipples
will be used for the frame.

@proc create_lightgc($)
{
    XtGCMask mask=0;
    XGCValues values;

    if ($lightgc != NULL) XtReleaseGC($, $lightgc);
    switch ($shadowScheme) {
    case XfwfColor:
        mask = GCForeground;
        values.foreground = $topShadowColor;
        break;
    case XfwfStipple:
        mask = GCFillStyle | GCStipple | GCForeground | GCBackground;
        values.fill_style = FillOpaqueStippled;
        values.background = $background_pixel;
        values.stipple = $topShadowStipple ? $topShadowStipple : GetGray($);
        values.foreground = WhitePixelOfScreen(XtScreen($));
        break;
    case XfwfBlack:
	mask = GCForeground;
	values.foreground = BlackPixelOfScreen(XtScreen($));
	break;
    case XfwfAuto:
	{
	  int ok;
	  if (DefaultDepthOfScreen(XtScreen($)) > 4) {
	    ok = $lighter_color($, $background_pixel, &values.foreground);
	  } else
	    ok = 0;
	    
	  if (ok) {
            mask = GCForeground;
	  } else {
            mask = GCFillStyle | GCBackground | GCForeground | GCStipple;
            values.fill_style = FillOpaqueStippled;
            /* values.background = $background_pixel; */
            values.background = WhitePixelOfScreen(XtScreen($));
            values.foreground = BlackPixelOfScreen(XtScreen($));
            values.stipple = GetLightGray($);
	  }
        }
        break;
    }
    $lightgc = XtGetGC($, mask, &values);
}

@proc create_fggc($)
{
    XtGCMask mask=0;
    XGCValues values;

    if ($fggc != NULL) XtReleaseGC($, $fggc);
    mask = GCForeground;
    $set_color($, $background_pixel, &values.foreground);
    $fggc = XtGetGC($, mask, &values);
}

@ The function |compute_topcolor| is a resource default proc. It is
used to compute the value of the |topShadowColor| relative to the
|background| color.

@proc compute_topcolor($, int offset, XrmValue* value)
{
    static Pixel color;
#if 1
    $lighter_color($, $background_pixel, &color);
#else
    (void) choose_color($, 1.35, $background_pixel, &color);
#endif
    value->addr = (XtPointer) &color;
}

@proc compute_bottomcolor($, int offset, XrmValue* value)
{
    static Pixel color;
#if 1
    $darker_color($, $background_pixel, &color);
#else
    (void) choose_color($, 0.6, $background_pixel, &color);
#endif
    value->addr = (XtPointer) &color;
}

@exports

@proc Pixmap GetGray($)
{
  if (!$gray) {
    $gray = XCreateBitmapFromData(XtDisplay($), XtWindow($),
        wx_gray_bits, wx_gray_width, wx_gray_height);
  }
  return $gray;
}

@proc Pixmap GetLightGray($)
{
  if (!$lightGray) {
    static char light_bits[] = { 0x02, 0x04, 0x01};

    $lightGray = XCreateBitmapFromData(XtDisplay($), XtWindow($),
        light_bits, 3, 3);
  }
  return $lightGray;
}

@proc Pixmap GetDarkGray($)
{
  if (!$darkGray) {
    static char dark_bits[] = { 0x05, 0x03, 0x06};

    $darkGray = XCreateBitmapFromData(XtDisplay($), XtWindow($),
        dark_bits, 3, 3);
  }
  return $darkGray;
}

@imports

@incl <string.h>
@incl <stdio.h>
@incl <X11/Xmu/Converters.h>
@incl <X11/Xmu/CharSet.h>

@ The stipple for the shadows is loaded from a bitmap file.

@incl "xwGray.h"

