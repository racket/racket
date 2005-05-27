# XfwfCommon -- the common superclass for all widgets
# Bert Bos <bert@let.rug.nl>
# Version 1.2 for FWF V4.0

@class XfwfCommon (Composite)  @file=xwCommon

@ The Common class is not meant to be instantiated. It only serves as
the common superclass for a family of widgets, to ensure that these
widgets have some common methods and resources.  The Common class
defines common types, symbolic constants, and type converters and it
also provides the basis for keyboard traversal.  The code for keyboard
traversal is roughly based on that in the Xw widget set (created by
Hewlett Packard), but it uses the |accept_focus| method.

When the resource |traversalOn| is set to |True| (either at creation
time, or via a |XtSetValues| later), a set of translations is added to
the widget. If the widget's parent is also a subclass of Common, these
translations will then implement keyboard traversal, using the cursor
keys (up, down, prev, etc.) Of course, when the widget already uses
these keys for other purposes, the keyboard traversal will not work.


@EXPORTS

@ The type |Alignment| is actually an integer, but it is given a
different name to allow a type converter to be installed for it.

	@type Alignment = int

@ The symbolic constants can be added together to form an alignment.
Various widgets use this to position labels, other widgets, etcetera.

	@def XfwfCenter = 0
	@def XfwfLeft = 1
	@def XfwfRight = 2
	@def XfwfTop = 4
	@def XfwfBottom = 8


@ For convenience, the eight possible combinations also have symbolic
names.

	@def XfwfTopLeft = (XfwfTop + XfwfLeft)
	@def XfwfTopRight = (XfwfTop + XfwfRight)
	@def XfwfBottomLeft = (XfwfBottom + XfwfLeft)
	@def XfwfBottomRight = (XfwfBottom + XfwfRight)


@ The directions of traversal are used as arguments to the |traverse|
method. They are probably only useful to subclasses.

	@type TraversalDirection = enum {
            TraverseLeft, TraverseRight, TraverseUp, TraverseDown,
            TraverseNext, TraversePrev, TraverseHome, TraverseNextTop }


@ To know the inside area of a Common widget might be useful to other
widgets than subclasses alone. Calling |XfwfCallComputeInside| will
call the |compute_inside| method, if available.

@proc XfwfCallComputeInside($,Position*x, Position*y, int*w, int*h)
{
    if (XtIsSubclass($, xfwfCommonWidgetClass) && $compute_inside) {
	int ww, hh;
        $compute_inside($, x, y, &ww, &hh);
	*w = max(0, ww);
	*h = max(0, hh);
    } else 
        XtWarning
            ("XfwfCallComputeInside only applies to subclasses of Common");
}

@ Another convenience function is |XfwfCallFrameWidth|, which uses the
method |total_frame_width| to compute the thickness of the frame that
the widget will draw.

@proc Dimension XfwfCallFrameWidth($)
{
    if (XtIsSubclass($, xfwfCommonWidgetClass) && $total_frame_width)
        return $total_frame_width($);
    else 
        XtWarning
            ("XfwfCallFrameWidth only applies to subclasses of Common");
    return 0;
}

@ To choose a color that is somewhat darker or lighter than another color,
the function |XfwfChooseColor| queries the RGB values of a pixel and
multiplies them with a factor. If all goes well, the function returns
|True|. If the chosen color ends up being the same as the original,
the color gray75 is returned instead.

@proc Boolean XfwfChooseColor($, double factor, Pixel base, Pixel *result)
{
    Colormap colormap;
    XColor color, dummy;
    static XColor gray75;

    if (XtIsRealized($))
	colormap = $colormap;
    else
	colormap = wx_default_colormap;
    color.pixel = base;

    XQueryColor(XtDisplay($), colormap, &color);
    color.red = min(65535, factor * color.red);
    color.green = min(65535, factor * color.green);
    color.blue = min(65535, factor * color.blue);
    if (!wxAllocColor(XtDisplay($), colormap, &color))
	return False;
    if (base == color.pixel) {
      if (!gray75.pixel)
    	if (!XAllocNamedColor(XtDisplay($), colormap, "gray75", &gray75, &dummy)) {
	  return False;
        }
      color.red = gray75.red;
      color.green = gray75.green;
      color.blue = gray75.blue;
      color.pixel = gray75.pixel;
     }
    *result = color.pixel;
    return True;
}


@PUBLIC

@ The resource |traversalOn| determines whether keyboard traversal is
used. If it is |True| initially, or if it is set to |True| later, a
set of translations will be added to the widget.

	@var Boolean traversalOn = True


@ The resource |traversalTranslationOn| determines whether full
keyboard traversal translations need to be installed.

	@var Boolean traversalTranslationDone = False


@ Keyboard focus is indicated by border highlighting. When keyboard
traversal is on and the widget receives the focus, the highlight border
is filled with the highlight color or tile. If the widget does not
have the focus, the area is left in the default background.

	@var Dimension highlightThickness = 2


@ The highlight border can have a color or it can be tiled with a
pixmap. Whichever of the resources |highlightColor| or
|highlightPixmap| is set latest, is used. When both are set, the
pixmap is used.

	@var Pixel highlightColor = <String> XtDefaultForeground


@ The |highlightPixmap| can be set to a pixmap with which the
highlight border will be tiled. Only one of |highlightPixmap| and
|highlightColor| can be set, see above.

	@var Pixmap highlightPixmap = None


@ When an application has several top level windows, it should have a
way of setting the focus between windows. The Enter key in any widget
with keyboard traversal on normally invokes the |traverseNextTop|
action, that will call the callbacks of the topmost Common (or
subclass) widget in the hierarchy. The callback may set the focus to
another top level widget, with |XtCallAcceptFocus|.

	@var <Callback> XtCallbackList nextTop = NULL

@ The resource |userData| is provided for applications that want to
attach their own data to a widget. It is not used by the widget itself
in any way.

	@var <Pointer> XtPointer userData = NULL

@ The Common widget provides a possibility to by-pass the standard expose
method. When |useExposeCallback| is set to |TRUE|, the callbacks registered
on |exposeCallback| will be called. The |callData| argument will be the a 
struct |XfwfExposeInfo| with the expose region and expose event. The standard
method can be called with |XfwfCallExpose|.

	@var Boolean useExposeCallback = False

	@var <Callback> XtCallbackList exposeCallback = NULL

@ The |focusHiliteChange| callback is simpler than trying to catch the
right focus event, and it's guranteed to be consistent with the border
hilite.

	@var <Callback> XtCallbackList focusHiliteChange = NULL

@ The |onDestroy| callback is called before the widget is destroyed.

        @var <Callback> XtCallbackList onDestroy = NULL


@EXPORTS

@ |XfwfExposeInfo| is the call_data argument of the |exposeCallback|.

@type XfwfExposeInfo = struct {Region region; XEvent *event;}

@ |XfwfCallExpose| calls the standard expose method.

@proc XfwfCallExpose($, XEvent *event, Region region)
{
    if (XtIsSubclass($, xfwfCommonWidgetClass) && $_expose)
        $_expose($, event, region);
}

@PRIVATE

@ |abs|, |min| and |max| are used often enough in various subclasses
to define them here. They will end up in the private(!) header file.

	@def max(a, b) = ((a) > (b) ? (a) : (b))
	@def min(a, b) = ((a) < (b) ? (a) : (b))


@ A private variable is used to track the keyboard focus, but only
while traversal is on. If |traversal_focus| is |True|, it means that
the widget has keyboard focus and that that focus is a result of
keyboard traversal. It also means that the widget's border is
highlighted, although that is only visible if the |highlightThickness|
is positive.

	@var Boolean traversal_focus


@ The highlight border is filled with a color or a tile.

	@var GC bordergc



@CLASSVARS

@ |traversal_trans| holds the compiled version of the
|extraTranslations|.

	@var XtTranslations traversal_trans = NULL

@ |traversal_trans_small| holds the compiled version of the
|extraTranslationsSmall|.

	@var XtTranslations traversal_trans_small = NULL

@ Set a few class variables.

	@var compress_motion = True
	@var compress_exposure = XtExposeCompressMaximal
	@var compress_enterleave = True

        @var Dimension travMode = 1

@METHODS
@proc realize
{
  if (wx_common_use_visual) {
    Display *dpy;
    int scrn;
    dpy = XtDisplay($);
    scrn = XScreenNumberOfScreen(XtScreen($));
    attributes->colormap = XCreateColormap(dpy, 
					   RootWindow(dpy, scrn),
					   wx_common_use_visual, AllocNone);
    *mask = *mask | CWColormap;
    XtCreateWindow($, InputOutput, wx_common_use_visual, *mask, attributes);
  } else {
    #realize($, mask, attributes);
  }
}

@ The type converter |cvtStringToAlignment| is installed in the
|class_initialize| method, after the quarks for the recognized strings
are created.

The converter from String to Icon needs one extra argument, viz., the
widget for which the icon is loaded. An offset of 0 should give a
pointer to the widget itself.

@proc class_initialize
{
    extern void XawInitializeWidgetSet(void);

    XtSetTypeConverter(XtRString, "Alignment", cvtStringToAlignment,
                       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter("Alignment", XtRString, cvtAlignmentToString,
                       NULL, 0, XtCacheNone, NULL);
    /* XawInitializeWidgetSet(); */ /* unnecessary for fvwm with >>Style "*" Lenience<< */
}


@ The |extraTranslations| are compiled into Xt's internal form and
stored in a class variable |traversal_trans|, but only if that hasn't
been done before. (It should have been done in the |class_initialize|
method, but wbuild's `|$|' syntax doesn't work there (yet)).

If the widget has the |traversalOn| resource set, the translations are
merged with the widgets existing translations.

@proc initialize
{
    Dimension frame;

    if ($traversal_trans == NULL)
        $traversal_trans = XtParseTranslationTable(extraTranslations);
    if ($traversal_trans_small == NULL)
        $traversal_trans_small = XtParseTranslationTable(extraTranslationsSmall);
    if ($traversalOn) {
	XtAugmentTranslations($, $traversal_trans_small);
        $visible_interest = True;
    }
    $traversal_focus = False;
    $bordergc = NULL;
    frame = $total_frame_width($);
    if ($width < 2 * frame) $width = 2 * frame;
    if ($height < 2 * frame) $height = 2 * frame;
    if ($width == 0) $width = 2;
    if ($height == 0) $height = 2;
    /* create_bordergc($); - Now done on demand */
}

@ The |set_values| method checks if the keyboard traversal has been
turned on and adds the traversal translations. (It can only be turned
on, not turned off.)

If something changes that causes the widget to loose keyboard focus,
the parent is asked to put the focus somewhere else. Otherwise the
whole application might suddenly loose keyboard focus.

@proc set_values
{
    Boolean need_redraw = False;
    Widget parent = XtParent($);
    Time time = CurrentTime;

    if ($traversalOn != $old$traversalOn && $traversalOn) {	
	XtAugmentTranslations($, $traversal_trans_small);
        $visible_interest = True;
    }
    if (($sensitive != $old$sensitive
    ||  $ancestor_sensitive != $old$ancestor_sensitive
    ||  $traversalOn != $old$traversalOn)
    &&  $traversal_focus) {
        if (XtIsSubclass(parent, xfwfCommonWidgetClass)) {
	    if ($sensitive == FALSE) {
        	$unhighlight_border($);
        	$traversal_focus = False;
	        $hilite_callbacks($);
	    }
            $parent$traverse(parent, TraverseHome, $, &time);
	}
    }
    if ($highlightThickness != $old$highlightThickness)
        need_redraw = True;
    if ($highlightPixmap != $old$highlightPixmap) {
        if ($bordergc) create_bordergc($);
        need_redraw = True;
    } else if ($highlightColor != $old$highlightColor) {
        $highlightPixmap = None;
        if ($bordergc) create_bordergc($);
        need_redraw = True;
    }
    return need_redraw;
}


@ A new method |compute_inside| is defined, that returns the area
inside the highlight border. Subclasses should use this to compute
their drawable area, in preference to computing it from |$width| and
|$height|. Subclasses, such as the Frame widget, redefine the method
if they add more border material.

@proc compute_inside($, Position *x, Position *y, int *w, int *h)
{
    *x = $highlightThickness;
    *y = $highlightThickness;
    *w = $width - 2 * $highlightThickness;
    *h = $height - 2 * $highlightThickness;
}

@ Another new method, |total_frame_width|, returns the thickness of
the frame that will be drawn onside the widget. Subclasses will need
to override this method if they draw other frames.

@proc Dimension total_frame_width($)
{
    return $highlightThickness;
}

@ The |expose| method draws the highlight border, if there is one.

@proc expose
{
    if ($useExposeCallback) {
	XfwfExposeInfo einfo;
	einfo.region = region;
	einfo.event  = event;
	XtCallCallbacks($, XtNexposeCallback, (XtPointer)&einfo);
    } else {
	$_expose($, event, region);
    }
}

@proc _expose($, XEvent *event, Region region)
{
    if (! XtIsRealized($)) return;
    if ($traversal_focus) {
      if (!$bordergc) create_bordergc($);
      if (region != NULL) XSetRegion(XtDisplay($), $bordergc, region);
      $highlight_border($);
      if (region != NULL) XSetClipMask(XtDisplay($), $bordergc, None);
    }
}


@ When the widget is destroyed and the widget still has the keyboard
focus, the parent is asked to give it to another widget.

@proc destroy
{
#if 0
    Widget parent = XtParent($);
    Time time = CurrentTime;

    /* For MrEd: no focus delegation. If the widget is being destroyed,
       it can't still have the efefctive focus. (Either the widget is hidden
       or its parent is hidden.) */
    if ($traversal_focus) {
        $sensitive = False;
        if (XtIsSubclass(parent, xfwfCommonWidgetClass))
            $parent$traverse(parent, TraverseHome, $, &time);
    }
#endif

    XtCallCallbackList($, $onDestroy, NULL);
    if ($bordergc) XtReleaseGC($, $bordergc); $bordergc = NULL;
}


@ The border highlight is drawn and removed with two methods, although
few subclasses will want to redefine them. The methods are called by
the |focusIn| and |focusOut| actions and |highlight_border| is also
called by |expose|.

@proc highlight_border($)
{
    XRectangle rect[4];
    int t;

    /* There's another function like this in xwEnforcer, for some reason */

    if ($highlightThickness == 0) return;

    t = 1 /* $highlightThickness */; 

    rect[0].x = 1;
    rect[0].y = 0;
    rect[0].width = $width - 2;
    rect[0].height = t;

    rect[1].x = 0;
    rect[1].y = 1;
    rect[1].width = t;
    rect[1].height = $height - 2;

    rect[2].x = $width - t;
    rect[2].y = 1;
    rect[2].width = t;
    rect[2].height = $height - 2;

    rect[3].x = 1;
    rect[3].y = $height - t;
    rect[3].width = $width - 2;
    rect[3].height = t;

    if (!$bordergc) create_bordergc($);
    XFillRectangles(XtDisplay($), XtWindow($), $bordergc, &rect[0], 4);
}

@proc unhighlight_border($)
{
    if ($highlightThickness == 0) return;

    XClearArea(XtDisplay($), XtWindow($), 
               0, 0, $width, $highlightThickness, False);
    XClearArea(XtDisplay($), XtWindow($),
               0, 0, $highlightThickness, $height, False);
    XClearArea(XtDisplay($), XtWindow($),
               $width - $highlightThickness, 0, 
               $highlightThickness, $height, False);
    XClearArea(XtDisplay($), XtWindow($),
               0, $height - $highlightThickness,
               $width, $highlightThickness, False);
}


@ When the |accept_focus| method is called, the widget should try to set
the focus to itself or one of its children. If it succeeds, it returns
|True| else |False|. If there are children, each is asked in turn,
until one is found that accepts the focus. If none is found, the
widget checks it's own |sensitive| resource, to see if it can receive
keyboard events. If so, it sets the focus to itself and returns
|True|, otherwise |False|.

@proc accept_focus
{
    int i;

    if (! XtIsRealized($) || ! $sensitive || ! $traversalOn
        /* || ! $visible */ || ! $ancestor_sensitive || ! $managed
        || ! $mapped_when_managed || $being_destroyed) return False;
    for (i = 0; i < $num_children; i++)
        if (XtCallAcceptFocus($children[i], time)) return True;
    if (! $traversal_focus) {
      int ok;
#if 0
      XSetInputFocus(XtDisplay($), XtWindow($), RevertToParent, *time);
      ok = 1;
#else
      Widget parent;
      parent = $;
      while (parent 
	     && !XtIsTopLevelShell(parent) 
	     && !XtIsTransientShell(parent))
	parent = XtParent(parent);
      
      if (parent) {
	XtSetKeyboardFocus(parent, $);
	ok = 1;
      } else 
	ok = 0;

      if (!$traversalTranslationDone) {
        XtAugmentTranslations($, $traversal_trans);
	$traversalTranslationDone = True;
      }
#endif
      if (ok) {
        $highlight_border($);
	$traversal_focus = True;
	$hilite_callbacks($);
      }

      return ok ? True : False;
    }
    return True;
}


@proc hilite_callbacks($)
{
  XtPointer on = (XtPointer)(long)$traversal_focus;

  while ($ && XtIsSubclass($, xfwfCommonWidgetClass) && !$focusHiliteChange)
    $ = XtParent($);

  if ($ && XtIsSubclass($, xfwfCommonWidgetClass))
    XtCallCallbackList($, $focusHiliteChange, on);
}

@ A Common widget (and most subclasses) return |True| for
|would_accept_focus|, if the |sensitive|, |visible| and |traversalOn|
resources are set and none of the children wants the focus.

@proc Boolean would_accept_focus($)
{
    int i;
    Widget child;

    if (! XtIsRealized($) || ! $sensitive || ! $ancestor_sensitive || ! $visible || ! $traversalOn)
        return False;
    else {
        for (i = 0; i < $num_children; i++) {
            child = $children[i];
            if (XtIsSubclass(child, xfwfCommonWidgetClass)
                && $child$would_accept_focus(child))
                return False;
        }
        return True;
    }
}


@ The algorithm behind keyboard traversal

@ * Handling focus events

If a widget receives a (non-virtual) FocusIn event, this is usually
caused by the |accept_focus| method of that widget, except in the case
that a top level widget receives the focus from the window manager. In
the first case, the window can just draw the highlight border, in the
second case, the widget should try to set the focus to one of its
children.

To be able to distinguish the two cases, the |accept_focus| method
sets the private instance variable |traversal_focus| to |True| before
it calls |XSetInputFocus|. The |focusIn| action then checks this
variable and if it is not set, calls the |accept_focus| method.

The |focusOut| action resets |traversal_focus| to |False|.

The |traversal_focus| variable can be interpreted to mean, that the
widget has the keyboard focus and that it is because of keyboard
traversal. At least in the Common widget, it can never be |True| when
|traversalOn| is not set. It can also only be |True| when the widget
actually has the focus, except in the short time between the
|XSetInputFocus| call and the delivery of the |FocusIn| event.
(However, this scheme depends on the |focusOut| action for resetting
|traversal_focus| to |False|, so, if the translation for the
|FocusOut| event is overridden, it will break down.)

@ * User events

The |traverseXXX| actions can be bound to keyboard events. They call
the |traverse| method, which will try to change the focus in the
indicated direction. The directions are: Home, Up, Left, Down, Right,
Next, Prev.  Each direction can be considered a constraint or
criterium for choosing the focus widget, e.g., `Up' selects the
nearest widget that is above the current widget. `Next' and `Prev' are
simpler, in that they do not check the distance, but only the order in
the list of children.

The |traverseCurrent| action is different. It is usually bound to a
mouse click and its task is to set the focus to the widget itself. It
does this by calling |accept_focus| on itself.

The |traverse| method looks for a widget in the indicated direction,
within the same application. If the direction is not `Next' or `Prev',
the method first recurses upwards, to the toplevel widget. From there
it recurses down again, to all children, grandchildren, etc., looking
for the widget that best matches the criterium. If a widget is found,
the focus will be set to it with a call to |XSetInputFocus|. The
private variable |traversal_focus| will be set to |True| to indicate
that the widget received the focus as a result of keyboard traversal,
and not from the window manager or any other source.

If the |direction| argument is `Next' or `Prev', |traverse| will try
to set the focus to a sister widget, using the |accept_focus| method.
If there is no suitable sister, the parent will be asked to find an
aunt widget, and so on.

Note that the |traverse| and |accept_focus| methods of the Common
widget only set the focus to a child, if the widget itself has
|traversalOn|.  Thus, setting |traversalOn| to |False| for a certain
widget not only excludes the widget itself from keyboard traversal,
but also all its children.

The |traverse| function is a method and not a utility function,
because it is expected that a few subclasses may want to redefine it.
E.g., the (not yet existing) Group widget may want to limit traversal
to widgets within itself. (And presumably define new actions to jump
outside the group.)

To check if a widget suits the criterium, two things must be
determined: is the widget eligible for the focus and what is the
distance between the widget and the target position. To be able to
determine if the widget can accept the focus without actually setting
it, a method |would_accept_focus| is defined, that returns |True| if
the widget is willing to set the focus to itself.

@ If the |dir| argument to |traverse| is |TraverseNext| or
|TraversePrev|, the |traverse_to_next| or |traverse_to_prev| utility
functions are called.  Otherwise, the |traverse| method checks the
class of the parent. If the parent is a subclass of |XfwfCommon|, it
also has a |traverse| method and the task of finding a widget to
traverse to is delegated to the parent. Otherwise, the desired widget
is looked for with the help of a utility function.

The |dir| argument is one of Home, Up, Down, Left, Right, Next or
Prev.  The |current| argument holds the widget that currently has the
focus and relative to which the focus will have to move.

@def LARGE_NUMBER = 2000000000

@proc traverse($, TraversalDirection dir, Widget current, Time *time)
{
    Widget w, parent = XtParent($);
    Position x, y;
    int distance = LARGE_NUMBER;

    if (dir == TraverseNextTop)
        traverse_to_next_top($, current, time);
    else if (dir == TraverseNext)
        traverse_to_next($, current, time);
    else if (dir == TraversePrev)
        traverse_to_prev($, current, time);
    else if (XtIsSubclass(parent, xfwfCommonWidgetClass))
        $parent$traverse(parent, dir, current, time);
    else {
        switch (dir) {
        case TraverseHome: x = 0; y = 0; break;
        case TraverseLeft: x = 0; y = $current$height/2; break;
        case TraverseDown: x = $current$width/2; y = $current$height; break;
        case TraverseRight: x = $current$width; y = $current$height/2; break;
        case TraverseUp: x = $current$width/2; y = 0; break;
	default: break;
        }
        if (dir != TraverseHome) XtTranslateCoords(current, x, y, &x, &y);
        if (traverse_to_direction($, dir, x, y, &w, &distance))
            XtCallAcceptFocus(w, time);
    }
}



@UTILITIES

@ The converter |cvtStringToAlignment| converts strings like `right',
`top left' and `bottom center' to values of type |Alignment|.

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

@proc Boolean cvtStringToAlignment(Display *display, XrmValuePtr args, Cardinal *num_args, XrmValuePtr from, XrmValuePtr to, XtPointer *converter_data)
{
    Alignment a = 0;
    char c, *t, *s = (char*) from->addr;

    if (*num_args != 0)
        XtAppErrorMsg(XtDisplayToApplicationContext(display),
                      "cvtStringToAlignment", "wrongParameters",
                      "XtToolkitError",
                      "String to Alignment conversion needs no arguments",
                      (String*) NULL, (Cardinal*) NULL);

    while (*s) {
        for (; isspace(*s); s++) ;
        for (t = s; *t && ! isspace(*t); t++) ;
        c = *t;
        *t = '\0';
        if (XmuCompareISOLatin1(s, "top") == 0) a |= XfwfTop;
        else if (XmuCompareISOLatin1(s, "bottom") == 0) a |= XfwfBottom;
        else if (XmuCompareISOLatin1(s, "center") == 0) ; /* skip */
        else if (XmuCompareISOLatin1(s, "left") == 0) a |= XfwfLeft;
        else if (XmuCompareISOLatin1(s, "right") == 0) a |= XfwfRight;
        else {
            XtDisplayStringConversionWarning(display, (char*) from->addr, 
                                             "Alignment");
            break;
        }
        *t = c;
        s = t;
    }
    done(Alignment, a);
}

@ The converter |cvtAlignmentToString| does the reverse: it convertes values of type |Alignment| (|int|'s) to strings.

@proc Boolean cvtAlignmentToString(Display *display, XrmValuePtr args, Cardinal *num_args, XrmValuePtr from, XrmValuePtr to, XtPointer *converter_data)
{
    Alignment *a = (Alignment*) from->addr;

    if (*num_args != 0)
        XtAppErrorMsg(XtDisplayToApplicationContext(display),
                      "cvtAlignmentToString", "wrongParameters",
                      "XtToolkitError",
                      "Alignment to String conversion needs no arguments",
                      (String*) NULL, (Cardinal*) NULL);
    switch (*a) {
    case XfwfCenter: done(String, "center");
    case XfwfBottom: done(String, "bottom");
    case XfwfTop: done(String, "top");
    case XfwfLeft: done(String, "left");
    case XfwfRight: done(String, "right");
    case XfwfBottom + XfwfLeft: done(String, "bottom left");
    case XfwfBottom + XfwfRight: done(String, "bottom right");
    case XfwfTop + XfwfLeft: done(String, "top left");
    case XfwfTop + XfwfRight: done(String, "top right");
    default: done(String, "unknown");
    }
}

@ The following string is the set of translations that will be added
to any widget that has |traversalOn| set to |True|. The string is
compiled into Xt's internal representation by the |class_initialize|
method.

@var char extraTranslations[] = "<Key>: checkTraverse()";

@var char extraTranslationsSmall[] = "\
        <FocusIn>: focusIn()\n\
        <FocusOut>: focusOut()";

@ The |traverse_to_direction| function returns the nearest child,
grandchild, etc. in the indicated direction that is willing to accept
the focus. It returns |False| if no widget is found. The position is the
absolute coordinates, i.e., relative to the root window. The |distance|
argument holds the distance from |x,y| of the best widget so far. If the
function finds a better one, it will return the new distance through
this parameter.

@proc Boolean traverse_to_direction($, TraversalDirection dir, int x, int y, Widget *found, int *distance)
{
    int i;
    Position rx, ry;
    int dist;
    Boolean found_child = False;

    if (! $traversalOn) return False;
    /*
     * First recurse to all descendants
     */
    for (i = 0; i < $num_children; i++)
        if (XtIsSubclass($children[i], xfwfCommonWidgetClass)
            && traverse_to_direction($children[i], dir, x, y, found, distance))
            found_child = True;
    if (found_child) return True;
    /*
     * No child found, now check own position and distance
     */
    switch (dir) {
    case TraverseHome: rx = 0; ry = 0; break;
    case TraverseLeft: rx = $width; ry = $height/2; break;
    case TraverseDown: rx = $width/2; ry = 0; break;
    case TraverseRight: rx = 0; ry = $height/2; break;
    case TraverseUp: rx = $width/2; ry = $height; break;
    default: break;
    }
    XtTranslateCoords($, rx, ry, &rx, &ry);
    if ((dir == TraverseUp && ry > y)
        || (dir == TraverseLeft && rx > x)
        || (dir == TraverseDown && ry < y)
        || (dir == TraverseRight && rx < x)) return False;
    dist = (rx - x)*(rx - x) + (ry - y)*(ry - y);
    if (dist >= *distance) return False;
    /*
     * We are the best so far, but do we want the focus?
     */
    if (! $would_accept_focus($)) return False;
    *distance = dist;
    *found = $;
    return True;
}


@ The |traverse_to_next| routine looks for the |current| widget among
its children. If it is found, all children following it will be tried
until one accepts the focus. If no child does, the routine will try to
ask the parent to find a sister widget instead.

@proc traverse_to_next($, Widget current, Time *time)
{
    int i = 0;
    Widget parent = XtParent($);

    while (i < $num_children && $children[i] != current) i++;
    for (i++; i < $num_children; i++)
        if (XtCallAcceptFocus($children[i], time)) return;
    if (XtIsSubclass(parent, xfwfCommonWidgetClass))
        $parent$traverse(parent, TraverseNext, $, time);
}

@ |traverse_to_prev| looks for the |current| widget among the children,
if it is found, all children before it will be asked in turn to accept
the focus. If none does, the parent is asked to set the focus to a
sister instead.

@proc traverse_to_prev($, Widget current, Time *time)
{
    int i = 0;
    Widget parent = XtParent($);

    while (i < $num_children && $children[i] != current) i++;
    for (i--; i >= 0; i--)
        if (XtCallAcceptFocus($children[i], time)) return;
    if (XtIsSubclass(parent, xfwfCommonWidgetClass))
        $parent$traverse(parent, TraversePrev, $, time);
}


@proc traverse_to_next_top($, Widget current, Time *time)
{
    Widget parent = XtParent($);

    if (XtIsSubclass(parent, xfwfCommonWidgetClass))
        $parent$traverse(parent, TraverseNextTop, current, time);
    else
        XtCallCallbackList($, $nextTop, NULL);
}



@METHODS

@ The method |lighter_color| uses |XfwfChooseColor| to compute a color
that is 1.35 times as bright as the color passed in as argument. The
function result is |True| if a color was allocated, else |False|.

@proc Boolean lighter_color($, Pixel base, Pixel *result)
{
  return get_scaled_color($, 1.35, base, result);
}


@ The method |darker_color| uses |XfwfChooseColor| to compute a color
that is 0.6 times as bright as the color passed in as argument. The
function result is |True| if a color was allocated, else |False|.

@proc Boolean darker_color($, Pixel base, Pixel *result)
{
  return get_scaled_color($, 0.6, base, result);
}


@ The method |set_color| uses |XfwfChooseColor| to compute a color
that is 0.85 times as bright as the color passed in as argument. The
function result is |True| if a color was allocated, else |False|.

@proc Boolean set_color($, Pixel base, Pixel *result)
{
  return get_scaled_color($, 0.85, base, result);
}



@ACTIONS

@ When the widget receives or looses the focus, the border highlight
is drawn or removed. This action function draws the highlight border
and in case the widget has set |traversalOn|, it also sets the
keyboard focus to the widget itself, or one of its children.

However, FocusIn events may also be so-called virtual events, meaning
that not the receiving widget, but one of its descendants gets the
real focus. When |focusIn| receives one of those, it removes the
highlight border.

@def focus_detail(detail) =
    (detail == NotifyAncestor ? "NotifyAncestor" :
     detail == NotifyVirtual ? "NotifyVirtual" :
     detail == NotifyInferior ? "NotifyInferior" :
     detail == NotifyNonlinear ? "NotifyNonlinear" :
     detail == NotifyNonlinearVirtual ? "NotifyNonlinearVirtual" :
     detail == NotifyPointer ? "NotifyPointer" :
     detail == NotifyPointerRoot ? "NotifyPointerRoot" :
     detail == NotifyDetailNone ? "NotifyDetailNone" :
     "???")

@proc focusIn
{
    Time time = CurrentTime;

    if (event->type != FocusIn)
        XtError("focusIn action may only be bound to FocusIn events");
    if (! $traversalOn)
        return;

    if (event->xfocus.detail == NotifyAncestor
        || event->xfocus.detail == NotifyInferior
        || event->xfocus.detail == NotifyNonlinear) {
        if (!$traversal_focus)
	  $accept_focus($, &time);
    } else if (event->xfocus.detail != NotifyPointer) {
      if ($traversal_focus) {
        $unhighlight_border($);
        $traversal_focus = False;
        $hilite_callbacks($);
      }
   }
}


@ This action removes the highlight border.

@proc focusOut
{
    if (event->type != FocusOut)
        XtError("focusOut action may only be bound to FocusOut events");
    if ($traversal_focus) {
      if (event->xfocus.detail == NotifyAncestor
        || event->xfocus.detail == NotifyInferior
        || event->xfocus.detail == NotifyNonlinear) {
          $unhighlight_border($);
          $traversal_focus = False;
          $hilite_callbacks($);
       }
    }
}

@proc checkTraverse
{
  static KeyCode up, down, left, right, next, prior, tab, enter, home;
  char *action;

  if (!up) {
    Display *d = XtDisplay($);
    up = XKeysymToKeycode(d, XK_Up);
    down = XKeysymToKeycode(d, XK_Down);
    left = XKeysymToKeycode(d, XK_Left);
    right = XKeysymToKeycode(d, XK_Right);
    next = XKeysymToKeycode(d, XK_Next);
    prior = XKeysymToKeycode(d, XK_Prior);
    enter = XKeysymToKeycode(d, XK_KP_Enter);
    home = XKeysymToKeycode(d, XK_Home);
    tab = XKeysymToKeycode(d, XK_Tab);
  }

  if (event->xkey.keycode == up)
    action = "traverseUp";
  else if (event->xkey.keycode == down)
    action = "traverseDown";
  else if (event->xkey.keycode == left)
    action = "traverseLeft";
  else if (event->xkey.keycode == right)
    action = "traverseRight";
  else if (event->xkey.keycode == next)
    action = "traverseNext";
  else if (event->xkey.keycode == prior)
    action = "traversePrev";
  else if (event->xkey.keycode == enter)
    action = "traverseNextTop";
  else if (event->xkey.keycode == home)
    action = "traverseHome";
  else if (event->xkey.keycode == tab) {
    if (event->xkey.state & ShiftMask)
      action = "traversePrev";
    else
      action = "traverseNext";
  } else
    action = NULL;

  if (action)
    XtCallActionProc($, action, event, NULL, 0);
  else if ($travMode == 2)
    $travMode = 0;
}

@ This and the following actions all call the |traverse| method of the
widget's parent, with the appropiate direction arguments.
|traverseDown| tries to set the focus to a widget that is located
roughly below the current one.

@proc traverseDown
{
    $traverse($, TraverseDown, $, &event->xkey.time);
}


@ The action tries to set the focus to a widget that is above the this
one.

@proc traverseUp
{
    $traverse($, TraverseUp, $, &event->xkey.time);
}


@ |traverseLeft| looks for a widget to the left of the current one and
sets the keyboard focus to that.

@proc traverseLeft
{
    $traverse($, TraverseLeft, $, &event->xkey.time);
}


@ The action looks for a widget that will aceept the focus to the
right of the current one.

@proc traverseRight
{
    $traverse($, TraverseRight, $, &event->xkey.time);
}


@ The next sibling gets the focus. The precise order is determined by
the parent, but usually is will be the order in which the widgets were
created. If there is no suitable sibling, the request is passed to the
grandparent, so that an `aunt widget' or other relation can get the
focus.

@proc traverseNext
{
    $traverse($, TraverseNext, $, &event->xkey.time);
}


@ The previous widget gets the focus. See also the description of
|traverseNext| above.

@proc traversePrev
{
    $traverse($, TraversePrev, $, &event->xkey.time);
}


@ |traverseNextTop| finds the topmost ancestor that is a subclass of
Common and lets it call the |nextTop| callbacks that have been
registered there. These callbacks can be used by an application that
has multiple top level windows to set the focus to another window.

@proc traverseNextTop
{
    $traverse($, TraverseNextTop, $, &event->xkey.time);
}


@ The action sets the focus to the sibling widget that is closest to
the upper left corner of the parent.

@proc traverseHome
{
    $traverse($, TraverseHome, $, &event->xkey.time);
}


@ The |traverseCurrent| action can be used by widgets to set the focus
to themselves. It is not used in the set of translations that is added
when |traversalOn| is set to |True|.

@proc traverseCurrent
{
    Time time = CurrentTime;

    if ($traversalOn) (void) $accept_focus($, &time);
}


@EXPORTS

@ The |create_bordergc| function creates a new GC for filling the
highlight border with.

@proc create_bordergc($)
{
    XtGCMask mask;
    XGCValues values;

    if ($bordergc) XtReleaseGC($, $bordergc);
    if ($highlightPixmap != None) {
        mask = GCFillStyle | GCTile;
        values.fill_style = FillTiled;
        values.tile = $highlightPixmap;
    } else {
	Pixel res;
        mask = GCFillStyle | GCForeground;
        values.fill_style = FillSolid;
	lighter_color($, $highlightColor, &res);
        values.foreground = res;
    }
    $bordergc = XtGetGC($, mask, &values);
}    

@proc Boolean get_scaled_color($, float scale, Pixel base, Pixel *result)
{
  /* These two must be mutually prime: */
#define CACHE_SIZE 29
#define CACHE_STEP 10
  static struct {
    int set;
    float scale;
    Pixel in, out;
  } cache[CACHE_SIZE];
  static int reset = 0;
  int i;

  for (i = 0; i < CACHE_SIZE; i++) {
    if (cache[i].set 
	&& (cache[i].scale == scale)
	&& (cache[i].in == base)) {
      *result = cache[i].out;
      return TRUE;
    }
  }

  if (!XfwfChooseColor($, scale, base, result))
    return FALSE;
  
  cache[reset].scale = scale;
  cache[reset].in = base;
  cache[reset].out = *result;
  cache[reset].set = 1;

  if (!cache[reset].set) {
    cache[reset].set = 1;
    reset++;
  } else
    reset += CACHE_STEP;

  reset = (reset % CACHE_SIZE);

  return TRUE;
}

@proc Boolean has_focus_now($)
{
  return $traversal_focus;
}

@IMPORTS

@incl <stdio.h>
@incl <ctype.h>
@incl <X11/Xmu/Converters.h>
@incl <X11/Xmu/CharSet.h>
@incl <X11/Shell.h>
@incl <X11/keysym.h>
@incl "wxAllocColor.h"
@incl "wxAllocColor.c"
@incl "wxgl.h"
