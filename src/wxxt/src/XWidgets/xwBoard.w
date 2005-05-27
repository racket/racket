

@class XfwfBoard (XfwfFrame) @file=xwBoard

@ The Board class adds one thing to the capabilities already pocation management is an improved version of the standard X geometry
management. Size and position of a Board widget (or subclass) can be
given as a combination of absolute and relative sizes.

In contrast to its superclass Frame, Board accepts any number of
children. No layout policy is enforced, however. The children are
expected to be positioned with the help of their own geometry or
location resources.



@public

@ The location management relies on a total of ten resources plus a
string resource that combines eight of them in a more convenient and
compact notation. The location is given by two values in each of the
four dimensions (x, y, width and height). One value holds the absolute
position in pixels, the other holds the position relative to the
parent's width. E.g., When |abs_x| is 20 and |rel_x| is 0.3, the x
position of the widget will be 20 pixels plus 0.3 times the width of
the parent. For more examples, see the |location| resource below.

The ninth and tenth resources are |hunit| and |vunit|. All assignments
to the |abs_*| resources are multiplied by |hunit| (horizontal) or
|vunit| (vertical). Normally the units are 1, but, e.g., a widget that
displays text might set them to the width and height of character
cells, so that |abs_width = 80| means a width of 80 characters,
instead of 80 pixels.

The geometry resources of the Core widget (|x|, |y|, |width| and
|height| are still available.  When they are set, the values are
copied to the |abs_*| variables and the |rel_*| variables are set to
0.0.

It is possible that the parent of the current widget doesn't grant the
preferred geometry. In that case the location variables and the geometry
variables will not be synchronized. The location variables will then be
taken to hold the preferred geometry, instead of the actual one.

@ The position is determined by the four resources |abs_x|, |rel_x|,
|abs_y| and |rel_y|.  When the parent is (a subclass of) a Board
widget, the position is not measured from the real size of the parent,
but from the size inside the frame.

(The representation of the float values as strings seems necessary,
because the compiler can't cast a float to a pointer.)

@var Position abs_x = 0
@var float rel_x = <String> "0.0"
@var Position abs_y = 0
@var float rel_y = <String> "0.0"

@ By setting default values for the |x| and |y| variables from Core
explicitly, we can be sure that the variables are synchronized from the
start. If the |initialize| method detects a change in any of them, it can
re-synchronize them.

@def MAGICNUM = 12349
@var x = MAGICNUM
@var y = MAGICNUM

@ The default values cause a Board widget to be the same size as it's
parent at all times, provided, of course, that the parent allows that.
If the parent is (a subclass of) a Board widget, the size is relative
to the area inside the parent's frame, instead of the total size of
the parent.

@var Position abs_width = 0
@var float rel_width = <String> "1.0"
@var Position abs_height = 0
@var float rel_height = <String> "1.0"

@ The Core variables are given strange defaults, in the hope that the
|initialize| method can detect a change in them.

@var width = MAGICNUM
@var height = MAGICNUM

@ |hunit| is a value in pixels by which |abs_x| and |abs_width| are
multiplied; |abs_y| and |abs_height| are multiplied by |vunit|. The
results are rounded to the next larger whole number.

@var float hunit = <String> "1.0"
@var float vunit = <String> "1.0"

@ Specifying eight resources in a resource file is more easily done
with the string resource |location|. The string contains four
expressions of the form |xa [+-] xr| or |xr [+-] xa| or |xa| or
|xr|, where |[+-]| is either |+| or |-|, |xa| is the absolute value and |xr| is the relative
value. The two are distinguished by the fact that |x_r| must
contain a decimal point.

Examples: |"0.5 - 20  5  40  1.0 - 50"| is a widget of fixed width (40
units) that is horizontally centered; the height is always 50 units
less than the height of the parent.

|"0 0 2.0 3.0"| is a widget that is twice as wide and three times as
high as its parent.

|"-20 0 20 20"| is a widget that will be invisible, because it is
located 20 units to the left of the parent and it is also 20 units
wide.

The initial value is |NULL|, but the |initialize| method will make sure
that the string is synchronized with the other variables.

@var String location = NULL



@methods

@ Changes in the location resources result in changes in the core
geometry resources. If the location resources didn't change, but the
core geometry resources did, the location variables are set
accordingly. If various resources are changes at the same time,
|location| takes precedence, followed by the |abs_*| and |rel_*|
variables, and finally the core geometry variables |x|, |y|, |width|
and |height|.

|set_values| takes care that all these resources always correspond to
each other; even the |location| string is re-generated when any of the
others change.

Since the location is handled by setting the core geometry resources,
there is never any need to redraw the widget.

A complication arises when the frame of the Board widget changes,
since children may have sizes that are relative to the area inside the
frame. The Board widget therefore gives its children a chance to
calculate their new locations in this case.

@proc set_values
{
    XtWidgetGeometry reply;
    int i;

    if ($location != $old$location) {
        XtFree($old$location);
        $location = XtNewString($location);
        interpret_location($);
        get_core_geometry($, &$x, &$y, &$width, &$height);
    } else if (ceil($abs_x*$hunit) != ceil($old$abs_x*$old$hunit)
               || ceil($abs_width*$hunit) != ceil($old$abs_width*$old$hunit)
               || ceil($abs_y*$vunit) != ceil($old$abs_y*$old$vunit)
               || ceil($abs_height*$vunit) != ceil($old$abs_height*$old$vunit)
               || $rel_x != $old$rel_x
               || $rel_y != $old$rel_y
               || $rel_width != $old$rel_width
               || $rel_height != $old$rel_height) {
        get_core_geometry($, &$x, &$y, &$width, &$height);
        generate_location($);
    } else if ($x != $old$x
               || $y != $old$y
               || $width != $old$width
               || $height != $old$height) {
      set_location($, ((($x != $old$x) ? CWX : 0)
		       | (($y != $old$y) ? CWY : 0)
		       | (($width != $old$width) ? CWWidth : 0)
		       | (($height != $old$height) ? CWHeight : 0)));
      generate_location($);
    }
    if ($total_frame_width(old) != $total_frame_width($)) {
        for (i = 0; i < $num_children; i++) {
            (void) XtQueryGeometry($children[i], NULL, &reply);
            XtConfigureWidget($children[i], reply.x, reply.y, reply.width,
                              reply.height, reply.border_width);
        }
    }
    return False;
}

@ The initialize method is used to synchronize the location and geometry
resources for the first time. It is difficult to find out which variables
have been set from resources and which still have their initial value, we
rely on the fact that the default value is unlikely to be used in
practice.

If the |location| string has been set, it will be used to set all other
variables. If the Core geometry resources have been set, we use them,
otherwise, the location variables will determine the size and position.

@proc initialize
{
    if ($location != NULL) {
        $location = XtNewString($location);
        interpret_location($);
        get_core_geometry($, &$x, &$y, &$width, &$height);
    } else if ($x != MAGICNUM || $y != MAGICNUM
               || $width != MAGICNUM || $height != MAGICNUM) {
        set_location($, CWX | CWY | CWWidth | CWHeight);
        generate_location($);
    } else {
        generate_location($);
        get_core_geometry($, &$x, &$y, &$width, &$height);
    }
}

@ The |set_abs_location| method is a convenience function for use by
subclasses. When they want to set the |x|, |y|, |width| or |height|
resources, they can call this function which will than also adjust the
other location resources accordingly. The flags determine which
resources are set, it is a bitwise combination of |CWX|, |CWY|,
|CWWidth| and |CWHeight|.

@proc set_abs_location($, unsigned int flags, int x, int y, int w, int h)
{
    if ((flags & (CWX | CWY | CWWidth | CWHeight)) == 0) return;
    if (flags & CWX) $x = x;
    if (flags & CWY) $y = y;
    if (flags & CWWidth) $width = max(1, w);
    if (flags & CWHeight) $height = max(1, h);
    set_location($, flags);
    generate_location($);
}

@ The |resize| method is called when the widget is resized.  The children
of the Board widget will be given a chance to re-compute their preferred
locations, which will then be granted them. It may be possible that the
parent of the current widget didn't grant the preferred geometry. In that
case the geometry variables will be different from the location variables.
The latter will not be changed, in the hope that the requested geometry
can be set later.

@proc resize
{
    int i;
    XtWidgetGeometry reply;
    Widget child;

    for (i = 0; i < $num_children; i++) {
        child = $children[i];
        (void) XtQueryGeometry(child, NULL, &reply);
        XtConfigureWidget(child, reply.x, reply.y, reply.width,
                          reply.height, reply.border_width);
    }
}

@ When the Board's parent asks for this widget's preferred geometry,
simply return the geometry as indicated by the location variables.
Currently, the method always returns |XtGeometryAlmost|. It doesn't bother
to check if the preferred geometry is equal to the current geometry (in
which case it should really return |XtGeometryNo|) or if the preferred
geometry is equal to what the parent proposed (in which case a return of
|XtGeometryYes| should have been given.

It seems that no harm is done by always returning |XtGeometryAlmost| and
letting Xt figure out what really needs to be changed.

@proc query_geometry
{
    reply->request_mode = CWX | CWY | CWWidth | CWHeight;
    get_core_geometry($, &reply->x, &reply->y,
                      &reply->width, &reply->height);
    return XtGeometryAlmost;
}

@ If a child requests to be resized, the request is always granted. We
ignore stacking order.

@proc geometry_manager
{
    /* Widget $ = XtParent(child); */
    Dimension wd, ht, bw;
    Position x, y;

    /* Get complete geometry, from request or current value */
    x = request->request_mode & CWX ? request->x : $child$x;
    y = request->request_mode & CWY ? request->y : $child$y;
    wd = request->request_mode & CWWidth ? request->width : $child$width;
    ht = request->request_mode & CWHeight ? request->height : $child$height;
    bw = request->request_mode & CWBorderWidth ? request->border_width
        : $child$border_width;

    if (wd <= 0) wd = 1; if (ht <= 0) ht = 1; /* MATTHEW: [5] */
    XtConfigureWidget(child, x, y, wd, ht, bw);
    return XtGeometryDone;
}

@ If a child becomes managed or unmanaged, the Board widget is given a
change to resize or reposition the child. The Board widget doesn't do
that, but it does install all accelerators of its descendants here.

@proc change_managed
{
#if 1
    Widget top = $, w;

    while (!XtIsSubclass(top, shellWidgetClass)) top = XtParent(top) ;
    for (w = $; w != top; w = XtParent(w)) XtInstallAllAccelerators(w, top);
#endif
}



@utilities

@def ceil(r) = (-(int)(-(r)))

@ The routine |generate_location| creates the string |location| from the
values of the location resources.

@proc generate_location($)
{
    char tmp[100];

    (void) sprintf(tmp, "%d+%f %d+%f %d+%f %d+%f",
                   $abs_x, $rel_x, $abs_y, $rel_y, $abs_width, $rel_width,
                   $abs_height, $rel_height);
    XtFree($location);
    $location = XtNewString(tmp);
}

@ To get the core geometry from the location variables, the function
|get_core_geometry| is used. It combines the relative and absolute
parts of the location and sets the result in the passed variables.
When the parent is a Board widget or a subclass thereof, the area
inside the parent's frame is used for calculations, otherwise the
whole area of the parent will be used.

As a safeguard against possible non-positive sizes, the width and
height cannot become smaller than 1 pixel.

@proc get_core_geometry($, Position *x, Position *y, Dimension *width, Dimension *height)
{
    Widget parent;
    Position px, py;
    int pw, ph, minsize;
    float h;

    parent = $parent;
    if (XtIsSubclass($parent, xfwfBoardWidgetClass))
        $parent$compute_inside(parent, &px, &py, &pw, &ph);
    else {
        px = 0;
        py = 0;
        pw = $parent$width;
        ph = $parent$height;
    }
    pw = max(0, pw);
    ph = max(0, ph);

    *x = ceil($rel_x * pw + $abs_x * $hunit) + px;
    *y = ceil($rel_y * ph + $abs_y * $vunit) + py;
    minsize = 2 * $total_frame_width($);
    minsize = max(1, minsize);
    h = ceil($rel_width * pw + $abs_width * $hunit);
    *width = h < minsize ? minsize : h;
    h = ceil($rel_height * ph + $abs_height * $vunit);
    *height = h < minsize ? minsize : h;
}

@ The reverse operation, computing the location variables from the core
geometry is done by |set_location|.

@proc set_location($, unsigned int flags)
{
    Widget parent;
    Position px, py;
    int pw, ph;

    parent = $parent;
    if (XtIsSubclass($parent, xfwfBoardWidgetClass))
        $parent$compute_inside(parent, &px, &py, &pw, &ph);
    else {
        px = 0;
        py = 0;
        pw = $parent$width;
        ph = $parent$height;
    }
    pw = max(0, pw);
    ph = max(0, ph);
    if (flags & CWX) {
        $rel_x = 0.0;
        $abs_x = ceil(($x - px)/$hunit);
    }
    if (flags & CWY) {
        $rel_y = 0.0;
        $abs_y = ceil(($y - py)/$vunit);
    }
    if (flags & CWWidth) {
        $rel_width = 0.0;
        $abs_width = ceil($width/$hunit);
    }
    if (flags & CWHeight) {
        $rel_height = 0.0;
        $abs_height = ceil($height/$vunit);
    }
}

@ Interpreting the |location| string is a little harder, but still
straightforward. Only numbers (with or without decimal points) and plus
and minus signs can appear in the string.

@ |scan| recognizes four formats: an integer followed by a plus or minus
and a float, a float followed by a plus or minus and an integer, a single
integer, or a single float.

@def skip_blanks(s) = while (isspace(*s)) s++

@ |strtonum| first skips everything that is not a digit or a period.
When it finds a digit, it computes the (decimal) number represented
there. If not, no error will be generated and |n| will simply be 0.

@def strtonum(t, n) =
    do {
        while ((*(t)) && !isdigit(*(t)) && *(t) != '.') (t)++;
        for ((n) = 0; isdigit(*t); (t)++)
            (n) = 10 * (n) + *(t) - '0';
    } while (0)

@ |strtofrac| assumes |t| points to the decimal point of a fractional
part. It computes the value of the fractional part as if it was a
decimal number and stores it in |n|, while |factor| holds the factor
(0.1, 0.01, etc.) with which |n| must be multiplied.

@def strtofrac(t, n, factor) =
    for ((factor) = 1.0, (n) = 0, (t)++; isdigit(*(t)); (t)++, (factor) /= 10.0)
        (n) = 10 * (n) + *(t) - '0'

@proc String scan(String s, Position *absval, float*relval)
{
    String p;
    char c;
    long n;
    float fract, factor;

    *absval = 0;
    *relval = 0.0;
    p = s;
    strtonum(p, n);
    if (*p != '.') {                            /* 1st number is integer */
        *absval = n;
        skip_blanks(p);
        c = *p;
        if (c != '+' && c != '-') return p;     /* No second number */
        s = p;
        strtonum(p, n);
        if (*p != '.') return s;                /* This is an error... */
        strtofrac(p, fract, factor);
        *relval = c == '-' ? -fract * factor - n : fract * factor + n;
        return p;
    } else {                                    /* 1st number is float */
        strtofrac(p, fract, factor);
        *relval = fract * factor + n;
        skip_blanks(p);
        c = *p;
        if (c != '+' && c != '-') return p;     /* No second number */
        strtonum(p, n);
        *absval = c == '-' ? -n : n;
        return p;
    }
}

@proc interpret_location($)
{
    char *s;

    s = $location;
    s = scan(s, &$abs_x, &$rel_x);
    s = scan(s, &$abs_y, &$rel_y);
    s = scan(s, &$abs_width, &$rel_width);
    s = scan(s, &$abs_height, &$rel_height);
}



@imports

@incl <stdio.h>
@incl <X11/Shell.h>
@incl <ctype.h>
