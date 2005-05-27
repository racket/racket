# Group.w -- Wbuild definition of XfwfGroup widget
# Author: Bert Bos <bert@let.rug.nl>
# Version: 1.0 for FWF 3.53
#

@CLASS XfwfGroup (XfwfRowCol)  @file=xwGroup

@ The Group widget is a subclass of RowCol, which means that it
arranges its children in rows and columns. (See the RowCol widget for
the resources that influence the layout.) The Group widget adds two
things to the capabilities already present in RowCol, namely a label
in the upper left hand corner and the ability to make a number of
Toggle buttons act as radio buttons.

The label is a short, one line piece of text, that is displayed over
the border in the top left corner. The border is interupted at that
point.  Since this works best with `ledged' or `chiseled' border
types, the default border is |XfwfChiseled|.

The special support for radio buttons works as follows: when a child
is added the Group widget checks if it is of class |XfwfToggle| or a
subclass thereof. If so, the Group widget installs a callback in it.
When the toggle button is then activated, the Group widget determines
which other buttons need to be turned off. All toggle buttons are
given an implicit number. The first one to be added is number 0.

There are three selection styles, settable through the
|selectionStyle| resource:

1) `single' (|XfwfSingleSelection|) means that at most one of the
toggle buttons may be on, but it is possible that they are all off.
When one of the buttons is turned on, all others are turned off
automatically. The resource |selection| holds the number of the button
that is on, or -1 if they are all off.

2) `one' (|XfwfOneSelection|) means that at all times exactly one
button is turned on. It is not possible to turn buttons off, except by
toggling another one to on. The resource |selection| holds the number
of the button that is currently on.

3) `multi' or `multiple' (|XfwfMultipleSelection|) means that any
number of buttons may be on. No buttons are turned off automatically.
The resource |selection| has one bit set for each button that is on.
Thus, if buttons 1, 4 and 5 are on, the value of |selection| is (1<<1
+ 1<<4 + 1<<5 =) 0x62. Note that this limits the number of buttons
that is recorded in |selection| to the first 32. It is still possible
to have more buttons, but the application will then have to use
callbacks or other means to keep track of the toggle buttons.

4) `none' (|XfwfNoSelection|) turns off any special handling of toggle
buttons. The value of the |selection| resource is undefined.

Applications may of course install callbacks on the toggle buttons,
but a simpler way is to attach to the |activateCallback| of the Group
widget itself, or use no callback at all and simply inspect the
|selection| resource when needed.

It is recommended that application visually differentiate between
selection styles. One way would be to use different graphics in the
toggle buttons, e.g., the Motif convention that radiobuttons have an
empty or colored diamond, and non-exclusive toggles a square.
Suitable icons are already defined by the Common widget.


@PUBLIC

@ The label must be a single line. It is displayed superimposed on the
frame, in the upper lefthand corner. Currently, it must be simple
string in a single font.

	@var String label = NULL

@ The font for the label is set with |font|.

	@var <FontStruct> XFontStruct *font = <String> XtDefaultFont

@ The foreground color is the color used to draw the text.

	@var Pixel foreground = <String> XtDefaultForeground

@ The |selectionStyle| resource determines how the Group widget treats
the child widgets that are of class |XfwfToggle| or a subclass
thereof. The possible values are |XfwfNoSelection|,
|XfwfSingleSelection| (default), |XfwfOneSelection| and
|XfwfMultipleSelection|. The meaning is as explained above. There is a
converter from strings, that recognizes the strings `no', `none',
`single', `one', `multi', and `multiple', in upper or lower case.

	@var SelectionType selectionStyle = XfwfSingleSelection

@ The resource |selection| holds the state of the toggle buttons (if
any). If |selectionType = XfwfSingleSelection| or |XfwfOneSelection|,
it holds the number of the buttons that is currently on, or -1 if they
are all off. If |selectionType = XfwfMultipleSelection|, it is a
bitmap with one bit set for each button that is on. (See the
introduction above for more information.)

The value can also be set (e.g., through |XtSetValues|); the result is
that the corresponding toggle buttons will be turned on or off.

	@var long selection = 0

@ The callback |activate| can be used by applications that want to be
informed of any change to the state of the toggle buttons as soon as
it happens. Other applications can simply use |XtGetValues| to get the
value of the |selection| resource. The callback will be called with
the value of |selection| as |call_data| argument.

	@var <Callback> XtCallbackList activate = NULL

@ The default border type is different from that of its superclass
RowCol. It is set to |XfwfChiseled| because that is the conventional
border type around radio buttons, and because it looks better when
there is a label superimposed on it.

	@var FrameType frameType = XfwfChiseled

@ The default value for |innerOffset| is set to 3 pixels, which makes
it a little bit more likely that the descenders of the label will stay
visible.

	@var innerOffset = 0

@EXPORTS

@ The |SelectionType| type is exported to the public header file.

	@type SelectionType = enum {
		XfwfNoSelection, XfwfSingleSelection,
		XfwfOneSelection, XfwfMultipleSelection }

@PRIVATE

@ The GC is used for the text.

	@var GC textgc

@ The private variable |toggle_ord| holds the number that will be
assigned to the next child that is a toggle button. The first toggle
will be number 0.

	@var Cardinal toggle_ord

@METHODS

@ The type converter from String to SelectionType is installed here.

@proc class_initialize
{
    XtAddConverter(XtRString, XtRLong, XmuCvtStringToLong, NULL, 0);
    XtSetTypeConverter(XtRString, XtRSelectionType, cvtStringToSelectionType,
		       NULL, 0, XtCacheNone, NULL);
    XtSetTypeConverter(XtRSelectionType, XtRString, cvtSelectionTypeToString,
		       NULL, 0, XtCacheNone, NULL);
}

@ The |initialize| method initializes the private variables.

@proc initialize
{
    $toggle_ord = 0;
    $textgc = NULL;
    make_textgc($);
    if ($label)
     $label = XtNewString($label);
    if ($selectionStyle == XfwfOneSelection && $selection == -1L) {
	XtWarning
	    ("Illegal combination of selectionStyle and selection resources");
	$selection = 0;
    }
}

@proc destroy
{
   if ($textgc) XtReleaseGC($, $textgc); $textgc = NULL;
}

@ The |set_values| method has to deal with changes in |label|, |font|,
|selectionType| or |selection|. A change in |selection| or
|selectionType| means that all toggle buttons must be set to on or off
according to the new values.

@proc set_values
{
    Boolean need_redraw = False;

    if ($old$label != $label) {
	XtFree($old$label);
	$label = XtNewString($label);
	need_redraw = True;
    }
    if ($font != $old$font) {
	make_textgc($);
	if ($label != NULL) need_redraw = True;
    }
    if ($old$selection != $selection
	|| $old$selectionStyle != $selectionStyle) {
	if ($selectionStyle == XfwfOneSelection && $selection == -1L)
	    $selection = 0;
	set_toggles($);
    }

    return need_redraw;
}

@ The |expose| method first calls the |expose| method of its
superclass -- which basically just draws the frame -- and then adds
the label to it.

@proc _expose
{
    int w, h;
    Position x, y;

    if (! XtIsRealized($)) return;
    #_expose($, event, region);
    if ($label) {
	$compute_inside($, &x, &y, &w, &h);

	XDrawImageString(XtDisplay($), XtWindow($), $textgc, x + 3,
			 y - $innerOffset, $label, strlen($label));
    }

   
}

@ When a child is added, the Group widget checks to see if it is a
Toggle button. If it is, and the |selectionStyle| resource is not
|XfwfNoSelection|, then the toggle button is assigned a number and two
callbacks are installed in it. The callbacks' task is to record any
changes in the state of the togle button in the Group widget's
|selection| resource and also to make sure that no more and no fewer
buttons are turned on then is allowed by the |selectionStyle|.

It doesn't matter whether the new child is managed or not: any child
that is (a subclass of) an XfwfToggle button gets two callbacks and a
number.

The |on| resource of the new child is also set in accordance with the
current value of the Group's |selection| resource.

@proc insert_child
{
    #insert_child(child);
    if ($selectionStyle != XfwfNoSelection
	&& XtIsSubclass(child, xfwfToggleWidgetClass)) {
	XtAddCallback(child, XtNonCallback, on_cb, (XtPointer) $toggle_ord);
	XtAddCallback(child, XtNoffCallback, off_cb, (XtPointer) $toggle_ord);
	switch ($selectionStyle) {
	case XfwfOneSelection:
	case XfwfSingleSelection:
	    XtVaSetValues(child, XtNon, $toggle_ord == $selection,
	    			 XtNindicatorType, XfwfDiamondIndicator, NULL);
	    break;
	case XfwfMultipleSelection:
	    XtVaSetValues(child, XtNon, ($selection & (1L<<$toggle_ord)) != 0,
	    			 XtNindicatorType, XfwfSquareIndicator, NULL);
	    break;
	default: ;
	}
	$toggle_ord++;
    }
}

@UTILITIES

@ The |make_textgc| routine creates the GC for the text. 

@proc make_textgc($)
{
    XtGCMask mask;
    XGCValues values;

    if ($textgc != NULL) XtReleaseGC($, $textgc);
    values.background = $background_pixel;
    values.foreground = $foreground;
    values.font = $font->fid;
    mask = GCFont | GCBackground | GCForeground;
    $textgc = XtGetGC($, mask, &values);
}

@ The task of the |on_cb| callback function is to record the changed
state of the toggle button and maybe turn off other toggle buttons.
When the new value of |selection| is computed, the |activate|
callbacks are called.

@proc on_cb(Widget toggle, XtPointer client_data, XtPointer call_data)
{
    Widget $ = XtParent(toggle);
    Cardinal toggle_ord = (Cardinal) client_data;
    Cardinal t, i, bits = sizeof($selection) * 8;

    switch ($selectionStyle) {
    case XfwfMultipleSelection:
	if (toggle_ord < bits) $selection |= 1L << toggle_ord;
	break;
    case XfwfSingleSelection:
    case XfwfOneSelection:
	if ($selection != -1L)
	    for (t = 0, i = 0; i < $num_children; i++)
		if (XtIsSubclass($children[i], xfwfToggleWidgetClass)) {
		    if ($selection == t) {
			XtVaSetValues($children[i], XtNon, False, NULL);
			break;
		    }
		    t++;
		}
	$selection = toggle_ord;
	break;
    default: ;
    }
    XtCallCallbackList($, $activate, (XtPointer) $selection);
}

@ The task of the |off_cb| callback function is to update the
|selection| resource and check if turning off the toggle button is
allowed. If the |selectionStyle| is |XfwfOneSelection|, toggles cannot
be turned off, except by turning on another one.

@proc off_cb(Widget toggle, XtPointer client_data, XtPointer call_data)
{
    Widget $ = XtParent(toggle);
    Cardinal toggle_ord = (Cardinal) client_data;
    Cardinal bits = sizeof($selection) * 8;

    switch ($selectionStyle) {
    case XfwfOneSelection:
	XtVaSetValues(toggle, XtNon, True, NULL); /* Undo */
	break;
    case XfwfSingleSelection:
	$selection = -1L;			/* Nothing selected */
	break;
    case XfwfMultipleSelection:
	if (toggle_ord < bits) $selection &= ~(1L << toggle_ord);
	break;
    default: ;
    }
    XtCallCallbackList($, $activate, (XtPointer) $selection);
}

@ The function |set_toggles| is used when the |selection| resource or
the |selectionStyle| resource changes. It inspects all child widgets
in turn and turns toggles on or off according to the values of these
two resources.

@proc set_toggles($)
{
    Cardinal i, t;

    for (t = 0, i = 0; i < $num_children; i++)
	if (XtIsSubclass($children[i], xfwfToggleWidgetClass)) {
	    switch ($selectionStyle) {
	    case XfwfNoSelection:
		break;
	    case XfwfSingleSelection:
	    case XfwfOneSelection:
		XtVaSetValues($children[i], XtNon, t == $selection, NULL);
		break;
	    case XfwfMultipleSelection:
		XtVaSetValues($children[i],
			      XtNon, ($selection & (1L<<t)) != 0, NULL);
		break;
	    }
	    t++;
	}
}

@ |cvtStringToSelectionType| converts the strings `no', `none',
`single', `one', `multi' and `multiple'. Case doesn't matter.

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

@proc Boolean cvtStringToSelectionType(Display *display, XrmValuePtr args,
  Cardinal *num_args, XrmValuePtr from, XrmValuePtr to,
  XtPointer *converter_data)
{
    String s = (String) from->addr;

    if (*num_args != 0)
	XtAppErrorMsg(XtDisplayToApplicationContext(display),
		      "cvtStringToSelectionType", "wrongParameters",
		      "XtToolkitError",
		      "String to SelectionType conversion needs no arguments",
		      (String*) NULL, (Cardinal*) NULL);

    if (XmuCompareISOLatin1(s, "no") == 0)
	done(SelectionType, XfwfNoSelection);
    if (XmuCompareISOLatin1(s, "none") == 0)
	done(SelectionType, XfwfNoSelection);
    if (XmuCompareISOLatin1(s, "single") == 0)
	done(SelectionType, XfwfSingleSelection);
    if (XmuCompareISOLatin1(s, "one") == 0)
	done(SelectionType, XfwfOneSelection);
    if (XmuCompareISOLatin1(s, "multi") == 0)
	done(SelectionType, XfwfMultipleSelection);
    if (XmuCompareISOLatin1(s, "multiple") == 0)
	done(SelectionType, XfwfMultipleSelection);

    XtDisplayStringConversionWarning(display, s, XtRSelectionType);
    done(SelectionType, XfwfSingleSelection);
}

@ A converter in the opposite direction.

@proc Boolean cvtSelectionTypeToString(Display *display, XrmValuePtr args, Cardinal *num_args, XrmValuePtr from, XrmValuePtr to, XtPointer *converter_data)
{
    char s[30];

    if (*num_args != 0)
	XtAppErrorMsg(XtDisplayToApplicationContext(display),
		      "cvtStringToSelectionStyle", "wrongParameters",
		      "XtToolkitError",
		      "String to SelectionStyle conversion needs no arguments",
		      (String*) NULL, (Cardinal*) NULL);
    switch (*((SelectionType*) from->addr)) {
    case XfwfNoSelection: done(String, "none");
    case XfwfSingleSelection: done(String, "single");
    case XfwfOneSelection: done(String, "one");
    case XfwfMultipleSelection: done(String, "multiple");
    }
    XtDisplayStringConversionWarning(display, s, XtRSelectionType);
    done(String, "none");
}

@IMPORTS

@incl <stdio.h>
@incl <X11/Xmu/Converters.h>
@incl <X11/Xmu/CharSet.h>
@incl <xwToggle.h>
