# Version 2.1 for FWF V4.0
#

@class XfwfLabel (XfwfBoard) @file=xwLabel

@ The Label class has the capability to display one or more lines of
text in a single font. Otherwise it is the same as the Board class.
The text can be left, right or center justified and it can be centered
vertically or put against the top or the bottom of the widget. There
is also a resource to set tab stops.

The text is `grayed out' when the widget becomes insensitive
(resource: |sensitive|), even though a Label widget has no actions of
its own.

There are two ways of highlighting portions of the label: reversing
the colors or changing the foreground. Both methods can be combined,
though the result when both highlighting methods are applied to the
same part of the text is undefined.


@public

@ The text is a single string, which may contain embedded newlines.
There is no provision for changing fonts in the middle of a text.

	@var String label = NULL

@ A tablist can be provided for tabbing to particular columns
within the label.

	@var String tablist = NULL

@ The text is drawn in the font which is given as the |font| resource.

	@var <FontStruct> XFontStruct *font = <String> XtDefaultFont

@ The |xfont| resource overrides font with an Xft font, if available.

	@var <void> void* xfont = <Pointer> NULL

@ Instead of a label a |pixmap| may be displayed if |label| is NULL.

	@var Pixmap pixmap = 0
	@var Pixmap maskmap = 0

@ The foreground color is the color used to draw the
text.

	@var Pixel foreground = <String> XtDefaultForeground
	@var Pixel text_bg = <Pointer> NULL

@ The text can be aligned in the widget in nine ways: left, right or
center, combined with top, center or bottom. Symbolic constants
|XfwfTop|, |XfwfBottom|, |XfwfLeft| and |XfwfRight| can be added together to
get the desired alignment.  The alignment is actually a four-bit
number made up of two parts of 2 bits added together: 1 is left, 2 is
right, 0 is center, 4 is top, 8 is bottom, 0 is vertical center. Thus
5 (= 1 + 4) means top left and 2 (= 2 + 0) means center right. For
easier specification, there is also a converter from strings, that
accepts string like `top left' or `center right'.

	@var Alignment alignment = 0

@ The |topmargin| is only used when the text is not centered. It gives
the number of pixels between the frame and the top of the text.

	@var Dimension topMargin = 2

@ The |bottomMargin| is only used to compute the preferred size of the
button in case |shrinkToFit = True|.

	@var Dimension bottomMargin = 2

@ The |leftMargin| is only used when the text is not centered. It
gives the number of pixels between the frame and the left edge of the
text, and if possible also between the frame and the right edge of the
text.

	@var Dimension leftMargin = 2

@ The |rightMargin| is only used to compute the preferred size of the
button in case |shrinkToFit = True|.

	@var Dimension rightMargin = 2

@ Buttons will normally not occupy the full area of their parents.
Most likely they will be a fixed size or a size depending on the
label. By setting the |shrinkToFit| resource to True, the width and
height are recomputed with every new label.

	@var Boolean shrinkToFit = False

@ It is possible to set a part of the label apart by drawing it in
reverse. The |rvStart| resource gives the index of the first
character to draw in reverse video.

	@var int rvStart = 0

@ The |rvLength| resource contains the number of characters to
draw in reverse video.

	@var int rvLength = 0

@ A label normally needs no keyboard interface, therefore traversal is
turned off.

	@var traversalOn = False

        @var Boolean drawgray = FALSE


@private

@ For faster drawing, the number of lines in the text is stored in a
private variable by the |set_values| and |initialize| methods.

	@var int nlines

@ The tablist is converted from string format to a list of int's for speed.

	@var int *tabs

@ For drawing the text, this GC is used.

	@var GC gc

@ For graying out the text, another GC is used.

	@var GC graygc

@ When the |shrinkToFit| resource is set, we need the minimum area
necessary for the complete label to be visible. |label_width| and
|label_height| include the size of |margin|.

	@var Dimension label_width
	@var Dimension label_height

@ For a pixmap label I need the |label_depth|.

	@var unsigned int label_depth
	@var unsigned int mask_depth

@methods

@ The new method |set_label| makes a copy of the string that is passed
in, counts the number of lines and also draws the new label. This
could have been done in |set_values|, but it is expected that
subclasses will redraw the label frequently, so a more efficient way
is provided.

Note that this method does not resize the widget in case |shrinkToFit|
is set.

@proc set_label($, String newlabel)
{
    Position x, y;
    int w, h;

    XtFree($label);
    $label = XtNewString(newlabel);
    count_lines($);
    if (XtIsRealized($)) {
	$compute_inside($, &x, &y, &w, &h);
	XClearArea(XtDisplay($), XtWindow($), x, y, max(w, 0), max(h, 0), True);
	/* $_expose($, NULL, NULL); */
    }
}


@ The |set_values| method checks the |background| resource, because is
is used in the GC |graygc|. When the text or the font change, the
private variables |nlines|, |label_height| and |label_width| are
updated.

|need_count| is set to |True| if the size of the label changes.
|need_count| implies |need_redisplay|.

@proc set_values
{
    Boolean need_redisplay = False, need_count = False;
    Position x, y;

    if ($background_pixel != $old$background_pixel)
	if ($graygc) make_graygc($);

    if ($tablist != $old$tablist) {
	XtFree((String) $old$tabs);
	$tabs = XfwfTablist2Tabs($tablist);
	if ($label != NULL) need_count = True;
    }

    if (($font != $old$font)
	|| ($xfont != $old$xfont)) {
	make_gc($);
	if ($label != NULL) need_count = True;
    }
    if ($foreground != $old$foreground
	|| $background_pixel != $old$background_pixel) {
	make_gc($);
	if ($label != NULL || $pixmap != 0) need_redisplay = True;
    }
    if ($topMargin != $old$topMargin
	|| $bottomMargin != $old$bottomMargin
	|| $leftMargin != $old$leftMargin
	|| $rightMargin != $old$rightMargin
	|| $alignment != $old$alignment)
	need_count = True;

    if (($sensitive != $old$sensitive)
        || ($drawgray != $old$drawgray))
	if ($label != NULL || $pixmap != 0) need_redisplay = True;

    if ($label != $old$label || $pixmap != $old$pixmap) {
	XtFree($old$label);
	$label = XtNewString($label);
	need_count = True;
    }
    if (need_count) {
	count_lines($);
	need_redisplay = True;
    }
    if (need_count && $shrinkToFit) {
        int w, h, wd, ht;
	$compute_inside($, &x, &y, &w, &h);
	wd = $label_width + $width - w;
	ht = $label_height + $height - h;
	if (wd != $width || ht != $height) {
	    $set_abs_location($, CWWidth | CWHeight, 0, 0, max(1, wd), max(1, ht));
	    need_redisplay = True;
	}
    }

    return need_redisplay;
}

@ The |initialize| methods creates the first GC's and initializes the
private variables. It sets the GC's to |NULL| and calls two utility
routines to actually create them.

@proc initialize
{
    Position x, y;
    int w, h, wd, ht;

    if ($label) $label = XtNewString($label);
    count_lines($);
    $gc = NULL;
    $graygc = NULL;
    $tabs = XfwfTablist2Tabs($tablist);
    if (!$text_bg)
      $text_bg = $background_pixel;
    if ($shrinkToFit) {
	$compute_inside($, &x, &y, &w, &h);
	wd = $label_width + $width - w;
	ht = $label_height + $height - h;
	$set_abs_location($, CWWidth | CWHeight, 0, 0, max(1, wd), max(1, ht));
    }
}

@proc destroy
{
  if ($gc) XtReleaseGC($, $gc); $gc = NULL;
  if ($graygc) XtReleaseGC($, $graygc); $graygc = NULL;
}

@proc realize
{
  #realize($, mask, attributes);
  make_gc($);
  /* make_graygc($); - now on demand */
}

@ The |expose| method is responsible for drawing the text. The text is
put in the position given in |alignment|. The text is always kept
within the frame. If necessary, the text is clipped. The routine ends
by calling the |expose| method from the superclass, which is
responsible for drawing the frame.

@def draw_line(dpy, win, from, to, reg) =
    do {
	w1 = XfwfTextWidth(dpy, $font, (wxExtFont)$xfont, $label + from, to - from, $tabs);
	if ($alignment & XfwfLeft)
	    x = rect.x;
	else if ($alignment & XfwfRight)
	    x = rect.x + rect.width - w1;
	else
	    x = rect.x + (rect.width - w1)/2;
	if (w1) {
	  int grayed;
	  grayed = ((!$sensitive || $drawgray) && wx_enough_colors(XtScreen($)));
	  XfwfDrawImageString(dpy, win, 
			      ($xfont
			       ? $gc
			       : (grayed
				  ? $graygc 
				  : $gc)), 
			      x, y, $label + from,
			      to - from, $tabs, $font,
			      (wxExtFont)$xfont, !grayed, reg);
	}
    } while (0)

@proc _expose
{
    Region reg;
    XRectangle rect;
    int baseline;
    int w1;
    int x, y, i, j;

    if (! XtIsRealized($)) return;

    if (!$sensitive || $drawgray)
      if (!$graygc) make_graygc($);

    #_expose($, event, region);
    reg = NULL;
    if ($label != NULL || $pixmap != 0) {
	int w, h;
	$compute_inside($, &rect.x, &rect.y, &w, &h);
	rect.x += $leftMargin;  w -= $leftMargin + $rightMargin;
	rect.y += $topMargin;  h -= $topMargin + $bottomMargin;
	rect.width = max(0, w);
	rect.height = max(0, h);
	reg = XCreateRegion();
	XUnionRectWithRegion(&rect, reg, reg);
	if (region != NULL) XIntersectRegion(region, reg, reg);
	XSetRegion(XtDisplay($), $gc, reg);
    }
    if ($label != NULL) {
        int ascent;
	ascent = wx_ASCENT($font, ((wxExtFont)$xfont));
	baseline = ascent + wx_DESCENT($font, ((wxExtFont)$xfont));
	if ($alignment & XfwfTop)
	    y = rect.y + ascent;
	else if ($alignment & XfwfBottom)
	    y = rect.y + rect.height - $nlines * baseline + ascent;
	else
	    y = rect.y + (rect.height - $nlines * baseline)/2 + ascent;
	for (i = 0, j = 0; $label[i]; i++) {
	    if ($label[i] == '\n') {
		draw_line(XtDisplay($), XtWindow($), j, i, reg);
		j = i + 1;
		y += baseline;
	    }
	}
	draw_line(XtDisplay($), XtWindow($), j, i, reg);

	/* Gray out if not sensitive */
	if (! $sensitive || $drawgray) {
	  if (!wx_enough_colors(XtScreen($))) {
	    XSetRegion(XtDisplay($), $graygc, reg);
	    XFillRectangle(XtDisplay($), XtWindow($), $graygc, rect.x,
			   rect.y, rect.width, rect.height);
	    XSetClipMask(XtDisplay($), $graygc, None);
	  }
	}
    } else if ($pixmap != 0) {
	Dimension width = $label_width - $leftMargin - $rightMargin;
	Dimension height = $label_height - $topMargin - $bottomMargin;
	if ($alignment & XfwfTop)
	    y = rect.y;
	else if ($alignment & XfwfBottom)
	    y = rect.y + rect.height - height;
	else
	    y = rect.y + (rect.height - height)/2;
	if ($alignment & XfwfLeft)
	    x = rect.x;
	else if ($alignment & XfwfRight)
	    x = rect.x + rect.width - width;
	else
	    x = rect.x + (rect.width - width)/2;

	wxDrawBitmapLabel(XtDisplay($), 
			  $pixmap, $maskmap, 
			  XtWindow($), $gc,
			  x, y, width, height, 
			  $label_depth, $mask_depth,
			  reg, 
			  (! $sensitive || $drawgray) ? $graygc : 0,
			  $background_pixel);
    }

    if ($label != NULL || $pixmap != 0) {
      XSetClipMask(XtDisplay($), $gc, None);
    }
    if (reg) XDestroyRegion(reg);
}



@utilities

@ The |make_gc| routine creates the GCs for the normal and highlighted
text.

@proc make_gc($)
{
    XtGCMask mask, fnt = 0;
    XGCValues values;

    if ($gc != NULL) XtReleaseGC($, $gc);
    values.background = $text_bg;
    if (!$xfont) {
      values.foreground = $foreground;
      values.font = $font->fid;
      fnt = GCFont;
    } else {
      if ($pixmap)
	values.foreground = $foreground;
      else
	values.foreground = values.background;
    }
    mask = fnt | GCBackground | GCForeground;
    $gc = XtGetGC($, mask, &values);
}

@ The |make_graygc| routine creates a GC for graying out the text. It
contains a stipple in the background color, that will be applied over
the text.

@proc make_graygc($)
{
    XtGCMask mask;
    XGCValues values;

    if ($graygc != NULL) XtReleaseGC($, $graygc);

    if (($pixmap != 0) || !wx_enough_colors(XtScreen($))) {
      /* A GC to draw over bitmaps/text: */
      values.foreground = $text_bg;
      values.stipple = GetGray($);
      values.fill_style = FillStippled;
      mask = GCForeground | GCStipple | GCFillStyle;
    } else {
      /* A GC for drawing gray text: */
      static Pixel color;
      values.background = $text_bg;
      $darker_color($, $text_bg, &color);
      values.foreground = color;
      mask = GCBackground | GCForeground;
      if ($font) {
	values.font = $font->fid;
	mask |= GCFont;
      }
    }
 
    $graygc = XtGetGC($, mask, &values);
}

@ The funtion |count_lines| computes the correct values for the
private variables |nlines|, |label_width| and |label_height|.

@proc count_lines($)
{
    String p, s;
    int w;

    $nlines = 0;
    $label_width = 0; $label_height = 0; $label_depth = 0;
    if ($label) {
	for (p = $label, $nlines = 1, s = $label; *s; s++) {
	    if (*s == '\n') {
		$nlines++;
		w = XfwfTextWidth(XtDisplay($), $font, (wxExtFont)$xfont, p, s - p, $tabs);
		p = s + 1;
		if (w > $label_width) $label_width = w;
	    }
	}
	w = XfwfTextWidth(XtDisplay($), $font, (wxExtFont)$xfont, p, s - p, $tabs);
	if (w > $label_width) $label_width = w;
	$label_height = $nlines * (wx_ASCENT($font, ((wxExtFont)$xfont)) + wx_DESCENT($font, ((wxExtFont)$xfont)));
    } else if ($pixmap) {
	Window        root;
	int           x, y;
	unsigned int  width, height, bw, depth;
	XGetGeometry(XtDisplay($), $pixmap, &root,
	             &x, &y, &width, &height, &bw, &depth);
	$label_width  = (Dimension)width;
	$label_height = (Dimension)height;
	$label_depth  = depth;
	if ($maskmap) {
	  XGetGeometry(XtDisplay($), $maskmap, &root,
		       &x, &y, &width, &height, &bw, &depth);
	  $mask_depth  = depth;
	}
    }
    /* add border */
    $label_width += $leftMargin + $rightMargin;
    $label_height += $topMargin + $bottomMargin;
}

@exports

@imports


@incl <stdio.h>
@incl <xwTabString.h>
