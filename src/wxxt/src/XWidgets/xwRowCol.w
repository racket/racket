


@CLASS XfwfRowCol (XfwfBoard)  @file=xwRowCol

@ The RowCol widget forces all its children into rows and columns. The
chion is
ignored. Resources determine how many rows or columns their should be
(or as many as will fit) and if the children should be layed out in
rows or in columns. In both methods, the children are placed on a
grid, the size of which is determined by the width (height) of the
widest (tallest) child.

The children can be aligned in several ways: they can be placed in the
center of their grid cell or against the edges. This is controlled by
a resource of type |Alignment|.

@PUBLIC

@ The child widgets can be layed out in rows (left to right) or in
columns (top to bottom). The resource |storeByRow| can be |True| or
|False|. |True| means children are added to the right of the previous
one until the row is full, like words are added to a text. |False|
means children are added below the previous one, until the column is
full.

@var Boolean storeByRow = True

@ The number of rows can be set with |rows|, or the number of columns
can be set with |columns|. If both are non-zero, |rows| will be
ignored.  If |rows| is zero, there will be as many rows as needed. If
|columns| is zero, there will be as many columns as needed. However,
if both are zero, there will be as many columns as will fit in the
current width of the RowCol widget. By default, both |rows| and
|columns| are zero.

@var int rows = 0
@var int columns = 0

@ The area of the RowCol widget is partitioned into rectangular cells
(a grid). The cells are just large enough to contain the widest and
the tallest of the children. Within the cell, the children can be put
in the top left corner (the default) or against one of the edges or in
the center.  This is set with the |alignment| resource. The type
|Alignment| is defined in the ancestor class `Common'.

@var Alignment alignment = XfwfTopLeft

@ The resource |shrinkToFit| determines how the size of the RowCol
widget itself is computed. When it is |False| (default), the
|location| resource is used to compute the widget's preferred size.
When the value is |True|, the preferred size is computed fromthe total
width and height of the children. For example, when the widest child
has a width of |w| and |columns > 0|, the preferred width will be
|columns * w +| frame width. A similar computation is used for the
height. If |columns > 0|, only the height is computed this way. If
|columns = 0, rows > 0|, only the width is computed.

@var Boolean shrinkToFit = False

@ The inherited resource |frameType| is given a default value of
|XfwfSunken|, instead of the inherited default |XfwfRaised|. The frame
width is set to a default of 2 pixels, instead of 0.

@var frameType = XfwfSunken
@var frameWidth = 2

@PRIVATE

@ The width of the widest and the height of the tallest child are kept
in private variables, for quicker access.

@var Dimension max_width
@var Dimension max_height

@METHODS

@ If a child becomes managed or unmanaged, the RowCol widget
recomputes the positions of all managed children. That is done by a
method |layout|. In the process, the widget may ask its parent for a
different size if |shrinkToFit = True|.

@proc change_managed
{
    $layout($, $shrinkToFit, True);
}

@ Do the same for inserting a child.

@proc insert_child
{
    #insert_child(child);
    if ($shrinkToFit)
	$layout($, $shrinkToFit, False);
}


@ The |layout| function is responsible for moving the children to their
positions in the grid. It is called from |change_managed|,
|geometry_manager| and |resize|.

The function first computes the maximum width and height of all the
children, including their borders. Then it computes the number of rows
and columns. All children are moved to their proper positions with the
help of a utility function |align_child|, which aligns a child widget
to the grid.

If |shrink = True|, the RowCol widget also asks its parent for a new
width and/or height, depending on the resulting layout.

If |align = False|, the RowCol widget may resize itself, but does not
position it's children.

@proc layout($, int shrink, Boolean align)
{
    int nrows, ncols, i, nchild, n;
    Position left, top, x, y;
    int width, height, w, h;
    Widget child;

    nchild = 0;
    $max_width = 0;
    $max_height = 0;
    for (i = 0; i < $num_children; i++) {
        child = $children[i];
        if (align && ! XtIsManaged(child)) continue;
        nchild++;
        $max_width = max($max_width, $child$width + 2*$child$border_width);
        $max_height = max($max_height, $child$height + 2*$child$border_width);
    }

    $compute_inside($, &left, &top, &width, &height);
    width = max(0, width);

    if ($columns != 0) {
        ncols = $columns;
        nrows = (nchild + ncols - 1)/ncols;
    } else if ($rows != 0) {
        nrows = $rows;
        ncols = (nchild + nrows - 1)/nrows;
    } else {
        ncols = $max_width != 0 ? width/$max_width : 1;
        if (ncols == 0) ncols = 1;
        nrows = (nchild + ncols - 1)/ncols;
    }

    x = left;
    y = top;
    n = 0;
    if ($storeByRow) {
        for (i = 0; i < $num_children; i++) {
            child = $children[i];
            if (align && ! XtIsManaged(child)) continue;
	    if (align)
		align_child(child, x, y, $max_width, $max_height, $alignment);
            n++;
            if (n == ncols) {
                n = 0;
                x = left;
                y += $max_height;
            } else
                x += $max_width;
        }
    } else {
        for (i = 0; i < $num_children; i++) {
            child = $children[i];
            if (align && ! XtIsManaged(child)) continue;
	    if (align)
		align_child(child, x, y, $max_width, $max_height, $alignment);
            n++;
            if (n == nrows) {
                n = 0;
                y = top;
                x += $max_width;
            } else
                y += $max_height;
        }
    }

    if (shrink) {
      w = 2*left + ncols * $max_width;
      h = 2*top + nrows * $max_height;
      XtVaSetValues($, XtNwidth, max(1, w), XtNheight, max(1, h), NULL);
    }
}

@ When a child wants to change its size or border width, it calls its
parent's |geometry_manager| method (through a call to
|XtMakeGeometryRequest| or |XtMakeResizeRequest|.) The RowCol widget
always grants size changes to its children. The size change is carried
out immediately and a new layout is computed. If a child requests a
change of position, the request is denied. A request for a change in
stacking order is ignored.

@proc geometry_manager
{
    Dimension newwd, newht, newbd;

    if (request->request_mode & (CWX | CWY)) return XtGeometryNo;
    if (request->request_mode & XtCWQueryOnly) return XtGeometryYes;

    newwd = request->request_mode & CWWidth ? request->width : $child$width;
    newht = request->request_mode & CWHeight ? request->height : $child$height;
    newbd = request->request_mode & CWBorderWidth
        ? request->border_width : $child$border_width;

    if (newwd == $child$width && newht == $child$height
        && newbd == $child$border_width) return XtGeometryNo;

    XtResizeWidget(child, newwd, newht, newbd);
    $layout($, $shrinkToFit, True);
    return XtGeometryDone;
}

@ The |resize| method is called when the widget is resized. If the
|rows| and |columns| resources are both zero, the children will have
to be be re-aligned. In this case, there is no sense in asking the
parent for a new size, so |layout| is passed a value of |False|.

@proc resize
{
    if ($rows == 0 && $columns == 0) $layout($, False, True);
}

@ The initialize method sets the private variables, in this case only
|max_width| and |max_height|. There is no need to check if the resources
have sensible values.

@proc initialize
{
    $max_width = $max_height = 0;
}

@ The RowCol widget needs to recompute the positions of the children
when one of the resources changes. When the layout changes, the widget
also needs to be redrawn, of course.  The private variables are not
dependent on the resources, so they don't need recomputing.

@proc set_values
{
    Boolean need_layout = False;
    Boolean need_redisplay = False;

    if ($old$storeByRow != $storeByRow) need_layout = True;
    if ($old$rows != $rows) need_layout = True;
    if ($old$columns != $columns) need_layout = True;
    if ($old$alignment != $alignment) need_layout = True;
    if ($old$shrinkToFit != $shrinkToFit) need_layout = True;
    if (need_layout) {
        $layout($, $shrinkToFit, True);
        need_redisplay = True;
    }
    return need_redisplay;
}

@UTILITIES

@ |align_child| puts a widget in the proper position in the cell given by
|cx|, |cy|, |width| and |height|.

@proc align_child($, int cx, int cy, int width, int height, Alignment alignment)
{
    Position x, y;

    if (alignment & XfwfLeft) x = cx;
    else if (alignment & XfwfRight) x = cx + width - $width;
    else x = cx + (width - $width) / 2;
    if (alignment & XfwfTop) y = cy;
    else if (alignment & XfwfBottom) y = cy + height - $height;
    else y = cy + (height - $height) / 2;
    XtMoveWidget($, x, y);
}

