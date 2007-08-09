#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[cursor% object% ()]{

A cursor is a small icon that indicates the location of the mouse
 pointer.  The bitmap image typically indicates the current mode or
 meaning of a mouse click at its current location.

A cursor is assigned to each window (or the window may use its parent's
 cursor; see
@method[window<%> set-cursor] for more information), and the pointer image is changed to match the
 window's cursor when the pointer is moved over the window. Each
 cursor object may be assigned to many windows.




@defconstructor*/make[([[image (is-a/c bitmap%)]
                        [mask (is-a/c bitmap%)]
                        [hot-spot-x (integer-in 0 15) 0]
                        [hot-spot-y (integer-in 0 15) 0]]
                       [[id (symbols/c size-nw/se size-ne/sw size-e/w size-n/s blank watch ibeam hand cross bullseye arrow)]])]{
First case:


Creates a cursor using an image bitmap and a mask bitmap. Both
 bitmaps must have depth 1 and size 16 by 16 pixels.

The @scheme[hot-spot-x] and @scheme[hot-spot-y] arguments determine the
 focus point of the cursor within the cursor image, relative to its
 top-left corner.

If the cursor is created successfully,
@method[cursor% ok?] returns @scheme[#t], otherwise the cursor object cannot be assigned to a
 window.



Second case:


Creates a cursor using a stock cursor, specified as one of the following:
@itemize{

 @item{@scheme['arrow] --- the default cursor}

 @item{@scheme['bullseye] --- concentric circles}

 @item{@scheme['cross] --- a crosshair}

 @item{@scheme['hand] --- an open hand}

 @item{@scheme['ibeam] --- a vertical line, indicating that clicks
  control a text-selection caret}

 @item{@scheme['watch] --- a watch or hourglass, indicating that
  the user must wait for a computation to complete}

 @item{@scheme['arrow+watch] --- the default cursor with a watch or
  hourglass, indicating that some computation is in progress, but the
  cursor can still be used}

 @item{@scheme['blank] --- invisible}

 @item{@scheme['size-e/w] --- arrows left and right}

 @item{@scheme['size-n/s] --- arrows up and down}

 @item{@scheme['size-ne/sw] --- arrows up-right and down-left}

 @item{@scheme['size-nw/se] --- arrows up-left and down-right}

}



}

@defmethod[(ok?)
           boolean?]{
@spec{

Returns @scheme[#t] if the cursor is can be assigned to a window,
 @scheme[#f] otherwise.

}
@impl{




}}}

