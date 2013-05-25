#lang scribble/doc
@(require "common.rkt")

@defclass/title[cursor% object% ()]{

A cursor is a small icon that indicates the location of the mouse
 pointer.  The bitmap image typically indicates the current mode or
 meaning of a mouse click at its current location.

A cursor is assigned to each window (or the window may use its
 parent's cursor; see @method[window<%> set-cursor] for more
 information), and the pointer image is changed to match the window's
 cursor when the pointer is moved over the window. Each cursor object
 may be assigned to many windows.


@defconstructor*/make[(([image (is-a?/c bitmap%)]
                        [mask (is-a?/c bitmap%)]
                        [hot-spot-x (integer-in 0 15) 0]
                        [hot-spot-y (integer-in 0 15) 0])
                       ([id (or/c 'arrow 'bullseye 'cross 'hand 'ibeam 'watch 'blank 
                                  'size-n/s 'size-e/w 'size-ne/sw 'size-nw/se)]))]{

The first case creates a cursor using an image bitmap and a mask
bitmap. Both bitmaps must have depth 1 and size 16 by 16
pixels. The @racket[hot-spot-x] and @racket[hot-spot-y] arguments
determine the focus point of the cursor within the cursor image,
relative to its top-left corner.

The second case creates a cursor using a stock cursor, specified
as one of the following:

@itemize[

 @item{@racket['arrow] --- the default cursor}

 @item{@racket['bullseye] --- concentric circles}

 @item{@racket['cross] --- a crosshair}

 @item{@racket['hand] --- an open hand}

 @item{@racket['ibeam] --- a vertical line, indicating that clicks
  control a text-selection caret}

 @item{@racket['watch] --- a watch or hourglass, indicating that
  the user must wait for a computation to complete}

 @item{@racket['arrow+watch] --- the default cursor with a watch or
  hourglass, indicating that some computation is in progress, but the
  cursor can still be used}

 @item{@racket['blank] --- invisible}

 @item{@racket['size-e/w] --- arrows left and right}

 @item{@racket['size-n/s] --- arrows up and down}

 @item{@racket['size-ne/sw] --- arrows up-right and down-left}

 @item{@racket['size-nw/se] --- arrows up-left and down-right}

]

If the cursor is created successfully, @method[cursor% ok?]
returns @racket[#t], otherwise the cursor object cannot be
assigned to a window.

}

@defmethod[(ok?)
           boolean?]{

Returns @racket[#t] if the cursor is can be assigned to a window,
 @racket[#f] otherwise.

}}

