#lang scribble/doc
@(require scribble/manual
          (for-label racket/base
                     racket/contract
                     games/gl-board-game
                     racket/gui/base
                     sgl))

@title{GL Board Game: 3-D Game Support}

@defmodule[games/gl-board-game]

@defclass[gl-board% canvas% ()]{


@defconstructor/auto-super[([min-x real?]
                            [max-x real?]
                            [min-y real?]
                            [max-y real?]
                            [lift real?]
                            [move (any/c gl-vector? . -> . any) void]
                            [who string? "this game"])]{

The @racket[min-x], @racket[max-x], @racket[min-y], and @racket[max-y]
arguments specify the dimensions of the board plane to be visible in
the window by default.

The @racket[lift] argument specifies how many units a piece moves
vertically when the user clicks on it.

The @racket[move] function is called when a piece is moved to a space
(possibly it's current space), when a space is clicked on, and when a
space is dragged to another space.  The @racket[move] function is
given the information of the piece or space selected and the
coordinates to which it is moved.

The @racket[who] argument is used for reporting an error to the user
when GL is unavailable at run time.}


@defmethod[(add-space [draw (-> any)] [info any/c]) void?]{

Adds a space to the board.  The @racket[draw] thunk should draw the
space (using GL commands) when called.  The @racket[info] value is
given to the @racket[_move] function (supplied to the constructor)
when the space is selected.}


@defmethod[(add-piece [x real?][y real?][z real?]
                      [draw ([shadow? boolean?] . -> . any)] [info any/c]) void?]{

Adds a piece to the board.  The @racket[draw] thunk should draw the
piece (using GL commands) when called.  The @racket[info] argument is
given to the @racket[_move] function (supplied to the constructor)
when the piece is moved.  The piece is translated by @racket[x],
@racket[y], and @racket[z] before drawing.}


@defmethod[(remove-piece [info any/c]) void?]{

Removes all pieces previously added with representative @racket[info].}


@defmethod[(add-heads-up [w real?] [h real?] [draw (-> any)] [info any/c]) void?]{

Add a ``heads-up'' display element whose size is @racket[w] by
@racket[h] units with the given @racket[draw] thunk and @racket[info]
reprsentative.}


@defmethod[(remove-heads-up [info any/c]) void?]{

Removes all ``heads-up'' displays elements previous added with
representative @racket[info].}


@defmethod[(set-space-draw [info any/c] [draw (-> any)]) void?]{

Sets the drawing function of all spaces added with
representative @racket[info].}


@defmethod[(set-piece-draw [info any/c]
                           [draw ([shadow? boolean?] . -> . any)]) void?]{

Sets the drawing function of all pieces added with
representative @racket[info].}


@defmethod[(enable-piece [info any/c][can-move? any/c]) void?]{

Enables or disables moving of all pieces added with
representative @racket[info].}


@defmethod[(enabled? [info any/c]) boolean?]{

reports whether the first piece with representative @racket[info] is enabled.}




@defmethod*[([(get-pieces) list?]
             [(get-spaces) list?]
             [(get-heads-up) list?])]{

Returns values for various kinds of content currently on the
board. The result corresponds to @racket[_info] values given to
@racket[add-piece], etc.}

}
