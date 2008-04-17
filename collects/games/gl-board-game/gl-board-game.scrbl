#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     scheme/contract
                     games/gl-board-game
                     scheme/gui/base
                     sgl))

@title{@bold{GL Board Game}: 3-D Game Support}

@defmodule[games/gl-board-game]

@defclass[gl-board% canvas% ()]{


@defconstructor/auto-super[([min-x real?]
                            [max-x real?]
                            [min-y real?]
                            [max-y real?]
                            [lift real?]
                            [move (any/c gl-vector? . -> . any) void]
                            [who string? "this game"])]{

The @scheme[min-x], @scheme[max-x], @scheme[min-y], and @scheme[max-y]
arguments specify the dimensions of the board plane to be visible in
the window by default.

The @scheme[lift] argument specifies how many units a piece moves
vertically when the user clicks on it.

The @scheme[move] function is called when a piece is moved to a space
(possibly it's current space), when a space is clicked on, and when a
space is dragged to another space.  The @scheme[move] function is
given the information of the piece or space selected and the
coordinates to which it is moved.

The @scheme[who] argument is used for reporting an error to the user
when GL is unavailable at run time.}


@defmethod[(add-space [draw (-> any)] [info any/c]) void?]{

Adds a space to the board.  The @scheme[draw] thunk should draw the
space (using GL commands) when called.  The @scheme[info] value is
given to the @scheme[_move] function (supplied to the constructor)
when the space is selected.}


@defmethod[(add-piece [x real?][y real?][z real?]
                      [draw ([shadow? boolean?] . -> . any)] [info any/c]) void?]{

Adds a piece to the board.  The @scheme[draw] thunk should draw the
piece (using GL commands) when called.  The @scheme[info] argument is
given to the @scheme[_move] function (supplied to the constructor)
when the piece is moved.  The piece is translated by @scheme[x],
@scheme[y], and @scheme[z] before drawing.}


@defmethod[(remove-piece [info any/c]) void?]{

Removes all pieces previously added with representative @scheme[info].}


@defmethod[(add-heads-up [w real?] [h real?] [draw (-> any)] [info any/c]) void?]{

Add a ``heads-up'' display element whose size is @scheme[w] by
@scheme[h] units with the given @scheme[draw] thunk and @scheme[info]
reprsentative.}


@defmethod[(remove-heads-up [info any/c]) void?]{

Removes all ``heads-up'' displays elements previous added with
representative @scheme[info].}


@defmethod[(set-space-draw [info any/c] [draw (-> any)]) void?]{

Sets the drawing function of all spaces added with
representative @scheme[info].}


@defmethod[(set-piece-draw [info any/c]
                           [draw ([shadow? boolean?] . -> . any)]) void?]{

Sets the drawing function of all pieces added with
representative @scheme[info].}


@defmethod[(enable-piece [info any/c][can-move? any/c]) void?]{

Enables or disables moving of all pieces added with
representative @scheme[info].}


@defmethod[(enabled? [info any/c]) boolean?]{

reports whether the first piece with representative @scheme[info] is enabled.}




@defmethod*[([(get-pieces) list?]
             [(get-spaces) list?]
             [(get-heads-up) list?])]{

Returns values for various kinds of content currently on the
board. The result corresponds to @scheme[_info] values given to
@scheme[add-piece], etc.}

}
