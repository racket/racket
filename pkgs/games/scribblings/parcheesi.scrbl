#lang scribble/doc
@(require "common.rkt")

@gametitle["Parcheesi" "parcheesi" "Board Game"]

@onscreen{Parcheesi} is a race game for four players.  The goal is for
each player to move their pieces from the starting position (the
circles in the corners) to the home square (in the center of the
board), passing a nearly complete loop around the board in the
counter-clockwise direction and then heads up towards the main row.
For example, the green player enters from the bottom right, travels
around the board on the light blue squares, passing each of the
corners, until it reaches the middle of the bottom of the board, where
it turns off the light blue squares and heads into the central region.

On each turn, the player rolls two dice and advances the pawn, based
on the die rolls.  Typically the players may move a pawn for each die.
The pawn moves by the number of pips showing on the die and all of the
dice must be used to complete a turn.

There are some exceptions, however:

@itemize[

  @item{You must roll a 5 (either directly or via summing) to enter from
    the start area to the main ring.}

  @item{If two pieces of the same color occupy a square, no pieces may
    pass that square.}

  @item{If an opponent's piece lands on your piece, you piece is
    returned to the starting area and the opponent receives a bonus of
    20 (which is treated just as if they had rolled a 20 on the
    dice).}

  @item{If your piece makes it home (and it must do so by exact count) you
    get a bonus of 10, to be used as an additional die roll.}

]

These rules induce a number of unexpected corner cases, but the GUI
only lets you make legal moves.  Watch the space along the bottom of
the board for reasons why a move is illegal or why you have not used
all of your die rolls.

The automated players are:

@itemize[

 @item{@onscreen{Reckless Renee}, who tries to maximize the chances
   that someone else bops her.}

 @item{@onscreen{Polite Polly}, who tries to minimize the distance her
   pawns move. (``No, after @emph{you}. I insist.'')}

 @item{@onscreen{Amazing Grace}, who tries to minimize the chance she
   gets bopped while moving as far as possible.}

]

