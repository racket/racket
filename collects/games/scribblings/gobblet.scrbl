#lang scribble/doc
@(require "common.rkt")

@gametitle["Gobblet" "gobblet" "Strategy Game"]

@bold{Gobblet!} is a board game from Blue Orange Games:

@centerline{@selflink{http://www.blueorangegames.com/}}

Our 3x3 version actually corresponds to @bold{Gobblet! Jr.}, while
the 4x4 version matches @onscreen{Gobblet!}.

The Blue Orange web site provides rules for @bold{Gobblet! Jr.} and
@bold{Gobblet!}. The rules below are in our own words; see also the
Blue Orange version.

@section{Game Rules}

The 3x3 game is a generalization of tic-tac-toe:

@itemize[

 @item{The object of the game is to get three in a row of your color,
   vertically, horizontally, or diagonally.  Size doesn't matter for
   determining a winner.}

 @item{Each player (red or yellow) starts with 6 pieces: two large,
   two medium, and two small.}

 @item{On each turn, a player can either place a new piece on the
   board, or move a piece already on the board---from anywhere to
   anywhere, as long as the ``from'' and ``to'' are different.}

 @item{A piece can be placed (or moved to) an empty space, or it can
   be placed/moved on top of a smaller piece already on the board,
   ``gobbling'' the smaller piece.  The smaller piece does not have to
   be an opponent's piece, and the smaller piece may itself have
   gobbled another piece previously.}

 @item{Only visible pieces can be moved, and only visible pieces count
   toward winning.  Gobbled pieces stay on the board, however, and
   when a piece is moved, any piece that it gobbled stays put and
   becomes visible.}

 @item{If moving a piece exposes a winning sequence for the opponent, and
   if the destination for the move does not cover up one of the other
   pieces in the sequence, then the opponent wins---even if the move
   makes a winning sequence for the moving player.}

 @item{Technically, if a player touches a piece, then the piece must
   be moved on that turn.  In other words, you're not allowed to peek
   under a piece to remind yourself whether it gobbled anything.  If
   the piece can't be moved, the player forfeits.  This particular
   rule is not enforced by our version --- in part because our version
   supports a rewind button, which is also not in the official game.}

]

The 4x4 game has a few changes:

@itemize[

 @item{The object of the game is to get four in a row of your color.}

 @item{Each player (red or yellow) starts with 12 pieces: three large,
   three medium-large, three medium-small, and three small.}

 @item{Each player's pieces are initially arranged into three stacks
   off the board, and only visible pieces can be moved onto the board.
   The initial stacks prevent playing a smaller piece before a
   corresponding larger piece.}

 @item{When a piece is moved from off-board onto the board, it must be
   moved to either (1) an empty space, or (2) a space to gobble an
   opponent's piece that is part of three in a row (for the opponent).
   In other words, a new piece can gobble only an opponent's piece,
   and only to prevent an immediate win on the opponent's next turn.
   These restrictions do not apply when a piece that is already on the
   board is moved.}

]

@section{Controls}

Click and drag pieces in the obvious way to take a turn.  The shadow
under a piece shows where it will land when you drop it.

Use the arrow keys on your keyboard to rotate the board.  Use the
@onscreen{-} and @onscreen{=} keys to zoom in and out.  Use
@onscreen{_} and @onscreen{+} to make the game smaller and larger.
(Changing the size adjusts perspective in a slightly different way
than zooming.)  Depending on how keyboard focus works on your machine,
you may have to click the board area to make these controls work.

The button labeled @onscreen{<} at the bottom of the window rewinds
the game by one turn.  The button labeled @onscreen{>} re-plays one
turn in a rewound game.  An alternate move can be made at any point in
a rewound game, replacing the old game from that point on.

@section{Auto-Play}

Turn on a computer player at any time by checking the
@onscreen{Auto-Play Red} or @onscreen{Auto-Play Yellow} checkbox.  If
you rewind the game, you can choose an alternate move for yourself or
for the auto-player to find out what would have happened.  The
auto-player is not always deterministic, so replying the same move
might lead to a different result.  You can disable an auto-player at
any point by unchecking the corresponding
@onscreen{Auto-Play"}checkbox.

Important: In the 3x3 game, you @emph{cannot} win as yellow against
the smart auto-player (if the auto-player is allowed to play red from
the start of the game).  In other words, red has a forced win in the
3x3 game, and the smart auto-player knows the path to victory.  You
might have a chance to beat the red player in the default mode,
though, which is represented by the @onscreen{Ok} choice (instead of
@onscreen{Smart}) in the @onscreen{Auto-Play Options} dialog.

Configure the auto-player by clicking the @onscreen{Auto-Play Options}
button.  Currently, there's no difference between @onscreen{Smart} and
@onscreen{Ok} in the 4x4 game.
