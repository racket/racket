#lang scribble/doc
@(require "common.rkt")

@gametitle["Minesweeper" "mines" "Logic Game"]

Remove all the tiles that have no bomb underneath. When you remove
such a tile, a number appears that indicates how many of the
surrounding squares (up to 8) have a bomb; a blank means zero bombs,
and the game automatically uncovers all surrounding tiles in that
case.

Right- or Control-click to flag a tile that you think has a bomb, so
that you cannot accidentally uncover it. Right- or Control-click again
to remove the flag.

You don't have to use flags. When all of the non-bomb tiles are
removed, the game is over, and the clock stops.
