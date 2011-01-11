#lang scribble/doc
@(require "common.ss")

@gametitle["Same" "same" "Dot-Removing Game"]

The object of @game{Same} is to score points by removing blobs from the
board.  To remove a blob, click on it.  As long the blob is not just
a simple circle, it will disappear.  After the blob disappears,
the remaining pieces of the board shift around, breaking up blobs into
new blobs as pieces of the old blobs fall down to fill in the empty space.
If an entire column is wiped out, all of the blobs from the
right will slide left to take up the empty column's space.

Your score increases for each ball removed from the board, in two ways.
First, when you remove a blob, you get as many points as the square of the number
of cells the blob occupied, so removing bigger blobs is better. Second, if there
are fewer than 20 cells occupied on the board, you get a bonus.
Specifically if you have 19 cells left, you
get a 50 point bonus, 18 cells left yields a 100 point bonus, 
17 cells a 150 point bonus etc., and if there
are no cells left, you get a 1000 point bonus.

Click the @onscreen{New Game} button to play again.
