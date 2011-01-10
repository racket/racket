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

Your score increases for each ball removed from the board. In general,
when you remove a blob, you get as many points as the square of the number
of cells the blob occupied, so removing bigger blobs is better. Also,
there is a penalty of 10 points for each colored cell left behind on the board,
so try to clear out the entire board.

Click the @onscreen{New Game} button to play again.
