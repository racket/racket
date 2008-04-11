#lang scribble/doc
@(require "common.ss")

@gametitle["Same" "same" "Dot-Removing Game"]

The object of @game{Same} is to score points by removing dots from the
board.  To remove a dot, click on it.  As long as there is another dot
of the same color next to the clicked dot, it will disappear along
with all adjacent dots of the same color.  After the dots disappear,
dots in the rows above the deleted dots will fall into the vacated
spaces.  If an entire column is wiped out, all of the dots from the
right will slide left to take up the empty column's space.

Your score increases for each ball removed from the board.  The score
for each click is a function of the number of balls that disappeared.
The @onscreen{This Click} label shows how many points you would score
for clicking the dots underneath the mouse pointer.  The score varies
quadratically with the number of balls, so eliminating many balls with
one click is advantageous.

Click the @onscreen{New Game} button to play again.
