#lang scribble/doc
@(require "common.rkt")

@gametitle["Jewel" "jewel" "3-D Skill Game"]

@author["Peter Ivanyi"]

The board is an 8 by 8 array of jewels of 7 types.  You need to get 3
or more in a row horizontally or vertically in order to score points.
You can swap any two jewels that are next to each other up and down or
left and right.  The mechanic is to either:

@itemize[

 @item{Click the mouse on the first one, then drag in the direction for
   the swap.}

 @item{Move a bubble using the arrow keys, lock the bubble to a jewel with
   the space bar, and the swap the locked jewel with another by using
   the arrow keys.  Space unlocks a locked bubble without swapping.}

]

Jewels can only be swapped if after the swap there are at least 3 or
more same shape or color in a row or column.  Otherwise the jewels
return to their original position.  There is a clock shown on the
left.  When it counts down to 0 the game is over.  Getting 3 in a row
adds time to the clock.

Hit spacebar to start a new game then select the difficulty number by
pressing @onscreen{0}, @onscreen{1}, @onscreen{2}, @onscreen{3}, or
@onscreen{0}.  You can always press ESC to exit.  During playing press
@onscreen{P} to pause the game.

The code is released under the LGPL.  The code is a conversion of Dave
Ashley's C program to Racket with some modifications and enhancements.

Enjoy.
