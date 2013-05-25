#lang scribble/doc
@(require "common.rkt")

@gametitle["Pousse" "pousse" "Tic-Tac-Toe-like Game"]

@onscreen{Pousse} (French for ``push,'' pronounced ``poo-ss'') is a 2
person game, played on an @math{N} by @math{N} board (usually 4 by 4).
Initially the board is empty, and the players take turns inserting one
marker of their color (@onscreen{X} or @onscreen{O}) on the board.
The color @onscreen{X} always goes first.  The columns and rows are
numbered from 1 to @math{N}, starting from the top left, as in:

@verbatim[#:indent 3]{
     1 2 3 4
    +-+-+-+-+
  1 | | | | |
    +-+-+-+-+
  2 | | | | |
    +-+-+-+-+
  3 | | | | |
    +-+-+-+-+
  4 | | | | |
    +-+-+-+-+
}

A marker can only be inserted on the board by sliding it onto a
particular row from the left or from the right, or onto a particular
column from the top or from the bottom.  So there are @math{4*N}
possible ``moves'' (ways to insert a marker).  They are named
L@math{i}, R@math{i}, T@math{i}, and B@math{i} respectively, where
@math{i} is the number of the row or column where the insertion takes
place.

When a marker is inserted, there may be a marker on the square where
the insertion takes place.  In this case, all markers on the insertion
row or column from the insertion square up to the first empty square
are moved one square further to make room for the inserted marker.
Note that the last marker of the row or column will be pushed off the
board (and must be removed from play) if there are no empty squares on
the insertion row or column.

A row or a column is a @defterm{straight} of a given color if it
contains @math{N} markers of the given color.

The game ends either when an insertion

@itemize[

@item{repeats a previous configuration of the board; in this case the
      player who inserted the marker LOSES.}

@item{creates a configuration with more straights of one color than
   straights of the other color; the player whose color is dominant
   (in number of straights) WINS.}

]

A game always leads to a win by one of the two players.  Draws are
impossible.

This game is from the 1998 ICFP programming contest.

