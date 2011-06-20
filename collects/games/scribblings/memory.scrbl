#lang scribble/doc
@(require "common.rkt")

@gametitle["Memory" "memory" "Kid's Game"]

Flip two cards in a row that have the same picture, and the cards are
removed. If the two cards don't match, they are flipped back over, and
you try again. Each card has a single match on the board. The game is
over and the clock stops when all cards are removed.
