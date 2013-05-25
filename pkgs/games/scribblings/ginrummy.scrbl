#lang scribble/doc
@(require "common.rkt")

@gametitle["Rummy" "ginrummy" "Card Game"]

This is a simple variant of Rummy.

Put all cards in your hand into straights (3 or more cards in the same
suit) and 3- or 4-of-a-kind sets to win.  Each card counts for only
one set.  Aces can be used in both A-2-3 sequences and Q-K-A
sequences.

When all of your cards fit into sets (the game detects this
automatically), you win.

On each turn, you can either draw from the deck or from the top of the
discard pile (drag from either to your hand), then you must discard
one of your own cards (by dragging from your hand to the discard
pile).

The status line at the bottom of the window provides instructions as
you go.  The computer player is fairly smart.
