#lang scribble/doc
@(require "common.rkt")

@gametitle["Aces" "aces" "Solitaire Card Game"]

Aces is a solitaire card game.  The object is to remove all of the
cards from the board, except the four Aces.

Remove a card by clicking it.  You may remove a card when two
conditions are true.  First, it must be at the bottom of one of the
four stacks of cards.  Second, either the ace of the same suit, or a
higher card of the same suit must also be at the bottom of one of the
four stacks of cards.

You may also move any card from the bottom of one of the stacks to an
empty stack by clicking it.  If there are still cards in the deck on
the left, you may click the deck to deal four new cards, one onto the
bottom of each stack.

Good Luck!
