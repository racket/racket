#lang scribble/doc
@(require "common.rkt")

@gametitle["Crazy 8s" "crazy8s" "Card Game"]

Try to get rid of all you cards by matching the value or suit of the
top card in the discard pile.  In the default mode, click a card to
discard it; you can adjust the options so that you discard by dragging
a card from your hand to the discard pile.

An @onscreen{8} can be discarded at any time, and in that case, the
player who discarded the @onscreen{8} gets to pick any suit for it
(hence the craziness of @onscreen{8}s).  When you discard an
@onscreen{8}, a panel of buttons appears to the right of the discard
pile, so you can pick the suit.

A player can choose to draw a card instead of discarding, as long as
cards are left in the draw pile.  A player's turn continues after
drawing, so a player can continue drawing to find something to
discard.  In the default mode, click the face-down draw pile in the
middle of the table; you can adjust the options to that you draw by
dragging it from the draw pile to your hand.

If no cards are left in the deck, a player may pass instead of
discarding.  To pass, click the @onscreen{Pass} button.

The status line at the bottom of the window provides instructions as
you go.
