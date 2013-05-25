#lang scribble/doc
@(require "common.rkt")

@gametitle["Go Fish" "gofish" "Kid's Card Game"]

@game{Go Fish} is the children's card game where you try to get rid of
all you cards by forming pairs.  You play against two computer
players.

On each turn, if you have a match in your hand, drag one of the
matching cards to your numbered box, and the match will move into the
box.

After forming matches from your own hand, drag one of your cards to an
opponent's area to ask the opponent for a matching card:

@itemize[

 @item{If the opponent has a card with the same value as the card that you
   drag, the opponent will give you the card, and they'll go into your
   match area.  Drag another card to an opponent.}

 @item{If the opponent has no matching card, the top card on draw pile
   will move, indicating that you must ``Go Fish!''  Draw a card by
   dragging it from the draw pile to your hand.  If the drawn card
   gives you a match, then the match will automatically move into your
   match area, and it's still your turn (so drag another card to one
   of the opponents).}

]

The game is over when one player runs out of cards.  The winner is the
one with the most matches.

The status line at the bottom of the window provides instructions as
you go.  The computer players are not particularly smart.
