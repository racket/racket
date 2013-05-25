#lang scribble/doc
@(require "common.rkt")

@gametitle["Blackjack" "blackjack" "21 Card Game"]

Standard Blackjack rules with the following specifics:

@itemize[

 @item{1 player (not counting the dealer).}

 @item{4 decks, reshuffled after 3/4 of the cards are used.}

 @item{Dealer stands on soft 17s.}

 @item{Splitting is allowed only on the first two cards, and only if
       they are equal. 10 and the face cards are all considered equal
       for splitting.}

 @item{Doubling is allowed on all unsplit hands, not on split hands.}

 @item{No blackjacks after splitting.}

 @item{No surrender.}

 @item{No insurance.}

 @item{No maximum under-21 hand size.}

 @item{Dealer's second card is not revealed if the player busts (or
       both halves of a split hand bust).}

]
