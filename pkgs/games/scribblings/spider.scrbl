#lang scribble/doc
@(require "common.rkt")

@gametitle["Spider" "spider" "Solitaire Card Game"]

Spider is a solitaire card game played with 104 cards.  The cards can
include either a single suit, two suits, or four suites.  (Choose your
variant through the @onscreen{Options} item in the @onscreen{Edit}
menu.)

Terminology:

@itemize[

 @item{@deftech{Tableau}: one of the ten stacks of cards in the play
   area.  The game starts with six cards in the first four
   @tech{tableau}s, and five cards in the rest; only the topmost card
   is face up, and others are revealed when they become the topmost
   card of the @tech{tableau}.}

 @item{@deftech{Sequence}: a group of cards on the top of a
   @tech{tableau} that are in the same suit, and that are in
   @tech{sequence}, with the lowest numbered card topmost (i.e.,
   closer to the bottom of the screen).  King is high and ace is low.}

]

The object of the game is to create a @tech{sequence} with ace through
king, at which point the @tech{sequence} is removed from play.  Create
eight such @tech{sequence}s to win the game.

On each move, you can take one of the following actions:

@itemize[

 @item{Move a @tech{sequence} from any @tech{tableau} to one whose
   topmost card (i.e., closest to the bottom of the screen) has a
   value that's one more than the @tech{sequence}'s value.  Note that
   if the top card of the target @tech{tableau} has the same suit as
   the @tech{sequence}, a larger @tech{sequence} is formed, but the
   target @tech{tableau}'s card is not required to have the same suit.}

 @item{Move a @tech{sequence} to an empty @tech{tableau}.}

 @item{Deal ten cards from the deck (in the upper left corder), one to
   each @tech{tableau}.  This move is allowed only if no
   @tech{tableau} is empty.}

]

To move a @tech{sequence}, either drag it to the target
@tech{tableau}, or click the @tech{sequence} and then click the top
card of the target @tech{tableau} (or the place where a single card
would be for an empty @tech{tableau}).  Click a select card to
de-select it.  Clicking a card that is not a valid target for the
currently selected @tech{sequence} causes the clicked card's
@tech{sequence} to be selected (if the card is face up in a
@tech{sequence}).

To deal, click the deck.

To undo a move, use @onscreen{Undo} from the @onscreen{Edit} menu.
