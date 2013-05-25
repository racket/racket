#lang scribble/doc
@(require scribble/manual
          (for-label games/cards
                     racket/gui/base))

@title{Cards: Virtual Playing Cards Library}

@defmodule[games/cards]{The @racketmodname[games/cards]
module provides a toolbox for creating card games.}

@; ----------------------------------------------------------------------
@section{Creating Tables and Cards}

@defproc[(make-table [title string? "Cards"] 
                     [w exact-nonnegative-integer? 7]
                     [h exact-nonnegative-integer? 3])
         table<%>]{

Returns a table.  The table is named by @racket[title], and it is
@racket[w] cards wide and @racket[h] cards high (assuming a standard
card of 71 by 96 pixels). The table is not initially shown;
@racket[(send table show #t)] shows it.}

@defproc[(make-deck)
         (listof card<%>)]{

Returns a list of 52 cards, one for each suit-value combination. The
cards are all face-down, sorted lowest-suit then lowest-value. A card
can only be on one table at a time.}

@defproc[(make-card [front-bm (is-a?/c bitmap?)]
                    [back-bm (or/c (is-a?/c bitmap%) false/c)]
                    [suit-id any/c]
                    [value any/c])
         (is-a?/c card<%>)]{

Returns a single card given a bitmap for the front, an optional bitmap
for the back, and arbitrary values for the card's suit and value
(which are returned by the card's @method[card<%> get-value] and
@method[card<%> get-suit-id] methods). All provided bitmaps should be
the same size.}

@defproc[(shuffle-list [lst list?] [n exact-nonnegative-integer?])
         list?]{

Shuffles the given @racket[lst] @racket[n] times, returning the new
list. Shuffling simulates an actual shuffle: the list is split into
halves which are merged back together by repeatedly pulling the top
card off one of the halves, randomly selecting one half or the
other. According to some mathematical theorem, 7 is a large enough
@racket[n] to get a perfect shuffle.}

@; ----------------------------------------------------------------------
@section{Regions and Buttons}

@defstruct[region ([x real?]
                   [y real?]
                   [w (and/c real? (not/c negative?))]
                   [h (and/c real? (not/c negative?))]
                   [label (or/c string? false/c)]
                   [(callback #:mutable) (or/c ((listof (is-a?/c card<%>)) . -> . any)
                                               false/c)])]{

The @racket[x], @racket[y], @racket[w], and @racket[h] fields
determine the region's location on the table.

When @racket[label] is a string, it is drawn in the region in 12-pixel
text, centered horizontally and 5 pixels down from the region's top
outline. If label is @racket[#f], no label or box is drawn for the
region. 

The @racket[callback] procedure takes a list of cards that were
dragged to the region; if callback is @racket[#f], the region is not
active (i.e., dragging cards to the region doesn't highlight the
region box). The region remains hilited until the callback returns.

The only available mutator on the structure is
@racket[set-region-callback!].  The structure created by
@racket[make-region] actually has extra hidden fields.}

@defproc[(make-button-region [x real?]
                             [y real?]
                             [w (and/c real? (not/c negative?))]
                             [h (and/c real? (not/c negative?))]
                             [label (or/c string? false/c)]
                             [callback (or/c ((listof (is-a?/c card<%>)) . -> . any)
                                             false/c)])
         region?]{

Returns a region like one made by @racket[make-region], but the is
 drawn slightly differently and it reacts differently to cards and the
 mouse. The label is drawn in the middle of the box instead of at the
 top, and the callback is called with no arguments when the user
 clicks the region (instead of dragging cards to the region).}

@defproc[(make-background-region [x real?]
                                 [y real?]
                                 [w (and/c real? (not/c negative?))]
                                 [h (and/c real? (not/c negative?))]
                                 [paint-callback
                                  ((is-a?/c dc<%>) real? real? real? real? . -> . any)])
         region?]{

 Returns a region that does not respond to mouse clicks, but which has
 a general paint callback. The @racket[paint-callback] function is
 called with a drawing context, x and y offsets, and the width and
 height (which are always @racket[w] and @racket[h]). The x and y
 offsets can be different than the supplied @racket[x] and @racket[y]
 when part of the table is drawn offscreen. Regions are painted in the
 order that they are added to a table, and all regions are painted
 before any card.  The @racket[paint-callback] procedure should not
 assume a particular state for the drawing context (i.e.,current brush
 or pen), and it should restore any modified drawing context state
 before returning.}

@defproc[(set-region-interactive-callback! 
          [r region?]
          [callback (or/c (boolean? (listof (is-a?/c card<%>)) . -> . any)
                          false/c)])
         void?]{

 Sets a callback procedure that is invoked when a region is
 (un)hilited as the user drags a set of cards to the region. The
 callback is provided two arguments: a boolean indicating whether the
 region is hilited, and the list of cards being dragged. Like
 region-callback, the default is @racket[#f], which indicates that the
 region has no interactive callback (but does not affect whether the
 region is hilited as cards are dragged). The final unhilite (when
 cards are potentially delivered) does not trigger this callback.}


@defproc[(region-interactive-callback [r region?])
         (boolean? (listof (is-a?/c card<%>)) . -> . any)]{

 Gets the current callback that is installed via
 @racket[set-region-interaction-callback!].}

@; ----------------------------------------------------------------------
@section{Table Methods}

@definterface[table<%> (frame%)]{

Create an instance with @racket[make-table].

@defmethod[(add-card [card (is-a?/c card<%>)]
                     [x real?]
                     [y real?])
           void?]{

 Adds @racket[card] to the table with its top-left corner at
 (@racket[x], @racket[y]) in table pixels.}

@defmethod[(add-cards [cards (listof (is-a?/c card<%>))]
                      [x real?]
                      [y real?]
                      [offset-proc (exact-nonnegative-integer? 
                                    . -> . (values real? real?))
                                   (lambda (i) (values 0 0))])
            void?]{

 Adds a list of cards at (@racket[x], @racket[y]). The optional
 @racket[offset-proc] procedure is called with an index @racket[_i]
 (counting from 0) and should return two values: @racket[_dx] and
 @racket[_dy]; the @racket[_i]th card is the placed at @racket[(+ x
 +dx)] and @racket[(+ y _dy)]. The cards are added in order on top of
 cards already one the table such that the first card in
 @racket[cards] is topmost.}

@defmethod[(add-cards-to-region [cards (listof (is-a?/c card<%>))]
                                [region? r])
           void?]{

 Adds @racket[cards] to fill the region @racket[r], fanning them out
 bottom-right to top-left, assuming that all cards in @racket[cards]
 have the same width and height. The region @racket[r] does not have
 to be added to the table.}

@defmethod[(remove-card [card (is-a?/c card<%>)])
           void?]{

Removes @racket[card] from the table.}

@defmethod[(remove-cards [cards (listof (is-a?/c card<%>))])
           void?]{

 Removes @racket[cards] from the table.}

@defmethod[(move-card [card (is-a?/c card<%>)] 
                      [x real?] 
                      [y real?])
           void?]{

 Moves @racket[card], which must be on the same already.  The movement
 of the cards is animated.  If the cards are in snap-back-after-move
 mode and a drag is active, snapping back will use the new location.}

@defmethod[(move-cards [cards (listof (is-a?/c card<%>))]
                       [x real?]
                       [y real?]
                       [offset-proc (exact-nonnegative-integer? 
                                     . -> . (values real? real?))
                                    (lambda (i) (values 0 0))])
            void?]{

 Like @method[table<%> add-cards], but moves cards that are already on
 the table like @method[table<%> move-card]. All of the cards are
 moved at once.}

@defmethod[(move-cards-to-region [cards (listof (is-a?/c card<%>))]
                                 [region? r])
           void?]{

 Like @method[table<%> add-cards-to-region], but moves cards that are
 already on the table like @racket[move-card]. All of the cards are
 moved at once.}


@defmethod*[([(flip-card [card (is-a?/c card<%>)]) void?]
             [(flip-cards [cards (listof (is-a?/c card<%>))]) void?])]{

 Flips @racket[card] or all @racket[cards] over (at once) with
 animation.}

@defmethod*[([(card-face-up [card (is-a?/c card<%>)]) void?]
             [(cards-face-up [cards (listof (is-a?/c card<%>))]) void?]
             [(card-face-down [card (is-a?/c card<%>)]) void?]
             [(cards-face-down [cards (listof (is-a?/c card<%>))]) void?])]{

 Like @method[table<%> flip-cards], but only for @racket[card] or
 elements of @racket[cards] that are currently face down/up.}

@defmethod*[([(rotate-card [card (is-a?/c card<%>)]
                           [mode (or/c 'cw 'ccw 0 90 -90 180 -180 270 -270 360)]) 
              void?]
             [(rotate-cards [cards (listof (is-a?/c card<%>))]
                            [mode (or/c 'cw 'ccw 0 90 -90 180 -180 270 -270 360)])
              void?])]{

 Rotates @racket[card] or all @racket[cards] (at once, currently
 without animation, but animation may be added in the future).
 The center of each card is kept in place, except that the card is
 moved as necessary to keep it on the table.  See @xmethod[card<%>
 rotate] for information on @racket[mode].}

@defmethod*[([(card-to-front [card (is-a?/c card<%>)]) void?]
             [(card-to-back [card (is-a?/c card<%>)]) void?])]{

 Moves @racket[card] before/behind of all other cards.}

@defmethod[(stack-cards [cards (listof (is-a?/c card<%>))]) void?]{

 The first card in @racket[cards] is not moved; the second card is
 moved to follow immediately behind the first one, then
 @method[table<%> stack-cards] is called on @racket[(cdr cards)]. If
 @racket[cards] is empty or contains only one card, no action is
 taken.}

@defmethod[(card-location [card (is-a?/c card<%>)])
           (values real? real?)]{

 Returns the location of the given card; an exception is raised if the
 card is not on the table.}

@defmethod[(all-cards) (listof (is-a?/c card<%>))]{

 Returns a list of all cards on the table in stacking order from front
 to back.}

@defmethod*[([(table-width) exact-nonnegative-integer?]
             [(table-height) exact-nonnegative-integer?])]{

 Returns the width/height of the table in pixels.}

@defmethod*[([(begin-card-sequence) void?]
             [(end-card-sequence) void?])]{

 Starts/ends a sequence of card or region changes that won't be
 animated or updated until the end of the sequence. Sequences can be
 nested via matching @racketidfont{begin-}/@racketidfont{end-} pairs.}

@defmethod[(add-region [r region?]) void]{

 Adds the region @racket[r] to the table; regions are drawn in the
 order that they are added to the table, and when a region added later
 is hilighted, it can obscure regions added earlier.}

@defmethod[(remove-region [r region?]) void]{

 Removes the region @racket[r] from the table.}

@defmethod*[([(hilite-region [r region?]) void?]
             [(unhilite-region [r region?]) void?])]{

 Manual (un)hilite, usually for animation.}

@defmethod[(set-button-action [which (one-of/c 'left 'middle 'right)]
                              [action symbol?])
           void?]{

 Sets the way that a mouse click is handled for a particular button
 indicated by @racket[which]. The @racket[action] argument must be one
 of the following:

 @itemize[

   @item{@racket['drag/one] --- drag only the clicked-on card.}

   @item{@racket['drag-raise/one] --- like drag/one, but raise the
                  card to the top on a click.}

   @item{@racket['drag/above] --- drag the card along with any card
                  on top of the card (i.e., more towards the front and
                  overlapping with the card). The on-top-of relation
                  is closed transitively.}

   @item{@racket['drag-raise/above] --- like @racket['drag/above],
                  but raises.}

   @item{@racket['drag-below] --- drag the card along with any card
                  underneath the card (i.e., more towards the back and
                  overlapping with the card). The underneath relation
                  is closed transitively.}

   @item{@racket['drag-raise/below] --- like @racket['drag/below],
                  but raises.}
 ]

 The initial settings are: @racket['drag-raise/above] for
 @racket['left], @racket['drag/one] for @racket['middle], and
 @racket['drag/below] for @racket['right].}

@defmethod[(set-double-click-action
            [proc ((is-a?/c card<%>) . -> . any)])
           void?]{

 Sets the procedure to be called when a card is double-clicked. The
 procedure is called with the double-clicked card. The default
 procedure flips the cards along with its on-top-of cards, raises the
 cards, and reverses the front-to-back order of the cards}

@defmethod[(set-single-click-action
            [proc ((is-a?/c card<%>) . -> . any)])
           void?]{

 Sets the procedure to be called when a card is single-clicked, after
 the button action is initiated. (If the card is double-clicked, this
 action is invoked for the first click, then the double-click action
 is invoked.) The default action does nothing.}

@defmethod[(pause [secs real?]) void?]{

 Pauses, allowing the table display to be updated (unless a sequence
 is active), but does not let the user click on the cards.}

@defmethod*[([(animated) boolean?]
             [(animated [on? any/c]) void?])]{

 Gets/sets animation enabled/diabled.}

@defmethod[(create-status-pane) (is-a?/c pane%)]{

 Creates a pane with a status message (initially empty) and returns
 the pane so that you can add additional controls.}

@defmethod[(set-status [str sring]) void?]{

 Sets the text message in the status pane.}

@defmethod[(add-help-button [pane (is-a?/c area-container<%>)]
                            [coll-path (listof string?)]
                            [str string?]
                            [tt? any/c])
            void?]{

 Adds a @onscreen{Help} button to the given pane, where clicking the
 button opens a new window to display @filepath{doc.txt} from the given
 collection. The @racket[str] argument is used for the help window
 title.  If @racket[tt?]  is true, then @filepath{doc.txt} is displayed
 verbatim, otherwise it is formatted as for @racket[show-help] from
 @racketmodname[games/show-help].}

@defmethod[(add-scribble-button [pane (is-a?/c area-container<%>)]
                                [mod-path module-path?]
                                [tag string?])
            void?]{

 Adds a @onscreen{Help} button to the given pane, where clicking the
 button opens Scribble-based documentation, as with
 @racket[show-scribbling] from @racketmodname[games/show-scribbling].}

}

@; ----------------------------------------------------------------------
@section{Card Methods}

@definterface[card<%> ()]{

Create instances with @racket[make-deck] or @racket[make-card].

@defmethod[(card-width) exact-nonnegative-integer?]{

 Returns the width of the card in pixels. If the card is rotated 90 or
 270 degrees, the result is the card's original height.}

@defmethod[(card-height) exact-nonnegative-integer?]{

 Returns the height of the card in pixels.  If the card is rotated 90 or
 270 degrees, the result is the card's original width.}

@defmethod[(flip) void?]{

 Flips the card without animation. This method is useful for flipping
 a card before it is added to a table.}

@defmethod[(face-up) void?]{

 Makes the card face up without animation.}

@defmethod[(face-down) void?]{

 Makes the card face down without animation.}

@defmethod[(face-down?) boolean?]{

 Returns @racket[#t] if the card is currently face down.}

@defmethod[(rotate [mode (or/c 'cw 'ccw 0 90 -90 180 -180 270 -270 360)]) void?]{

 Rotates the card. Unlike using the @xmethod[table<%> rotate-card] method,
 the card's top-left position is kept in place.

 If @racket[mode] is @racket['cw], the card is
 rotated clockwise; if @racket[mode] is @racket['ccw], the card is
 rotated counter-clockwise; if @racket[mode] is one of the allowed
 numbers, the card is rotated the corresponding amount in degrees
 counter-clockwise.}

@defmethod[(orientation) (or/c 0 90 180 270)]{

 Returns the orientation of the card, where @racket[0] corresponds to
 its initial state, @racket[90] is rotated 90 degrees counter-clockwise, and so on.}

@defmethod[(get-suit-id) any/c]{

 Normally returns @racket[1], @racket[2], @racket[3], or @racket[4]
 (see @method[card<%> get-suit] for corresponding suit names), but the
 result can be anything for a card created by @racket[make-card].}

@defmethod[(get-suit) symbol?]{

 Returns @racket['clubs], @racket['diamonds], @racket['hearts],
 @racket['spades], or @racket['unknown], depending on whether
 @method[card<%> get-suit-id] returns @racket[1], @racket[2],
 @racket[3], @racket[4], or something else.}

@defmethod[(get-value) any/c]{

 Normally returns @racket[1] (Ace), @racket[2], ... @racket[10],
 @racket[11] (Jack), @racket[12] (Queen), or @racket[13] (King), but
 the result can be anything for a card created by @racket[make-card].}

@defmethod*[([(user-can-flip) boolean?]
             [(user-can-flip [can? any/c]) void?])]{

 Gets/sets whether the user can flip the card interactively, usually
 by double-clicking it. Initially @racket[#t].}

@defmethod*[([(user-can-move) boolean?]
             [(user-can-move [can? any/c]) void?])]{

 Gets/sets whether the user can move the card interactively, usually
 by dragging it. Disabling moves has the side-effect of disabling
 raises and double-clicks. Initially @racket[#t].}

@defmethod*[([(snap-back-after-move) boolean?]
             [(snap-back-after-move [on? any/c]) void?])]{

 Assuming user can move the card interactively, gets/sets whether the
 card stays where the user dragged it or snaps back to its original
 place. Initially @racket[#f]. 

 A region's @italic{interactive} callback can disable snap-back for a
 card so that the card can be delivered to the region. (A region's
 normal callback cannot release the card, because it's too late.)}

@defmethod*[([(stay-in-region) (or/c region? false/c)]
             [(stay-in-region [r (or/c region? false/c)]) void?])]{


 Gets/sets a constraining region @racket[r]. If @racket[r] is not
 @racket[#f], the user cannot move the card out of @racket[r].
 Initially @racket[#f].}

@defmethod*[([(home-region) (or/c region? false/c)]
             [(home-region [r (or/c region? false/c)]) void?])]{

 Gets/sets a home region @racket[r]. If @racket[r] is not @racket[#f],
 then the user can move the card freely within the region, but it
 snaps back if moved completely out of the region. If moved partly out
 of the region, the card is moved enough to get completely back
 in. Initially @racket[#f]. 

 A region's @italic{interactive} callback can disable snap-back for a
 card so that the card can be delivered to the region. (A region's
 normal callback cannot release the card, because it's too late.)}

@defmethod*[([(dim) boolean?]
             [(dim [can? any/c]) void?])]{

 Gets/sets a hilite on the card, which is rendered by drawing it dimmer
 than normal.}

@defmethod[(copy) (is-a?/c card<%>)]{

 Makes a new card with the same suit and value.}

}
