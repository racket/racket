#lang racket/base
(require racket/gui/base games/cards racket/class racket/unit)

(provide game@)
(define game@ (unit (import) (export)

;; Initial card count
(define DEAL-COUNT 10)

;; Messages
(define YOUR-TURN-MESSAGE "Your turn.  (Draw a card or pickup a discard.)")
(define DISCARD-MESSAGE "Drag a card from your hand to discard.")
(define GAME-OVER-MESSAGE "GAME OVER")

;; Area labels
(define YOU-NAME "You")
(define MACHINE-NAME "Opponent")

;; Region layout constants
(define MARGIN 5)
(define LABEL-H 15)

;; Randomize
(random-seed (modulo (current-milliseconds) 10000))

;; Set up the table
(define t (make-table "Rummy" 8 4.5))
(define status-pane (send t create-status-pane))
(send t add-scribble-button status-pane
      '(lib "games/scribblings/games.scrbl") "ginrummy")
(send t show #t)
(send t set-double-click-action #f)
(send t set-button-action 'left 'drag-raise/one)
(send t set-button-action 'middle 'drag/one)
(send t set-button-action 'right 'drag/above)

;; Get table width & height
(define w (send t table-width))
(define h (send t table-height))

;; Set up the cards
(define deck (shuffle-list (make-deck) 7))
(for-each (lambda (card)
            (send card user-can-move #f)
            (send card user-can-flip #f))
          deck)

;; Function for dealing or drawing cards
(define (deal n)
  (let loop ([n n][d deck])
    (if (zero? n)
      (begin (set! deck d) null)
      (cons (car d) (loop (sub1 n) (cdr d))))))

;; Card width & height
(define cw (send (car deck) card-width))
(define ch (send (car deck) card-height))

;; Draw and discard pile locations
(define draw-x (/ (- w (* 3 cw)) 2))
(define draw-y (/ (- h ch) 2))
(define discard-x (+ draw-x (* 2 cw)))
(define discard-y draw-y)

;; Put the cards on the table
(send t add-cards deck draw-x draw-y)

;; Player region size
(define pw (- w (* 2 MARGIN)))
(define ph (- (* 1.75 ch) (* 4 MARGIN)))

;; Define the regions
(define machine-region
  (make-region MARGIN MARGIN pw ph MACHINE-NAME #f))
(define you-region
  (make-region MARGIN (- h ph MARGIN) pw ph YOU-NAME void))
(define discard-region
  (make-region (- discard-x MARGIN) (- discard-y MARGIN)
               (+ cw (* 2 MARGIN)) (+ ch (* 2 MARGIN))
               "" #f))

;; Install the visible regions
(send t add-region machine-region)
(send t add-region you-region)
(send t add-region discard-region)

;; Deal the initial hands
(define machine-hand (deal DEAL-COUNT))
(define you-hand (deal DEAL-COUNT))

      ;; Function to inset a region
(define (region->display-region r)
  (define m MARGIN)
  (make-region (+ m (region-x r)) (+ m (region-y r))
               (- (region-w r) (* 2 m)) (- (region-h r) (* 2 m))
               #f #f))

;; Place cards nicely
(define machine-display-region (region->display-region machine-region))
(send t move-cards-to-region machine-hand machine-display-region)
(send t move-cards-to-region you-hand (region->display-region you-region))

;; All cards in your hand are movable, but must stay in your region
(for-each (lambda (card) 
            (send card home-region you-region)
            (send card user-can-move #t))
          you-hand)

;; More card setup: Show your cards
(send t cards-face-up you-hand)

;; Start the discard pile
(define discards (deal 1))
(send t card-face-up (car discards))
(send t move-card (car discards) discard-x discard-y)

;; ;;;;;;;;;;;;; Finding Rummy and The Machine Player Strategy ;;;;;;;;

;; Check whether a group of (at least 3) cards forms a set (building
;; up to gin).
(define (set? cards)
  (let ([values (map (lambda (c) (send c get-value)) cards)]
        [suits (map (lambda (c) (send c get-suit-id)) cards)])
    ;; All same value? ... or
    (or (apply = values)
        ;; ... All same suit and a straight?
        (and (apply = suits)
             (let ([sorted (sort values <)]
                   [try (lambda (l)
                          (let loop ([l l])
                            (or (null? (cdr l))
                                (and (= (car l) (sub1 (cadr l)))
                                     (loop (cdr l))))))])
               ;; Try with Ace at end and at beginning
               (or (try sorted)
                   (and (= 1 (car sorted))
                        (try (append (cdr sorted) (list 14))))))))))

;; Check how close a hand comes to winning by returning the maximum number of
;; cards that can be arranged into sets. This function is used both to detect
;; gin for the end-of-game condition, and also as part of the machine player's
;; strategy.
(define (gin-size cards)
  (if (<= (length cards) 2)
    0
    (let* ([sort (lambda (get)
                   (sort cards (lambda (a b) (< (get a) (get b)))))]

           ;; It's not reasonable to test every combination of 10 cards, but we
           ;; can cut down the search space a lot by starting with two
           ;; different sorts on the card list.

           ;; We sort by value, to find 3-of-a-kind sets, and by
           ;; suit-then-value, to find straights. Whatever the best allocation
           ;; of cards to sets, one of the sets must show up as three cards
           ;; together in one of the sorted lists.  Also, if an extension to
           ;; that set leads to an optimal allocation, the extended set
           ;; corresponds to an extended section of the list.
           [value-sorted (sort (lambda (c) (send c get-value)))]
           [suit-sorted (sort (lambda (c) (+ (* 20 (send c get-suit-id)) (send c get-value))))]

           ;; Procedure to find a set allocation given one of the sorted
           ;; lists. It picks each group of three consecutive items from the
           ;; list and see how that choice works out.  (We're still performing
           ;; a lot of redundant work here, but it's fast enough.)
           [find-set
            (lambda (l)
              ;; 3loop tries each group of three items
              (let 3loop ([pre null] ; prefix we've tried already
                          [group (list (car l) (cadr l) (caddr l))] ; the group to try
                          [post (cdddr l)]) ; suffix we haven't tried yet
                (max (if (set? group)
                       ;; We have a start; try to extend or not, and
                       ;; make gin with the rest, then try the next 3-set
                       (max (let exloop ([set group][post post])
                              (cond
                                [(null? post)
                                 ;; No more items? Can't extend the set. Does the
                                 ;; set we found work out in the long run?
                                 (+ (length set)
                                    (if (null? pre) 0 (gin-size pre)))]
                                ;; Try to extend the set...
                                [(set? (cons (car post) set))
                                 ;; The set can be extended.  Maybe this
                                 ;; extension works in the long run...
                                 (max (exloop (cons (car post) set) (cdr post))
                                      ;; or maybe without extension works in
                                      ;; the long run...
                                      (+ (length set) (gin-size (append pre post))))]
                                ;; Can't extend the set, so try without
                                ;; extension
                                [else (+ (length set)
                                         (gin-size (append pre post)))])))
                       0)
                     ;; Try next three, if possible
                     (if (null? post)
                       0
                       ;; Rotate the group, pulling a new last item in from
                       ;; post and kicking the first item out to pre.
                       (3loop (cons (car group) pre)
                              (list (cadr group) (caddr group) (car post))
                              (cdr post))))))])
      ;; Try the value-sorted list, the suit-sorted list, then...
      (max (find-set value-sorted)
           (find-set suit-sorted)
           ;; the suit-sorted list with with Aces at the end instead of the
           ;; beginning
           (let ace-loop ([pre null][l suit-sorted])
             (cond
               [(null? l)
                ;; No more aces to find
                (find-set (reverse pre))]
               [(null? (cdr l))
                ;; No more aces to find
                (find-set (reverse (cons (car l) pre)))]
               ;; Is the front card an ace (before something else of the same
               ;; suit)?
               [(and (= 1 (send (car l) get-value))
                     (= (send (car l) get-suit-id) (send (cadr l) get-suit-id)))
                ;; Ace is at beginning; move it to the end
                (let* ([ace (car l)]
                       [ace-suit (send ace get-suit-id)])
                  (let loop ([pre (cons (cadr l) pre)][l (cddr l)])
                    ;; At end of this suit?
                    (if (or (null? l) (> (send (car l) get-suit-id) ace-suit))
                      ;; At the end; add Ace here
                      (ace-loop (cons ace pre) l)
                      ;; still looking for new spot for Ace
                      (loop (cons (car l) pre) (cdr l)))))]
               [else
                ;; Didn't find an ace; keep looking
                (ace-loop (cons (car l) pre) (cdr l))]))))))

;; A hand wins if the biggest gin configuration includes all the cards
(define (gin? cards)
  (= (gin-size cards) (length cards)))

;; This procedure is the second part of the machine's strategy. If the machine
;; sees two choices that are equally good according to gin-size, then it
;; computes a rating based on pairs, i.e., cards that might eventually go
;; together in a set.
(define (pair-rating cards gone-cards)
  (let ([suits (map (lambda (card) (send card get-suit-id)) cards)]
        [values (map (lambda (card) (send card get-value)) cards)])
    ;; Its O(n*n), but n is always 10 or 11
    (apply
     + (map (lambda (suit value)
              (apply
               + (map (lambda (suit2 value2)
                        (cond [(= value value2)
                               (- 2 (count-gone value gone-cards))]
                              [(= suit suit2)
                               (rate-straight suit value value2 gone-cards)]
                              [else 0]))
                      suits values)))
            suits values))))

;; count-gone checks how many of a given value are known to be permanently
;; discarded
(define (count-gone value gone-cards)
  (cond [(null? gone-cards) 0]
        [(= value (send (car gone-cards) get-value))
         (+ 1 (count-gone value (cdr gone-cards)))]
        [else (count-gone value (cdr gone-cards))]))

;; count-avail checks whether a given value/suit is
;;  known to be discarded (returns 0) or not (returns 1)
(define (count-avail value suit gone-cards)
  (cond [(null? gone-cards) 1]
        [(and (= value (send (car gone-cards) get-value)) 
              (= suit (send (car gone-cards) get-suit-id))) 
         0]
        [else (count-avail value suit (cdr gone-cards))]))

;; rates the possibility for forming a straight given two card values in a
;; particular suit, and taking into account cards known to be discarded; the
;; rating is the number of non-discarded cards that would form a straight with
;; the given values
(define (rate-straight suit value value2 gone-cards)
  (let ([v1 (if (= value 1)
              (if (value2 . > . 6) 14 1)
              value)]
        [v2 (if (= value2 1)
              (if (value . > . 6) 14 1)
              value2)])
    (let ([delta (abs (- v1 v2))])
      (cond [(= delta 1)
             (cond [(or (= v1 1) (= v2 1))
                    ;; Might get the 3?
                    (count-avail 3 suit gone-cards)]
                   [(or (= v1 14) (= v2 14))
                    ;; Might get the queen?
                    (count-avail 12 suit gone-cards)]
                   [(or (= v1 13) (= v2 13))
                    ;; Might get the jack or ace?
                    (+ (count-avail 11 suit gone-cards)
                       (count-avail 1 suit gone-cards))]
                   [else
                    ;; Might get top or bottom?
                    (+ (count-avail (sub1 (min v1 v2)) suit gone-cards)
                       (count-avail (add1 (max v1 v2)) suit gone-cards))])]
            [(= delta 2)
             ;; Might get the middle one?
             (let ([middle (quotient (+ v1 v2) 2)])
               (count-avail middle suit gone-cards))]
            [else 0]))))

;; The procedure implements the machine's card-drawing choice
(define (machine-wants-card? machine-hand card gone-cards)
  ;; Simple strategy: the machine wants the card if taking it will make the
  ;; gin-size of its hand increase, or if taking it will not make the gin-size
  ;; decrease but will increase the pair rating.
  (let* ([orig-size (gin-size machine-hand)]
         [new-hand (remq (machine-discard (cons card machine-hand) gone-cards) 
                         (cons card machine-hand))]
         [new-size (gin-size new-hand)])
    (or (> new-size orig-size)
        (and (= new-size orig-size)
             (> (pair-rating new-hand gone-cards) 
                (pair-rating machine-hand gone-cards))))))

;; The procedure implements the machine's discard choice
(define (machine-discard machine-hand gone-cards)
  ;; Discard the card that leaves the hand with the largest gin-size.  If
  ;; multiple cards leave the same largest gin size, pick card leaving the best
  ;; pair rating.
  (let* ([gin-size-card-pairs
          (map (lambda (card) (cons (gin-size (remq card machine-hand)) card))
               machine-hand)]
         [most (apply max (map car gin-size-card-pairs))]
         [best (filter (lambda (x) (= most (car x))) gin-size-card-pairs)]
         [best-cards (map cdr best)]
         [rating-card-pairs
          (map (lambda (card)
                 (cons (pair-rating (remq card machine-hand) gone-cards) card))
               best-cards)]
         [most (apply max (map car rating-card-pairs))]
         [best (filter (lambda (x) (= most (car x))) rating-card-pairs)])
    (cdar best)))

;; ;;;;;; Game Loop  ;;;;;;;;

;; This procedure finalizes the display when the game is over
(define (end-of-game why)
  (send t set-status
        (format
         "~aGame over. ~a."
         why
         (cond [(and (gin? you-hand) (gin? machine-hand)) "Tie"] ; only on deal
               [(gin? you-hand) "You win"]
               [else "Opponent wins"])))
  (send t cards-face-up machine-hand))

;; Deck empty? Shuffle the discard pile (preserving the top discard)
(define (check-empty-deck)
  (when (null? deck)
    (set! deck (shuffle-list (cdr discards) 7))
    (set! discards (list (car discards)))
    (send t cards-face-down deck)
    (send t stack-cards deck)
    (send t move-cards deck draw-x draw-y)))

;; Check for starge start...
(if (or (gin? you-hand) (gin? machine-hand))
  ;; Someone was delt gin - game over
  (end-of-game "Dealt gin. ")

  ;; This is the main game loop
  (let loop ()
    (check-empty-deck)

    ;; Your turn; you can select the top card on the deck or on the discard
    ;; pile
    (send (car discards) user-can-move #t)
    (send (car discards) snap-back-after-move #t)
    (send (car deck) user-can-move #t)
    (send (car deck) snap-back-after-move #t)
    (send t set-status YOUR-TURN-MESSAGE)
    (let ([something-happened (make-semaphore 0)])
      ;; Set callback in your region to receive the deck/discard card
      (set-region-callback!
       you-region
       (lambda (cards)
         (let ([card (car cards)])
           ;; Adjust the deck, discard pile, and your hand
           (if (eq? card (car discards))
             (set! discards (cdr discards))
             (set! deck (cdr deck)))
           (set! you-hand (cons card you-hand))
           (send t card-face-up card))

         ;; Action done - clean up and move on
         (semaphore-post something-happened)
         (unless (null? deck)
           (send (car deck) user-can-move #f)
           (send (car deck) home-region #f))
         (unless (null? discards)
           (send (car discards) user-can-move #f)
           (send (car discards) home-region #f))
         (set-region-callback! you-region #f)
         (set-region-interactive-callback! you-region #f)))
      ;; Interactive callback: change home of card if region is hilited.  As a
      ;; result, the card snaps to where you put it instead of back to its
      ;; original position.
      (set-region-interactive-callback!
       you-region
       (lambda (on? cards)
         (send (car cards) snap-back-after-move (not on?))
         (send (car cards) home-region (and on? you-region))))
      ;; Wait for action (the action itself is handled by the callback
      ;; for you-region)
      (yield something-happened))

    ;; Time for you to discard something
    (send t set-status DISCARD-MESSAGE)
    (let ([something-happened (make-semaphore 0)])
      ;; This time, the discard pile is the active region
      (set-region-callback!
       discard-region
       (lambda (cards)
         (let ([card (car cards)])
           ;; Adjust the discard pile and your hand
           (set! you-hand (remq card you-hand))
           (set! discards (cons card discards))
           (send t card-to-front card)
           (send t move-card card discard-x discard-y)

           ;; Discarded card is now relatively immobile
           (send card user-can-move #t)
           (send card home-region #f))

         ;; Action done - clean up and move on
         (semaphore-post something-happened)
         (set-region-callback! discard-region #f)
         (set-region-interactive-callback! discard-region #f)))
      ;; Interactive callback: change home of card if region is hilited,
      ;; so the card you drag snaps to the discard pile.
      (set-region-interactive-callback!
       discard-region
       (lambda (on? cards)
         (send (car cards) home-region (if on? discard-region you-region))))
      ;; Wait for action
      (yield something-happened))

    (if (gin? you-hand)
      ;; Game over
      (end-of-game "")

      ;; Keep going; machine's turn
      (begin
        (check-empty-deck)
        ;; Machine picks a card
        (if (machine-wants-card? machine-hand (car discards) (cdr discards))
          (let ([card (car discards)])
            (set! discards (cdr discards))
            (send t card-face-down card)
            (send card user-can-move #f)
            (set! machine-hand (cons card machine-hand)))
          (let ([card (car deck)])
            (send t card-to-front card)
            (set! deck (cdr deck))
            (send card user-can-move #f)
            (set! machine-hand (cons card machine-hand))))
        (send t move-cards-to-region machine-hand machine-display-region)

        ;; Machine discards
        (let ([card (machine-discard machine-hand discards)])
          (send t card-face-up card)
          (send t card-to-front card)
          (send t move-card card discard-x discard-y)
          (set! discards (cons card discards))
          (set! machine-hand (remq card machine-hand))
          (send t move-cards-to-region machine-hand machine-display-region))

        (if (gin? machine-hand)
          ;; Game over
          (end-of-game "")

          ;; Next turn
          (loop))))))

))
