;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                          Blackjack
;;
;; The standard rules apply. Specifics:
;;
;;   1 player (not counting the dealer)
;;
;;   4 decks, reshuffled after 3/4 of the cards are used
;;
;;   Dealer stands on soft 17s
;;
;;   Splitting allowed only on the first two cards, and only if they
;;    are equal; 10 and the face cards are all considered equal for
;;    splitting
;;
;;   Doubling allowed on all unsplit hands, not on split hands
;;
;;   No blackjacks after splitting
;;
;;   No surrender
;;
;;   No insurance
;;
;;   No maximum under-21 hand size
;;
;;   Dealer's second card is not revealed if the player busts (or
;;     both halves of a split hand bust)
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang mzscheme

(require games/cards mred mzlib/class mzlib/list mzlib/unit)

(provide game@)
(define game@ (unit (import) (export)

;; Number of decks to use
(define DECK-COUNT 4)

;; Region layout constants
(define MARGIN 10)
(define SUBMARGIN 10)
(define LABEL-H 15)

;; Randomize
(random-seed (modulo (current-milliseconds) 10000))

;; Reshuffle when 3/4 of the deck is used
(define min-deck-size (/ (* DECK-COUNT 52) 4))

;; Set up the table
(define t (make-table "Blackjack" 6 3))
(define status-pane (send t create-status-pane))
(send t add-scribble-button status-pane
      '(lib "games/scribblings/games.scrbl") "blackjack")
(send t show #t)
(send t set-double-click-action #f)
(send t set-button-action 'left 'drag/one)
(send t set-button-action 'middle 'drag/one)
(send t set-button-action 'right 'drag/one)

;; Get table width & height
(define w (send t table-width))
(define h (send t table-height))

;; Build the deck
(define deck
  (let loop ([n DECK-COUNT])
    (if (zero? n)
      null
      (append (make-deck) (loop (sub1 n))))))

;; Card width & height
(define cw (send (car deck) card-width))
(define ch (send (car deck) card-height))

;; Size of buttons
(define BUTTON-HEIGHT 16)
(define BUTTON-WIDTH cw)

;; Cards are not movable
(for-each (lambda (card) (send* card (user-can-move #f) (user-can-flip #f)))
          deck)

;; Set up card regions
(define deck-region
  (make-region MARGIN MARGIN cw ch #f #f))

(define discard-region
  (make-region (- w cw MARGIN) MARGIN cw ch #f #f))

(define dealer-region
  (make-region (+ cw (* 2 MARGIN)) MARGIN
               (- w (* 2 cw) (* 4 MARGIN)) ch
               #f #f))

(define player-region
  (make-region (+ cw (* 2 MARGIN)) (- h (* 2 MARGIN) ch BUTTON-HEIGHT)
               (- w (* 2 cw) (* 4 MARGIN)) ch
               #f #f))

;; In case of split, we need more regions
(define ww (* 3/2 cw))
(define player-2-region
  (make-region MARGIN (region-y player-region)
               (- w ww (* 3 MARGIN)) (region-h player-region)
               #f #f))
(define player-2-wait-region
  (make-region (region-x player-2-region) (region-y player-2-region)
               ww (region-h player-2-region)
               #f #f))
(define player-1-region
  (make-region (- w MARGIN (region-w player-2-region)) (region-y player-2-region)
               (region-w player-2-region) (region-h player-2-region)
               #f #f))
(define player-1-wait-region
  (make-region (- (+ (region-x player-1-region) (region-w player-1-region)) ww)
               (region-y player-1-region)
               ww (region-h player-1-region)
               #f #f))
(define (make-border-region r)
  (define hm (/ MARGIN 2))
  (make-region (- (region-x r) hm) (- (region-y r) hm)
               (+ (region-w r) MARGIN) (+ (region-h r) MARGIN)
               "" #f))
(define player-1-border (make-border-region player-1-region))
(define player-2-border (make-border-region player-2-region))

;; Player buttons
(define (make-button title pos)
  (make-button-region (+ (/ (- w (* 4 BUTTON-WIDTH) (* 3 MARGIN)) 2)
                         (* pos (+ BUTTON-WIDTH MARGIN)))
                      (- h MARGIN BUTTON-HEIGHT)
                      BUTTON-WIDTH BUTTON-HEIGHT
                      title void))
(define hit-button (make-button "Hit" 1))
(define stand-button (make-button "Stand" 2))
(define split-button (make-button "Split" 0))
(define double-button (make-button "Double" 3))

;; Put the cards on the table
(send t add-cards-to-region deck deck-region)

;; Function to compute the normal or minimum value of a card
(define (min-card-value c)
  (let ([v (send c get-value)]) (if (> v 10) 10 v)))

;; Function to compute the value of a hand, counting aces as 1 or 11
;;  to get the highest total possible under 21
(define (best-total l)
  (let* ([ace? (lambda (is?) (lambda (c) (eq? is? (= (send c get-value) 1))))]
         [aces (filter (ace? #t) l)]
         [others (filter (ace? #f) l)]
         [base (apply + (map min-card-value others))])
    (let loop ([l aces][base base])
      (cond [(null? l) base]
            [(<= (+ base (* (length aces) 11)) 21)
             (+ base (* (length aces) 11))]
            [else (loop (cdr l) (add1 base))]))))

;; Function to test whether a hand is a bust
(define (bust? p)
  (> (best-total p) 21))

;; Very simple betting...
(define money 100)
(define (update-money! d)
  (set! money (+ money d))
  (send t set-status (format "You have $~a. (Each bet is $2.)" money)))

;; Let's play!
(let shuffle-loop ()
  ;; Shuffle the cards, none are discarded, yet
  (let* ([deck (shuffle-list deck 7)]
         [discard null]
         [deal (lambda (n)
                 (let deal ([n n])
                   (if (zero? n)
                     null
                     (let ([c (car deck)])
                       (set! deck (cdr deck))
                       (cons c (deal (sub1 n)))))))])
    ;; Put the shuffled deck in place
    (send t move-cards-to-region deck deck-region)
    (send t stack-cards deck)
    ;; Loop rounds over while there's enough cards in the deck
    (let loop ()
      ;; All bets are $2
      (update-money! -2)
      ;; Deal to player
      (let ([p (deal 2)]
            [p2 null] ; in case of splitting
            [double? #f]) ; in case of doubling (flag is needed to adjust money)
        ;; Move the player's cards into place and show them
        (send t move-cards-to-region p player-region)
        (send t cards-face-up p)
        ;; Deal to dealer
        (let ([d (deal 2)])
          ;; Move the dealer's cards into place and show one
          (send t move-cards-to-region d dealer-region)
          (send t card-face-up (car d))
          (let* ([continue (make-semaphore)]
                 ;; Make a button in the center to show results
                 [make-status
                  (lambda (title continue)
                    (let ([r (make-button-region
                              (/ (- w (* 2 cw)) 2)
                              (region-y hit-button)
                              (* 2 cw) BUTTON-HEIGHT
                              title #f)])
                      (set-region-callback! r (lambda ()
                                                (send t remove-region r)
                                                (semaphore-post continue)))
                      r))]
                 ;; Done with hand:
                 [done
                  (lambda (title continue)
                    (send t remove-region hit-button)
                    (send t remove-region stand-button)
                    (send t add-region (make-status title continue)))]
                 ;; Compute winnings (not called for busts by the player)
                 [finish
                  (lambda (p blackjack?)
                    (let ([pt (best-total p)]
                          [dt (best-total d)]
                          [continue (make-semaphore)])
                      (cond
                        [(or (> dt 21) (> pt dt))
                         (update-money! (if blackjack? 5 (if double? 8 4)))
                         (done (if blackjack?
                                 "Blackjack"
                                 "You Win")
                               continue)]
                        [(> dt pt)
                         (done (if blackjack?
                                 "Dealer Blackjack"
                                 "You Lose")
                               continue)]
                        [else (update-money! (if double? 4 2))
                              (done "Push" continue)])
                      (yield continue)))]
                 ;; Done with the first hand of a split
                 [finish-split
                  (lambda (p player-region player-wait-region player-border)
                    (unless (bust? p)
                      (send t move-cards-to-region p player-region)
                      (send t add-region player-border)
                      (finish p #f)
                      (send t remove-region player-border)
                      (send t move-cards-to-region p player-wait-region)))]
                 ;; Player busts
                 [bust (lambda ()
                         (done "Bust" continue))]
                 ;; Bust in one hand of a split
                 [local-bust (lambda ()
                               (let ([cont (make-semaphore)])
                                 (done "Bust" cont)
                                 (yield cont)))]
                 ;; Callback for the hit button; the button's callback is
                 ;; changed for different modes: normal, split part 1, or split
                 ;; part 2
                 [make-hit-callback
                  (lambda (get-p set-p! player-region bust)
                    (lambda ()
                      (send t remove-region double-button)
                      (send t remove-region split-button)
                      (set-p! (append (deal 1) (get-p)))
                      (send t stack-cards (get-p))
                      (send t move-cards-to-region (get-p) player-region)
                      (send t cards-face-up (get-p))
                      ;; Check for bust
                      (when (bust? (get-p)) (bust))))])
            ;; Blackjack by player or dealer?
            (if (or (= 21 (best-total p))
                    (= 21 (best-total d)))
              (begin
                ;; Show the dealers cards...
                (send t cards-face-up d)
                ;; ... and compute the result
                (finish p #t))
              (begin
                ;; Three basic actions are allowed:
                (send t add-region hit-button)
                (send t add-region stand-button)
                (send t add-region double-button)
                ;; Set the callbacks for normal (unsplit) hands
                (set-region-callback!
                 hit-button
                 (make-hit-callback (lambda () p)
                                    (lambda (v) (set! p v))
                                    player-region
                                    bust))
                (set-region-callback!
                 stand-button
                 (lambda () (semaphore-post continue)))
                (set-region-callback!
                 double-button
                 (lambda ()
                   ;; Note the double for adjusting money on a win
                   (set! double? #t)
                   ;; Double the bet
                   (update-money! -2)
                   ;; Deal one more card
                   ((region-callback hit-button))
                   ;; No more cards or actions, but if the player busted, the
                   ;; hit callback has already continued
                   (unless (bust? p) (semaphore-post continue))))
                ;; Split allowed?
                (when (= (min-card-value (car p)) (min-card-value (cadr p)))
                  ;; Yes, we can split. If the player hits the split button, we
                  ;; have to split the cards, deal one more to each split half
                  ;; and adjust the callbacks for hit and stand.  (If aces are
                  ;; split, the round is over.)
                  (send t add-region split-button)
                  (set-region-callback!
                   split-button
                   (lambda ()
                     ;; Double our bet...
                     (update-money! -2)
                     ;; Split the hand
                     (set! p2 (list (cadr p)))
                     (set! p (list (car p)))
                     ;; Move the split halves to the "waiting" area. The active
                     ;; area is reserved for hands that are being played
                     (send t move-cards-to-region p player-1-wait-region)
                     (send t move-cards-to-region p2 player-2-wait-region)
                     ;; Deal one more card to each half and move them into
                     ;; place
                     (set! p (append (deal 1) p))
                     (set! p2 (append (deal 1) p2))
                     (send t stack-cards p)
                     (send t stack-cards p2)
                     (send t move-cards-to-region p player-1-wait-region)
                     (send t move-cards-to-region p2 player-2-wait-region)
                     ;; Show the newly dealt cards
                     (send t flip-cards (list (car p) (car p2)))
                     ;; No more splits, no doubling
                     (send t remove-region split-button)
                     (send t remove-region double-button)
                     ;; Function called when the last split hand is done
                     (let* ([close-split
                             (lambda ()
                               ;; Unhilite the second hand
                               (send t remove-region player-2-border)
                               (send t move-cards-to-region p2 player-2-wait-region)
                               ;; Let the main loop finish up
                               (semaphore-post continue))]
                            ;; Callback to swicth from the first split hand to
                            ;; the second
                            [switch
                             (lambda ()
                               ;; Unhilite the first hand
                               (send t remove-region player-1-border)
                               (send t move-cards-to-region p player-1-wait-region)
                               ;; Hilite the second hand
                               (send t move-cards-to-region p2 player-2-region)
                               (send t add-region player-2-border)
                               ;; Adjust callbacks to operate on the second hand
                               (set-region-callback!
                                hit-button
                                (make-hit-callback (lambda () p2)
                                                   (lambda (v) (set! p2 v))
                                                   player-2-region
                                                   (lambda ()
                                                     (local-bust)
                                                     (close-split))))
                               (set-region-callback!
                                stand-button
                                close-split))])
                       ;; Did we split aces?
                       (if (= 1 (send (cadr p) get-value))
                         ;; Split aces; no more cards
                         (semaphore-post continue)
                         (begin
                           ;; The first of the split hands is ready to go
                           (send t move-cards-to-region p player-1-region)
                           ;; Hilite the first hand
                           (send t add-region player-1-border)
                           ;; Adjust callbacks to work on the first of a split
                           ;; hand
                           (set-region-callback!
                            hit-button
                            (make-hit-callback (lambda () p)
                                               (lambda (v) (set! p v))
                                               player-1-region
                                               (lambda ()
                                                 (local-bust)
                                                 (switch)
                                                 (send t add-region hit-button)
                                                 (send t add-region stand-button))))
                           (set-region-callback! stand-button switch)))))))
                ;; Wait until the player is done
                (yield continue)
                ;; No more player actions; get rid of the buttons
                (send t remove-region hit-button)
                (send t remove-region stand-button)
                (send t remove-region double-button)
                (send t remove-region split-button)
                ;; If all the player's hards are bust, the dealer doesn't do
                ;; anything
                (unless (and (bust? p) (or (null? p2) (bust? p2)))
                  ;; Show the dealer's starting hand
                  (send t card-face-up (cadr d))
                  (let loop ()
                    ;; Hit on 16 or lower, stand on 17 and higher
                    (when (< (best-total d) 17)
                      ;; Hit the dealer
                      (set! d (append (deal 1) d))
                      (send t stack-cards d)
                      (send t move-cards-to-region d dealer-region)
                      (send t cards-face-up d)
                      (loop)))
                  (if (null? p2)
                    ;; Finish normal game (adjusts winnings)
                    (finish p #f)
                    ;; Finish split game (adjusts winnings for each hand)
                    (begin
                      (finish-split p player-1-region player-1-wait-region player-1-border)
                      (finish-split p2 player-2-region player-2-wait-region player-2-border))))))
            ;; Move all the discarded cards to the back
            (unless (null? discard)
              (send t card-to-back (car discard))
              (send t stack-cards discard))
            ;; Discard all the cards we used
            (set! discard (append p p2 d discard))
            (send t cards-face-down discard)
            (send t move-cards-to-region discard discard-region)
            ;; Go again. Check whether we should reshuffle the deck or keep
            ;; going with this one
            (if (< (length deck) min-deck-size)
              (begin (send t move-cards-to-region deck discard-region)
                     (shuffle-loop))
              (loop))))))))

))
