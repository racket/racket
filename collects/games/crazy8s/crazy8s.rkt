
#lang mzscheme

(require games/cards mred mzlib/class mzlib/unit mzlib/etc mzlib/list mzlib/file
         mzlib/async-channel)

;; Player record
(define-struct player (r hand-r  ; region
                         hand))    ; cards

;; Messages
(define YOUR-NAME "You")
(define OPPONENT-X-NAME "Opponent ~a")
(define OPPONENT-NAME "Opponent")
(define YOUR-TURN-MESSAGE "Your turn - discard a ~a or crazy 8, or else ~a")
(define PICK-A-SUIT "Pick a suit")
(define GAME-OVER-YOU-WIN "Game over - you win!")
(define GAME-OVER-STUCK "Game over - no one wins")
(define GAME-OVER "Game over - opponent wins")
(define NEW-GAME "New Game")

;; Region layout constants
(define MARGIN 10)
(define SUBMARGIN 10)
(define LABEL-H 15)
(define BUTTON-HEIGHT 18)
(define PASS-W 40)
(define NEW-GAME-W 80)
(define SEL-WIDTH 32)
(define SEL-HEIGHT 32)

(provide game@)

(define-signature configuration^
  (opponents-count init-hand-size drag-mode? new-game))

;; This unit drives multiple Crazy 8 instances:
(define game@
  (unit (import) (export)

    ;; Configuration
    (define opponents-count (get-preference 'crazy8s:num-opponents (lambda () 1)))
    (define init-hand-size (get-preference 'crazy8s:hand-size (lambda () 7)))
    (define drag-mode? (get-preference 'crazy8s:drag-mode (lambda () #f)))

    (define (start-new-game opponents-count init-hand-size drag-mode?)
      (define orig-eventspace (current-eventspace))
      ;; Procedure for a game to use to start a sibling game
      (define (new-game oc ihs dm?)
        (parameterize ([current-eventspace orig-eventspace])
          (queue-callback
           (lambda ()
             (start-new-game oc ihs dm?)))))
      ;; Start a new game as a child process:
      (parameterize* ([current-custodian (make-custodian)]
                      [exit-handler
                       (lambda (v)
                         (custodian-shutdown-all (current-custodian)))]
                      [current-eventspace (make-eventspace)])
        (queue-callback
         (lambda () (invoke-unit configured-game@ (import configuration^))))))

    ;; Start the initial child game:
    (start-new-game opponents-count init-hand-size drag-mode?)))

;; This unit is for a particular Crazy 8 instance:
(define configured-game@
  (unit (import configuration^) (export)

    ;; Randomize
    (random-seed (modulo (current-milliseconds) 10000))

    ;; ========== GUI ========================================

    ;; Set up the table
    (define t (make-table "Crazy 8s" 8 5.5))

    ;; Add status line and buttons:
    (define status-pane (send t create-status-pane))
    (new button%
         [parent status-pane]
         [label "Options..."]
         [callback (lambda (b e) (configure-dialog))])
    (send t add-scribble-button status-pane
          '(lib "games/scribblings/games.scrbl") "crazy8s")

    ;; The "Options.." button opens a configuration dialog that
    ;;  starts a new game:
    (define (configure-dialog)
      (define d (new dialog%
                     [parent t]
                     [label "Crazy 8 Options"]))
      (define kinds (new radio-box%
                         [label #f]
                         [choices '("1 opponent, 10 cards"
                                    "1 opponent, 7 cards"
                                    "2 opponents, 7 cards"
                                    "3 opponents, 7 cards")]
                         [parent (new group-box-panel%
                                      [parent d]
                                      [label "Players and Cards"])]
                         [callback void]))
      (define drag-mode-check
        (new check-box%
             [parent d]
             [label "Drag cards instead of single-click"]
             [callback void]))
      (define button-panel (new horizontal-pane%
                                [parent d]
                                [alignment '(right center)]))
      (new button%
           [label "Close"]
           [parent button-panel]
           [callback (lambda (b e) (send d show #f))])
      (new button%
           [label "New Game"]
           [parent button-panel]
           [callback (lambda (b e)
                       (let-values ([(oc ihs)
                                     (case (send kinds get-selection)
                                       [(0) (values 1 10)]
                                       [(1) (values 1 7)]
                                       [(2) (values 2 7)]
                                       [(3) (values 3 7)])]
                                    [(dm?) (send drag-mode-check get-value)])
                         (put-preferences
                          '(crazy8s:num-opponents crazy8s:hand-size crazy8s:drag-mode)
                          (list oc ihs dm?)
                          void)
                         (new-game oc ihs dm?)
                         (send d show #f)))]
           [style '(border)])
      (send kinds set-selection
            (case opponents-count
              [(1) (if (= init-hand-size 7) 1 0)]
              [(2) 2]
              [(3) 3]))
      (send drag-mode-check set-value drag-mode?)
      (send d show #t))

    ;; Show the table
    (send t show #t)

    ;; ========== Cards setup ========================================

    ;; Set the default card actions:
    (send t set-double-click-action #f)
    (send t set-button-action 'left 'drag-raise/one)
    (send t set-button-action 'right 'drag/above)

    ;; Get table width & height
    (define w (send t table-width))
    (define h (send t table-height))

    ;; Set up the cards
    (define all-cards (shuffle-list (make-deck) 7))
    (define deck all-cards)
    (define discards null)
    (for-each (lambda (card) (send card user-can-flip #f)) deck)

    ;; We'll need an 8 of each suit for substitutions later
    (define (find-8 suit)
      (ormap (lambda (c)
               (and (= 8 (send c get-value))
                    (eq? suit (send c get-suit))
                    (send c copy)))
             deck))
    (define 8-hearts (find-8 'hearts))
    (define 8-spades (find-8 'spades))
    (define 8-clubs (find-8 'clubs))
    (define 8-diamonds (find-8 'diamonds))

    ;; Function for dealing or drawing cards
    (define (deal n)
      (let loop ([n n][d deck])
        (if (zero? n)
          (begin (set! deck d) null)
          (cons (car d) (loop (sub1 n) (cdr d))))))

    ;; Card width & height
    (define cw (send (car deck) card-width))
    (define ch (send (car deck) card-height))

    ;; Make regions for deck and descard
    (define deck-region
      (make-region (- (/ (- w cw) 2) (/ (+ cw MARGIN) 2))
                   (/ (- h ch) 2)
                   cw ch
                   #f #f))
    (define discard-region
      (make-region (+ (region-x deck-region) cw MARGIN)
                   (region-y deck-region)
                   cw ch
                   #f #f))
    (define discard-target-region
      (make-region (- (region-x discard-region) (/ MARGIN 2))
                   (- (region-y discard-region) (/ MARGIN 2))
                   (+ cw MARGIN) (+ ch MARGIN)
                   "" #f))
    (when drag-mode?
      (send t add-region discard-target-region))

    ;; Put the cards on the table
    (send t add-cards-to-region deck deck-region)

    ;; Make regions for choosing a suit
    (define (make-suit-region x y label card)
      (let ([bm (make-object bitmap%
                             (build-path (collection-path "games" "crazy8s")
                                         "images"
                                         (format "~a.png" label))
                             'unknown/mask)])
        (make-button-region x y
                            SEL-WIDTH SEL-HEIGHT
                            bm
                            ;; The callback for the region sends
                            ;;  a clonable card to the game driver
                            (lambda () (async-channel-put msg card)))))
    (define hearts-region
      (make-suit-region (+ (region-x discard-target-region) cw (* 2 MARGIN))
                        (+ (region-y discard-target-region)
                           (/ (- ch (* 2 SEL-HEIGHT) (/ MARGIN 2)) 2))
                        "heart" 8-hearts))
    (define spades-region
      (make-suit-region (+ (region-x hearts-region) SEL-WIDTH (/ MARGIN 2))
                        (region-y hearts-region)
                        "spade" 8-spades))
    (define clubs-region
      (make-suit-region (region-x hearts-region)
                        (+ (region-y hearts-region) SEL-HEIGHT (/ MARGIN 2))
                        "club" 8-clubs))
    (define diamonds-region
      (make-suit-region (region-x spades-region)
                        (region-y clubs-region)
                        "diamond" 8-diamonds))

    ;; Make the "Pass" button:
    (define pass-button
      (make-button-region (+ (region-x deck-region)
                             (/ (- cw PASS-W) 2))
                          (+ (region-y deck-region)
                             (/ (- ch BUTTON-HEIGHT) 2))
                          PASS-W BUTTON-HEIGHT
                          "Pass" (lambda ()
                                   (async-channel-put msg 'pass))))

    ;; Player region size
    (define pw (/ (- w (* opponents-count MARGIN)) opponents-count))
    (define ph (- (/ (- h ch) 2) (* 2 MARGIN)))

    ;; Define the players with their regions
    (define (make-a-player x y w h lbl)
      (let ([r (make-region x y w h lbl #f)])
        (send t add-region r)
        (make-player
         r
         (make-region (+ x SUBMARGIN) (+ y SUBMARGIN LABEL-H)
                      (- w (* 2 SUBMARGIN))
                      (- h (* 2 SUBMARGIN) LABEL-H)
                      #f #f)
         null)))
    (define players
      (cons
       ;; You
       (make-a-player
        (/ MARGIN 2) (- h ph (/ MARGIN 2))
        (- w MARGIN) ph
        YOUR-NAME)
       (build-list
        opponents-count
        (lambda (delta)
          (make-a-player
           (+ (* (+ pw MARGIN) delta) (/ MARGIN 2)) (/ MARGIN 2)
           pw ph
           (if (= opponents-count 1)
             OPPONENT-NAME
             (format OPPONENT-X-NAME (+ 1 delta))))))))
    (define you (car players))
    (define opponents (cdr players))

    ;; Add the "Clean" and "Sort" buttons:
    (define (sort-hand! card<)
      (let ([sorted (sort (player-hand you) card<)])
        (set-player-hand! you sorted)
        (send t stack-cards sorted)
        (send t move-cards-to-region sorted (player-hand-r you))))
    (define clean-button
      (make-button-region
       (region-x (player-r you))
       (- (region-y (player-r you))
          (+ BUTTON-HEIGHT MARGIN))
       PASS-W BUTTON-HEIGHT
       "Clean" (lambda ()
                 (sort-hand!
                  (lambda (a b)
                    (let-values ([(ax ay) (send t card-location a)]
                                 [(bx by) (send t card-location b)])
                      (> ax bx)))))))
    (send t add-region clean-button)
    (define (remap v)
      ;; So that black and red suits are interleaved
      (case v [(2) 1][(1) 2][else v]))
    (define (card< a b)
      (cond
        [(= 8 (send a get-value))
         (or (not (= 8 (send b get-value)))
             (< (remap (send a get-suit-id)) (remap (send b get-suit-id))))]
        [(= 8 (send b get-value))
         #f]
        [(= (send a get-suit-id) (send b get-suit-id))
         (< (send a get-value) (send b get-value))]
        [else
         (< (remap (send a get-suit-id)) (remap (send b get-suit-id)))]))
    (when drag-mode?
      (send t add-region
            (make-button-region (+ (region-x clean-button) PASS-W MARGIN)
                                (region-y clean-button)
                                PASS-W BUTTON-HEIGHT
                                "Sort" (lambda () (sort-hand! card<)))))

    ;; ========== Game engine ========================================

    ;; Callbacks communicate back to the main loop
    (define msg (make-async-channel))

    ;; Utility: Determine whether a list of cards corresponds to a
    ;; valid discard; return one card or #f
    (define (get-discard-card cs)
      (and (= 1 (length cs))
           (let ([c (car cs)])
             (and (memq c (player-hand you))
                  (or (= (send (car discards) get-value) (send c get-value))
                      (= (send (car discards) get-suit-id) (send c get-suit-id))
                      (= (send c get-value) 8))
                  c))))

    ;; Utility: detect a stuck game
    (define (stuck-game?)
      (and (null? deck)
           (not (ormap (lambda (p)
                         (and (pair? (player-hand p))
                              (ormap (lambda (c) (get-discard-card (list c)))
                                     (player-hand p))))
                       players))))

    ;; Auto-player strategy: Choose which valid card to discard
    (define (pick-to-discard cards)
      (let ([non-8s (filter (lambda (c) (not (= 8 (send c get-value)))) cards)])
        (car (if (null? non-8s) cards non-8s))))

    ;; Auto-player: take a turn
    (define (play-opponent p)
      (let ([suit-id (send (car discards) get-suit-id)]
            [value (send (car discards) get-value)])
        ;; Which cards can we discard?
        (let ([matches (filter (lambda (c)
                                 (or (= suit-id (send c get-suit-id))
                                     (= value (send c get-value))
                                     (= 8 (send c get-value))))
                               (player-hand p))])
          (if (null? matches)
            ;; Can't discard, so draw or pass
            (if (pair? deck)
              ;; Draw
              (begin
                (send t card-to-front (car deck))
                (set-player-hand! p (append (deal 1) (player-hand p)))
                (send t move-cards-to-region (player-hand p) (player-hand-r p))
                (play-opponent p))
              ;; Pass
              (begin
                (send t hilite-region (player-r p))
                (send t pause 0.25)
                (send t unhilite-region (player-r p))
                #t))
            ;; Discard
            (let ([c (pick-to-discard matches)])
              (set-player-hand! p (remq c (player-hand p)))
              (send t flip-card c)
              (send t card-to-front c)
              (send t move-cards-to-region (list c) discard-region)
              (send t move-cards-to-region (player-hand p) (player-hand-r p))
              (set! discards (cons c discards))
              ;; Did we just discard an 8? (And we still have cards?)
              (when (and (= 8 (send (car discards) get-value))
                         (pair? (player-hand p)))
                ;; Pick a suit based on our hand
                (let ([counts
                       (map (lambda (v)
                              (cons v
                                    (length
                                     (filter
                                      (lambda (c)
                                        (and (= v (send c get-suit-id))
                                             (not (= 8 (send c get-value)))))
                                      (player-hand p)))))
                            '(1 2 3 4))])
                  (let ([suit-id
                         ;; Sort based on counts, then pick the first one:
                         (sub1 (caar (sort counts (lambda (a b)
                                                    (> (cdr a) (cdr b))))))])
                    ;; Find the clonable 8 for the chosen suit, and
                    ;; reset the discard
                    (reset-8
                     (list-ref
                      (list 8-clubs 8-diamonds 8-hearts 8-spades)
                      suit-id)))))
              ;; Return #f if this player has just won:
              (pair? (player-hand p)))))))

    ;; Utility: disables cards for "you"
    (define (allow-cards on?)
      (when (pair? deck)
        (send (car deck) user-can-move (and drag-mode? on?)))
      (for-each (lambda (c) (send c user-can-move (and drag-mode? on?)))
                (player-hand you))
      (send t set-single-click-action
            (cond [(and on? (not drag-mode?)) click-card]
                  [drag-mode? void]
                  [else (lambda (x) (bell))]))
      (when (null? deck)
        (if on?
          (send t add-region pass-button)
          (send t remove-region pass-button))))

    ;; Utility: replaces the top discard, which is an 8, with an 8
    ;; of a particular suit (possibly the same).
    (define (reset-8 got-8)
      (unless (eq? (send (car discards) get-suit) (send got-8 get-suit))
        (let ([c (send got-8 copy)])
          (send c user-can-move #f)
          (send t flip-card (car discards))
          (send t add-cards-to-region (list c) discard-region)
          (send t card-to-front c)
          (send t remove-card (car discards))
          (set! discards (cons c (cdr discards)))
          (send t flip-card c))))

    ;; Sub-game: the user just discarded an 8, so pick a suit:
    (define (pick-suit)
      (allow-cards #f)
      (send t add-region hearts-region)
      (send t add-region spades-region)
      (send t add-region clubs-region)
      (send t add-region diamonds-region)
      (send t set-status PICK-A-SUIT)
      ;; Clicking one of these regions returns a clonable 8 card:
      (let ([got-8 (yield msg)]) (reset-8 got-8))
      (send t remove-region hearts-region)
      (send t remove-region spades-region)
      (send t remove-region clubs-region)
      (send t remove-region diamonds-region)
      (allow-cards #t))

    ;; Install interactive callback for discard: accept the card
    ;; (from the player's hand) and release it from its home:
    (set-region-interactive-callback!
     discard-target-region
     (lambda (in? cs)
       (let ([c (get-discard-card cs)])
         (when c
           (send c home-region (if in? #f (player-r you)))))))

    ;; Install final callback for discard: perform the discard
    (set-region-callback!
     discard-target-region
     (lambda (cs)
       (let ([c (get-discard-card cs)])
         (when c (you-discard c)))))

    (define (you-discard c)
      (send c home-region #f)
      (set! discards (cons c discards))
      (set-player-hand! you (remq c (player-hand you)))
      (send t card-to-front c)
      (send t move-cards-to-region (list c) discard-region)
      (send c user-can-move #f)
      (async-channel-put msg 'discard))

    ;; Install interactive callback for hand: accept the card
    ;; (from the deck) and release it from its home:
    (set-region-interactive-callback!
     (player-r you)
     (lambda (in? cs)
       (send (car cs) home-region (if in? (player-r you) deck-region))))

    ;; Install final callback for hand: draw the card:
    (set-region-callback!
     (player-r you)
     (lambda (cs) (let ([c (car cs)]) (you-draw c))))

    (define (you-draw c)
      (send t flip-card c)
      (send c home-region (player-r you))
      (set-player-hand! you (let loop ([l (player-hand you)])
                              (cond [(null? l) (list c)]
                                    [(card< c (car l)) (cons c l)]
                                    [else (cons (car l) (loop (cdr l)))])))
      (deal 1)
      (unless drag-mode?
        (send t stack-cards (player-hand you))
        (send t move-cards-to-region (player-hand you) (player-hand-r you)))
      (async-channel-put msg 'draw))

    (define (click-card c)
      (cond [(memq c deck) (you-draw c)]
            [(memq c (player-hand you))
             (if (get-discard-card (list c)) (you-discard c) (bell))]
            [else (bell)]))

    (unless drag-mode?
      (send t set-single-click-action click-card))

    ;; Run a loop for multiple games
    (let gloop ()

      ;; Card setup: Deal the cards
      (for-each (lambda (player)
                  (set-player-hand! player (sort (deal init-hand-size) card<))
                  (send t stack-cards (player-hand player))
                  (send t move-cards-to-region
                        (player-hand player)
                        (player-hand-r player)))
                players)

      ;; Opponents's cards and deck initially can't be moved
      (for-each (lambda (card) (send card user-can-move #f))
                (append
                 (apply append
                        (map player-hand (if drag-mode? opponents players)))
                 deck))
      ;; Your cards stay home:
      (for-each (lambda (c)
                  (send c home-region (player-r you))
                  (send c user-can-move drag-mode?))
                (player-hand you))

      ;; Initial discard
      ;;  If it's an eight, then shuffle and try again
      (let loop ()
        (when (= 8 (send (car deck) get-value))
          (set! deck (shuffle-list deck 1))
          (send t stack-cards deck)
          (loop)))
      (set! discards (deal 1))
      (send t flip-cards discards)
      (send t move-cards-to-region discards discard-region)

      ;; Show your cards
      (send t flip-cards (player-hand you))

      ;; Run a single-game loop
      (let loop ()
        ;; Ready deck and/or pass button:
        (when (pair? deck)
          (when drag-mode? (send (car deck) user-can-move #t))
          (send (car deck) home-region deck-region))
        (when (null? deck) (send t add-region pass-button))
        ;; Tell the player what to do:
        (send t set-status
              (format YOUR-TURN-MESSAGE
                      (let ([v (send (car discards) get-value)]
                            [suit (case (send (car discards) get-suit)
                                    [(hearts) "heart"]
                                    [(spades) "spade"]
                                    [(diamonds) "diamond"]
                                    [(clubs) "club"])])
                        (if (= v 8)
                          suit
                          (format "~a, ~a,"
                                  suit
                                  (case v
                                    [(1) "ace"]
                                    [(11) "jack"]
                                    [(12) "queen"]
                                    [(13) "king"]
                                    [else v]))))
                      (if (null? deck) "pass" "draw")))
        ;; What for something to happen:
        (let ([what (yield msg)])
          ;; Discarded a crazy 8? (And not as our last card?)
          (when (and (eq? what 'discard)
                     (= 8 (send (car discards) get-value))
                     (pair? (player-hand you)))
            ;; Yes, so pick suit before continuing
            (pick-suit))
          ;; What did we do?
          (case what
            [(draw)
             ;; Go again
             (loop)]
            [(discard pass)
             ;; Hide pass button...
             (when (null? deck) (send t remove-region pass-button))
             ;; ... and run opponents
             (send t set-status "Opponent's turn...")
             (unless (null? (player-hand you))
               (let oloop ([l opponents])
                 (if (null? l)
                   ;; Check for a stuck game here:
                   (unless (stuck-game?) (loop))
                   (when (play-opponent (car l)) (oloop (cdr l))))))])))

      ;; Game over: disable player:
      (allow-cards #f)

      ;; Report result:
      (send t set-status (cond [(null? (player-hand you)) GAME-OVER-YOU-WIN]
                               [(stuck-game?) GAME-OVER-STUCK]
                               [else GAME-OVER]))

      (let ([button
             (make-button-region
              (+ (region-x discard-region) cw (* 2 MARGIN))
              (+ (region-y discard-region) (/ (- ch LABEL-H) 2))
              NEW-GAME-W LABEL-H
              NEW-GAME (lambda () (async-channel-put msg 'new-game)))])
        (send t add-region button)
        (yield msg)
        (send t remove-region button))

      (let ([all (send t all-cards)])
        ;; Gather up cards, with animation
        (let ([flip (filter (lambda (c) (not (send c face-down?))) all)])
          (send t flip-cards flip)
          (send t move-cards-to-region all deck-region))
        ;; Reset all cards (no animation)
        (send t begin-card-sequence)
        (send t remove-cards all)
        (send t add-cards-to-region all-cards deck-region)
        (set! deck (shuffle-list all-cards 7))
        (for-each (lambda (c) (unless (send c face-down?) (send c flip))) deck)
        (send t stack-cards deck)
        (send t end-card-sequence))

      ;; Re-enable player:
      (allow-cards #t)

      (gloop))))
