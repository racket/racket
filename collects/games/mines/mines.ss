
;; An example implementation of the ever-popular Minesweeper game.

;; The graphics are primitive, but the event-handling is general.  For
;; example, clicking on a tile hilites the tile, but moving the mouse
;; off the tile before releasing the mouse button unhilites the tile
;; and ignores the click.

;;;;;;;;;;;;;;;;; Configuration ;;;;;;;;;;;;;;;;;;

(module mines mzscheme
  
  (require (lib "etc.ss") ; defines build-vector
	   (lib "class.ss")
           (lib "unit.ss")
	   (lib "mred.ss" "mred"))
  
  (provide game-unit)
  
  ;; The game is implemented in a unit so it can be started multiple times
  (define game-unit
    (unit
      (import)
      (export)
      
      (define TILE-HW 24)        ; height/width of a tile
      (define B-WIDTH 16)        ; number of tiles across
      (define B-HEIGHT 16)       ; number of tiles down
      (define THE-BOMB-COUNT 30) ; number of bombs to hide
      
      (define DIGIT-COLOR-NAMES
        ;; 0th is background; 8th is foreground
        (vector "LIGHT GRAY"  "BLUE"  "GREEN"  "RED"  "PURPLE" 
                "ORANGE"  "YELLOW"  "BROWN"  "BLACK"))
      
      (define DIGIT-COLORS
        (build-vector 9 (lambda (i)
                          (send the-color-database find-color 
                                (vector-ref DIGIT-COLOR-NAMES i)))))
      
      (define BG-COLOR (vector-ref DIGIT-COLORS 0))
      (define FG-COLOR (vector-ref DIGIT-COLORS 8))
      (define EXPLODE-COLOR (send the-color-database find-color "RED"))
      
      (define BG-PEN (make-object pen% BG-COLOR 1 'solid))
      (define FG-PEN (make-object pen% FG-COLOR 1 'solid))
      
      ;; A general function for looping over numbers:
      (define (step-while first test until  f accum init)
        (let loop ([n first][a init])
          (if (test n until)
              (loop (add1 n) (accum a (f n)))
              a)))
      
      ;; ;;;;;;;;;;;;;;; Tiles ;;;;;;;;;;;;;;;;;;
      
      ;; Class for a tile object
      (define tile:plain%
        (class object%
          (define state 'covered)    ; 'covered, 'flagged, 'semi-flagged, or 'uncovered
          (define neighbor-bomb-count 0) ; 0 to 8
          
          (public*
           [set-state
            (lambda (newstate)
              (set! state newstate))]
           [get-state
            (lambda ()
              state)]
           [set-neighbor-bomb-count
            (lambda (c)
              (set! neighbor-bomb-count c))]
           [get-neighbor-bomb-count
            (lambda ()
              neighbor-bomb-count)]
           [draw-text-tile
            (lambda (dc x y w h hilite? border? str color)
              (if border?
                  (send dc set-pen FG-PEN)
                  (send dc set-pen BG-PEN))
              (send dc draw-rectangle x y w h)
              (when hilite?
                (send dc draw-rectangle 
                      (add1 x) (add1 y) 
                      (- w 2) (- h 2)))
              (when str
                (send dc set-text-foreground (or color FG-COLOR))
                ;; Draw text centered in the tile's box:
                (let-values ([(tw th d a) (send dc get-text-extent str)])
                  (send dc draw-text str 
                        (+ x (/ (- w tw) 2))
                        (+ y (/ (- h th) 2))))))]
           [draw
            (lambda (dc x y w h hilite?)
              (case state
                [(covered) (draw-text-tile dc x y w h hilite? #t #f #f)]
                [(flagged) (draw-text-tile dc x y w h hilite? #t "X" #f)]
                [(semi-flagged) (draw-text-tile dc x y w h hilite? #t "?" #f)]
                [(uncovered) (draw-text-tile 
                              dc x y w h #f #f
                              (if (zero? neighbor-bomb-count)
                                  #f
                                  (number->string neighbor-bomb-count))
                              (vector-ref DIGIT-COLORS neighbor-bomb-count))]))])
          
          (super-instantiate ())))
      
      ;; Class for a tile with a bomb underneath
      (define tile:bomb%
        (class tile:plain%
          (inherit get-state draw-text-tile)
          (define explode-source? #f) ; draw this bomb as the one that exploded?
          
          (public*
           [set-explode-source
            (lambda (s?)
              (set! explode-source? s?))])
          
          (override*
           [draw
            (lambda (dc x y w h hilite?)
              (if (eq? (get-state) 'uncovered)
                  (draw-text-tile dc x y w h #f #f "*"
                                  (and explode-source? EXPLODE-COLOR))
                  (super draw dc x y w h hilite?)))])
          
          (super-instantiate ())))
      
      (define (is-bomb? x)
        (is-a? x tile:bomb%))
      
      ;; ;;;;;;;;;;;;;;; Board Operations ;;;;;;;;;;;;;;;;;;
      ;; A board is a vector of vectors of tiles
      
      (define board #f) ; initialized by calling make-board!
      
      (define (get-tile x y)
        (vector-ref (vector-ref board x) y))
      
      (define (set-tile! x y t)
        (vector-set! (vector-ref board x) y t))
      
      (define (do-surrounding x y accum start default f)
        (step-while -1 <= 1
                    (lambda (dx)
                      (step-while -1 <= 1
                                  (lambda (dy)
                                    (if (and (not (and (zero? dx) (zero? dy)))
                                             (< -1 (+ x dx) B-WIDTH)
                                             (< -1 (+ y dy) B-HEIGHT))
                                        (f dx dy)
                                        default))
                                  accum start))
                    accum start))
      
      (define (count-surrounding-bombs x y)
        (do-surrounding 
         x y + 0 0
         (lambda (dx dy)
           (if (is-bomb? (get-tile (+ x dx) (+ y dy)))
               1
               0))))
      
      (define (for-each-tile f)
        (step-while 0 < B-WIDTH
                    (lambda (x)
                      (step-while 0 < B-HEIGHT
                                  (lambda (y)
                                    (f (get-tile x y) x y))
                                  void (void)))
                    void (void)))
      
      (define (make-board!)
        ;; Create the board
        (set! board
              (build-vector B-WIDTH
                            (lambda (i)
                              (build-vector B-HEIGHT
                                            (lambda (j)
                                              (make-object tile:plain%))))))
        ;; Randomly insert bombs
        (let loop ([n THE-BOMB-COUNT])
          (unless (zero? n)
            (let rloop ()
              (let* ([x (random B-WIDTH)]
                     [y (random B-HEIGHT)]
                     [t (get-tile x y)])
                (if (is-a? t tile:bomb%)
                    (rloop)
                    (begin
                      (set-tile! x y (make-object tile:bomb%))
                      (loop (sub1 n))))))))
        ;; Set surrounding-bomb counts for each tile:
        (for-each-tile (lambda (t x y)
                         (send t
                               set-neighbor-bomb-count
                               (count-surrounding-bombs x y)))))
      
      ;; ;;;;;;;;;;;;;;; Graphic Interface ;;;;;;;;;;;;;;;;;;
      
      ;; Make a frame:
      (define frame (instantiate
                        (class frame% 
                          (augment*
                           [on-close ; stop the timer, in case it's running
                            (lambda ()
                              (send board-canvas stop-timer)
			      (inner () on-close))])
                          (super-instantiate ()))
                      ("Minesweeper") 
                      [style '(no-resize-border)]))
      
      ;; Make the row of controls at the top of the frame:
      (define panel (make-object horizontal-panel% frame))
      (send panel stretchable-height #f)
      (define (make-centering-pane parent)
        (let ([p (make-object vertical-pane% parent)])
          (send p set-alignment 'center 'center)
          p))
      
      (define time-display (make-object message% "Time: 00000" (make-centering-pane panel)))
      (make-object button% "Reset" (make-centering-pane panel)
        (lambda (b e) (send board-canvas reset)))
      (define count-display (make-object message% "Count: 000" (make-centering-pane panel)))
      
      (define (set-time t)
        (send time-display set-label (string-append "Time: " (number->string t))))
      (define (set-count c)
        (send count-display set-label (string-append "Bombs: " (number->string c))))
      
      ;; Most of the work is in this class, which extends the basic canvas
      ;;  class for drawing the Minesweeper board and handling clicks.
      (define board-canvas%
        (class canvas% 
          (init frame)
          (inherit get-dc min-client-width min-client-height 
                   stretchable-width stretchable-height)
          
          (define clicking #f)    ; #t => click in progress
          (define clicking-x 0)   ; x position of click in progress
          (define clicking-y 0)   ; y position of click in progress
          (define ready? #t)      ; #t => accept clicks
          (define start-time #f)  ; time of first click
          (define elapsed-time 0) ; seconds since first click
          (define timer #f)       ; a timer that updates elapsed-time
          (define bomb-count THE-BOMB-COUNT) ; number of bombs minus the number of flags
          (define cover-count (* B-HEIGHT B-WIDTH)) ; number of uncovered tiles
          
          (public*
           [stop-timer      ; stop the clock
            (lambda ()
              (when timer
                (send timer stop)
                (set! timer #f)))]
           [start-timer     ; start the clock
            (lambda ()
              (set! start-time (current-seconds))
              (set! timer
                    (make-object
                        (class timer% ()
                          (override*
                           [notify
                            (lambda ()
                              (let ([e (- (current-seconds) start-time)])
                                (when (> e elapsed-time)
                                  (set! elapsed-time e)
                                  (set-time e))))])
                          (super-instantiate ()))))
              (send timer start 100 #f))] ; check time roughly every .1 secs
           [end-of-game     ; stop the game
            (lambda (win?)
              (stop-timer)
              (set! ready? #f)
              (set! start-time #f)
              (unless win?
                (show-all-bombs))
              (set-count THE-BOMB-COUNT))]
           [explode         ; stop the game because the player hit a bomb
            (lambda ()
              (end-of-game #f))]
           [win             ; stop the game because the player won
            (lambda ()
              (end-of-game #t))]
           [reset           ; quit the current game and reset the board
            (lambda ()
              (stop-timer)
              (set! ready? #t)
              (set! start-time #f)
              (set! elapsed-time 0)
              (set! cover-count (* B-HEIGHT B-WIDTH))
              (send dc clear)
              (set-time 0)
              (set! bomb-count THE-BOMB-COUNT)
              (set-count THE-BOMB-COUNT)
              (make-board!)
              (on-paint))]
           [show-all-bombs  ; show the location of each bomb (after end-of-game)
            (lambda ()
              (for-each-tile (lambda (t x y)
                               (when (is-bomb? t)
                                 (change-state t (send t get-state) 'uncovered #f)
                                 (paint-one t x y)))))]
           [autoclick-surrounding   ; autoclick tiles (after a 0 tile is uncovered)
            (lambda (x y)
              (do-surrounding 
               x y void (void) (void)
               (lambda (dx dy)
                 (let* ([x2 (+ x dx)]
                        [y2 (+ y dy)]
                        [t (get-tile x2 y2)]
                        [state (send t get-state)]
                        [nc (send t get-neighbor-bomb-count)])
                   (unless (eq? state 'uncovered)
                     (change-state t state 'uncovered #t)
                     (paint-one t x2 y2)
                     (when (zero? nc)
                       (autoclick-surrounding x2 y2)))))))]
           [change-state         ; update counters after a tile changes
            (lambda (t old-state new-state update-count?)
              (send t set-state new-state)
              (when (and update-count? (not (eq? new-state old-state)))
                (when (eq? new-state 'uncovered)
                  (set! cover-count (sub1 cover-count)))
                (when (eq? old-state 'uncovered)
                  (set! cover-count (add1 cover-count)))
                (when (eq? new-state 'flagged)
                  (set! bomb-count (sub1 bomb-count))
                  (set-count bomb-count))
                (when (eq? old-state 'flagged)
                  (set! bomb-count (add1 bomb-count))
                  (set-count bomb-count))))]
           [do-select            ; handle a click on a tile
            (lambda (x y flag?)
              (let* ([t (get-tile x y)]
                     [state (send t get-state)]
                     [new-state
                      (case state
                        [(covered)
                         (if flag? 'flagged 'uncovered)]
                        [(flagged)
                         (if flag? 'semi-flagged state)]
                        [(semi-flagged)
                         (if flag? 'covered 'uncovered)]
                        [else state])]
                     [nc (send t get-neighbor-bomb-count)]
                     [new-uncover? (and (eq? new-state 'uncovered)
                                        (not (eq? state 'uncovered)))]
                     [bomb? (is-bomb? t)])
                (change-state t state new-state #t)
                (when (and new-uncover? bomb?)
                  (send t set-explode-source #t))
                (paint-one t x y)
                (when new-uncover?
                  (if bomb?
                      (explode)
                      (begin
                        (when (zero? nc)
                          (autoclick-surrounding x y))))
                  (when (and ready? (= cover-count THE-BOMB-COUNT))
                    (win)))))]
           [paint-one        ; draw one tile
            (lambda (t x y)
              (let ([xloc (* x TILE-HW)]
                    [yloc (* y TILE-HW)])
                (send t draw dc xloc yloc TILE-HW TILE-HW
                      (eq? t clicking))))])
          
          (override*
           [on-event        ; handle a click
            (lambda (e)
              (when ready?
                (unless start-time    ; if the timer's not running, start it
                  (when (send e button-down?)
                    (start-timer)))
                ;; Find the time for an (x,y) pixel position in the canvas
                (let* ([x (quotient (inexact->exact (floor (send e get-x))) 
                                    TILE-HW)]
                       [y (quotient (inexact->exact (floor (send e get-y)))
                                    TILE-HW)]
                       [t (if (and (< -1 x B-WIDTH)
                                   (< -1 y B-HEIGHT))
                              (get-tile x y)
                              #f)]) ; not a tile
                  (cond
                    [(and clicking (or (not (eq? t clicking))
                                       (not (or (send e button-up?)
                                                (send e dragging?)))))
                     ;; We're already in the middle of a click, and the mouse
                     ;;   was moved. Paint the tile to show whether releasing the
                     ;;   mouse button selects the tile.
                     (let ([old clicking])
                       (set! clicking #f)
                       (paint-one old clicking-x clicking-y))]
                    [(and t
                          (not (eq? (send t get-state) 'uncovered))
                          (or (send e button-down?)
                              (and (send e dragging?)
                                   (= x clicking-x)
                                   (= y clicking-y))))
                     ;; Start a click on a covered tile
                     (set! clicking t)
                     (set! clicking-x x)
                     (set! clicking-y y)
                     (paint-one t x y)]
                    [(send e button-down?) 
                     ;; fallthough to here => clicking, but not on a tile
                     (set! clicking-x -1)]
                    [(and clicking (send e button-up?))
                     ;; User released the button
                     (set! clicking #f)
                     (do-select x y (send e button-up? 'right))]
                    [else 'ok]))))]
           [on-paint       ; refresh the board
            (lambda ()
              (for-each-tile (lambda (tile x y) (paint-one tile x y))))])
          
          (super-instantiate (frame))
          
          ;; Make canvas size always match the board size:
          (min-client-width (* TILE-HW B-WIDTH))
          (min-client-height (* TILE-HW B-HEIGHT))
          (stretchable-width #f)
          (stretchable-height #f)
          
          (define dc (get-dc))
          
          (reset)  ; initialize the game
          (send dc set-text-background BG-COLOR)
          (send dc set-brush (send the-brush-list find-or-create-brush 
                                   BG-COLOR 'solid))))
      
      ;; Make the board canvas:
      (define board-canvas (make-object board-canvas% frame))
      
      ;; Show the frame (and handle events):
      (send frame show #t))))
    