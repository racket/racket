#|

When playing and it's a user's turn, the history has an extra step at the end that 
corresponds to the unplayed move! that's confusing.

|#

(module admin-gui mzscheme
  (require "gui.rkt"
           "die.rkt"
           "interfaces.rkt"
           "admin.rkt"
           "board.rkt"
           "moves.rkt"
           "rules.rkt"
           "best-players.rkt"
           framework
           mzlib/class
           mzlib/list
           mred)
  
  (provide gui-game%)
  
  ;; move-candidate = (make-move-candidate coordinate move (listof number))
  (define-struct move-candidate (move dice) (make-inspector))
  
  (define-struct past (board color roll) (make-inspector))
  (print-struct #t)
  
  (define gui-game%
    (class* object% (game<%>)
      
      (define the-game (new game%))
      (define/public (register o) (send the-game register o))
      (define/public (start) (send the-game start))
      (define game-observer
        (new
         (class* object% (game-observer<%>)
           (define/public (introduce . x) (void))
           (define/public (taking-turn color roll)
             (queue-callback
              (lambda ()
                (set-box! (cdr (assq color latest-dice)) roll)
                (set! history (append history (list (make-past partial-history color roll))))
                (update-players-dice color roll))))
           (define/public (took-turn color board) 
             (queue-callback
              (lambda ()
                (set! partial-history board)
                (send board-pasteboard set-board board))))
           (define/public (game-over winner-name color)
             (queue-callback
              (lambda ()
                (if winner-name
                    (set-bottom-message (format "~a (~s) won!" winner-name color))
                    (set-bottom-message "everone cheated")))))
           (super-new))))
      (send the-game set-observer game-observer)
      
      (define gui-player%
        (class* object% (player<%>)
          (define color #f)
          (define/public (start-game _color)
            (set! color _color)
            "Human")
          (define/public (do-move board roll)
            (let ([chan (make-channel)])
              (queue-callback
               (lambda ()
                 (enable-gui board roll color chan)))
              (channel-get chan)))
          (define/public (doubles-penalty) 
            (let ([sema (make-semaphore 0)])
              (queue-callback
               (lambda ()
                 (message-box "Parcheesi" 
                              (format 
                               "~a rolled doubles 3 times, so the front-most piece goes back to the start."
                               color))
                 (semaphore-post sema)))
              (semaphore-wait sema)))
          (super-new)))
      
      (define board-pasteboard (new board-pasteboard% (admin-gui this)))
      
      ;; history : (listof past)
      (define history '())
      ;; partial-history : board
      ;; temporary holding variable until a move is complete and history can be updated.
      (define partial-history (new-board))
      
      (define frame (new board-frame% (label "Parcheesi") (style '(metal)) (board-pasteboard board-pasteboard)))
      (define main-hp (new horizontal-panel% (parent frame)))
      (define bottom-panel (new horizontal-panel% 
                                (parent frame)
                                (stretchable-height #f)))
      (define bottom-msg (new message% (parent bottom-panel) (stretchable-width #t) (label "")))
      (define left-vp (new vertical-panel% (parent main-hp) (stretchable-width #f)))
      (define board-ec (new editor-canvas% 
                            (style '(no-hscroll no-vscroll))
                            (parent main-hp)
                            (editor board-pasteboard)
                            (min-height 400)
                            (min-width 400)))
      (define right-vp (new vertical-panel% (parent main-hp) (stretchable-width #f)))
      
      (define blue-player-panel (new vertical-panel% 
                                     (alignment '(center top))
                                     (parent left-vp)
                                     (stretchable-height #f)))
      (new horizontal-panel% (parent left-vp))
      (define yellow-player-panel (new vertical-panel% 
                                       (alignment '(center bottom))
                                       (parent left-vp)
                                       (stretchable-height #f)))
      (define red-player-panel (new vertical-panel% 
                                    (alignment '(center top))
                                    (parent right-vp)
                                    (stretchable-height #f)))
      (new horizontal-panel% (parent right-vp))
      (define green-player-panel (new vertical-panel% 
                                      (alignment '(center bottom))
                                      (parent right-vp)
                                      (stretchable-height #f)))
      
      (define dice '())
      
      (define/private (make-dice color parent)
        (let* ([p (new horizontal-panel% (parent parent) (stretchable-height #f))]
               [die-objects (list (new die% (parent p) (dim? #t))
                                  (new die% (parent p) (dim? #t))
                                  (new die% (parent p) (dim? #t))
                                  (new die% (parent p) (dim? #t)))]
               [dice-in-order
                (case color
                  [(yellow blue) (reverse die-objects)]
                  [else die-objects])])
          (set! dice (cons (list color dice-in-order) dice))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  controlling the players
      ;;
      
      (define/private (add-gui-player-controls color parent-panel)
        (let* ([accept-move-button (new button%
                                        (label "Done")
                                        (parent parent-panel)
                                        (callback (lambda (x y) (accept-move))))]
               [reset-move-button (new button%
                                       (label "Reset")
                                       (parent parent-panel)
                                       (callback (lambda (x y) (reset-button-callback))))]
               [bp-panel (new horizontal-panel% 
                              (parent parent-panel)
                              (stretchable-height #f)
                              (stretchable-width #f))])
          (send accept-move-button enable #f)
          (send reset-move-button enable #f)
          (make-dice color parent-panel)
          (set! gui-player-accept-move-buttons
                (cons (cons color accept-move-button) gui-player-accept-move-buttons))
          (set! gui-player-reset-move-buttons
                (cons (cons color reset-move-button) gui-player-reset-move-buttons))))
      
      (define gui-player-accept-move-buttons '())
      (define gui-player-reset-move-buttons '())
      
      (define/private (go-back)
        (reset-move)
        (set! viewing-index 
              (cond
                [(eq? viewing-index 'latest) (- (length history) 1)]
                [(and this-move-color (zero? viewing-index)) 'latest]  ;; only go to latest when its time to show a move
                [(zero? viewing-index) (- (length history) 1)]
                [else (- viewing-index 1)]))
        (update-gui))
      (define/private (go-forw)
        (reset-move)
        (set! viewing-index (cond
                              [(eq? viewing-index 'latest) 0]
                              [(and this-move-color (= viewing-index (- (length history) 1)))
                               'latest]
                              [(= viewing-index (- (length history) 1))
                               0]
                              [else (+ viewing-index 1)]))
        (update-gui))
      
      (define/private (reset-button-callback)
        (reset-move)
        (update-gui))
      
      (define/private (reset-move)
        (set! current-moves '())
        (set! current-board start-board)
        (set! current-dice start-dice)
        (set! dice-used '()))
      
      (define/private (accept-move)
        (update-players-dice this-move-color start-dice)
        (for-each (lambda (die) (send die set-dim #t))
                  (cadr (assq this-move-color dice)))
        (let ([accept (cdr (assq this-move-color gui-player-accept-move-buttons))]
              [reset (cdr (assq this-move-color gui-player-reset-move-buttons))])
          (send reset enable #f)
          (send accept enable #f))
        (channel-put answer-chan current-moves))
      
      (define current-moves '())
      (define answer-chan #f)
      (define start-board #f)
      (define current-board #f)
      (define start-dice #f)
      (define current-dice '())
      (define dice-used '())
      (define this-move-color #f)
      (define viewing-index 'latest)
      (define latest-dice (list (cons 'green (box null))
                                (cons 'red (box null))
                                (cons 'blue (box null))
                                (cons 'yellow (box null))))
      
      (define/private (enable-gui board roll color _answer-chan)
        (set! current-board board)
        (set! start-board board)
        (set! current-dice roll)
        (set! start-dice roll)
        (set! dice-used '())
        (set! this-move-color color)
        (set! answer-chan _answer-chan)
        (set! current-moves '())
        (set-bottom-message "")
        (update-gui))
      
      (define/public (build-new-board/register-move mc)
        (let ([new-move (move-candidate-move mc)])
          (with-handlers ([exn:bad-move? 
                           (lambda (x) 
                             ;; call this first, since it calls set-bottom-message
                             ;; and we don't want that one to survive
                             (update-gui) 
                             
                             (set-bottom-message (exn-message x)))])
            (let-values ([(new-board total-bonuses) 
                          (make-moves start-board (append current-moves (list new-move)))])
              (set-bottom-message "")
              (set! dice-used (append dice-used (move-candidate-dice mc)))
              (set! current-board new-board)
              (set! current-moves (append current-moves (list new-move)))
              (set! current-dice (foldl remq (append start-dice total-bonuses) dice-used))
              (update-gui)))))
      
      (define/private (update-gui)
        (cond
          [(eq? viewing-index 'latest)
           (dim-dice-except this-move-color)
           (for-each (lambda (latest-dice-line)
                       (if (eq? this-move-color (car latest-dice-line))
                           (update-players-dice this-move-color current-dice)
                           (update-players-dice (car latest-dice-line) (unbox (cdr latest-dice-line)))))
                     latest-dice)
           (send board-pasteboard set-board current-board)
           (let-values ([(highlights move-candidates)
                         (find-roll-coordinates current-board current-dice this-move-color)])
             (send board-pasteboard set-highlighted-squares highlights move-candidates))]
          [(number? viewing-index)
           (let ([past (list-ref history viewing-index)])
             (clear-dice-except (past-color past))
             (update-players-dice (past-color past) (past-roll past))
             (send board-pasteboard set-board (past-board past))
             (send board-pasteboard set-highlighted-squares '() '()))]
          [else (error 'update-gui "unknown viewing index ~e" viewing-index)])
        (reset-accept/move-buttons)
        (reset-forw-back-buttons))
      
      (define/private (reset-accept/move-buttons)
        (when this-move-color
          (let ([accept (cdr (assq this-move-color gui-player-accept-move-buttons))]
                [reset (cdr (assq this-move-color gui-player-reset-move-buttons))])
            (send reset enable (and (eq? viewing-index 'latest) (not (null? current-moves))))
            (send accept enable 
                  (and (eq? viewing-index 'latest) 
                       (with-handlers ([exn:bad-move? (lambda (x) 
                                                        (set-bottom-message 
                                                         (string-append 
                                                          (format "~a is not done: " this-move-color)
                                                          (exn-message x)))
                                                        #f)])
                         (take-turn this-move-color start-board start-dice current-moves)
                         #t))))))
      
      (define/private (reset-forw-back-buttons)
        (send forw enable (not (null? history)))
        (send back enable (not (null? history))))
      
      (define/private (set-bottom-message msg) (send bottom-msg set-label msg))
      
      (define/private (dim-dice-except color)
        (for-each (lambda (die-roll) 
                    (cond
                      [(eq? (car die-roll) color)
                       (for-each (lambda (die) (send die set-dim #f)) (cadr die-roll))]
                      [else 
                       (for-each (lambda (die) (send die set-dim #t)) (cadr die-roll))]))
                  dice))
      
      (define/private (clear-dice-except color)
        (for-each (lambda (die-roll) 
                    (cond
                      [(eq? (car die-roll) color)
                       (for-each (lambda (die) (send die set-dim #f)) (cadr die-roll))]
                      [else 
                       (for-each (lambda (die) (send die set-digit #f)) (cadr die-roll))]))
                  dice))
      
      (define/private (update-players-dice color roll)
        (let loop ([roll roll]
                   [dice (cadr (assq color dice))])
          (cond
            [(null? dice) (void)]
            [(null? roll) (send (car dice) set-digit #f)
                          (loop roll (cdr dice))]
            [else (send (car dice) set-digit (car roll))
                  (loop (cdr roll) (cdr dice))])))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  player choice gui
      ;;

      (define/private (index->player i) 
        (case i
          [(0) best-player%]
          [(1) polite-player%]
          [(2) reckless-player%]
          [(3) gui-player%]
          [else
           (message-box "Parcheesi" (format "index->player got ~s" i))
           gui-player%]))
      
      (define players (vector 'unfilled-in-players-array
                              'unfilled-in-players-array
                              'unfilled-in-players-array
                              'unfilled-in-players-array))
      
      (define/private (add-choose-player-controls color parent-panel init-selection)
        (let* ([color-order '((green . 0) (red . 1) (blue . 2) (yellow . 3))]
               [color-index (cdr (assq color color-order))])
          (vector-set! players color-index (index->player init-selection))
          (new radio-box%
               (parent parent-panel)
               (selection init-selection)
               (label #f)
               (choices '("Amazing Grace"
                          "Polite Polly"
                          "Reckless Renee"
                          "You"))
               (callback
                (lambda (rb y)
                  (vector-set! players
                               color-index
                               (index->player
                                (send rb get-selection))))))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  put all the gui elements together
      ;;
      
      (define/private (make-player-control-panel parent color ah aw init-selection)
        (let* ([parent 
                (new panel:single% 
                     (stretchable-height #f)
                     (parent parent))]
               [choose-player-panel (new vertical-panel% 
                                         (parent parent)
                                         (style '(border))
                                         (alignment `(,aw ,ah))
                                         (stretchable-width #f)
                                         (stretchable-height #f))]
               [control-player-panel (new vertical-panel% 
                                          (parent parent)
                                          (style '(border))
                                          (alignment `(,aw ,ah))
                                          (stretchable-width #f)
                                          (stretchable-height #f))])
          (add-gui-player-controls color control-player-panel)
          (add-choose-player-controls color choose-player-panel init-selection)
          (list color parent choose-player-panel control-player-panel)))
      
      (define gui-player-control-panels
        (list (make-player-control-panel green-player-panel 'green 'top 'left 0)
              (make-player-control-panel red-player-panel 'red 'bottom 'left 1)
              (make-player-control-panel yellow-player-panel 'yellow 'top 'right 2)
              (make-player-control-panel blue-player-panel 'blue 'bottom 'right 3)))
      
      (define/private (get-player-panel color i)
        (let ([e (assq color gui-player-control-panels)])
          (unless e
            (error 'get-player-panel "bad color ~e" color))
          (list-ref e i)))
      (define/private (get-player-parent-panel c) (get-player-panel c 1))
      (define/private (get-choose-player-panel c) (get-player-panel c 2))
      (define/private (get-control-player-panel c) (get-player-panel c 3))

      (define sbf-panel (new panel:single% 
                             (parent bottom-panel) 
                             (stretchable-width #f)
                             (stretchable-height #f)
                             (alignment '(right center))))
      
      (define start-button (new button%
                                (label "Start Game")
                                (parent sbf-panel)
                                (callback
                                 (lambda (x y)
                                   (for-each (lambda (color)
                                               (send (get-player-parent-panel color)
                                                     active-child
                                                     (get-control-player-panel color)))
                                             '(red blue green yellow))
                                   (send sbf-panel active-child bf-panel)
                                   (start-game)))))
      (define bf-panel (new horizontal-panel% (parent sbf-panel) (stretchable-width #f) (stretchable-height #f)))
      (define back (new button%
                        (label "<")
                        (parent bf-panel)
                        (callback (lambda (x y) (go-back)))))
      (define forw (new button%
                        (label ">")
                        (parent bf-panel)
                        (callback (lambda (x y) (go-forw)))))
      (define rules-button (new button% (parent bottom-panel) (label "Rules") (callback (lambda (x y) (show-rules)))))

      (super-new)
      (send frame show #t)
      
      ;; start the game
      (define/private (start-game)
        (for-each (lambda (player%) (send the-game register (new player%))) (vector->list players))
        (thread (lambda () (send the-game start))))))
  
  (define board-pasteboard%
    (class pasteboard%
      (inherit get-admin get-view-size)
      (init-field admin-gui)
      (init-field [board (new-board)])
      
      (define/public (set-board _board)
        (set! board _board)
        (frame-size-changed))
      
      ;; highlighted-squares : (listof (cons coordinate (listof coordinate)))
      ;; the first element in the list is the key and the rest are
      ;; squares that should be highlighted when the cursor moves over
      ;; the key square
      (define highlighted-squares '())
      ;; move-candidates : (listof (cons coordinate (listof move-candidate)))
      (define move-candidates '())
      (define/public (set-highlighted-squares hs mcs)
        (set! highlighted-squares hs)
        (set! move-candidates mcs))
      
      ;; current-highlight : (listof coordinate)
      (define current-highlight '())
      (define current-mouse #f)
      
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (let ([admin (get-admin)])
          (when admin
            (let-values ([(w h) (get-size)])
              (let ([pen (send dc get-pen)]
                    [brush (send dc get-brush)])
                (when before? (draw-board board dc w h dx dy #f))
                (unless before? (draw-highlighted-squares dc dx dy w h))
                (send dc set-pen pen)
                (send dc set-brush brush))))))
      
      (define/private (draw-highlighted-squares dc dx dy w h)
        (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
        (send dc set-brush (send the-brush-list find-or-create-brush "black" 'hilite))
        (for-each (lambda (highlighted-offset)
                    (let-values ([(x y cw ch) (coordinate->xywh highlighted-offset w h)])
                      (send dc draw-rectangle (+ dx x) (+ dy y) cw ch)))
                  current-highlight))
      
      (inherit dc-location-to-editor-location)
      (define/override (on-event event)
        (let-values ([(w h) (get-size)])
          (cond
            [(and (send event button-up?)
                  (find-next-selected-snip #f))
             =>
             (lambda (snip)
               (super on-event event)
               (let ([pawn-id (send snip get-pawn-id)])
                 (let-values ([(x y) (dc-location-to-editor-location (send event get-x) (send event get-y))])
                   (let* ([potential-move-line
                           (memf (lambda (mcl) (in-coord? (car mcl) x y w h))
                                 move-candidates)])
                     (if potential-move-line
                         (let* ([potential-moves (cdar potential-move-line)]
                                [new-move (ormap (lambda (mc)
                                                   (and (= pawn-id (get-move-id (move-candidate-move mc)))
                                                        mc))
                                                 potential-moves)])
                           (if new-move
                               (send admin-gui build-new-board/register-move new-move)
                               (reset-snips)))
                         (reset-snips)))))
               (update-current-mouse (get-mouse-coordinate event w h)))]
            [else
             (update-current-mouse (get-mouse-coordinate event w h))
             (super on-event event)])))
      
      (inherit find-next-selected-snip)
      (define/private (get-mouse-coordinate event w h)
        (cond
          [(and (send event dragging?)
                (find-next-selected-snip #f))
           =>
           (lambda (snip) (send snip get-coord))]
          [else
           (let-values ([(x y) (dc-location-to-editor-location (send event get-x) (send event get-y))])
             (let loop ([highlights highlighted-squares])
               (cond
                 [(null? highlights) #f]
                 [else
                  (let* ([highlight-list (car highlights)]
                         [highlight (car highlight-list)])
                    (let-values ([(sx sy cw ch) (coordinate->xywh highlight w h)])
                      (if (and (<= sx x (+ sx cw))
                               (<= sy y (+ sy ch)))
                          highlight
                          (loop (cdr highlights)))))])))]))
      
      (inherit invalidate-bitmap-cache)
      (define/private (update-current-mouse mse)
        (unless (equal? current-mouse mse)
          (set! current-mouse mse)
          (set! current-highlight 
                (if mse 
                    (let ([ent (assoc current-mouse highlighted-squares)])
                      (if ent 
                          (cdr ent)
                          '()))
                    '()))
          (invalidate-bitmap-cache)))
      
      
      (inherit insert begin-edit-sequence end-edit-sequence find-first-snip set-min-width set-min-height)
      (define/public (frame-size-changed) (reset-snips))
      (define/private (reset-snips)
        (begin-edit-sequence)
        (let loop ([s (find-first-snip)])
          (when s
            (let ([n (send s next)])
              (send s release-from-owner)
              (loop n))))
        (let-values ([(w h) (get-size)])
          (let ([pawn-size (get-piece-size w h)])
            (set-min-width w)
            (set-min-height h)
            (for-each-piece/position 
             board w h
             (lambda (pawn x y coord)
               (insert (new coordinate-snip%
                            (color (pawn-color pawn))
                            (id (pawn-id pawn))
                            (coord coord)
                            (w pawn-size)
                            (h pawn-size))
                       x 
                       y)))))
        (end-edit-sequence))
      
      (define/private (get-size)
        (let ([wb (box 0)]
              [hb (box 0)])
          (get-view-size wb hb)
          (values (max 10 (- (unbox wb) 3))
                  (max 10 (- (unbox hb) 3)))))
      
      (super-new)
      (inherit set-selection-visible)
      (set-selection-visible #f)))
  
  (define coordinate-snip%
    (class snip%
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)]
              [old-smoothing (send dc get-smoothing)])
          (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush (pawn-drawn-color color) 'solid))
          (send dc set-smoothing 'aligned)
          (send dc draw-ellipse x y w h)
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)
          (send dc set-smoothing old-smoothing)))
      (define/override (get-extent dc x y wb hb descent space lspace rspace)
        (set-box/f! wb w)
        (set-box/f! hb h)
        (set-box/f! descent 0)
        (set-box/f! space 0)
        (set-box/f! lspace 0)
        (set-box/f! rspace 0))
      (init-field id coord color w h)
      (define/public (get-coord) coord)
      (define/public (get-pawn-id) id)
      (super-new)
      (inherit set-snipclass)
      (set-snipclass coordinate-snipclass)))
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  (define coordinate-snipclass
    (new
     (class snip-class%
       (define/override (read in)
         (let ([id (send in get-fixed)]
               [coord '???])
           (new coordinate-snip% (id id) (coord coord))))
       (super-new))))
  (send coordinate-snipclass set-classname "parcheesi:coordinate-snipclass")
  (send coordinate-snipclass set-version 1)
  (send (get-the-snip-class-list) add coordinate-snipclass)
  
  (define board-frame%
    (class frame%
      (init-field [board-pasteboard #f])
      (define/public (set-board-pasteboard bpb) (set! board-pasteboard bpb))
      (define/override (on-size w h)
        (when board-pasteboard
          (send board-pasteboard frame-size-changed)))
      (define/override (on-superwindow-show shown?)
        (when board-pasteboard
          (send board-pasteboard frame-size-changed)))
      (super-new)))
  
  (define (in-coord? coord x y w h)
    (let-values ([(cx cy cw ch) (coordinate->xywh coord w h)])
      (and (<= cx x (+ cx cw))
           (<= cy y (+ cy ch)))))
  
  ;; coordinate->xywh : coordinate -> (values number number number number)
  (define (coordinate->xywh loc w h)
    (cond
      [(main-c? loc)
       (let*-values ([(x y horizontal?) (find-main-coordinates (main-c-count loc) w h)]
                     [(cw ch) (get-cell-size horizontal? w h)])
         (values (- x (/ cw 2)) (- y (/ ch 2)) cw ch))]
      [(home-row-c? loc)
       (let*-values ([(x y horizontal?) (find-home-row-coordinates (home-row-c-color loc)
                                                                   (home-row-c-count loc)
                                                                   w
                                                                   h)]
                     [(cw ch) (get-cell-size horizontal? w h)])
         (values (- x (/ cw 2)) (- y (/ ch 2)) cw ch))]
      [(home-c? loc)
       (values (* w 1/3) (* h 1/3) (* w 1/3) (* h 1/3))]
      [(start-c? loc)
       (case (start-c-color loc)
         [(blue) (values 0 0 (* w 1/3) (* h 1/3))]
         [(red) (values (* w 2/3) 0 (* w 1/3) (* h 1/3))]
         [(yellow) (values 0 (* h 2/3) (* w 1/3) (* h 1/3))]
         [(green) (values (* w 2/3) (* h 2/3) (* w 1/3) (* h 1/3))])]
      [else (error 'coordinate->xywh "unk loc ~e" loc)]))
  
  ;; find-roll-coordinates : board
  ;;                         (listof number) 
  ;;                         color 
  ;;                      -> (values (listof (cons coordinate (listof coordinate)))
  ;;                                 (listof (cons coordinate (listof move-candidate))))
  ;; finds the coordinates for the moves that the GUI highlights.
  ;; in the resulting list, the first entry is the place with the
  ;; piece and subsequent entries are places that it can move to.
  (define (find-roll-coordinates board roll color)
    (let-values ([(start-coords start-moves)
                  (find-start-roll-coordinates board roll color)]
                 [(main-coords main-moves)
                  (find-main-roll-coordinates board roll color)]
                 [(home-coords home-moves)
                  (find-home-roll-coordinates board roll color)])
      (values (map (lambda (x) (cons (car x) (eliminate-duplicates (cdr x))))
                   (append start-coords main-coords home-coords))
              (collapse-same-coordinates (append start-moves main-moves home-moves)))))
  
  ;; eliminate-duplicates : (listof X) -> (listof X)
  (define (eliminate-duplicates lst)
    (let ([ht (make-hash-table 'equal)])
      (for-each (lambda (x) (hash-table-put! ht x #t)) lst)
      (hash-table-map ht (lambda (x y) x))))
  
  ;; collapse-same-coordinates : (listof (cons coordinate (listof move-candidate)))
  ;;                          -> (listof (cons coordinate (listof move-candidate)))
  (define (collapse-same-coordinates l)
    (let ([ht (make-hash-table 'equal)])
      (for-each (lambda (pr)
                  (hash-table-put! ht (car pr)
                                   (append (cdr pr)
                                           (hash-table-get ht (car pr) (lambda () '())))))
                l)
      (hash-table-map ht cons)))
  
  ;; like find-roll-coordinates, but only for the main track of the board
  (define (find-home-roll-coordinates board roll color)
    (find-move-coordinates board-home-row-size
                           (lambda (i) (board-home-row-i board color i))
                           roll
                           color
                           (lambda (num) (make-home-row-c num color))
                           make-move-piece-home
                           home-row-add))
  
  ;; like find-roll-coordinates, but only for the main track of the board
  (define (find-main-roll-coordinates board roll color)
    (find-move-coordinates board-main-size
                           (lambda (i) (board-main-i board i))
                           roll
                           color
                           make-main-c
                           make-move-piece-main
                           main-ring-add))
  
  ;; find-move-coordinates : number
  ;;                         (number -> (listof pawn))
  ;;                         number color
  ;;                         (number -> coordinate)
  ;;                         (pawn number number -> move)
  ;;                         (color number number -> (union #f number))
  (define (find-move-coordinates len ref roll color make-coordinate make-move-piece find-end-spot)
    (let loop ([i len]
               [coords null]
               [move-candidates null])
      (cond
        [(= i 0) (values coords move-candidates)]
        [else (let* ([pos (- i 1)]
                     [ent (ref pos)])
                (if (and (pair? ent)
                         (eq? (pawn-color (car ent)) color))
                    (let* ([build-list
                            (lambda (f)
                              (foldl
                               (lambda (die sofar)
                                 (let ([final-spot (find-end-spot color pos die)])
                                   (if final-spot
                                       (cons (f die final-spot) sofar)
                                       sofar)))
                               '()
                               roll))]
                           
                           [new-coord (cons (make-coordinate pos) 
                                            (build-list (lambda (die final-spot) final-spot)))]
                           [new-moves 
                            (build-list (lambda (die final-spot)
                                          (cons final-spot
                                                (map (lambda (pawn)
                                                       (make-move-candidate (make-move-piece pawn pos die)
                                                                            (list die)))
                                                     ent))))])
                      (loop (- i 1)
                            (cons new-coord coords)
                            (append new-moves move-candidates)))
                    (loop (- i 1) coords move-candidates)))])))
  
  (define (main-ring-add color start dist)
    (let ([landed (find-end-spot color start dist)])
      (cond
        [(eq? landed 'too-far) #f]
        [(eq? landed 'home) (make-home-c color)]
        [(eq? (car landed) 'home-row)
         (make-home-row-c (cdr landed) color)]
        [(eq? (car landed) 'main)
         (make-main-c (cdr landed))])))
  
  (define (home-row-add color pos die)
    (let ([final-spot (+ pos die)])
      (cond
        [(< final-spot board-home-row-size)
         (make-home-row-c final-spot color)]
        [(= final-spot board-home-row-size)
         (make-home-c color)]
        [else #f])))
  
  ;; like find-roll-coordinates, but only for the start position of the board
  (define (find-start-roll-coordinates board roll color)
    (let ([available-pawns (filter (lambda (pawn) (eq? (pawn-color pawn) color)) (board-start board))]
          [ent (board-main-i board (get-enter-pos color))])
      (cond
        [(and (has-entering-roll? roll)
              (not (null? available-pawns))
              (or (null? ent)
                  (null? (cdr ent))))
         (let ([entry-coord (make-main-c (get-enter-pos color))])
           (values 
            (list (list (make-start-c color) entry-coord))
            (list 
             (cons entry-coord
                   (map
                    (lambda (pawn)
                      (make-move-candidate (make-enter-piece pawn)
                                           (if (memq 5 roll) '(5) roll)))
                    available-pawns)))))]
        [else (values null null)]))))
