#lang racket
(require games/gl-board-game/gl-board
         (except-in racket/gui define-signature provide-signature-elements)
         sgl/gl-vectors
         sgl
         "sig.rkt")

(provide gui-unit@)

(define-unit gui-unit@ 
  (import config^ model^ restart^ heuristics^ explore^)
  (export)
  ;; Configuration ------------------------------
  
  (define JR? (= BOARD-SIZE 3))
  
  (define PIECE-SIZES (if JR?
                          '(0.4 0.6 0.75)
                          '(0.3 0.45 0.65 0.8)))
  
  ;; Auto-play:
  (define smart? (get-preference 'gobblet:auto-play-smart? (lambda () #f)))
  (define timeout (let ([v (get-preference 'gobblet:auto-play-timeout (lambda () #f))])
                    (if (and (number? v) (real? v))
                        v
                        3.0)))
  
  ;; GUI ------------------------------
  
  (define yellow (gl-float-vector 1.0 1.0 0.0 1.0))
  (define red (gl-float-vector 1.0 0.0 0.0 1.0))
  (define light-blue (gl-float-vector 0.5 0.5 1.0 1.0))
  (define dark-blue (gl-float-vector 0.0 0.0 1.0 1.0))
  
  ;; A gui-piece is
  ;;  (make-gui-piece piece gl-description num num)
  ;;  where the nums might be < 0 or > BOARD-SIZE
  (define-struct gui-piece (piece dl i j))
  
  ;; State ------------------------------
  
  ;; The state of the game, as reflected in the GUI:
  (define board empty-board)
  (define turn 'red)
  
  ;; past, future : (list-of (cons thunk thunk))
  ;;                where first thunk is do and second is undo
  (define past null)
  (define future null)
  
  ;; When `playing?' is true, double-check reset request
  (define playing? #f)
  
  ;; GUI Move ------------------------------
  
  ;; This function is called when the user tries to move `gp'
  ;;  to location `to'
  (define (gui-move gp to)
    (when (gui-piece? gp)
      ;; Get dest and source locations:
      (let* ((to-i (inexact->exact (floor (gl-vector-ref to 0))))
             (to-j (inexact->exact (floor (gl-vector-ref to 1))))
             (from-i (gui-piece-i gp))
             (from-j (gui-piece-j gp))
             (on-board? (<= 0 from-i (sub1 BOARD-SIZE))))
        ;; Only move if the requent lands on the board:
        (when (and (<= 0 to-i (sub1 BOARD-SIZE))
                   (<= 0 to-j (sub1 BOARD-SIZE)))
          ;; Only move if the model says that it's ok:
          (move board (gui-piece-piece gp) 
                (and on-board? from-i) (and on-board? from-j)
                to-i to-j
                (lambda (new-board)
                  (install-board new-board gp to-i to-j))
                (lambda ()
                  ;; Move not allowed by model
                  (void)))))))
  
  (define (install-board new-board gp to-i to-j)
    ;; Move allowed by the model. Create a thunk to
    ;; execute this move and a thunk to undo this
    ;; move:
    (let ([new-gp (make-gui-piece (gui-piece-piece gp) (gui-piece-dl gp)
                                  to-i to-j)]
          [old-board board]
          [old-turn turn])
      (action!
       ;; Forward thunk:
       (lambda ()
         (set! board new-board)
         (send gui-board remove-piece gp)
         (gui-add-piece new-gp)
         (let ([r? (winner? new-board 'red)]
               [y? (winner? new-board 'yellow)])
           (cond
             [(and r? y?) (set-winner! (case old-turn
                                         [(red) "Yellow"]
                                         [(yellow) "Red"]))]
             [r? (set-winner! "Red")]
             [y? (set-winner! "Yellow")]
             [else (set-turn! (other old-turn))])))
       ;; Rewind thunk:
       (lambda ()
         (set! board old-board)
         (send gui-board remove-piece new-gp)
         (gui-add-piece gp)
         (set-turn! old-turn)))))
  
  ;; GUI Board and Pieces ------------------------------
  
  (define f (new (class frame% 
                   (define/augment (on-close)
                     (inner (void) on-close)
                     (exit))
                   (super-new))
                 (label "Gobblet") (width 800) (height 600)))
  (define gui-board
    (new gl-board% (parent f)  (who "Gobblet")
         (min-x (if JR? (- 1 BOARD-SIZE) -1))
         (max-x (if JR? (sub1 (* 2 BOARD-SIZE)) (add1 BOARD-SIZE)))
         (min-y 0)
         (max-y BOARD-SIZE)
         (lift 1.2)
         (move gui-move)
         (theta 30)))
  
  (define q
    (send gui-board with-gl-context
          (lambda () (gl-new-quadric))))
  
  ;; Space description:
  (define space-dl
    (send gui-board with-gl-context
          (lambda ()
            (let ((list-id (gl-gen-lists 1)))
              (gl-quadric-draw-style q 'fill)
              (gl-quadric-normals q 'smooth)
              (gl-new-list list-id 'compile)
              (gl-material-v 'front 'ambient-and-diffuse dark-blue)
              (gl-begin 'polygon)
              (gl-vertex 0.0 0.0 -0.02)
              (gl-vertex 1.0 0.0 -0.02)
              (gl-vertex 1.0 1.0 -0.02)
              (gl-vertex 0.0 1.0 -0.02)
              (gl-end)
              (gl-material-v 'front 'ambient-and-diffuse light-blue)
              (gl-push-matrix)
              (gl-translate 0.5 0.5 -0.01)
              (gl-disk q 0.0 .40 25 1)
              (gl-pop-matrix)
              (gl-end-list)
              list-id))))
  
  ;; Install spaces on board:
  (fold-board (lambda (i j v)
                (send gui-board add-space
                      (lambda ()
                        (gl-push-matrix)
                        (gl-translate i j 0.01)
                        (gl-call-list space-dl)
                        (gl-pop-matrix))
                      (cons i j)))
              void)
  
  ;; Piece description-maker:
  (define (make-piece-dl color scale)
    (send gui-board with-gl-context
          (lambda ()
            (let ((list-id (gl-gen-lists 1)))
              (gl-quadric-draw-style q 'fill)
              (gl-quadric-normals q 'smooth)
              (gl-new-list list-id 'compile)
              (gl-material-v 'front 'ambient-and-diffuse color)
              (gl-cylinder q (/ scale 2) (/ scale 2) (* 1.5 scale) 25 1)
              (gl-push-matrix)
              (gl-translate 0.0 0.0 (* 1.5 scale))
              (gl-disk q 0.0 (/ scale 2) 25 1)
              (gl-pop-matrix)
              (gl-end-list)
              list-id))))
  
  ;; Red piece descriptions:
  (define red-dls (map (lambda (size)
                         (make-piece-dl red size))
                       PIECE-SIZES))
  
  ;; Yellow piece descriptions:
  (define yellow-dls (map (lambda (size)
                            (make-piece-dl yellow size))
                          PIECE-SIZES))
  
  ;; GUI piece records, with each piece at its initial place:
  (define gui-pieces
    (let loop ([red-dls red-dls][yellow-dls yellow-dls]
                                [red-pieces red-pieces][yellow-pieces yellow-pieces]
                                [sizes PIECE-SIZES][z 0])
      (if (null? red-dls)
          null
          (append
           (let ([sz (car sizes)])
             (let loop ([dw (if JR?
                                (- BOARD-SIZE 2)
                                (- BOARD-SIZE 1.5))])
               (if (negative? dw)
                   null
                   (list*
                    (make-gui-piece (car red-pieces) (car red-dls) 
                                    (if JR? (- dw BOARD-SIZE -1) -1)
                                    (if JR? z dw))
                    (make-gui-piece (car yellow-pieces) (car yellow-dls) 
                                    (if JR? (+ BOARD-SIZE dw) BOARD-SIZE)
                                    (if JR? z dw))
                    (loop (sub1 dw))))))
           (loop (cdr red-dls) (cdr yellow-dls) 
                 (cdr red-pieces) (cdr yellow-pieces) 
                 (cdr sizes) (+ z 1))))))
  
  ;; Places a gui-piece at its location on the board:
  (define (gui-add-piece gp)
    (send gui-board add-piece 
          (+ (gui-piece-i gp) 0.5) (+ (gui-piece-j gp) 0.5) 0
          (lambda (for-shadow?)
            (when for-shadow?
              (gl-disable 'lighting))
            (gl-call-list (gui-piece-dl gp))
            (when for-shadow? 
              (gl-enable 'lighting)))
          gp))
  
  ;; Extra GUI controls ----------------------------------------
  
  ;; Define a 3-element pane that makes the left and right parts
  ;;  the same width (so that the middle part is centered):
  (define controls (new (class horizontal-pane% 
                          ;; Override place-children for the 3-child case,
                          ;;  make first and third the same width
                          (define/override (place-children l w h)
                            (let ([r (super place-children l w h)])
                              (if (= (length r) 3)
                                  (let ([a (list-ref r 0)]
                                        [b (list-ref r 1)]
                                        [c (list-ref r 2)])
                                    (let* ([aw (list-ref a 2)]
                                           [cw (list-ref c 2)]
                                           [naw (quotient (+ aw cw) 2)])
                                      (list
                                       (list (car a) (cadr a) 
                                             naw (cadddr a))
                                       (list (+ (car b) (- naw aw)) (cadr b) 
                                             (caddr b) (cadddr b))
                                       (list (+ naw (caddr b)) (cadr c)
                                             (- (+ cw aw) naw) (cadddr c)))))
                                  r)))
                          (super-new))
                        (parent f)
                        (stretchable-height #f)))
  
  ;; Status message:
  (define msg
    (new message% (label "") (parent controls) (stretchable-width #t)))
  
  ;; Forward & Reverse buttons
  (define controls-middle (new horizontal-pane% 
                               (parent controls)
                               (stretchable-height #f)
                               (stretchable-width #f)))
  (define arrows? (let ([f (make-object font% 12 'system)])
                    (and (send f screen-glyph-exists? #\u25C0 #t)
                         (send f screen-glyph-exists? #\u25B6 #t))))
  (define backward-button
    (new button% (label (if arrows? " \u25C0 " " < ")) (parent controls-middle) 
         (callback (lambda (b e) (backward!)))))
  (define forward-button
    (new button% (label (if arrows? " \u25B6 " " > ")) (parent controls-middle)
         (callback (lambda (b e) (forward!)))))
  (define (enable-buttons)
    (send backward-button enable (pair? past))
    (send forward-button enable (pair? future)))
  
  ;; Reset & Help buttons:
  (define controls-right (new horizontal-pane% 
                              (parent controls)
                              (stretchable-height #f)
                              (alignment '(right center))))
  (new button% (label "Reset") (parent controls-right)
       (callback (lambda (b e)
                   (when (or (not playing?)
                             (equal? 1 (message-box/custom
                                        "Warning"
                                        "Stop game in progress and reset?"
                                        "Reset"
                                        "Cancel"
                                        #f
                                        f
                                        '(default=1 caution))))
                     (reset!)))))
  (new button% (label (if (= BOARD-SIZE 3) "4x4 Game" "3x3 Game"))
       (parent controls-right)
       (callback (lambda (b e)
                   (new-game (if (= BOARD-SIZE 3) 4 3)))))
  (new button% (label "Help") (parent controls-right)
       (callback (lambda (b e)
                   (show-gobblet-help))))
  
  (define bottom (new horizontal-pane% 
                      (parent f)
                      (stretchable-height #f)
                      (alignment '(left center))))
  
  (define auto-red (new check-box% 
                        (label "Auto-Play Red")
                        (parent bottom)
                        (callback (lambda (c e)
                                    (when (eq? turn 'red)
                                      (check-auto-play))))))
  (define auto-yellow (new check-box% 
                           (label "Auto-Play Yellow")
                           (parent bottom)
                           (callback (lambda (c e)
                                       (when (eq? turn 'yellow)
                                         (check-auto-play))))))
  
  (define auto-play-msg (new message%
                             (label "") (parent bottom) (stretchable-width #t)))
  
  (new button%
       [label "Auto-Play Options..."]
       [parent bottom]
       [callback
        (lambda (b e)
          (letrec ([d (new dialog% 
                           [label "Auto-Play Options"]
                           [alignment '(left center)]
                           [parent f])]
                   [mode (new choice%
                              [label "Mode:"]
                              [parent d]
                              [choices '("Smart - plays Red perfectly in 3x3 game"
                                         "Ok - tries to plan for next move")]
                              [callback void])]
                   [timeout-field (new text-field%
                                       [label "Auto-play Think Time (seconds):"]
                                       [parent d]
                                       [init-value (number->string timeout)]
                                       [callback 
                                        (lambda (t e)
                                          (let* ([e (send t get-editor)]
                                                 [val (string->number (send e get-text))]
                                                 [bad? (or (not val)
                                                           (not (real? val))
                                                           (val . < . 0))])
                                            (send ok-button enable (not bad?))
                                            (send e change-style 
                                                  (send (make-object style-delta%)
                                                        set-delta-background 
                                                        (if bad? "yellow" "white"))
                                                  0 (send e last-position))))])]
                   [button-panel (new horizontal-pane% 
                                      [parent d]
                                      [alignment '(right center)]
                                      [stretchable-height #f])]
                   [ok-button
                    (new button%
                         [label "Ok"] [parent button-panel] [style '(border)]
                         [callback (lambda (b e)
                                     (set! smart? (= 0 (send mode get-selection)))
                                     (set! timeout (string->number
                                                    (send timeout-field get-value)))
                                     (put-preferences '(gobblet:auto-play-smart?) 
                                                      (list smart?)
                                                      void)
                                     (send d show #f))])])
            (new button%
                 [label "Cancel"] [parent button-panel]
                 [callback (lambda (b e) (send d show #f))])
            (send mode set-selection (if smart? 0 1))
            (send d center)
            (send d show #t)))])
  
  (new grow-box-spacer-pane% [parent bottom])
  
  ;; Extra controls ----------------------------------------
  
  (define (action! forward backward)
    (set! playing? #t)
    (set! future null)
    (set! past (cons (cons forward backward) past))
    (forward)
    (check-auto-play)
    (enable-buttons))
  
  (define (backward!)
    (let ([fb (car past)])
      (set! past (cdr past))
      (set! future (cons fb future))
      ((cdr fb))
      (enable-buttons)
      (send gui-board refresh)))
  
  (define (forward!)
    (let ([fb (car future)])
      (set! future (cdr future))
      (set! past (cons fb past))
      ((car fb))
      (enable-buttons)
      (send gui-board refresh)
      (check-auto-play)))
  
  (define (reset!)
    (for-each (lambda (p)
                (send gui-board remove-piece p))
              (send gui-board get-pieces))
    (init-game!)
    (send gui-board refresh)
    (check-auto-play))
  
  (define (set-turn! c)
    (set! turn c)
    (send msg set-label (format "~a's turn" (if (eq? turn 'red) "Red" "Yellow")))
    (enable-for-turn! c)
    (check-auto-play))
  
  (define (enable-for-turn! who)
    (for-each (lambda (p)
                (send gui-board enable-piece p (eq? who (piece-color (gui-piece-piece p)))))
              (send gui-board get-pieces)))
  
  (define (set-winner! who)
    (set! playing? #f)
    (set! turn #f)
    (send msg set-label (format "~a wins!" who))
    (enable-for-turn! #f)
    (check-auto-play))
  
  (define (init-game!)
    (set! board empty-board)
    (set! past null)
    (set! future null)
    (set! playing? #f)
    (enable-buttons)
    (for-each gui-add-piece gui-pieces)
    (set-turn! 'red))
  
  ;; Auto-play ----------------------------------------
  
  (define auto-play-key #f)
  (define auto-play-custodian #f)
  
  (define (check-auto-play)
    (when auto-play-custodian
      (set! auto-play-key (gensym))
      (custodian-shutdown-all auto-play-custodian)
      (set! auto-play-custodian #f)
      (send auto-play-msg set-label "")
      (enable-for-turn! turn))
    (when (and (null? future)
               turn
               (send (if (eq? turn 'red) auto-red auto-yellow) get-value))
      (let ([key (gensym)]
            [board board]
            [turn turn])
        (enable-for-turn! #f)
        (set! auto-play-key key)
        (set! auto-play-custodian (make-custodian))
        (parameterize ([current-custodian auto-play-custodian])
          (thread
           (lambda () 
             (let ([move (auto-play board turn)])
               (queue-callback
                (lambda ()
                  (when (eq? auto-play-key key)
                    (auto-move board turn move))))))))
        (send auto-play-msg set-label 
              (format "   Auto-play thinking for ~a..."
                      (if (eq? turn 'red) "Red" "Yellow"))))))
  
  
  (define (auto-play board turn)
    (let ([search (make-search (if (= BOARD-SIZE 3)
                                   make-3x3-rate-board
                                   make-4x4-rate-board)
                               (if (= BOARD-SIZE 3)
                                   (if smart?
                                       make-3x3-canned-moves
                                       make-3x3-no-canned-moves)
                                   make-4x4-canned-moves))])
      (search timeout ; timeout
              2 ; lookahead steps (non-exhaustive)
              3 ; single-step lookahead (exhaustive)
              turn board null)))
  
  (define (auto-move board turn move)
    (send auto-play-msg set-label "")
    (let ([gp (let ([piece (list-ref move 0)]
                    [from-i (list-ref move 1)]
                    [from-j (list-ref move 2)])
                (ormap (lambda (gp)
                         (and (eq? piece (gui-piece-piece gp))
                              (if from-i
                                  (and (= from-i (gui-piece-i gp))
                                       (= from-j (gui-piece-j gp)))
                                  (not (<= 0 (gui-piece-i gp) (sub1 BOARD-SIZE))))
                              gp))
                       (send gui-board get-pieces)))]
          [to-i (list-ref move 3)]
          [to-j (list-ref move 4)])
      (let ([new-board (apply-play board move)])
        (install-board new-board gp to-i to-j))
      (send gui-board refresh)))
  
  ;; Go ----------------------------------------
  
  (init-game!)
  
  (send f show #t))
