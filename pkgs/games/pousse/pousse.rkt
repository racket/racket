#lang racket
(require "utils.rkt"
         "board.rkt"
         "board-size.rkt"
         "../show-scribbling.rkt"
         racket/gui
         (prefix-in robot: "robot.rkt"))

(provide game@)

(define game@
  (unit
    (import)
    (export)
    
    ;; Graphical size of a cell in pixels
    (define cell-size 40)
    
    ;; x/o font size (points)
    (define font-size 24)
    
    ;; Automated players
    (define x-player #f)
    (define o-player #f)
    
    ;; Built-in program players
    (define-struct robot (name player))
    (define robots (list (make-robot "Stupid (always B1)" 
                                     (lambda (n moves) 
                                       (sleep 1) 
                                       '(B 1)))
                         (make-robot "Smart (30-sec strategy)"
                                     robot:robot)))
    
    (define (local-file f)
      (build-path (collection-path "games" "pousse") f))
    
    ;;; Game State ;;;
    
    (define board (new-board (current-board-size)))
    (define history (new-history))
    
    (define current-player x)
    
    (define playing? #f) ; lock out board clicks when running an automated player
    
    (define winner #f)
    (define loser #f)
    
    (define moves null)
    
    ;; For moving back and forth in the history:
    (define past-state null)
    (define future-state null)
    
    (define (convert-move s)
      (list (string->symbol (string (char-downcase (string-ref s 0))))
            (string->number (substring s 1 (string-length s)))))
    
    ;;; More Utilities ;;;
    
    (define (get-state) 
      (list board moves current-player history winner loser))
    
    
    ;; The move functions
    (define (mk-push! side side-char)
      (lambda (i)
        (send canvas animate-push side i current-player)
        (set! future-state null)
        (set! past-state (cons (get-state) past-state))
        (set! board (push board side i current-player))
        (set! moves (append moves (list (format "~a~a" side-char (add1 i)))))
        (set! current-player (other-player current-player))
        (send clock-timer reset)
        (send canvas repaint)))
    (define push-left! (mk-push! 'left #\L))
    (define push-right! (mk-push! 'right #\R))
    (define push-top! (mk-push! 'top #\T))
    (define push-bottom! (mk-push! 'bottom #\B))
    
    (define (check-winner)
      (let ([v (find-winner board)])
        (when v
          (set! winner v))))
    
    (define (check-loser)
      (when (find-board-in-history board history)
        (set! loser (other-player current-player))))
    
    (define (in-straight? board v i j)
      (let ([n (current-board-size)])
        (or (andmap (lambda (x) x) 
                    (n-map n (lambda (j) (eq? (board-cell board i j) v))))
            (andmap (lambda (x) x)
                    (n-map n (lambda (i) (eq? (board-cell board i j) v)))))))
    
    ;; past! and future! rewind or un-rewind the game:
    (define-values (past! future!)
      (let ([set-past (lambda (x) (set! past-state x))]
            [set-future (lambda (x) (set! future-state x))])
        (values
         (lambda () (time-travel! (lambda () past-state) (lambda () future-state)
                                  set-past set-future))
         (lambda ()
           (time-travel! (lambda () future-state) (lambda () past-state)
                         set-future set-past)))))
    
    (define (time-travel! get-src get-dest set-src! set-dest!)
      ;; If it's a person versus a robot, and it's the person's turn, then
      ;; skip past the robot's turn. Cancel a running robot. If the game
      ;; is over because a person lost (by repeating a board position)
      ;; back up just once.
      (define skip-robot (and (or x-player o-player) 
                              ; Robot running?
                              (not (send canvas kill-robot))
                              ; Person lost?
                              (not (and loser
                                        (eq? loser (if x-player o x))))))
      (set-dest! (cons (get-state) (get-dest)))
      (when skip-robot
        (set-dest! (cons (car (get-src)) (get-dest))))
      (let ([a ((if skip-robot cadr car) (get-src))])
        (set-src! ((if skip-robot cddr cdr) (get-src)))
        (set! board (car a))
        (set! moves (cadr a))
        (set! current-player (caddr a))
        (set! history (cadddr a))
        (set! winner (list-ref a 4))
        (set! loser (list-ref a 5)))
      (send canvas repaint)
      (send canvas refresh-controls))
    
    ;; Used to reset a game (via the "Setup..." dialog)
    (define (init-game size)
      (current-board-size size)
      (set! board (new-board size))
      (set! history (new-history))
      (set! past-state null)
      (set! current-player x)
      (set! winner #f)
      (set! loser #f)
      (set! moves null)
      (set! future-state null)
      (set-canvas-size))
    
    ;; Restart for regular playing mode
    (define (reset-game size)
      (init-game size)
      (send canvas repaint)
      (send clock-timer reset)
      (send canvas do-next-action))
    
    ;;; GUI ;;;
    
    (define animate-step 2/10)
    (define animate-delay 0.05)
    
    (define red (make-object color% "RED"))
    (define green (make-object color% "GREEN"))
    (define black (make-object color% "BLACK"))
    (define gray (make-object color% "GRAY"))
    (define white (make-object color% "WHITE"))
    
    (define the-font (make-object font% font-size 'decorative 'normal 'bold))
    (define the-pen (send the-pen-list find-or-create-pen "GRAY" 1 'solid))
    
    (define transparent-brush (send the-brush-list find-or-create-brush "WHITE" 'transparent))
    (define solid-brush (send the-brush-list find-or-create-brush "GRAY" 'solid))
    
    (define watch-cursor (make-object cursor% 'watch))
    
    ; The canvas (drawing/event area) class
    (define pousse-canvas%
      (class canvas%
        (init-rest args)
        (inherit get-dc)
        (define dc #f)
        (define do-kill-robot (lambda () #f)) ; installed by refresh-controls
        (public*
         [kill-robot (lambda () (do-kill-robot))]
         [draw-box 
          ; Draw a string in a box
          (lambda (i j str)
            (when str
              (let-values ([(w h d s) (send dc get-text-extent str)])
                (send dc draw-text str
                      (+ (* i cell-size) (/ (- cell-size w) 2))
                      (+ (* j cell-size) (/ (- cell-size h) 2))))))]
         [do-next-action
          (lambda ()
            ;; See if anything interesting happened, then call refresh-controls (below)
            (check-loser)
            (check-winner)
            (set! history (extend-history board history))
            (refresh-controls))]
         [refresh-controls
          ;; Update the GUI to reflect the current game state, and run
          ;; aa program player if it's time.
          (lambda ()
            (send history-text show-moves)
            (send clock show (not (or winner loser)))
            (if (or loser winner)
                (begin
                  ;; Game over
                  (enable-arrows)
                  (repaint)
                  (send status set-label
                        (format "Game over: ~a ~a!" 
                                (if (equal? (or winner loser) x) "X" "O")
                                (if winner "wins" "loses")))
                  (send clock show #f))
                ;; Check for automated player
                (let* ([killed? 'not-yet]
                       [action void]
                       [lock (make-semaphore 1)]
                       [run-player-in-background
                        ;; Lots of messy stuff for calling the OS to run a player. The
                        ;; kill-robot method is installed for killing of the player process.
                        (lambda (player)
                          (let ([result #f]
                                [done (make-semaphore)]
                                [player-custodian (make-custodian)])
                            (parameterize ([current-eventspace 
                                            (parameterize ([current-custodian player-custodian])
                                              (make-eventspace))])
                              (queue-callback
                               (lambda ()
                                 (let ([move (player 
                                              ;; board size
                                              (current-board-size)
                                              ;; change move representation:
                                              (map convert-move moves))])
                                   (semaphore-wait lock)
                                   (set! result move)
                                   (set! killed? #f)
                                   (semaphore-post lock)
                                   (semaphore-post done)))))
                            ;; Install the process killer. Must return #f
                            ;;  if the robot is already done.
                            (set! do-kill-robot (lambda ()
                                                  (semaphore-wait lock)
                                                  (begin0
                                                    (if (eq? killed? 'not-yet)
                                                        (begin
                                                          (custodian-shutdown-all player-custodian)
                                                          (set! killed? #t)
                                                          (set! result #f)
                                                          (semaphore-post done))
                                                        #f)
                                                    (semaphore-post lock))))
                            
                            ;; Wait for a response (or kill)...
                            (send canvas set-cursor watch-cursor)
                            (semaphore-wait done)
                            (custodian-shutdown-all player-custodian) ;; just in case
                            (send canvas set-cursor #f)
                            (when result
                              (unless (and (list? result)
                                           (= 2 (length result))
                                           (symbol? (car result))
                                           (regexp-match "^[tblrTBLR]$" (symbol->string (car result)))
                                           (number? (cadr result))
                                           (<= 1 (cadr result) (current-board-size)))
                                (error 'play "unacceptable reply: ~a" result))
                              (let* ([d (char-upcase (string-ref (symbol->string (car result)) 0))]
                                     [p (cadr result)])
                                (set! action
                                      (lambda ()
                                        (case d
                                          [(#\T) (push-top! (sub1 p))]
                                          [(#\B) (push-bottom! (sub1 p))]
                                          [(#\L) (push-left! (sub1 p))]
                                          [(#\R) (push-right! (sub1 p))])))))))]
                       [run-player
                        ;; A wrapper for monitoring the program player in a GRacket thread.
                        ;; Also handle the possibility that something goes wrong.
                        (lambda (robot who)
                          (send status set-label (format "~a: running ~a"
                                                         who 
                                                         (robot-name robot)))
                          (let ([s (make-semaphore)])
                            (thread (lambda () 
                                      (with-handlers ([void (lambda (exn)
                                                              (message-box
                                                               "Error"
                                                               (format
                                                                (string-append
                                                                 "There was an error running the "
                                                                 "program player for ~a.\n"
                                                                 "We'll assume a default move, T1.\n"
                                                                 "Here is the error message:\n~a")
                                                                who
                                                                (if (exn? exn)
                                                                    (exn-message exn)
                                                                    exn))
                                                               #f '(ok))
                                                              (set! action (lambda () (push-top! 0))))])
                                        (run-player-in-background (robot-player robot)))
                                      (semaphore-post s)))
                            (set! playing? #t)
                            (enable-arrows)
                            ;; Handle GUI events while we wait...
                            (yield s)
                            (set! playing? #f))
                          (unless killed?
                            (send status set-label "")
                            (action)
                            (do-next-action)))])
                  ;; Run a program? Let a person play?
                  (cond
                    [(and (eq? current-player x) x-player) (run-player x-player "X")]
                    [(and (eq? current-player o) o-player) (run-player o-player "O")]
                    [else (send status set-label (format "~a's turn (click a number)"
                                                         (if (eq? current-player x) "X" "O")))
                          (enable-arrows)]))))])
        ;; Animation state
        (define tracking-i 0) ;; for tracking mouse clicks
        (define tracking-j 0)
        (define tracking-highlight? #f)
        
        (define pushpiece #f) ;; piece being pushed onto board, #f for none
        (define pushrow -1)   ;; row being pushed, -1 for none
        (define pushcol -1)   ;; col being pushed, -1 for none
        (define pushdown? #t) ;; left or top push?
        (define amt 0)        ;; displacement for push, between -1 and 1
        
        (public*
         [do-draw
          ;;;;;;;;;;;;;;;;;;;; Draw the Board ;;;;;;;;;;;;;;;;;;;;;;;
          (lambda ()
            (let ([n (current-board-size)])
              (send dc clear)
              (send dc set-pen the-pen)
              (send dc set-font the-font)
              (send dc set-text-foreground gray)
              (n-times (+ n 2) 
                       (lambda (i)
                         (when (<= 1 i (add1 n))
                           (send dc draw-line cell-size (* i cell-size) 
                                 (* (+ n 1) cell-size) (* i cell-size))
                           (send dc draw-line (* i cell-size) cell-size 
                                 (* i cell-size) (* (+ n 1) cell-size)))
                         (when (<= 1 i n)
                           (let ([draw-box
                                  (lambda (i j s)
                                    (if (and tracking-highlight?
                                             (= i tracking-i)
                                             (= j tracking-j))
                                        (begin
                                          (send dc set-text-foreground white)
                                          (send dc set-brush solid-brush)
                                          (send dc draw-ellipse
                                                (+ 2 (* i cell-size))
                                                (+ 2 (* j cell-size))
                                                (- cell-size 4)
                                                (- cell-size 4))
                                          (draw-box i j s)
                                          (send dc set-brush transparent-brush)
                                          (send dc set-text-foreground gray))
                                        (draw-box i j s)))])
                             (draw-box i 0 (number->string i))
                             (draw-box 0 i (number->string i))
                             (draw-box i (add1 n) (number->string i))
                             (draw-box (add1 n) i (number->string i))))))
              (send dc set-text-foreground black)
              (n-times n
                       (lambda (i)
                         (n-times n (lambda (j) 
                                      (let ([v (board-cell board i j)])
                                        (when (and (eq? winner v)
                                                   (in-straight? board v i j))
                                          (send dc set-text-foreground green))
                                        (when (eq? loser v) 
                                          (send dc set-text-foreground red))
                                        (draw-box (+ i 1
                                                     ;; Need to offset for animation?
                                                     (if (= j pushrow)
                                                         (if (let ([step (if pushdown? -1 1)])
                                                               (let loop ([i i])
                                                                 (cond
                                                                   [(or (= i -1) (= i n)) #t]
                                                                   [(eq? (board-cell board i j) none) #f]
                                                                   [else (loop (+ i step))])))
                                                             amt
                                                             0)
                                                         0))
                                                  (+ j 1
                                                     ;; Need to offset for animation?
                                                     (if (= i pushcol)
                                                         (if (let ([step (if pushdown? -1 1)])
                                                               (let loop ([j j])
                                                                 (cond
                                                                   [(or (= j -1) (= j n)) #t]
                                                                   [(eq? (board-cell board i j) none) #f]
                                                                   [else (loop (+ j step))])))
                                                             amt
                                                             0)
                                                         0))
                                                  (cond
                                                    [(eq? v none) #f]
                                                    [(eq? v x) "x"]
                                                    [(eq? v o) "o"]))
                                        (when (or (eq? winner v) (eq? loser v))
                                          (send dc set-text-foreground black)))))))
              (when pushpiece
                (draw-box (if (>= pushrow 0)
                              (if pushdown?
                                  amt
                                  (+ n 1 amt))
                              (+ 1 pushcol))
                          (if (>= pushcol 0)
                              (if pushdown?
                                  amt
                                  (+ n 1 amt))
                              (+ 1 pushrow))
                          (cond
                            [(eq? pushpiece x) "x"]
                            [(eq? pushpiece o) "o"])))))])
        (define bitmap #f)
        (public*
         [repaint (lambda ()
                    (set! pushpiece #f)
                    (set! pushcol -1)
                    (set! pushrow -1)
                    (unless dc
                      (set! bitmap (make-object bitmap%
                                     (* (+ (current-board-size) 2) cell-size)
                                     (* (+ (current-board-size) 2) cell-size)))
                      (set! dc (make-object bitmap-dc% bitmap)))
                    (do-draw)
                    (on-paint))]
         
         [new-bitmap (lambda () 
                       (set! bitmap #f)
                       (set! dc #f))]
         
         [animate-push (lambda (side pos player)
                         (let ([n (current-board-size)])
                           (set! pushpiece player)
                           (set! pushrow (if (memq side '(right left))
                                             pos
                                             -1))
                           (set! pushcol (if (memq side '(top bottom))
                                             pos
                                             -1))
                           (set! pushdown? (memq side '(left top)))
                           (set! tracking-i (if (memq side '(top bottom))
                                                (add1 pushcol)
                                                (if pushdown? 0 (add1 n))))
                           (set! tracking-j (if (memq side '(right left))
                                                (add1 pushrow)
                                                (if pushdown? 0 (add1 n))))
                           (set! tracking-highlight? #t)
                           (let loop ([a 0])
                             (set! amt ((if pushdown? + -) a))
                             (do-draw)
                             (send (get-dc) draw-bitmap bitmap 0 0)
                             (sleep animate-delay)
                             (if (= a 1)
                                 (set! tracking-highlight? #f) ;; expects redraw triggered afterwards...
                                 (loop (+ a animate-step))))))])
        
        (override*
         [on-paint (lambda ()
                     (when bitmap
                       (send (get-dc) draw-bitmap bitmap 0 0)))]
         
         ;;;;;;;;;;;;;;;;;;;; Handle Clicks ;;;;;;;;;;;;;;;;;;;;;;;
         [on-event (lambda (e)
                     ;; There are a lot of reasons why you might not be allowed to click...
                     (cond
                       [(and (not winner) (not loser) 
                             (or (send e button-down?)
                                 (send e dragging?)
                                 (send e button-up?))
                             (not playing?)
                             (not (if (eq? current-player x) x-player o-player)))
                        (let ([i (inexact->exact (floor (/ (send e get-x) cell-size)))]
                              [j (inexact->exact (floor (/ (send e get-y) cell-size)))])
                          (cond
                            [(send e button-down?)
                             (set! tracking-i i)
                             (set! tracking-j j)
                             (set! tracking-highlight? #t)
                             (repaint)]
                            [(send e moving?)
                             (let ([th? tracking-highlight?])
                               (set! tracking-highlight? (and
                                                          (= tracking-i i)
                                                          (= tracking-j j)))
                               (unless (eq? th? tracking-highlight?)
                                 (repaint)))]
                            [(send e button-up?)
                             (if (and (= tracking-i i)
                                      (= tracking-j j))
                                 (let ([n (current-board-size)])
                                   (when (cond
                                           [(and (= j 0) (<= 1 i n)) (push-top! (sub1 i)) #t]
                                           [(and (= j (add1 n)) (<= 1 i n)) (push-bottom! (sub1 i)) #t]
                                           [(and (= i 0) (<= 1 j n)) (push-left! (sub1 j)) #t]
                                           [(and (= i (add1 n)) (<= 1 j n)) (push-right! (sub1 j)) #t]
                                           [else #f]) ; not on a number
                                     ; Check for win/loss, run automated player
                                     (do-next-action)))
                                 (when tracking-highlight?
                                   (set! tracking-highlight? #f)
                                   (repaint)))]))]
                       [else
                        (when tracking-highlight?
                          (set! tracking-highlight? #f)
                          (repaint))]))])
        (apply super-make-object args)))
    
    ;; Create the GUI interface with the above pieces ;;
    
    ; Instantiate the canvas in a frame (= a top-level window)
    (define frame (new (class frame%
                         (augment*
                          [can-close? (lambda () (inner #t can-close?))]
                          ;; Close the frame => exit the program
                          ;; No fancy "Quit" menus here!
                          [on-close (lambda () (inner (void) on-close) (exit))])
                         (super-new))
                       [label "Pousse"] [style '(metal no-resize-border)]))
    
    ;; Panels are for GUI item layout (auto geometry management)
    (define h-layout-panel (make-object horizontal-panel% frame))
    (send h-layout-panel spacing 5)
    (define game-panel (make-object vertical-panel% h-layout-panel))
    (send game-panel stretchable-width #f)
    (define history-panel (make-object vertical-panel% h-layout-panel))
    
    ;; Make the left and right arrow buttons
    (define button-panel (make-object horizontal-panel% game-panel))
    (send button-panel stretchable-height #f)
    (define left-panel (make-object vertical-panel% button-panel))
    (define past-button (make-object button% (make-object bitmap% (local-file "left.gif"))
                          button-panel (lambda (b e) (past!))))
    (define future-button (make-object button% (make-object bitmap% (local-file "right.gif"))
                            button-panel (lambda (b e) (future!))))
    (define right-panel (make-object vertical-panel% button-panel))
    (define clock (make-object message% "00:00" right-panel))
    (send left-panel min-width (send clock min-width)) ; layout trick
    (send right-panel set-alignment 'right 'bottom)
    
    (define clock-timer (make-object
                            (class timer%
                              (define init 0)
                              (define dinged 0)
                              (rename-super [super-start start])
                              (public* [reset (lambda ()
                                                (send clock set-label "00:00")
                                                (set! dinged 0)
                                                (set! init (current-seconds)))])
                              (override*
                               [notify 
                                (lambda ()
                                  (let* ([v (- (current-seconds) init)])
                                    ;; Ring bell once at 30 seconds, twice at 60 seconds
                                    (when (send clock is-shown?)
                                      (when (>= v 30)
                                        (unless (> dinged 0) (bell) (set! dinged 1))
                                        (when (>= v 60)
                                          (unless (> dinged 1) (bell) (bell) (set! dinged 2)))))
                                    (let ([v (if (>= v 3600) ; more than an hour
                                                 (quotient v 3600)
                                                 v)])
                                      (send clock set-label
                                            (format "~a~a:~a~a"
                                                    (quotient v 600)
                                                    (modulo (quotient v 60) 10)
                                                    (quotient (modulo v 60) 10)
                                                    (modulo v 10))))))]
                               [start (lambda ()
                                        (set! init (current-seconds))
                                        (super-start 1000 #f))])
                              (super-make-object))))
    (send clock-timer start)
    
    ;; This procedure is called to enable/disable the arrow buttons
    (define (enable-arrows)
      (let ([ok? (lambda (state)
                   (and ;; Something to rewind to?
                    (pair? state)
                    ;; Is it program vs. program?
                    (not (and x-player o-player))
                    ;; If we're playing a program, can we rewind twice?
                    (or (not (or x-player o-player))
                        (pair? (cdr state)))))])
        (send past-button enable (ok? past-state))
        (send future-button enable (ok? future-state))))
    
    ;; Make the status line
    (define status (make-object message% "Pousse" game-panel))
    (send status stretchable-width #t)
    
    ;; Make the canvas for drawing the game board
    (define canvas (make-object pousse-canvas% game-panel))
    
    ; The canvas should stretch/shrink to fit the board
    (define (set-canvas-size)
      (let ([n (current-board-size)])
        (send canvas min-client-width (* (+ n 2) cell-size))
        (send canvas min-client-height (* (+ n 2) cell-size))
        (send canvas new-bitmap)))
    (set-canvas-size)
    (send canvas focus)
    
    ; Make a text window for showing the board history to the right.
    ; Uses the built-in text editor in GRacket, adding a show-moves
    ; method to refresh the window after a move or rewind.
    (make-object message% "Moves" history-panel)
    (define history-canvas (make-object editor-canvas% history-panel #f '(no-hscroll)))
    (define history-text (make-object (class text%
                                        (inherit begin-edit-sequence end-edit-sequence
                                                 erase insert delete change-style hide-caret
                                                 set-position line-start-position line-end-position)
                                        ; Ignore all user actions:
                                        (override* [on-char (lambda (e) (void))] [on-event (lambda (e) (void))])
                                        (public*
                                         [show-moves
                                          (lambda ()
                                            (begin-edit-sequence)
                                            (erase)
                                            (change-style (make-object style-delta% 'change-normal))
                                            (change-style (make-object style-delta% 'change-family 'swiss))
                                            (for-each
                                             (lambda (m) (insert m) (insert #\newline))
                                             (if (null? future-state)
                                                 moves
                                                 (cadr (list-ref future-state (sub1 (length future-state))))))
                                            (delete) ; delete that last newline
                                            (if (null? moves)
                                                (set-position 0)
                                                (let* ([past-move (sub1 (length moves))]
                                                       [start (line-start-position past-move)])
                                                  (change-style (send 
                                                                 (make-object style-delta% 'change-bold)
                                                                 set-delta-foreground "BLUE")
                                                                start
                                                                (line-end-position past-move))
                                                  (set-position start)))
                                            (end-edit-sequence))])
                                        (super-make-object)
                                        (hide-caret #t))))
    (send history-canvas set-editor history-text)
    (send history-canvas min-client-width 30)
    
    ;; Setup and miscellaneous buttons at the bottom
    
    (define misc-panel (make-object horizontal-panel% frame))
    (send misc-panel stretchable-height #f)
    
    (make-object button% "Help" misc-panel (lambda (b e) (help)))
    (make-object button% "Setup..." misc-panel (lambda (b e) (setup)))
    (make-object vertical-pane% misc-panel) ; spacer
    
    ;; Makes the setup dialog. Options dialogs are always a pain.
    (define (make-setup-dialog)
      (define d (make-object dialog% "Pousse Setup" frame 300 200))
      (define config-panel (make-object vertical-panel% d))
      (define game-panel (make-object vertical-panel% config-panel))
      (define (make-player name)
        (letrec ([p (make-object vertical-panel% game-panel '(border))]
                 [m (make-object choice% (format "~a Player:" name) '("Person" "Program") p
                      (lambda (m e) 
                        (send l enable (positive? (send m get-selection)))
                        (enable-ok)))]
                 [l (make-object list-box% "Programs:" (map robot-name robots) p 
                      (lambda (l e) (enable-ok)))])
          (send l enable #f)
          (values m l)))
      (define board-size (make-object slider% "Board Size:" 3 20 game-panel void (current-board-size)))
      (define-values (x-kind x-robot) (make-player "X"))
      (define-values (o-kind o-robot) (make-player "O"))
      
      (define button-panel (make-object horizontal-pane% d))
      (define load-button (make-object button% "Add a Player Program..." button-panel
                            (lambda (b e)
                              (with-handlers ([void
                                               (lambda (exn)
                                                 (message-box "Error"
                                                              (format "There was an error:\n~a"
                                                                      (if (exn? exn)
                                                                          (exn-message exn)
                                                                          exn))))])
                                (let ([f (get-file "Get Player Program" d)])
                                  (when f
                                    (let ([player (dynamic-require f 'robot)])
                                      (let ([name (get-text-from-user "Player Name"
                                                                      "Player Program Name:"
                                                                      d
                                                                      (let-values ([(base name dir?)
                                                                                    (split-path f)])
                                                                        (path->string name)))])
                                        (when name
                                          (set! robots (append robots
                                                               (list (make-robot name player))))
                                          (send x-robot set (map robot-name robots))
                                          (send o-robot set (map robot-name robots)))))))))))
      (define spacer (make-object vertical-pane% button-panel))
      (define cancel-button (make-object button% "Cancel" button-panel 
                              (lambda (b e) (send d show #f))))
      (define ok-button (make-object button% "Start" button-panel 
                          ;; Callback procedure invoked when the button is hit:
                          (lambda (b e) 
                            (send d show #f)
                            (send canvas kill-robot) ; in case a robot was running
                            (queue-callback
                             (lambda ()
                               (let ([get-robot
                                      (lambda (l)
                                        (list-ref robots
                                                  (send l get-selection)))]
                                     [size (send board-size get-value)])
                                 (if (zero? (send x-kind get-selection))
                                     (set! x-player #f) ; person player
                                     (set! x-player (get-robot x-robot)))
                                 (if (zero? (send o-kind get-selection))
                                     (set! o-player #f) ; person player
                                     (set! o-player (get-robot o-robot)))
                                 (reset-game size)))))
                          '(border)))
      
      (define enable-ok (lambda () (send ok-button enable (and
                                                           (or (zero? (send x-kind get-selection))
                                                               (send x-robot get-selection))
                                                           (or (zero? (send o-kind get-selection))
                                                               (send o-robot get-selection))))))
      
      (send button-panel set-alignment 'right 'center)
      (send button-panel stretchable-height #f)
      
      d)
    
    (define setup-dialog (make-setup-dialog))
    (define setup-once? #f)
    
    (define (setup) 
      (unless setup-once?
        (send setup-dialog center)
        (set! setup-once? #t))
      (send setup-dialog show #t))
    
    ;; Help or source code window:
    (define help
      (show-scribbling
       '(lib "games/scribblings/games.scrbl")
       "pousse"))
    
    
    ; Draw initial board
    (send canvas repaint)
    
    ; Arrow buttons initially enabled?
    (enable-arrows)
    
    ; Don't allowing resizing the frame. Everything fits just right.
    (send frame stretchable-width #f)
    (send frame stretchable-height #f)
    
    ; Show the frame - we're off and running, now!
    (send frame show #t)
    
    ; Take the first action.
    (send canvas do-next-action)
    
    ; Loop forever (handling events). Frame's on-close callback method will exit.
    (yield (make-semaphore 0))))
