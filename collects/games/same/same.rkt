#lang racket/base
(require racket/class
         racket/unit
         racket/list
         racket/gui/base
         racket/math
         "../show-scribbling.rkt"
         "same-lib.rkt")

(provide game@)

(define game@
  (unit
    (import)
    (export)
    
    (define board-width 20)
    (define board-height 16)
    
    ;; build-board : (-> (vectorof (vectorof (vector (union num #f) boolean))))
    ;   this represents the board. Each entry is the color index of
    ;   the piece and a node to mark for the depth-first traversal.
    ;   #f for the color index indicates an eliminated piece.
    (define (build-board)
      (define board
        (build-vector
         board-width
         (lambda (i)
           (build-vector
            board-height
            (lambda (j)
              (vector
               (random (length colors))
               #f))))))
      (for* ([x (in-range 1 board-width)]
             [y (in-range 1 board-height)])
        (when (zero? (random 5))
          (define-values (prev-x prev-y)
            (if (zero? (random 2))
                (values x (- y 1))
                (values (- x 1) y)))
          (define this-vector (board-ref board x y))
          (define prev-vector (board-ref board prev-x prev-y))
          (vector-set! this-vector 0 (vector-ref prev-vector 0))))
      board)
    
    (define board (build-board))
    
    (define game-over? #f)
    
    ;; adds up as the user clicks
    (define clicked-score 0)
    (define (calc-score n) (* n n))
    (define (reset-score)
      (set! clicked-score 0)
      (set-score-label))
    (define (update-score balls-going-away)
      (set! clicked-score (+ clicked-score (calc-score balls-going-away)))
      (set-score-label))
    (define (set-score-label)
      (define cells-filled-in 0)
      (for ([v (in-vector board)])
        (for ([v (in-vector v)])
          (when (vector-ref v 0)
            (set! cells-filled-in (+ cells-filled-in 1)))))
      (define bonus-start 50) ;; bonus for getting down to 49 (or fewer) balls
      (define bonus-per-ball 100) ;; number of points for clearing each of those last 'bonus-start' balls
      (define bonus (if (<= cells-filled-in bonus-start)
                        (* bonus-per-ball (- bonus-start cells-filled-in))
                        0))
      (send score-message set-label 
            (format "~a + ~a = ~a"
                    clicked-score
                    bonus
                    (+ clicked-score bonus))))
    
    (define same-canvas%
      (class canvas%
        (inherit get-dc get-client-size)
        (define/private (get-width) (let-values ([(w h) (get-client-size)]) w))
        (define/private (get-height) (let-values ([(w h) (get-client-size)]) h))
        (define/private (get-x-step) (/ (get-width) board-width))
        (define/private (get-y-step) (/ (get-height) board-height))
        
        (define mouse-current-x #f)
        (define mouse-current-y #f)
        (define mouse-clicked-x #f)
        (define mouse-clicked-y #f)
        
        (define background-valid? #f)
        (define background #f)
        
        (define/public (invalidate-board-bitmap)
          (set! background-valid? #f))
          
        (define/override (on-size w h)
          (define-values (cw ch) (get-client-size))
          (when background
            (unless (and (= cw (send background get-width))
                         (= ch (send background get-height)))
              (set! background #f)
              (set! background-valid? #f))))
        
        (define/override (on-paint)
          (define-values (cw ch) (get-client-size))
          (define dc (get-dc))
          (send dc set-smoothing 'smoothed)
          (build-background)
          (send dc set-scale 1 1)
          (send dc draw-bitmap background 0 0)
          
          (define current-blob 
            (and mouse-current-x 
                 (find-same-colors board board-width board-height 
                                   mouse-current-x
                                   mouse-current-y)))
          (cond
            [(and mouse-clicked-x
                  mouse-current-x
                  (equal? mouse-clicked-x mouse-current-x)
                  (equal? mouse-clicked-y mouse-current-y))
             
             ;; don't know what to do here
             
             (define blob
               (find-same-colors board board-width board-height 
                                 mouse-current-x
                                 mouse-current-y))
             (unless (null? blob)
               (define color
                 (vector-ref (board-ref board mouse-current-x mouse-current-y)
                             0))
               (define-values (cw ch) (get-client-size))
               (update-dc-scale dc cw ch board-width board-height)
               (update-pen/draw-blob
                blob dc color
                mouse-current-x mouse-current-y
                mouse-clicked-x mouse-clicked-y))]
            [mouse-current-x
             (define blob
               (find-same-colors board board-width board-height 
                                 mouse-current-x
                                 mouse-current-y))
             (unless (null? blob)
               (define color
                 (vector-ref (board-ref board mouse-current-x mouse-current-y)
                             0))
               (define-values (cw ch) (get-client-size))
               (update-dc-scale dc cw ch board-width board-height)
               (update-pen/draw-blob
                blob dc color
                mouse-current-x mouse-current-y
                mouse-clicked-x mouse-clicked-y))])
          
          (when game-over?
            (update-dc-scale dc cw ch board-width board-height)
            (paint-game-over)))
        
        (define/private (build-background)
          (unless background-valid?
            (define-values (cw ch) (get-client-size))
            (unless background
              (set! background (make-bitmap cw ch)))
            (define bdc (make-object bitmap-dc% background))
            (draw-board bdc board-width board-height board cw ch #f #f #f #f)
            (send bdc set-bitmap #f)
            (set! background-valid? #t)))
        
        (define/private (paint-game-over)
          (define dc (get-dc))
          (define game-over "Game Over")
          (send dc set-font 
                (send the-font-list find-or-create-font
                      24 'decorative 'normal 'normal #f))
          (define border 5)
          (define-values (text-width text-height d l)
            (send dc get-text-extent game-over))
          (define x (- (/ (* cell-w board-width) 2) (/ text-width 2)))
          (define y (- (/ (* cell-h board-height) 2) (/ text-height 2)))
          (send dc set-pen "white" 1' transparent)
          (send dc set-brush "white" 'solid)
          (send dc set-alpha .8)
          (send dc draw-rectangle
                (- x border border) (- y border)
                (+ text-width border border border border)
                (+ text-height border border))
          (send dc set-alpha 1)
          (send dc draw-text game-over x y))
        
        (inherit refresh) 
        (define/override (on-event evt)
          (define x (send evt get-x))
          (define y (send evt get-y))
          (define-values (cw ch) (get-client-size))
          (define bx (floor (* (/ x cw) board-width)))
          (define by (floor (* (/ y ch) board-height)))
          (unless (<= 0 bx (- board-width 1)) (set! bx #f))
          (unless (<= 0 by (- board-height 1)) (set! by #f))
          (when (send evt leaving?)
            (set! bx #f)
            (set! by #f))
          
          (when (send evt button-up?)
            (when (and (equal? mouse-clicked-x bx)
                       (equal? mouse-clicked-y by))
              (define removed-ball-count
                (make-a-move mouse-clicked-x mouse-clicked-y 
                             board board-width board-height))
              (when removed-ball-count
                (update-score removed-ball-count)
                (invalidate-board-bitmap)
                (update-game-over)
                (refresh))))
          
          (define-values (new-mouse-clicked-x new-mouse-clicked-y)
            (cond
              [(send evt button-down?) (values bx by)]
              [(send evt button-up?) (values #f #f)]
              [else (values mouse-clicked-x mouse-clicked-y)]))
          
          (define this-score-needs-update? #f)
          
          (unless (and (equal? mouse-clicked-x new-mouse-clicked-x)
                       (equal? mouse-clicked-y new-mouse-clicked-y))
            (set! mouse-clicked-x new-mouse-clicked-x)
            (set! mouse-clicked-y new-mouse-clicked-y)
            (set! this-score-needs-update? #t)
            (refresh))
          
          (unless (and (equal? bx mouse-current-x)
                       (equal? by mouse-current-y))
            (set! mouse-current-x bx)
            (set! mouse-current-y by)
            (set! this-score-needs-update? #t)
            (refresh))
          
          (when this-score-needs-update?
            (update-this-score (if mouse-clicked-x
                                   mouse-clicked-x
                                   mouse-current-x)
                               (if mouse-clicked-y
                                   mouse-clicked-y
                                   mouse-current-y))))
        
        (define/private (update-this-score x y)
          (send this-score-message set-label 
                (cond
                  [(and x y)
                   (define num (length (find-same-colors board
                                                         board-width
                                                         board-height
                                                         x y)))
                   (if (= num 1)
                       ""
                       (format "~a" (calc-score num)))]
                  [else ""])))
        
        (define/public-final (update-game-over)
          (set! game-over?
                (not
                 (let loop ([i board-width]
                            [continue? #f])
                   (cond
                     [(zero? i) continue?]
                     [else
                      (or continue?
                          (loop
                           (sub1 i)
                           (let loop ([j board-height]
                                      [continue? continue?])
                             (cond
                               [(zero? j) continue?]
                               [else
                                (or continue?
                                    (loop
                                     (sub1 j)
                                     (> (length (find-same-colors board 
                                                                  board-width
                                                                  board-height
                                                                  (sub1 i)
                                                                  (sub1 j)))
                                        1)))]))))])))))
        
        
        
        (super-new)))
    
    (define semaphore (make-semaphore 0))
    (define same-frame%
      (class frame%
        [define/augment on-close
          (lambda ()
            (semaphore-post semaphore)
            (inner (void) on-close))]
        (super-new [style '(metal)])))
    
    (define (new-game-callback redraw?)
      (set! game-over? #f)
      (set! board (build-board))
      (reset-score)
      (send canvas invalidate-board-bitmap)
      (send canvas update-game-over)
      (when redraw?
        (send canvas refresh)))
    
    (define frame (make-object same-frame% "Same"))
    (define panel (make-object vertical-panel% frame))
    (define canvas (make-object same-canvas% panel))
    (define hp (new horizontal-panel% [parent panel] [stretchable-height #f]))
    (new message% [label "Total Score: "] [parent hp])
    (define score-message (new message% 
                               [label "10000 + 10000 = 20000"] ;; get a reasonable min size
                               [parent hp] [stretchable-width #t]))
    (new message% [label "This Score: "] [parent hp])
    (define this-score-message (new message% 
                                    [label "10000"] ;; get a reasonable min size
                                    [parent hp]
                                    [stretchable-width #t]))
    (define button (make-object button% "New Game" hp (lambda x (new-game-callback #t))))
    
    (define help-button (make-object button% "Help"
                          hp
                          (let ([show-help
                                 (show-scribbling
                                  '(lib "games/scribblings/games.scrbl")
                                  "same")])
                            (lambda (_1 _2)
                              (show-help)))))
    
    (send canvas update-game-over)
    (reset-score)
    (send canvas min-width (ceiling (* board-width cell-w #e2.5)))
    (send canvas min-height (ceiling (* board-height cell-h #e2.5)))
    (send frame show #t)
    (void (yield semaphore))))

; (make-same-bitmap "same.png")

