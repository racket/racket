#lang racket/base
(require racket/class
         racket/unit
         racket/list
         racket/gui/base
         racket/math
         "../show-scribbling.ss")

(provide game@)

(define game@
  (unit
    (import)
    (export)
    
    (define board-width 20)
    (define board-height 16)
    (define colors (map (lambda (x) (make-object color% x))
                        (list "blue" "red" "brown" "forestgreen" "purple")))
    (define pale-colors 
      (for/list ([x (in-list colors)])
        (define (paleize x) (- 255 (floor (* (- 255 x) 2/3))))
        (make-object color%
          (paleize (send x red))
          (paleize (send x green))
          (paleize (send x blue)))))
    
    ;; these are the sizes that the on-paint callback draws at;
    ;; a scaling factor is applied to make the board fit the window
    (define cell-w 11)
    (define cell-h 11)
    (define pen-size 10)
    
    (define (blob-sel-x b) (vector-ref b 1))
    (define (blob-sel-y b) (vector-ref b 2))
    
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
          (define this-vector (vector-ref (vector-ref board x) y))
          (define prev-vector (vector-ref (vector-ref board prev-x) prev-y))
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
      (define bonus-per-ball 50) ;; number of points for clearing each of those last 'bonus-start' balls
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
        (define mouse-over-chosen? #t)
        
        (define/override (on-paint)
          (define dc (get-dc))
          (send dc erase)
          (define-values (cw ch) (get-client-size))
          (define pen-size (- (floor (min cell-w cell-h)) 2))
          (send dc set-brush "black" 'transparent)
          (send dc set-smoothing 'smoothed)
          (send dc set-scale 
                (/ cw (* board-width cell-w)) 
                (/ ch (* board-height cell-h)))
          (define painted (make-hash))
          (for* ([i (in-range 0 board-width)]
                 [j (in-range 0 board-height)])
            (unless (hash-ref painted (xy->key i j) #f)
              (define color (vector-ref (vector-ref (vector-ref board i) j) 0))
              (when color
                (define blob (find-same-colors i j))
                (for ([x (in-list blob)])
                  (hash-set! painted (xy->key (blob-sel-x x) (blob-sel-y x)) #t))
                (update-pen/draw-blob blob dc color cell-w cell-h i j))))
          (when game-over?
            (paint-game-over)))
        
        (define/private (update-pen/draw-blob blob dc color cell-w cell-h i j)
          (define mouse-over? #f)
          (define mouse-clicked-over? #f)
          (define multiple-cells? #f)
          
          (when (or (number? mouse-current-x)
                    (number? mouse-clicked-x))
            (for ([obj (in-list blob)])
              (define x (blob-sel-x obj))
              (define y (blob-sel-y obj))
              (when (or (not (equal? x i))
                        (not (equal? y j)))
                (set! multiple-cells? #t))
              (when (and (equal? x mouse-current-x)
                         (equal? y mouse-current-y))
                (set! mouse-over? #t))
              (when (and (equal? x mouse-clicked-x)
                         (equal? y mouse-clicked-y))
                (set! mouse-clicked-over? #t))))
          
          (cond
            [mouse-clicked-x ;; has the mouse been clicked in a clickable place?
             (cond 
               [(and mouse-over? mouse-clicked-over? multiple-cells?)
                (send dc set-pen (list-ref pale-colors color) (* pen-size 2/3) 'solid)
                (draw-blob blob i j)]
               [(and mouse-over? mouse-clicked-over?)
                (send dc set-pen
                      (list-ref colors color)
                      pen-size
                      'solid)
                (draw-blob blob i j)]
               [else
                (send dc set-pen
                   (list-ref colors color)
                   pen-size
                   'solid)
                (draw-blob blob i j)])]
            [else
             (cond
               [mouse-over?
                (send dc set-pen (list-ref pale-colors color) (* pen-size 2/3) 'solid)
                (draw-blob blob i j)]
               [else
                (send dc set-pen (list-ref colors color) pen-size 'solid)
                (draw-blob blob i j)])]))
        
        (define (draw-blob blob i j)
          (define dc (get-dc))
          (define (connect x1 y1 x2 y2)
            (send dc draw-line 
                  (+ (/ cell-w 2) (* x1 cell-w))
                  (+ (/ cell-h 2) (* y1 cell-h))
                  (+ (/ cell-w 2) (* x2 cell-w))
                  (+ (/ cell-h 2) (* y2 cell-h))))
          (cond
            [(null? (cdr blob))
             (define pt (car blob))
             (connect (blob-sel-x pt) (blob-sel-y pt) (blob-sel-x pt) (blob-sel-y pt))]
            [else
             (for* ([b1 (in-list blob)]
                    [b2 (in-list blob)])
               (when (= (+ (abs (- (blob-sel-x b1) (blob-sel-x b2)))
                           (abs (- (blob-sel-y b1) (blob-sel-y b2))))
                        1)
                 (connect (blob-sel-x b1) (blob-sel-y b1) (blob-sel-x b2) (blob-sel-y b2))))]))
        
        (define/private (xy->key x y) (+ (* board-width y) x))
        
        (define/public (find-same-colors i j)
          (let* ([index (vector-ref (vector-ref (vector-ref board i) j) 0)]
                 [ans
                  (let loop ([i i]
                             [j j]
                             [ps null])
                    (cond
                      [(not (and (<= 0 i) (< i board-width)
                                 (<= 0 j) (< j board-height)))
                       ps]
                      [(vector-ref (vector-ref (vector-ref board i) j) 1) ps]
                      [(not (vector-ref (vector-ref (vector-ref board i) j) 0)) ps]
                      [(= index (vector-ref (vector-ref (vector-ref board i) j) 0))
                       (let ([v (vector-ref (vector-ref board i) j)])
                         (vector-set! v 1 #t)
                         (loop (+ i 1)
                               j
                               (loop (- i 1)
                                     j
                                     (loop i
                                           (- j 1)
                                           (loop i
                                                 (+ j 1)
                                                 (cons (vector v i j)
                                                       ps))))))]
                      [else ps]))])
            (for-each (lambda (p) (vector-set! (vector-ref p 0) 1 #f)) ans)
            ans))
        
        (define/public (paint-game-over)
          (define dc (get-dc))
          (send dc set-font font)
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
        
        
        [define game-over "Game Over"]
        [define font (make-object font% 24 'decorative 'normal 'normal #f)]
        [define turned null]
        
        [define/public (call-with-dc proc)
          ;; Since we're not in `on-paint', need to manually
          ;; suspend and resume flushing, so that intermediate
          ;; states are not flushed to the screen.
          (let ([dc (get-dc)])
            (send dc suspend-flush)
            (proc dc)
            (send dc resume-flush))]
        
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
              (make-a-move)
              (update-game-over)
              (refresh)))
          
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
                   (define num (length (find-same-colors x y)))
                   (if (= num 1)
                       ""
                       (format "~a" (calc-score num)))]
                  [else ""])))
          
        (define/private (make-a-move)
          (define i mouse-clicked-x)
          (define j mouse-clicked-y)
          (let ([same-colors (find-same-colors i j)])
            
            (when (>= (length same-colors) 2)
              
              ;; slide down empty pieces
              (let ([is null])
                (for-each
                 (lambda (p)
                   (let ([i (blob-sel-x p)]
                         [j (blob-sel-y p)])
                     (unless (member i is)
                       (set! is (cons i is)))
                     (let loop ([x j])
                       (cond
                         [(<= 1 x)
                          (let ([next (vector-ref (vector-ref board i) (- x 1))]
                                [this (vector-ref (vector-ref board i) x)])
                            (vector-set! this 0 (vector-ref next 0))
                            (loop (- x 1)))]
                         [else
                          (vector-set! (vector-ref (vector-ref board i) x) 0 #f)]))))
                 (sort same-colors
                       (lambda (x y) (<= (blob-sel-y x) (blob-sel-y y)))))
                
                ;; slide empty over empty rows
                (set! is (sort is >))
                (let ([empty-is 
                       (filter (lambda (i)
                                 (not (vector-ref 
                                       (vector-ref (vector-ref board i) (- board-height 1))
                                       0)))
                               is)])
                  (let ([is (if (null? empty-is)
                              is
                              (filter (lambda (x) (< x (car empty-is)))
                                      is))])
                    (for-each (lambda (empty-i)
                                (let loop ([i empty-i])
                                  (cond
                                    [(<= i (- board-width 2))
                                     (vector-set! board i (vector-ref board (+ i 1)))
                                     (loop (+ i 1))]
                                    [(= i (- board-width 1))
                                     (vector-set! 
                                      board
                                      i
                                      (build-vector board-height
                                                    (λ (i) (vector #f #f))))])))
                              empty-is))))
              
              
              ;; tally disappearing balls
              (update-score (length same-colors)))))
        
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
                                     (> (length (find-same-colors (sub1 i) (sub1 j))) 1)))]))))])))))
        
        
        
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
      (send canvas update-game-over)
      (when redraw?
        (send canvas refresh)))
    
    (define frame (make-object same-frame% "Same"))
    (define panel (make-object vertical-panel% frame))
    (define canvas (make-object same-canvas% panel))
    (define hp (new horizontal-panel% [parent panel] [stretchable-height #f]))
    (new message% [label "Total Score: "] [parent hp])
    (define score-message (new message% [label ""] [parent hp] [stretchable-width #t]))
    (new message% [label "This Score: "] [parent hp])
    (define this-score-message (new message% [label ""] [parent hp] [stretchable-width #t]))
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
    (send canvas min-width (* board-width cell-w 3))
    (send canvas min-height (* board-height cell-h 3))
    (send frame show #t)
    (void (yield semaphore))))
