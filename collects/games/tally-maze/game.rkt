#lang racket/base

(require "maze.rkt" 
         (except-in "godel.rkt" unit/s)
         "../show-scribbling.rkt"
         racket/gui/base
         racket/class
         racket/set
         racket/list
         racket/path
         racket/runtime-path
         racket/unit)

(provide game@)

(define-runtime-path bmps "images")
(define big-pumpkin (read-bitmap (build-path bmps "pumpkin" "pumpkin-64x64.png")))
(define two-pumpkins (make-bitmap (send big-pumpkin get-width) (send big-pumpkin get-height)))
(let ([small-pumpkin (read-bitmap (build-path bmps "pumpkin" "pumpkin-48x48.png"))]
      [bdc (make-object bitmap-dc% two-pumpkins)])
  (send bdc draw-bitmap small-pumpkin 0 0)
  (send bdc draw-bitmap small-pumpkin
        (- (send big-pumpkin get-width)
           (send small-pumpkin get-width))
        (- (send big-pumpkin get-height)
           (send small-pumpkin get-height)))
  (send bdc set-bitmap #f))

(define small-icon-size 4)
(define (mk-small color)
  (define bmp (make-bitmap small-icon-size small-icon-size))
  (define bdc (make-object bitmap-dc% bmp))
  (send bdc set-brush color 'solid)
  (send bdc set-pen "black" 1 'transparent)
  (send bdc draw-ellipse 0 0 small-icon-size small-icon-size)
  (send bdc set-bitmap #f)
  bmp)

(define small-pumpkin (mk-small "orange"))
(define small-player (mk-small "blue"))

(define game@
  (unit (import)
        (export)
(define maze-w 12)
(define maze-h 12)

(struct state 
  (maze-index maze edges 
              player
              computer1
              computer2
              player-icon)
  #:transparent)
(define maze-count (spec-k (maze/s maze-w maze-h)))

(define (state-next-edges the-state)
  (build-walls
   (decode (maze/s maze-w maze-h)
           (modulo (+ (state-maze-index the-state) 1)
                   maze-count))
   maze-w
   maze-h))

(define (fill-in-maze the-state new-val)
  (define current-maze (decode-maze maze-w maze-h new-val))
  (struct-copy state the-state
               [maze-index new-val]
               [maze current-maze]
               [edges (build-walls current-maze maze-w maze-h)]))

(define initial-number (pick-a-maze maze-w maze-h))
(define the-states
  (list (fill-in-maze (state #f #f #f 
                             (cons 0 0)
                             (cons (- maze-w 1) (- maze-h 1))
                             (cons (- maze-w 1) (- maze-h 1))
                             21) 
                      initial-number)))
(define (current-state) (car the-states))
(define (set-the-states! new-states)
  (set! the-states new-states)
  (send game-canvas refresh))

(define (next-state! state)
  (set-the-states! (cons state the-states)))

(define (get-player-icon the-state)
  (cond
    [(and (= (car (state-player the-state)) (- maze-w 1))
          (= (cdr (state-player the-state)) (- maze-h 1)))
     ;; winner
     (pick '(1))]
    [(edge-connecting? (state-edges the-state)
                       (state-player the-state)
                       (cons (- maze-w 1) (- maze-h 1)))
     ;; about to win
     (pick '(19))]
    [(or (edge-connecting? (state-edges the-state)
                           (state-computer1 the-state)
                           (state-player the-state))
         (edge-connecting? (state-edges the-state)
                           (state-computer2 the-state)
                           (state-player the-state)))
     ;; about to lose
     (pick '(20 35))]
    [else 
     ;; nothing much going on
     (pick '(21 36 37))]))

(define (edge-connecting? edges a b) (set-member? (hash-ref edges a) b))

(define (pick args)
  (define pr (state-player (current-state)))
  (list-ref args (modulo (+ (car pr) (cdr pr))
                         (length args))))

(define players (make-hash))
(for ([file (directory-list (build-path bmps "very-emotional") #:build? #t)])
  (define m (regexp-match #rx"([0-9]+)[.]png$" file))
  (when m
    (hash-set! players (string->number (cadr m)) (read-bitmap file))))

(define (move dx dy)
  (unless (game-over?)
    (define new-x (+ dx (car (state-player (current-state)))))
    (define new-y (+ dy (cdr (state-player (current-state)))))
    (define new-pr (cons new-x new-y))
    (when (and (<= 0 new-x (- maze-w 1))
               (<= 0 new-y (- maze-w 1))
               (edge-connecting? (state-edges (current-state))
                                 (state-player (current-state))
                                 new-pr))
      (next-state!
       (struct-copy state (move-computer (current-state))
                    [player new-pr])))))

(define (stay-put)
  (next-state! (move-computer (current-state))))

(define (next-maze)
  (define next-maze-state
    (fill-in-maze (current-state)
                  (modulo (+ (state-maze-index (current-state)) 1) maze-count)))
  (next-state!
   (if (game-over?)
       next-maze-state
       (move-computer next-maze-state))))

(define (undo-maze)
  (unless (null? (cdr the-states))
    (set-the-states! (cdr the-states))))

(define (move-computer the-state)
  (cond
    [(or (equal? (state-player (current-state)) 
                 (state-computer1 (current-state)))
         (equal? (state-player (current-state)) 
                 (state-computer2 (current-state))))
     the-state]
    [else
     (define end (state-player the-state))
     (define this-edges (state-edges the-state))
     (define next-edges (state-next-edges the-state))
     
     (define-values (this-maze-c1 this-maze-c1-dist)
       (preferred-direction this-edges (state-computer1 the-state) end))
     (define-values (this-maze-c2 this-maze-c2-dist)
       (preferred-direction this-edges (state-computer2 the-state) end))
     
     (define-values (next-maze-c1 next-maze-c1-dist)
       (preferred-direction next-edges (state-computer1 the-state) end))
     (define-values (next-maze-c2 next-maze-c2-dist)
       (preferred-direction next-edges (state-computer2 the-state) end))
     (cond
       [(<= this-maze-c1-dist this-maze-c2-dist)
        (struct-copy state the-state
                     [computer1 this-maze-c1]
                     [computer2 (if (edge-connecting? this-edges 
                                                      (state-computer2 the-state)
                                                      next-maze-c2)
                                    next-maze-c2
                                    (state-computer2 the-state))])]
       [else
        (struct-copy state the-state
                     [computer1 (if (edge-connecting? this-edges 
                                                      (state-computer1 the-state)
                                                      next-maze-c1)
                                    next-maze-c1
                                    (state-computer1 the-state))]
                     [computer2 this-maze-c2])])]))

(define (preferred-direction edges start end)
  (define visited (make-hash))
  (define dir
    (let loop ([node start]
               [dist 0])
      (cond
        [(hash-ref visited node #f) #f]
        [else
         (hash-set! visited node dist)
         (cond
           [(equal? node end) 
            node]
           [else
            (for/or ([neighbor (in-set (hash-ref edges node))])
              (and (loop neighbor (+ dist 1))
                   neighbor))])])))
  (values dir (hash-ref visited end)))
  

(define (add1/f n) (and n (+ n 1)))

(define game-canvas%
  (class canvas%
    (inherit get-dc get-client-size)
    (define/override (on-paint)
      (define dc (get-dc))
      (send dc set-smoothing 'smoothed)
      (define-values (w h) (get-client-size))
      (draw-a-state dc 0 0 w h (current-state) #f))
    (define/override (on-char evt) 
      (case (send evt get-key-code)
        [(left) (move -1 0)]
        [(up) (move 0 -1)]
        [(right) (move 1 0)]
        [(down) (move 0 1)]
        [(#\space #\.) (stay-put)]
        [(#\n) (next-maze)]
        [(#\z) (undo-maze)]))
    (super-new)))

(define (draw-a-state dc dx dy w h the-state small?)
  (draw-maze dc dx dy 
             w h (state-edges the-state)
             maze-w maze-h
             #:images 
             (cons (list (if small?
                             (list small-player) 
                             (list (hash-ref players (get-player-icon the-state))))
                         (car (state-player the-state))
                         (cdr (state-player the-state)))
                   (if (equal? (state-computer1 the-state)
                               (state-computer2 the-state))
                       (list (list (if small? (list small-pumpkin) (list two-pumpkins))
                                   (car (state-computer1 the-state))
                                   (cdr (state-computer1 the-state))))
                       (list (list (if small? (list small-pumpkin) (list big-pumpkin))
                                   (car (state-computer1 the-state))
                                   (cdr (state-computer1 the-state)))
                             (list (if small? (list small-pumpkin) (list big-pumpkin))
                                   (car (state-computer2 the-state))
                                   (cdr (state-computer2 the-state))))))))

(define (game-over?)
  (or (equal? (state-player (current-state)) 
              (state-computer1 (current-state)))
      (equal? (state-player (current-state)) 
              (state-computer2 (current-state)))
      (and (= (car (state-player (current-state))) (- maze-w 1))
           (= (cdr (state-player (current-state))) (- maze-h 1)))))
  
(define min-cell-size 55)
(define f (new frame% [label "Tally Maze"] [width 600] [height 600]))
(define game-canvas (new game-canvas% 
                         [parent f]
                         [min-width (* maze-w min-cell-size)]
                         [min-height (* maze-h min-cell-size)]))
(define hp (new horizontal-panel% [parent f] [alignment '(right center)] [stretchable-height #f]))
(define msg (new message% [parent hp] [label (format "Game #~a" initial-number)]))
(void (new vertical-panel% [parent hp]))
(define show-help (show-scribbling
                   '(lib "games/scribblings/games.scrbl")
                   "tally-maze"))
(define help-button (new button%
                         [label "Help"]
                         [parent hp]
                         [callback (lambda (_1 _2) (show-help))]))
(send f show #t)))

(module+ main (invoke-unit game@))
