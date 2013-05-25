#lang racket
(require "board.rkt"
         "../show-scribbling.rkt"
         racket/gui
         racket/class
         racket/unit)

(provide game@ lights-out^)

(define-signature lights-out^ (init-board))

(define game@ (unit (import)
(export lights-out^) ;; : (board -> void) resets the window(s)

(define frame (make-object frame% "Lights Out"))

(define label-size 30)

(define orange (make-object color% 255 165 0))
(define light-orange (make-object color% 255 220 100))

(define on-pen (send the-pen-list find-or-create-pen orange 1 'solid))
(define on-brush (send the-brush-list find-or-create-brush orange 'solid))
(define off-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
(define off-brush (send the-brush-list find-or-create-brush "BLACK" 'solid))

(define dull-on-pen (send the-pen-list find-or-create-pen light-orange 1 'solid))
(define dull-on-brush (send the-brush-list find-or-create-brush light-orange 'solid))
(define dull-off-pen (send the-pen-list find-or-create-pen "DARK GRAY" 1 'solid))
(define dull-off-brush (send the-brush-list find-or-create-brush "DARK GRAY" 'solid))

(define (flip-one i j)
  (when (and (<= 0 i (- (vector-length current-board) 1))
             (<= 0 j (- (vector-length current-board) 1)))
    (vector-set!
     (vector-ref current-board j)
     i
     (case (vector-ref (vector-ref current-board j) i)
       [(x) 'o]
       [(o) 'x]))))

(define (flip-surrounding i j)
  (flip-one i j)
  (flip-one (- i 1) j)
  (flip-one i (- j 1))
  (flip-one (+ i 1) j)
  (flip-one i (+ j 1)))

(define current-board #f)
(define original-board #f)

(define board-canvas%
  (class canvas%
    (inherit get-dc get-client-size)

    (define/private (get-width) (let-values ([(w h) (get-client-size)]) w))
    (define/private (get-height) (let-values ([(w h) (get-client-size)]) h))

    [define dull-i 1]
    [define dull-j 1]
    [define/private tile->screen
      (lambda (i j)
        (let ([x (inexact->exact (floor (* (/ i (vector-length current-board)) (- (get-width) 2))))]
              [y (inexact->exact (floor (* (/ j (vector-length current-board)) (- (get-height) 2))))]
              [w (inexact->exact (floor (* (/ (- (get-width) 2) (vector-length current-board)))))]
              [h (inexact->exact (floor (* (/ (- (get-height) 2) (vector-length current-board)))))])
          (values (+ x 2)
                  (+ y 2)
                  (max 0 (- w 2))
                  (max 0 (- h 2)))))]
    [define/private screen->tile
      (lambda (x y)
        (values (inexact->exact (floor (* (/ x (get-width)) (vector-length current-board))))
                (inexact->exact (floor (* (/ y (get-height)) (vector-length current-board))))))]
    [define/private draw-tile
      (lambda (dc i j)
        (when (and (<= 0 i (- (vector-length current-board) 1))
                   (<= 0 j (- (vector-length current-board) 1)))
          (let ([ent (vector-ref (vector-ref current-board j) i)]
                [dull? (and dull-i
                            dull-j
                            (or (and (= i dull-i) (= j dull-j))
                                (and (= i (- dull-i 1)) (= j dull-j))
                                (and (= i (+ dull-i 1)) (= j dull-j))
                                (and (= i dull-i) (= j (- dull-j 1)))
                                (and (= i dull-i) (= j (+ dull-j 1)))))])
            (if dull?
              (if (eq? ent 'x)
                (begin (send dc set-pen dull-off-pen)
                       (send dc set-brush dull-off-brush))
                (begin (send dc set-pen dull-on-pen)
                       (send dc set-brush dull-on-brush)))
              (if (eq? ent 'x)
                (begin (send dc set-pen on-pen)
                       (send dc set-brush on-brush))
                (begin (send dc set-pen off-pen)
                       (send dc set-brush off-brush)))))
          (let-values ([(x y w h) (tile->screen i j)])
            (send dc draw-rectangle x y w h))))]
    [define/private get-changed
      (lambda (x y)
        (if (and x y)
          (list (cons x y)
                (cons (+ x 1) y)
                (cons (- x 1) y)
                (cons x (- y 1))
                (cons x (+ y 1)))
          null))]
    [define/public redraw
      (lambda ()
        (let* ([dc (get-dc)])
          (let loop ([j (vector-length current-board)])
            (if (zero? j)
              (void)
              (begin (let loop ([i (vector-length current-board)])
                       (if (zero? i)
                         (void)
                         (begin (draw-tile dc (- i 1) (- j 1))
                                (loop (- i 1)))))
                     (loop (- j 1)))))))]

    [define/override on-event
      (lambda (evt)
        (cond
          [(send evt button-up?)
           (let-values ([(x y) (screen->tile (send evt get-x) (send evt get-y))])
             (flip-surrounding x y)
             (redraw))]
          [(send evt leaving?)
           (let ([changed (get-changed dull-i dull-j)])
             (set! dull-i #f)
             (set! dull-j #f)
             (for-each (lambda (pair) (draw-tile (get-dc) (car pair) (cdr pair)))
                       changed))]
          [(send evt moving?)
           (let ([changed-one (get-changed dull-i dull-j)])
             (let-values ([(x y) (screen->tile (send evt get-x) (send evt get-y))])
               (set! dull-i x)
               (set! dull-j y))
             (let ([changed-two (get-changed dull-i dull-j)])
               (for-each (lambda (pair) (draw-tile (get-dc) (car pair) (cdr pair)))
                         (append changed-one changed-two))))]
          [else (void)]))]
    [define/override on-paint
      (lambda ()
        (send (get-dc) clear)
        (redraw))]
    (super-instantiate () (parent frame))))

(define board-canvas (make-object board-canvas%))
(send board-canvas min-width 100)
(send board-canvas min-height 100)

(define (copy-board board)
  (list->vector
   (map (lambda (x) (list->vector (vector->list x)))
        (vector->list board))))

(define (init-board new-board)
  (set! current-board new-board)
  (set! original-board (copy-board new-board))
  (send board-canvas on-paint))

(define button-panel (make-object horizontal-panel% frame))

(make-object button% "New" button-panel
  (lambda x
    (let ([res (new-board)])
      (when res
        (init-board res)))))

(make-object button% "Reset" button-panel
  (lambda x
    (init-board original-board)))

(let ([help (show-scribbling '(lib "games/scribblings/games.scrbl") "lights-out")])
  (make-object button% "Help" button-panel (lambda x (help))))

(make-object grow-box-spacer-pane% button-panel)
(send button-panel stretchable-height #f)

(init-board (random-board
             (+ 3 (random 2) (random 2) (random 2) (random 2) (random 2))))
;; (send frame stretchable-width #f)
;; (send frame stretchable-height #f)
(send frame show #t)))
