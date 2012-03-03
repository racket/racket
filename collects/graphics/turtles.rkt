#lang racket/base

(require racket/gui/base
         (for-syntax racket/base)
         racket/class)

(provide turtles
         clear home
         turn turn/radians
         move move-offset
         draw draw-offset
         erase erase-offset
         
         save-turtle-bitmap
         
         turtle-window-size
         
         split split* tprompt)

(define turtles:window #f)
(define turtles:shown? #f)

(define pi 3.141592653589793)
(define pi/2 (/ pi 2))

(define icon-pen (send the-pen-list find-or-create-pen "SALMON" 1 'solid))
(define icon-brush (send the-brush-list find-or-create-brush "SALMON" 'solid))
(define blank-pen (send the-pen-list find-or-create-pen "BLACK" 1 'transparent))
(define w-pen (send the-pen-list find-or-create-pen "white" 2 'solid))
(define b-pen (send the-pen-list find-or-create-pen "black" 1 'solid))

(define show-turtle-icons? #t)

;; turtle-style : (union 'triangle 'line 'empty)
(define turtle-style 'triangle)

(define plot-window%
  (class frame% 
    (init-field name width height)
    
    (define bitmap (make-bitmap width height))      
    
    (inherit show)
    (define memory-dc (new bitmap-dc%))
    (define pl (make-object point% 0 0))
    (define pr (make-object point% 0 0))
    (define ph (make-object point% 0 0))
    (define points (list pl pr ph))
    
    (define/public (get-canvas) canvas)
    (define/private (draw-turtle-icons)
      (case turtle-style
        [(triangle line)
         (flatten (lambda (x) x))
         (let* ([dc (send canvas get-dc)]
                [proc
                 (if (eq? turtle-style 'line)
                     (lambda (turtle)
                       (let ([x (turtle-x turtle)]
                             [y (turtle-y turtle)]
                             [theta (turtle-angle turtle)]
                             [size 2])
                         (send dc draw-line
                               x y
                               (+ x (* size (cos theta)))
                               (+ y (* size (sin theta))))))
                     (lambda (turtle)
                       (let* ([x (turtle-x turtle)]
                              [y (turtle-y turtle)]
                              [theta (turtle-angle turtle)]
                              [long-size 20]
                              [short-size 7]
                              [l-theta (+ theta pi/2)]
                              [r-theta (- theta pi/2)])
                         (send ph set-x (+ x (* long-size (cos theta))))
                         (send ph set-y (+ y (* long-size (sin theta))))
                         (send pl set-x (+ x (* short-size (cos l-theta))))
                         (send pl set-y (+ y (* short-size (sin l-theta))))
                         (send pr set-x (+ x (* short-size (cos r-theta))))
                         (send pr set-y (+ y (* short-size (sin r-theta))))
                         (send dc draw-polygon points))))])
           (if (eq? turtle-style 'line)
               (send dc set-pen icon-pen)
               (begin
                 (send dc set-pen blank-pen)
                 (send dc set-brush icon-brush)))
           (for-each proc turtles-state)
           (send dc set-pen b-pen))]
        [else
         (void)]))
    
    (define/public (clear)
      (send memory-dc clear)
      (send canvas refresh))
    
    (send memory-dc set-bitmap bitmap)
    (send memory-dc clear)
    (send memory-dc set-smoothing 'smoothed)
    (super-new [label name] [width width] [height height])
  
    (define/public (on-menu-command op) (turtles #f))
    
    (define menu-bar (make-object menu-bar% this))
    (define file-menu (make-object menu% "&File" menu-bar))
    (new menu-item%
         [label "&Print"]
         [parent file-menu]
         [callback (lambda (_1 _2) (print))]
         [shortcut #\p])
    (new menu-item%
         [label "&Close"]
         [parent file-menu]
         [callback (lambda (_1 _2) (turtles #f))]
         [shortcut #\w])
    
    (define/public (save-turtle-bitmap fn type)
      (send bitmap save-file fn type))
    
    (define t-canvas% 
      (class canvas%
        (inherit get-dc)
        (define/override (on-paint)
          (define dc (get-dc))
          (send dc set-smoothing 'aligned)
          (send dc clear)
          (send dc draw-bitmap (send memory-dc get-bitmap) 0 0)
          (draw-turtle-icons))
        (super-new)))
    (define canvas (make-object t-canvas% this))
    (define dc (send canvas get-dc))
    
    (define/public (wipe-line a b c d)
      (send memory-dc set-pen w-pen)
      (send dc set-pen w-pen)
      (send memory-dc draw-line a b c d)
      (send dc draw-line a b c d)
      (send memory-dc set-pen b-pen)
      (send dc set-pen b-pen))
    (define/public (draw-line a b c d)
      (send memory-dc draw-line a b c d)
      (send dc draw-line a b c d))
    
    (send canvas min-width width)
    (send canvas min-height height)
    (send this clear)))

(define turtle-window-size
  (let-values ([(w h) (get-display-size)]
               [(user/client-offset) 65]
               [(default-size) 800])
    (min default-size
         (- w user/client-offset)
         (- h user/client-offset))))

(define-struct turtle (x y angle))
                                        ; x : int
                                        ; y: int
                                        ; angle : int

(define-struct cached (turtles cache))
                                        ; turtles : (list-of turtle)
                                        ; cache : turtle -> turtle

(define-struct tree (children))
                                        ; children : (list-of cached)

(define clear-turtle (make-turtle (/ turtle-window-size 2)
                                  (/ turtle-window-size 2) 0))

;; turtles-state is either a
;;    - (list-of turtle) or
;;    - tree 
(define turtles-state (list clear-turtle))

;; the cache contains a turtle-offset, which is represented
;; by a turtle -- but it is a delta not an absolute.
(define empty-cache (make-turtle 0 0 0))
(define turtles-cache empty-cache)

(define init-error (lambda _ (error 'turtles "Turtles not initialized. Evaluate (turtles).")))
(define inner-line init-error)
(define inner-wipe-line init-error)
(define inner-clear-window init-error)
(define inner-save-turtle-bitmap init-error)

(define line
  (lambda (a b c d)
    (set! lines-in-drawing (cons (make-draw-line a b c d) lines-in-drawing))
    (inner-line a b c d)
    (update-icon)))
(define do-wipe-line
  (lambda (a b c d)
    (set! lines-in-drawing (cons (make-wipe-line a b c d) lines-in-drawing))
    (inner-wipe-line a b c d)
    (update-icon)))

(define clear-window (lambda () (inner-clear-window)))
(define save-turtle-bitmap (lambda (x y) (inner-save-turtle-bitmap x y)))

(define (turtles [x #t])
  (set! turtles:shown? x)
  (unless turtles:window
    (set! turtles:window
          (make-object plot-window%
            "Turtles"
            turtle-window-size
            turtle-window-size))
    (set! inner-line (lambda x (send turtles:window draw-line . x)))
    (set! inner-wipe-line (lambda x (send turtles:window wipe-line . x)))
    (set! inner-clear-window (lambda x (send turtles:window clear . x)))
    (set! inner-save-turtle-bitmap (lambda x (send turtles:window save-turtle-bitmap . x))))
  (send turtles:window show x)
  (send turtles:window get-canvas)
  (void))

(define (clear) 
  (set! turtles-cache empty-cache)
  (set! turtles-state (list clear-turtle))
  (set! lines-in-drawing null)
  (clear-window))

(define (update-icon)
  (when turtles:window
    (send turtles:window refresh)))

(define (home)
  (set! turtles-cache empty-cache)
  (set! turtles-state (list clear-turtle))
  (update-icon))

;; cache elements:
(define-struct c-forward (distance))
(define-struct c-turn (angle))
(define-struct c-draw (distance))
(define-struct c-offset (x y))

;; combines a cache-element and a turtle-offset.
;; turtle-offsets are represented as turtles, 
;; however they are deltas, not absolutes.
(define (combine entry cache)
  (cond 
    [(c-forward? entry)
     (let* ([n (c-forward-distance entry)]
            [angle (turtle-angle cache)]
            [x (turtle-x cache)]
            [y (turtle-y cache)]
            [newx (+ x (* n (cos angle)))]
            [newy (+ y (* n (sin angle)))])
       (make-turtle newx newy angle))]
    [(c-offset? entry)
     (let* ([tx (turtle-x cache)]
            [ty (turtle-y cache)]
            [newx (+ tx (c-offset-x entry))]
            [newy (+ ty (c-offset-y entry))])
       (make-turtle newx newy 
                    (turtle-angle cache)))]
    [(c-turn? entry)
     (make-turtle (turtle-x cache)
                  (turtle-y cache)
                  (- (turtle-angle cache)
                     (c-turn-angle entry)))]
    [else
     (error 'turtles-cache "illegal entry in cache: ~a" entry)]))

;; this applies an offset to a turtle.
;; an offset is a turtle, representing what would happen 
;;    if the turtle had started at zero.
(define apply-cache
  (lambda (offset)
    (let ([x (turtle-x offset)]
          [y (turtle-y offset)]
          [offset-angle (turtle-angle offset)])
      (lambda (turtle)
        (let* ([angle (turtle-angle turtle)])
          (let* ([c (cos angle)]
                 [s (sin angle)]
                 [rx (- (* x c) (* y s))]
                 [ry (+ (* y c) (* x s))])
            (make-turtle (+ rx (turtle-x turtle))
                         (+ ry (turtle-y turtle))
                         (+ offset-angle angle))))))))

(define flatten
  (lambda (at-end)
    (letrec ([walk-turtles
              (lambda (turtles cache list)
                (cond
                 [(tree? turtles)
                  (let ([children (tree-children turtles)]
                        [ac (apply-cache cache)])
                    (foldl (lambda (child list)
                             (walk-turtles (cached-turtles child)
                                           (ac (cached-cache child))
                                           list))
                           list
                           children))]
                 [else
                  (let ([f (compose at-end (apply-cache cache))])
                    (foldl (lambda (t l) (cons (f t) l)) list turtles))]))])
      (set! turtles-state (walk-turtles turtles-state turtles-cache null))
      (set! turtles-cache empty-cache))))

(define draw/erase
  (lambda (doit)
    (lambda (n)
      (flatten
       (lambda (turtle)
         (let* ([x (turtle-x turtle)]
                [y (turtle-y turtle)]
                [angle (turtle-angle turtle)]
                [d (if (zero? n) 0 (sub1 (abs n)))]
                [res (if (< n 0) (- d) d)]
                [c (cos angle)]
                [s (sin angle)]
                [drawx (+ x (* res c))]
                [drawy (+ y (* res s))]
                [newx (+ x (* n c))]
                [newy (+ y (* n s))])
           (unless (zero? n)
             (doit x y drawx drawy))
           (make-turtle newx newy angle)))))))

(define draw (draw/erase (lambda (a b c d) (line a b c d))))
(define erase (draw/erase (lambda (a b c d) (do-wipe-line a b c d))))

(define move
  (lambda (n)
    (set! turtles-cache (combine (make-c-forward n) turtles-cache))
    (update-icon)))

(define turn/radians
  (lambda (d)
    (set! turtles-cache (combine (make-c-turn d) turtles-cache))
    (update-icon)))

(define turn
  (lambda (c)
    (turn/radians (* (/ c 360) 2 pi))))

(define move-offset
  (lambda (x y)
    (set! turtles-cache (combine (make-c-offset x y) turtles-cache))
    (update-icon)))

(define erase/draw-offset
  (lambda (doit)
    (lambda (x y)
      (flatten
       (lambda (turtle)
         (let* ([tx (turtle-x turtle)]
                [ty (turtle-y turtle)]
                [newx (+ tx x)]
                [newy (+ ty y)])
           (doit tx ty newx newy)
           (make-turtle newx newy (turtle-angle turtle))))))))

(define erase-offset (erase/draw-offset (lambda (a b c d) (do-wipe-line a b c d))))
(define draw-offset (erase/draw-offset (lambda (a b c d) (line a b c d))))

(define splitfn
  (lambda (e)
    (let ([t turtles-state]
          [c turtles-cache])
      (e)
      (set! turtles-state
            (make-tree (list (make-cached turtles-state turtles-cache)
                             (make-cached t c))))
      (set! turtles-cache empty-cache)
      (update-icon))))

(define split*fn
  (lambda (es)
    (let ([t turtles-state]
          [c turtles-cache]
          [l '()])
      (for-each (lambda (x)
                  (x)
                  (set! l (cons (make-cached turtles-state turtles-cache) l))
                  (set! turtles-state t)
                  (set! turtles-cache c))
                es)
      (set! turtles-cache empty-cache)
      (set! turtles-state (make-tree l))
      (update-icon))))


(define tpromptfn
  (lambda (thunk)
    (let ([save-turtles-cache #f]
          [save-turtles-state #f])
      (dynamic-wind
          (lambda ()
            (set! save-turtles-cache turtles-cache)
            (set! save-turtles-state turtles-state))
          (lambda ()
            (thunk))
          (lambda ()
            (set! turtles-cache save-turtles-cache)
            (set! turtles-state save-turtles-state)
            (update-icon))))))


(define-struct drawing-line (x1 y1 x2 y2))
(define-struct (wipe-line drawing-line) ())
(define-struct (draw-line drawing-line) ())
(define lines-in-drawing null)

(define (draw-lines-into-dc dc)
  (for ([line (in-list lines-in-drawing)])
    (cond
      [(wipe-line? line) (send dc set-pen w-pen)]
      [(draw-line? line) (send dc set-pen b-pen)])
    (send dc draw-line
          (drawing-line-x1 line)
          (drawing-line-y1 line)
          (drawing-line-x2 line)
          (drawing-line-y2 line))))

;; used to test printing
(define (display-lines-in-drawing)
  (let* ([lines-in-drawing-canvas%
          (class canvas% 
            (init-field frame)
            (inherit get-dc)
            (define/override (on-paint)
              (draw-lines-into-dc (get-dc)))
            (super-new [parent frame]))]
         [frame (make-object frame% "Lines in Drawing")]
         [canvas (make-object lines-in-drawing-canvas% frame)])
    (send frame show #t)))


(define (print)
  (case (system-type)
    [(macosx windows)
     (let ([dc (make-object printer-dc%)])
       (send dc start-doc "Turtles")
       (send dc start-page)
       (draw-lines-into-dc dc)
       (send dc end-page)
       (send dc end-doc))]
    [(unix)
     (let ([dc (make-object post-script-dc%)])
       (send dc start-doc "Turtles")
       (send dc start-page)
       (draw-lines-into-dc dc)
       (send dc end-page)
       (send dc end-doc))]
    [else
     (message-box "Turtles"
                  "Printing is not supported on this platform")]))


(define-syntaxes (split)
  (lambda (x)
    (syntax-case x ()
      ((_ args ...)
       (syntax (splitfn (lambda () args ...)))))))

(define-syntaxes (split*)
  (syntax-rules ()
    [(_ e0 e ...)
     (split*fn (list (lambda () e0) (lambda () e) ...))]))

(define-syntaxes (tprompt)
  (lambda (x)
    (syntax-case x ()
      ((_ e1 ...)
       (syntax (tpromptfn (lambda () e1 ...)))))))
