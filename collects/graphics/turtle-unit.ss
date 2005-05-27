(module turtle-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred-sig.ss" "mred")
	   (lib "class.ss")
	   (lib "class100.ss")
	   (lib "list.ss")
	   (lib "etc.ss")
	   "turtle-sig.ss")
  (provide turtle@)
  
  (define turtle@
    (unit/sig turtle^
      (import [mred : mred^])
      
      (define turtles:window #f)
      (define turtles:shown? #f)
      
      (define pi 3.141592653589793)
      (define pi/2 (/ pi 2))
      
      (define icon-pen (send mred:the-pen-list find-or-create-pen "SALMON" 1 'xor))
      (define icon-brush (send mred:the-brush-list find-or-create-brush "SALMON" 'xor))
      (define blank-pen (send mred:the-pen-list find-or-create-pen "BLACK" 1 'transparent))
      (define w-pen (send mred:the-pen-list find-or-create-pen "white" 1 'solid))
      (define b-pen (send mred:the-pen-list find-or-create-pen "black" 1 'solid))
      
      (define show-turtle-icons? #t)
      
      ;; turtle-style : (union 'triangle 'line 'empty)
      (define turtle-style 'triangle)
      
      (define plot-window%
        (class100 mred:frame% (name width height)
          
          (private-field
           [bitmap (make-object mred:bitmap% width height #t)])      
          
          (inherit show)
          (private-field
           [memory-dc (make-object mred:bitmap-dc%)]
           [pl (make-object mred:point% 0 0)]
           [pr (make-object mred:point% 0 0)]
           [ph (make-object mred:point% 0 0)]
           [points (list pl pr ph)])
          (public
            [get-canvas
             (lambda ()
               canvas)]
            [flip-icons
             (lambda ()
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
                  (void)]))]
            [clear
             (lambda () 
               (send memory-dc clear)
               (send canvas on-paint))])
          (sequence
            (send memory-dc set-bitmap bitmap)
            (send memory-dc clear)
            (super-init name #f width height))
          
          (public
            [on-menu-command (lambda (op) (turtles #f))])
          (private-field
           [menu-bar (make-object mred:menu-bar% this)]
           [file-menu (make-object mred:menu% "File" menu-bar)])
          (sequence 
            (make-object mred:menu-item%
              "Print"
              file-menu
              (lambda (_1 _2)
                (print)))
            (make-object mred:menu-item%
              "Close"
              file-menu
              (lambda (_1 _2)
                (turtles #f))))
          
          (public
            [save-turtle-bitmap
             (lambda (fn type)
               (send bitmap save-file fn type))])
          
          (private-field
           [canvas% 
            (class100 mred:canvas% args
              (inherit get-dc)
              (override
                [on-paint
                 (lambda ()
                   (let ([dc (get-dc)])
                     (send dc clear)
                     (send dc draw-bitmap (send memory-dc get-bitmap) 0 0)
                     (flip-icons)))])
              (sequence (apply super-init args)))]
           [canvas (make-object canvas% this)]
           [dc (send canvas get-dc)])
          
          (public
            [wipe-line (lambda (a b c d)
                         (send memory-dc set-pen w-pen)
                         (send dc set-pen w-pen)
                         (send memory-dc draw-line a b c d)
                         (send dc draw-line a b c d)
                         (send memory-dc set-pen b-pen)
                         (send dc set-pen b-pen))]
            [draw-line (lambda (a b c d)
                         (send memory-dc draw-line a b c d)
                         (send dc draw-line a b c d))])
          (sequence
            (send canvas min-width width)
            (send canvas min-height height)
            (send this clear))))
      
      (define turtle-window-size
        (let-values ([(w h) (mred:get-display-size)]
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
      (define inner-flip-icons init-error)
      (define inner-save-turtle-bitmap init-error)
      
      (define line
        (lambda (a b c d)
          (set! lines-in-drawing (cons (make-draw-line a b c d) lines-in-drawing))
          (inner-line a b c d)))
      (define do-wipe-line
        (lambda (a b c d)
          (set! lines-in-drawing (cons (make-wipe-line a b c d) lines-in-drawing))
          (inner-wipe-line a b c d)))
      (define (flip-icons) (inner-flip-icons))
      
      (define clear-window (lambda () (inner-clear-window)))
      (define save-turtle-bitmap (lambda (x y) (inner-save-turtle-bitmap x y)))
      
      (define turtles
        (case-lambda
          [() (turtles #t)]
          [(x)
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
             (set! inner-save-turtle-bitmap (lambda x (send turtles:window save-turtle-bitmap . x)))
             (set! flip-icons (lambda x (send turtles:window flip-icons . x))))
           (send turtles:window show x)
           (send turtles:window get-canvas)]))
      
      (define clear 
        (lambda ()
          (set! turtles-cache empty-cache)
          (set! turtles-state (list clear-turtle))
          (set! lines-in-drawing null)
          (clear-window)))
      
      ;; cache elements:
      (define-struct c-forward (distance))
      (define-struct c-turn (angle))
      (define-struct c-draw (distance))
      (define-struct c-offset (x y))
      
      ;; combines a cache-element and a turtle-offset.
      ;; turtle-offsets are represented as turtles, 
      ;; however they are deltas, not absolutes.
      (define combine
        (lambda (entry cache)
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
             (error 'turtles-cache "illegal entry in cache: ~a" entry)])))
      
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
            (flip-icons)
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
                 (make-turtle newx newy angle))))
            (flip-icons))))
      
      (define draw (draw/erase (lambda (a b c d) (line a b c d))))
      (define erase (draw/erase (lambda (a b c d) (do-wipe-line a b c d))))
      
      (define move
        (lambda (n)
          (flip-icons)
          (set! turtles-cache (combine (make-c-forward n) turtles-cache))
          (flip-icons)))
      
      (define turn/radians
        (lambda (d)
          (flip-icons)
          (set! turtles-cache (combine (make-c-turn d) turtles-cache))
          (flip-icons)))
      
      (define turn
        (lambda (c)
          (turn/radians (* (/ c 360) 2 pi))))
      
      (define move-offset
        (lambda (x y)
          (flip-icons)
          (set! turtles-cache (combine (make-c-offset x y) turtles-cache))
          (flip-icons)))
      
      (define erase/draw-offset
        (lambda (doit)
          (lambda (x y)
            (flip-icons)
            (flatten
             (lambda (turtle)
               (let* ([tx (turtle-x turtle)]
                      [ty (turtle-y turtle)]
                      [newx (+ tx x)]
                      [newy (+ ty y)])
                 (doit tx ty newx newy)
                 (make-turtle newx newy (turtle-angle turtle)))))
            (flip-icons))))
      
      (define erase-offset (erase/draw-offset (lambda (a b c d) (do-wipe-line a b c d))))
      (define draw-offset (erase/draw-offset (lambda (a b c d) (line a b c d))))
      
      (define splitfn
        (lambda (e)
          (let ([t turtles-state]
                [c turtles-cache])
            (e)
            (flip-icons)
            (set! turtles-state
                  (make-tree (list (make-cached turtles-state turtles-cache)
                                   (make-cached t c))))
            (set! turtles-cache empty-cache)
            (flip-icons))))
      
      (define split*fn
        (lambda (es)
          (let ([t turtles-state]
                [c turtles-cache]
                [l '()])
            (for-each (lambda (x)
                        (x)
                        (set! l (cons (make-cached turtles-state turtles-cache) l))
                        (flip-icons)
                        (set! turtles-state t)
                        (set! turtles-cache c)
                        (flip-icons))
                      es)
            (flip-icons)
            (set! turtles-cache empty-cache)
            (set! turtles-state (make-tree l))
            (flip-icons))))
      
      
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
               (flip-icons)
               (set! turtles-cache save-turtles-cache)
               (set! turtles-state save-turtles-state)
               (flip-icons))))))
      
      
      (define-struct drawing-line (x1 y1 x2 y2))
      (define-struct (wipe-line drawing-line) ())
      (define-struct (draw-line drawing-line) ())
      (define lines-in-drawing null)
      
      (define (draw-lines-into-dc dc)
        (for-each (lambda (line)
                    (cond
                      [(wipe-line? line) (send dc set-pen w-pen)]
                      [(draw-line? line) (send dc set-pen b-pen)])
                    (send dc draw-line
                          (drawing-line-x1 line)
                          (drawing-line-y1 line)
                          (drawing-line-x2 line)
                          (drawing-line-y2 line)))
                  lines-in-drawing))
      
      ;; used to test printing
      (define (display-lines-in-drawing)
        (let* ([lines-in-drawing-canvas%
                (class100 mred:canvas% (frame)
                  (inherit get-dc)
                  (override
                    [on-paint
                     (lambda ()
                       (draw-lines-into-dc (get-dc)))])
                  (sequence
                    (super-init frame)))]
               [frame (make-object mred:frame% "Lines in Drawing")]
               [canvas (make-object lines-in-drawing-canvas% frame)])
          (send frame show #t)))
      
      
      (define (print)
        (case (system-type)
          [(macos macosx windows)
           (let ([dc (make-object mred:printer-dc%)])
             (send dc start-doc "Turtles")
             (send dc start-page)
             (draw-lines-into-dc dc)
             (send dc end-page)
             (send dc end-doc))]
          [(unix)
           (let ([dc (make-object mred:post-script-dc%)])
             (send dc start-doc "Turtles")
             (send dc start-page)
             (draw-lines-into-dc dc)
             (send dc end-page)
             (send dc end-doc))]
          [else
           (mred:message-box "Turtles"
                             "Printing is not supported on this platform")])))))