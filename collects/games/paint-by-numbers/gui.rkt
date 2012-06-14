#lang racket/base
#|

The paint-by-numbers-canavas% class accepts two initalization
arguments. They must be lists of lists of numbers and they must be the
same length. paint-by-numbers-canvas% objects accepts four methods:

set-rect : (int int (union 'on 'off 'unknown) -> void)
Sets the grid point specified by the first two arguments to the third.
The coordinates are from the top-left and the x coordinate comes first.

get-rect : (int int ->  (union 'on 'off 'unknown))
Gets the value of the grid at the coordinates specified by the two integers

paint-rect : (int int -> void)
Draws the rectangle specified by the arguments.
Call this after calling set-rect to see the changes updated on the screen.

get-grid : (-> (list-of (list-of (union 'on 'off 'unknown 'wrong))))
Returns the current state of the entire board as a list of lists.

set-grid : ((vector-of (vector-of (union 'on 'off 'unknown 'wrong)))-> void)
Sets the state of the board. No drawing takes place

on-paint : (-> void)
Redraws the entire canvas. May be used if many rects were set.

all-unknown : (-> void)
Sets all board positions to 'unknown

close-up : (-> void)
call when canvas is closed.

See the bottom of this file for the creation of a file and a test
paint by numbers.

|#

  (require mred
           framework
           mzlib/etc
           mzlib/class)

  (provide paint-by-numbers-canvas%
           design-paint-by-numbers-canvas%)
  
  (define UNKNOWN-BRUSH (send the-brush-list find-or-create-brush "DARK GRAY" 'solid))
  (define ON-BRUSH (send the-brush-list find-or-create-brush "BLUE" 'solid))
  (define OFF-BRUSH (send the-brush-list find-or-create-brush "WHITE" 'solid))
  (define WRONG-BRUSH (send the-brush-list find-or-create-brush "RED" 'solid))
  
  (define LINES/NUMBERS-PEN (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
  
  (define BLACK-PEN (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
  (define WHITE-PEN (send the-pen-list find-or-create-pen "WHITE" 1 'solid))
  (define WHITE-BRUSH (send the-brush-list find-or-create-brush "WHITE" 'solid))
  
  (define BAR-PEN (send the-pen-list find-or-create-pen "SALMON" 1 'solid))
  (define BAR-BRUSH (send the-brush-list find-or-create-brush "SALMON" 'solid))
  
  (define-struct ado (x y before after))
  
  (define-struct pt (x y))
  
  (define paint-by-numbers-canvas%
    (class canvas%
      (init-field row-numbers col-numbers)
      (inherit get-dc get-client-size)
      
      (define/private (get-font) (send (get-dc) get-font))
      
      (define/public (get-row-numbers) row-numbers)
      (define/public (get-col-numbers) col-numbers)
      
      (define/public (get-max-col-entries)
        (apply max (map length (get-col-numbers))))
      
      [define extra-space-every 5]
      
      [define grid-x-size (length (get-col-numbers))]
      [define grid-y-size (length (get-row-numbers))]
      [define y-margin 1]
      [define x-margin 3]
      [define row-label-width 10]
      [define row-label-height 10]
      [define col-label-width 10]
      [define col-label-height 10]
      
      [define/private (get-row-label-string l)
        (if (null? l)
            ""
            (let ([first (car l)]
                  [rest (cdr l)])
              (apply string-append 
                     (number->string first)
                     (map (lambda (x) (format " ~a" x)) rest))))]
      
      [define/private get-col-label-strings
        (lambda (l)
          (map number->string l))]
      
      [define grid (build-vector grid-x-size (lambda (i) (make-vector grid-y-size UNKNOWN-BRUSH)))]
      
      [define/private get-string-height
        (lambda (s)
          (let ([dc (get-dc)])
            (let-values ([(width height descent ascent) 
                          (send dc get-text-extent s)])
              (- height descent))))]
      [define/private get-string-height/descent
        (lambda (s)
          (let ([dc (get-dc)])
            (let-values ([(width height descent ascent) 
                          (send dc get-text-extent s)])
              height)))]
      [define/private get-string-ascent
        (lambda (s)
          (let ([dc (get-dc)])
            (let-values ([(width height descent ascent) 
                          (send dc get-text-extent s)])
              ascent)))]
      [define/private get-string-width
        (lambda (s)
          (let ([dc (get-dc)])
            (let-values ([(width height descent ascent) 
                          (send dc get-text-extent s)])
              width)))]
      
      [define/private loc->string
        (lambda (x y)
          (format "(~a,~a)" x y))]
      
      [define/private xy->grid
        (lambda (x y)
          (let* ([grid-width (/ (- (get-canvas-width) row-label-width) grid-x-size)]
                 [grid-height (/ (- (get-canvas-height) col-label-height) grid-y-size)]
                 [xp (- x row-label-width)]
                 [yp (- y col-label-height)]
                 [x (inexact->exact (floor (/ xp grid-width)))]
                 [y (inexact->exact (floor (/ yp grid-height)))])
            (if (and (<= 0 x)
                     (< x grid-x-size)
                     (<= 0 y)
                     (< y grid-y-size))
                (make-pt x y)
                #f)))]
      
      [define/private grid->rect
        (lambda (x y)
          (let* ([grid-width (- (get-canvas-width)
                                row-label-width
                                (quotient grid-x-size extra-space-every))]
                 [grid-height (- (get-canvas-height)
                                 col-label-height
                                 (quotient grid-y-size extra-space-every))]
                 [left (+ row-label-width
                          (quotient x extra-space-every)
                          (* x (/ grid-width grid-x-size)))]
                 [top (+ col-label-height
                         (quotient y extra-space-every)
                         (* y (/ grid-height grid-y-size)))]
                 [width (/ grid-width grid-x-size)]
                 [height (/ grid-height grid-y-size)])
            (values left top width height)))]

      (define/private (get-canvas-width) (let-values ([(w h) (get-client-size)]) w))
      (define/private (get-canvas-height) (let-values ([(w h) (get-client-size)]) h))
      
      [define undo-history null]
      [define redo-history null]
      [define/private do-do
        (lambda (do current-sel new-sel)
          (let* ([x (ado-x do)]
                 [y (ado-y do)]
                 [actual (get-raw-rect x y)]
                 [current (current-sel do)]
                 [new (new-sel do)]
                 [color->val
                  (lambda (brush)
                    (let ([color (send brush get-color)])
                      (list (send color red)
                            (send color green)
                            (send color blue))))])
            (unless (eq? current actual)
              (error 'do-do "expected ~a found ~a at (~a,~a)"
                     (color->val current)
                     (color->val actual)
                     x y))
            (set-raw-rect x y new)
            (paint-rect x y)))]
      
      [define/private brush->symbol
        (lambda (res)
          (cond
	    [(eq? res UNKNOWN-BRUSH) 'unknown]
	    [(eq? res OFF-BRUSH) 'off]
	    [(eq? res ON-BRUSH) 'on]
	    [(eq? res WRONG-BRUSH) 'wrong]))]
      [define/private sym->brush
        (lambda (sym)
          (case sym
            [(unknown) UNKNOWN-BRUSH]
            [(off) OFF-BRUSH]
            [(on) ON-BRUSH]
            [(wrong) WRONG-BRUSH]))]
      
      [define/private in-rect?
        (lambda (p cp1 cp2)
          (or (and (<= (pt-x cp1) (pt-x p) (pt-x cp2))
                   (<= (pt-y cp1) (pt-y p) (pt-y cp2)))
              (and (<= (pt-x cp2) (pt-x p) (pt-x cp1))
                   (<= (pt-y cp2) (pt-y p) (pt-y cp1)))))]
      
      ;; ((list-of (list-of (union 'unknown 'off 'on 'wrong))) -> void)
      [define/public set-grid
        (lambda (g)
          (set! undo-history null)
          (set! redo-history null)
          (set! grid
                (list->vector
                 (map (lambda (x) (list->vector (map (lambda (x) (sym->brush x)) x)))
                      g))))]
      
      ;; (-> (list-of (list-of (union 'unknown 'off 'on 'wrong))))
      [define/public get-grid
        (lambda ()
          (map (lambda (x) (map (lambda (x) (brush->symbol x)) (vector->list x)))
               (vector->list grid)))]
      
      ;; (-> void)
      [define/public undo
        (lambda ()
          (cond
	    [(null? undo-history) (bell)]
	    [else
	     (let ([do (car undo-history)])
	       (set! undo-history (cdr undo-history))
	       (set! redo-history (cons do redo-history))
	       (do-do do ado-after ado-before))]))]
      
      ;; (-> void)
      [define/public redo
        (lambda ()
          (cond
	    [(null? redo-history) (bell)]
	    [else
	     (let ([do (car redo-history)])
	       (set! redo-history (cdr redo-history))
	       (set! undo-history (cons do undo-history))
	       (do-do do ado-before ado-after))]))]
      
      [define/public paint-rect
        (lambda (i j)
          (send (get-dc) set-pen LINES/NUMBERS-PEN)
          (paint-rect/lines-numbers-pen i j))]
      
      ;; (int int -> void)
      [define/public paint-rect/lines-numbers-pen
        (lambda (i j)
          (let ([dc (get-dc)])
            (let-values ([(left top width height) (grid->rect i j)])
              (cond
                [(and draw-small-start-p
                      draw-small-end-p
                      (in-rect? (make-pt i j)
                                draw-small-start-p
                                draw-small-end-p))
		 (send dc set-pen WHITE-PEN)
		 (send dc set-brush WHITE-BRUSH)
		 (send dc draw-rectangle left top width height)
                 
		 (let ([spacing 2])
		   (send dc set-pen LINES/NUMBERS-PEN)
		   (send dc set-brush (new-brush (get-raw-rect (pt-x draw-small-start-p)
                                                               (pt-y draw-small-start-p))
                                                 modifier-on?))
		   (send dc draw-rectangle
			 (+ left spacing)
			 (+ top spacing)
			 (- width spacing spacing)
			 (- height spacing spacing)))]
                
		[else
		 (send dc set-brush (get-raw-rect i j))
		 (send dc draw-rectangle left top width height)]))))]
      
      
      ;; (int int -> (instance brush%))
      [define/public get-raw-rect
        (lambda (i j)
          '(unless (and (<= 0 i)
			(< i grid-x-size)
			(<= 0 j)
			(< j grid-y-size))
	     (error 'get-raw-rect "cannot get (~a, ~a) in ~ax~a board"
		    i j grid-x-size grid-y-size))
          (vector-ref (vector-ref grid i) j))]
      
      ;; (int int -> (union 'on 'off 'unknown 'wrong))
      [define/public get-rect
        (lambda (i j)
          (brush->symbol (get-raw-rect i j)))]
      
      ;; (int int (instance brush%) -> void)
      [define/public set-raw-rect
        (lambda (i j brush)
          '(unless (and (<= 0 i)
			(< i grid-x-size)
			(<= 0 j)
			(< j grid-y-size))
	     (error 'set-raw-rect "cannot set (~a, ~a) in ~ax~a board"
		    i j grid-x-size grid-y-size))
          (vector-set! (vector-ref grid i) j brush))]
      
      ;; (int int (union 'on 'off 'unknown 'wrong) -> void)
      [define/public set-rect
        (lambda (i j sym)
          (set-raw-rect i j (sym->brush sym)))]
      
      
      ;; (int int -> void)
      [define/public set-to-error
        (lambda (i j)
          (let ([brush (sym->brush 'wrong)])
            (set! undo-history (cons (make-ado i j (get-raw-rect i j) brush) undo-history))
            (set-raw-rect i j brush)
            (paint-rect i j)))]
      
      ;; (-> void)
      [define/public all-unknown
        (lambda ()
          (let loop ([i grid-x-size])
            (cond
	      [(zero? i) (void)]
	      [else
	       (let loop ([j grid-y-size])
		 (cond
                   [(zero? j) (void)]
                   [else (set-rect (- i 1) (- j 1) 'unknown)
                         (loop (- j 1))]))
	       (loop (- i 1))])))]
      
      [define highlight-row #f]
      [define highlight-col #f]
      (define/public draw-row-label
        (lambda (n)
          (let-values ([(gx gy gw gh) (grid->rect 0 n)])
            (when (and (gx . >= . 0)
                       (gy . >= . 0)
                       (gw . >= . 0)
                       (gh . >= . 0))
              (let* ([nums (list-ref (get-row-numbers) n)]
                     [dc (get-dc)]
                     [str (get-row-label-string nums)]
                     [str-height (get-string-height str)]
                     [str-ascent (get-string-ascent str)]
                     [str-width (get-string-width str)]
                     [sy (+ gy
                            (- (/ gh 2)
                               (/ str-height 2)))]
                     [sx (- row-label-width str-width x-margin)]
                     
                     [x 0]
                     [y gy]
                     [w gx]
                     [h gh])
                
                (if (and highlight-row
                         (= highlight-row n))
                    (begin
                      (send dc set-pen BAR-PEN)
                      (send dc set-brush BAR-BRUSH))
                    (begin
                      (send dc set-pen WHITE-PEN)
                      (send dc set-brush WHITE-BRUSH)))
                
                (send dc draw-rectangle x y w h)
                (send dc draw-text str sx sy))))))
      
      [define/public draw-col-label
        (lambda (n)
          (let-values ([(gx gy gw gh) (grid->rect n 0)])
            (when (and (gx . >= . 0)
                       (gy . >= . 0)
                       (gw . >= . 0)
                       (gh . >= . 0))
              (let* ([nums (list-ref (get-col-numbers) n)]
                     [strs (get-col-label-strings nums)]
                     [dc (get-dc)])
                
                (if (and highlight-col
                         (= highlight-col n))
                    (begin
                      (send dc set-pen BAR-PEN)
                      (send dc set-brush BAR-BRUSH))
                    (begin
                      (send dc set-pen WHITE-PEN)
                      (send dc set-brush WHITE-BRUSH)))
                
                (send dc draw-rectangle gx 0 gw gy)
                (let loop ([ss strs]
                           [line (- (get-max-col-entries) (length strs))])
                  (cond
                    [(null? ss) (void)]
                    [else
                     (let* ([s (car ss)]
                            [str-width (get-string-width s)]
                            [str-height (get-string-height s)]
                            [x (+ gx
                                  (- (/ gw 2)
                                     (/ str-width 2)))]
                            [y (* line (+ str-height y-margin))])
                       (send dc draw-text (car ss) x y)
                       (loop (cdr ss)
                             (+ line 1)))]))))))]
      
      [define/private new-brush
        (lambda (prev modifier?)
          (cond
            [(eq? prev UNKNOWN-BRUSH)
             (if modifier?
                 OFF-BRUSH
                 ON-BRUSH)]
            [(eq? prev ON-BRUSH) UNKNOWN-BRUSH]
            [(eq? prev OFF-BRUSH) UNKNOWN-BRUSH]
            [(eq? prev WRONG-BRUSH) UNKNOWN-BRUSH]
            [else
             (error 'internal-error
                    "unkown brush in board ~s\n" prev)]))]
      
      [define/private check-modifier
        (lambda (evt)
          (or (send evt get-right-down)
              (send evt button-up? 'right)
              (send evt get-alt-down)
              (send evt get-meta-down)
              (send evt get-control-down)
              (send evt get-shift-down)))]
      
      [define modifier-on? #f]
      [define last-p #f]
      
      
      ;; (union #f                   if button not down
      ;;        (make-pt num num))   if button down
      [define draw-small-start-p #f]
      
      ;; (union #f                   if button dragged outside board
      ;;        (make-pt num num))   if button dragged in board
      [define draw-small-end-p #f]
      
      [define coordinate-p #f]
      
      [define/private update-range-of-rects
        (lambda (p1 p2)
          (let ([x-small (min (pt-x p1) (pt-x p2))]
                [x-large (max (pt-x p1) (pt-x p2))]
                [y-small (min (pt-y p1) (pt-y p2))]
                [y-large (max (pt-y p1) (pt-y p2))])
            (let loop ([x x-small])
              (when (<= x x-large)
                (let loop ([y y-small])
                  (when (<= y y-large)
                    (paint-rect x y)
                    (loop (+ y 1))))
                (loop (+ x 1))))))]
      
      [define/override on-event
	(lambda (evt)
	  (let* ([x (send evt get-x)]
		 [y (send evt get-y)]
		 [p (xy->grid x y)])
	    (cond
              [(or (send evt moving?)
                   (send evt entering?)
                   (send evt leaving?))
               
               ;; update depressed squares
               (when draw-small-start-p
                 (let ([old-draw-small-end-p draw-small-end-p])
                   (cond
                     [(and draw-small-start-p
                           p
                           (or (= (pt-x p) (pt-x draw-small-start-p))
                               (= (pt-y p) (pt-y draw-small-start-p))))
                      (unless (equal? draw-small-end-p p)
                        (set! draw-small-end-p p)
                        (when old-draw-small-end-p
                          (update-range-of-rects draw-small-start-p old-draw-small-end-p))
                        (when draw-small-end-p
                          (update-range-of-rects draw-small-start-p draw-small-end-p)))]
                     [draw-small-start-p
                      (set! draw-small-end-p #f)
                      (when old-draw-small-end-p
                        (update-range-of-rects draw-small-start-p old-draw-small-end-p))])))
               
               (let ([dc (get-dc)])
                 
                 ;; update the bars
                 (let ([new-highlight-col
                        (if (and p
                                 (not (send evt leaving?)))
                            (pt-x p)
                            #f)]
                       [old-highlight-col highlight-col])
                   (unless (equal? old-highlight-col new-highlight-col)
                     (set! highlight-col new-highlight-col)
                     (when new-highlight-col
                       (draw-col-label new-highlight-col))
                     (when old-highlight-col
                       (draw-col-label old-highlight-col))))
                 
                 (let ([new-highlight-row
                        (if (and p
                                 (not (send evt leaving?)))
                            (pt-y p)
                            #f)]
                       [old-highlight-row highlight-row])
                   (unless (equal? old-highlight-row new-highlight-row)
                     (set! highlight-row new-highlight-row)
                     (when new-highlight-row
                       (draw-row-label new-highlight-row))
                     (when old-highlight-row
                       (draw-row-label old-highlight-row))))
                 
                 (set! last-p p)
                 
                 ;; update the coordinates
                 (send dc set-pen WHITE-PEN)
                 (send dc set-brush WHITE-BRUSH)
                 (send dc draw-rectangle 0 0 row-label-width col-label-height)
                 (when (and (not (send evt leaving?))
                            p)
                   (unless (equal? coordinate-p p)
                     (let* ([i (pt-x p)]
                            [j (pt-y p)]
                            [string (loc->string (+ i 1) (+ j 1))]
                            [width (get-string-width string)]
                            [height (get-string-height string)]
                            [sx (- (/ row-label-width 2)
                                   (/ width 2))]
                            [sy (- (/ col-label-height 2)
                                   (/ height 2))])
                       (send dc draw-text string sx sy)))))]
              [(send evt button-down?)
               (set! draw-small-start-p p)
               (set! draw-small-end-p p)
               (set! modifier-on? (check-modifier evt))
               (when p
                 (paint-rect (pt-x p) (pt-y p)))]
              [(send evt button-up?)
               (cond
                 [(and p (or (= (pt-x p) (pt-x draw-small-start-p))
                             (= (pt-y p) (pt-y draw-small-start-p))))
                  (let ([new (new-brush (get-raw-rect 
                                         (pt-x draw-small-start-p)
                                         (pt-y draw-small-start-p))
                                        (check-modifier evt))])
                    ;(set! undo-history (cons (make-ado i j prev new) undo-history))
                    ;(set! redo-history null)
                    (let ([x-small (min (pt-x draw-small-start-p) (pt-x p))]
                          [x-large (max (pt-x draw-small-start-p) (pt-x p))]
                          [y-small (min (pt-y draw-small-start-p) (pt-y p))]
                          [y-large (max (pt-y draw-small-start-p) (pt-y p))])
                      
                      (set! draw-small-start-p #f)
                      (set! draw-small-end-p #f)
                      (set! modifier-on? #f)
                      
                      (let loop ([x x-small])
                        (when (<= x x-large)
                          (let loop ([y y-small])
                            (when (<= y y-large)
                              (set-raw-rect x y new)
                              (paint-rect x y)
                              (loop (+ y 1))))
                          (loop (+ x 1))))))]
                 [else
                  (let ([old-draw-small-start-p draw-small-start-p]
                        [old-draw-small-end-p draw-small-end-p])
                    (set! draw-small-start-p #f)
                    (set! draw-small-end-p #f)
                    (set! modifier-on? (check-modifier evt))
                    (when (and old-draw-small-start-p
                               old-draw-small-end-p)
                      (update-range-of-rects old-draw-small-start-p
                                             old-draw-small-end-p)))])])))]
      [define/override on-paint
	(lambda ()
	  (let ([dc (get-dc)])
	    (send dc clear)
	    (let-values ([(width height) (get-client-size)])
              
	      (send dc set-pen LINES/NUMBERS-PEN)
              (let loop ([i grid-x-size])
                (cond
		  [(zero? i) (void)]
		  [else (let loop ([j grid-y-size])
			  (cond
                            [(zero? j) (void)]
                            [else (paint-rect/lines-numbers-pen (- i 1) (- j 1))
                                  (loop (- j 1))]))
			(loop (- i 1))]))
              
	      (let loop ([l (get-col-numbers)]
			 [n 0])
		(cond
                  [(null? l) (void)]
                  [else
                   
                   (draw-col-label n)
                   (loop (cdr l) (+ n 1))]))
              
	      (let loop ([l (get-row-numbers)]
			 [n 0])
		(cond
                  [(null? l) (void)]
                  [else
                   (if (and last-p
                            (= (pt-y last-p) n))
                       (begin
                         (send dc set-pen BAR-PEN)
                         (send dc set-brush BAR-BRUSH))
                       (begin
                         (send dc set-pen WHITE-PEN)
                         (send dc set-brush WHITE-BRUSH)))
                   (draw-row-label n)
                   (loop (cdr l)
                         (+ n 1))]))
	      
	      (void))))]
      
      [define/public calculate-row-margins
        (lambda ()
          (let* ([dc (get-dc)])
            (set! row-label-width
                  (max (get-string-width (loc->string grid-x-size grid-y-size))
                       (apply max (map (lambda (x) (+ x-margin
                                                      (get-string-width (get-row-label-string x))
                                                      x-margin))
                                       (get-row-numbers)))))
            
            (let-values ([(width height descent ascent) (send dc get-text-extent "0123456789")])
              (set! row-label-height (+ y-margin height y-margin)))))]
      [define/public calculate-col-margins
        (lambda ()
          (let* ([dc (get-dc)])
            
            (set! col-label-height
                  (max
                   (get-string-height/descent (loc->string grid-x-size grid-y-size))
                   (apply max
                          (map (lambda (l)
                                 (let* ([strs (get-col-label-strings l)]
                                        [margins (* (length strs) y-margin)]
                                        [height (apply + (map (lambda (x) (get-string-height x)) strs))])
                                   (+ margins height)))
                               (get-col-numbers)))))
            
            (set! col-label-width
                  (apply max
                         (map (lambda (l)
                                (let ([label-strings (get-col-label-strings l)])
                                  (if (null? label-strings)
                                      (+ x-margin x-margin) ;; Minimum column label width (no labels)
                                      (apply max
                                             (map (lambda (x) (+ x-margin
                                                                 (get-string-width x)
                                                                 x-margin))
                                                  label-strings)))))
                              (get-col-numbers))))))]
      [define/public update-min-spacing
        (lambda ()
          (min-width (inexact->exact (ceiling (+ row-label-width (* grid-x-size col-label-width)))))
          (min-height (inexact->exact (ceiling (+ col-label-height (* grid-y-size row-label-height))))))]
      
      (inherit min-width min-height)
      (super-instantiate ())
      
      [define/public close-up
        (lambda ()
          (remove-pref-callback))]
      [define/public reset-font
        (lambda (font)
          (send (get-dc) set-font font)
          (calculate-row-margins)
          (calculate-col-margins)
          (update-min-spacing))]
      
      (define pref-callback (preferences:add-callback
                             'paint-by-numbers:font
                             (lambda (pref new-value)
                               (reset-font new-value))))
      [define/public (remove-pref-callback) (pref-callback)]
      
      (reset-font (preferences:get 'paint-by-numbers:font))
      (calculate-row-margins)
      (calculate-col-margins)
      (update-min-spacing)))
  
  (define design-paint-by-numbers-canvas%
    (class paint-by-numbers-canvas% 
      (init-field width height)
      [define row-spacing 5]
      [define col-spacing 5]
      [define row-numbers
        (vector->list (make-vector height (vector->list (make-vector row-spacing 1))))]
      [define col-numbers
        (vector->list (make-vector width (vector->list (make-vector col-spacing 1))))]
      
      [define/override get-max-col-entries
        (lambda ()
          col-spacing)]
      [define/override get-row-numbers
        (lambda ()
          row-numbers)]
      [define/override get-col-numbers
        (lambda ()
          col-numbers)]
      
      (inherit draw-col-label draw-row-label get-rect
               calculate-row-margins
               calculate-col-margins
               update-min-spacing
               on-paint)
      
      [define/private calculate-col/row
        (lambda (get-rect col/row-numbers num-row/cols)
          (let loop ([i num-row/cols]
                     [block-count 0]
                     [ans null])
            (cond
              [(zero? i) (if (= block-count 0)
                             ans
                             (cons block-count ans))]
              [else
               (let ([this (get-rect (- i 1))])
                 (case this
                   [(unknown off wrong)
                    (if (zero? block-count)
                        (loop (- i 1) 0 ans)
                        (loop (- i 1) 0 (cons block-count ans)))]
                   [(on) (loop (- i 1) (+ block-count 1) ans)]
                   [else (error 'calculate-col "unknown response from get-rect: ~a\n" this)]))])))]
      
      [define/private calculate-col
        (lambda (col)
          (calculate-col/row
           (lambda (i) (get-rect col i))
           col-numbers
           (length row-numbers)))]
      
      [define/private calculate-row
        (lambda (row)
          (calculate-col/row
           (lambda (i) (get-rect i row))
           row-numbers
           (length col-numbers)))]
      
      [define/private update-col/row
        (lambda (col/row col/row-numbers calculate-col/row)
          (let loop ([l col/row-numbers]
                     [n col/row])
            (cond
              [(null? l) (error 'update-col/row "col/row too big: ~a\n" col/row)]
              [(zero? n)
               (cons (calculate-col/row col/row)
                     (cdr l))]
              [else
               (cons (car l)
                     (loop (cdr l)
                           (- n 1)))])))]
      
      [define/private update-col
        (lambda (col)
          (set! col-numbers
                (update-col/row col
                                col-numbers
                                (lambda (x) (calculate-col x))))
          (draw-col-label col)
          (let ([len (length (list-ref col-numbers col))])
            (when (< col-spacing len)
              (set! col-spacing len)
              (calculate-col-margins)
              (update-min-spacing)
              (on-paint))))]
      
      [define/private update-row
        (lambda (row)
          (set! row-numbers
                (update-col/row row
                                row-numbers
                                (lambda (x) (calculate-row x))))
          (draw-row-label row)
          (let ([len (length (list-ref row-numbers row))])
            (when (< row-spacing len)
              (set! row-spacing len)
              (calculate-row-margins)
              (update-min-spacing)
              (on-paint))))]
      
      [define update-row-col? #t]
      [define/override set-raw-rect
        (lambda (i j n)
          (super set-raw-rect i j n)
          (when update-row-col?
            (update-col i)
            (update-row j)))]
      
      [define/private update-all-rows-cols
        (lambda ()
          (let loop ([i width])
            (unless (zero? i)
              (update-col (- i 1))
              (loop (- i 1))))
          (let loop ([i height])
            (unless (zero? i)
              (update-row (- i 1))
              (loop (- i 1)))))]
      (inherit set-rect)
      [define/public set-bitmap
        (lambda (bitmap)
          (set! update-row-col? #f)
          (let ([dc (make-object bitmap-dc% bitmap)]
                [c (make-object color%)]
                [width (send bitmap get-width)]
                [height (send bitmap get-height)]
                [warned? #f])
            (let loop ([i width])
              (unless (zero? i)
                (let loop ([j height])
                  (unless (zero? j)
                    (let ([m (- i 1)]
                          [n (- j 1)])
                      (send dc get-pixel m n c)
                      (when (and (not warned?)
                                 (not (or (and (= 0 (send c red))
                                               (= 0 (send c blue))
                                               (= 0 (send c green)))
                                          (and (= 255 (send c red))
                                               (= 255 (send c blue))
                                               (= 255 (send c green))))))
                        (set! warned? #t)
                        (message-box
                         "Paint by Numbers"
                         "WARNING: This is a color bitmap; non-white pixels will be considered black"))
                      (set-rect m n
                                (if (and (= 255 (send c red))
                                         (= 255 (send c blue))
                                         (= 255 (send c green)))
                                    'off
                                    'on)))
                    (loop (- j 1))))
                (loop (- i 1)))))
          
          (set! update-row-col? #t)
          (update-all-rows-cols))]
      
      [define/override set-grid
        (lambda (g)
          (set! update-row-col? #f)
          (super set-grid g)
          (set! update-row-col? #t)
          (update-all-rows-cols))]
      
      (super-new (row-numbers null) (col-numbers null))
      (set! row-numbers (vector->list (make-vector height null)))
      (set! col-numbers (vector->list (make-vector width null)))))
