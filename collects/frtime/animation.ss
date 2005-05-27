(module animation (lib "frtime.ss" "frtime")
  
  (require (all-except "graphics.ss" make-posn posn-x posn-y make-rgb)
           (lifted "graphics.ss" posn-x posn-y make-posn make-rgb)
           (all-except (lib "match.ss") match)
           (lib "class.ss")
           (lib "list.ss" "frtime")
           (lib "etc.ss" "frtime")
           (lib "math.ss" "frtime"))
  
  (open-graphics)
  
  (define fresh-anim
    (let ([first #t])
      (opt-lambda ([x 400] [y 400] [title "Animation - DrScheme"])
        (if first
            (set! first #f)
            (begin
              (set! window
                    (open-viewport title x y))
              
              (set! pixmap
                    (open-pixmap "" x y))
              
              (set! mouse-pos
                    (hold ((viewport-mouse-events window)
                           . ==> . 
                           (lambda (ev) (make-posn
                                         (send ev get-x)
                                         (send ev get-y))))
                          (query-mouse-posn window)))
              
              (set! key-strokes ((viewport-key-events window) . ==> . sixkey-value))
              
              (set! left-clicks ((viewport-mouse-events window) . =#> . (lambda (ev) (send ev button-down? 'left))))
              (set! middle-clicks ((viewport-mouse-events window) . =#> . (lambda (ev) (send ev button-down? 'middle))))
              (set! right-clicks ((viewport-mouse-events window) . =#> . (lambda (ev) (send ev button-down? 'right)))))))))
    
  (define window
    (open-viewport "Animation - DrScheme" 400 400))
  
  (define pixmap
    (open-pixmap "" 400 400))
  
  (define mouse-pos
    (hold ((viewport-mouse-events window)
           . =#=> . 
           (lambda (ev) (if (send ev moving?)
                            (make-posn
                             (send ev get-x)
                             (send ev get-y))
                            nothing)))
          (query-mouse-posn window)))
  
  (define filtered-keys (viewport-key-events window))
  (define shift-down (hold (filtered-keys . ==> . sixkey-shift)))
  (define control-down (hold (filtered-keys . ==> . sixkey-control)))
  (define meta-down (hold (filtered-keys . ==> . sixkey-meta)))
  (define alt-down (hold (filtered-keys . ==> . sixkey-alt)))
  (define key-strokes ((viewport-key-events window) . ==> . sixkey-value))
  (define left-clicks ((viewport-mouse-events window) . =#> . (lambda (ev) (send ev button-down? 'left))))
  (define middle-clicks ((viewport-mouse-events window) . =#> . (lambda (ev) (send ev button-down? 'middle))))
  (define right-clicks ((viewport-mouse-events window) . =#> . (lambda (ev) (send ev button-down? 'right))))
  (define left-releases ((viewport-mouse-events window) . =#> . (lambda (ev) (send ev button-up? 'left))))
  (define middle-releases ((viewport-mouse-events window) . =#> . (lambda (ev) (send ev button-up? 'middle))))
  (define right-releases ((viewport-mouse-events window) . =#> . (lambda (ev) (send ev button-up? 'right))))
  
  (define-struct ring (center radius color))
  (define-struct solid-ellipse (ul w h color))
  (define-struct graph-string (pos text color))
  (define-struct line (p1 p2 color))
  (define-struct rect (ul w h color))
  (define-struct rrect (ur w h color))
  (define-struct curve (xmin xmax ymin ymax fn))
  (define-struct polygon (posn-list posn color))
  
  (define (make-circle center r color)
    (make-solid-ellipse (make-posn (- (posn-x center) r)
                                   (- (posn-y center) r))
                                   (* 2 r) (* 2 r) color))
  
  (define l (new-cell empty))
  
  (define (display-shapes x)
    (set-cell! l x))
  
  (define (top-level-draw-list a-los)
    ((clear-viewport pixmap))
    (draw-list a-los)
    (copy-viewport pixmap window))
  
  (define (draw-list a-los)
    (for-each
     (match-lambda
       [(? undefined?) (void)]
       [($ ring center radius color)
        ((draw-ellipse pixmap)
         (make-posn (- (posn-x center) radius)
                    (- (posn-y center) radius))
         (* 2 radius)
         (* 2 radius)
         color)]
       [($ solid-ellipse ul w h color)
        ((draw-solid-ellipse pixmap) ul w h color)]
       [($ graph-string pos text color) ((draw-string pixmap) pos text color)]
       [($ line p1 p2 color) ((draw-line pixmap) p1 p2 color)]
       [($ rect ul w h color)
        (cond
          [(and (>= w 0) (>= h 0)) ((draw-solid-rectangle pixmap) ul w h color)]
          [(>= h 0) ((draw-solid-rectangle pixmap) (make-posn (+ (posn-x ul) w) (posn-y ul)) (- w) h color)]
          [(>= w 0) ((draw-solid-rectangle pixmap) (make-posn (posn-x ul) (+ (posn-y ul) h)) w (- h) color)]
          [else ((draw-solid-rectangle pixmap) (make-posn (+ (posn-x ul) w) (+ (posn-y ul) h)) (- w) (- h) color)])]
       [($ polygon pts offset color) ((draw-solid-polygon pixmap) pts offset color)]
       [(? list? x) (draw-list x)]
       [(? void?) (void)])
     a-los))
  
  (define d (lift #t top-level-draw-list l))
  
  (define-struct graph-color (fn xmin xmax ymin ymax))
  
  (define (draw-graph-color pm gc)
    (let ([dp (draw-pixel pm)])
      (match gc
        [($ graph-color fn xmin xmax ymin ymax)
         (let ([xincr (/ (- xmax xmin) 300)]
               [yincr (/ (- ymax ymin) 300)])
           (let loop ([i 50] [y ymin])
             (let loop ([j 50] [x xmin])
               (dp (make-posn j i) (fn x y))
               (when (< j 350)
                 (loop (add1 j) (+ x xincr))))
             (when (< i 350)
               (loop (add1 i) (+ y yincr)))))])))
  
  (define (valid-posn? v)
    (and (posn? v) (number? (posn-x v)) (number? (posn-y v))))
  
  (define seconds~ (/ time-b 1000.0))
  
  (define (key sym)
    (key-strokes
     . =#> .
     (lambda (x) (eq? x (value-now sym)))))
  
  (define (draw vp pm posl)
    ((clear-viewport pm))
    (for-each (lambda (elt)
                 (cond
                   [(graph-color? elt) (draw-graph-color pm elt)]
                   [(string? elt) ((draw-string pm) (make-posn 8 20) elt)]
                   [(valid-posn? elt) ((draw-solid-ellipse pm)
                                       (make-posn (- (posn-x elt) 10)
                                                  (- (posn-y elt) 10))
                                       20 20
                                       (make-rgb 0 .6 .6))]
                   [(and (cons? elt)
                         (valid-posn? (first elt))
                         (valid-posn? (rest elt))) ((draw-line pm)
                                                    (first elt)
                                                    (rest elt)
                                                    "black")]
                   [else (void)])) posl)
    (copy-viewport pm vp))

  #|
  (define foldl
    (case-lambda
      [(f i l) (if (cons? l)
                   (foldl f (f (first l) i) (rest l))
                   i)]))
  |#
  
  (define (drop n l)
    (if (empty? l)
        empty
        (if (<= n 0)
            l
            (drop (sub1 n) (rest l)))))
  
  (define (inc-max n)
    (lambda (x) (if (>= x n)
                    n
                    (add1 x))))
  
  (define (dec-min n)
    (lambda (x) (if (<= x n)
                    n
                    (sub1 x))))
  
  (define (fix-rgb r g b)
    (let ([fix (lambda (n) (min 1 (max 0 n)))])
      (apply make-rgb (map fix (list r g b)))))

  (define range-control
    (opt-lambda (up down limit [init 0])
      (accum-b
       (merge-e (up   . -=> . (inc-max limit))
                (down . -=> . (dec-min 0)))
       init)))
  
  (define (keyboard-control up down limit)
    (accum-b
     (key-strokes
      . =#=> .
      (match-lambda
        [(? (lambda (x) (eq? x up))) (inc-max limit)]
        [(? (lambda (x) (eq? x down))) (dec-min 0)]
        [_ nothing]))
     0))
  
  (define-struct wave-state (hz offset))
  
  (define (wave hz)
    (let* ([state (collect-b
                   (snapshot-e (changes hz) time-b)
                   (make-wave-state (value-now hz) 0)
                   (lambda (new-freq+time old-state)
                     (match new-freq+time
                       [(h1 t)
                        (match old-state
                          [($ wave-state h0 o0)
                           (make-wave-state
                            h1
                            (+ o0 (* .002 pi t (- h0 h1))))])])))])
      (+ (lift #f wave-state-offset state)
         (* time-b pi (lift #f wave-state-hz state) .002))))
  
  (define (current-and-last-value signal)
    (let ([init (value-now signal)])
      (collect-b (changes signal)
                 (list init init)
                 (lambda (new-value previous-two)
                   (list new-value (first previous-two))))))
  
  (define (last-value signal)
    (second (current-and-last-value signal)))
  
;   (define (last-value signal)
;    (let ([init (value-now signal)])
;      (rest
;       (collect-b (changes signal)
;                  (cons init init)
;                  (lambda (new old-pair)
;                    (cons new (first old-pair)))))))
                 
  (define (posn+ . args)
    (make-posn (apply + (map posn-x args))
               (apply + (map posn-y args))))
  
  (define (posn- . args)
    (make-posn (apply - (map posn-x args))
               (apply - (map posn-y args))))
  
  (define (posn/ p s)
    (make-posn (/ (posn-x p) s)
               (/ (posn-y p) s)))
  
  (define (posn* p s)
    (make-posn (* (posn-x p) s)
               (* (posn-y p) s)))
  
  (define (posn-dot p1 p2)
    (+ (* (posn-x p1) (posn-x p2))
       (* (posn-y p1) (posn-y p2))))
  
  (define (posn-len p)
    (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))
  
  (define (normalize p)
    (posn/ p (posn-len p)))
  
  (define (current-mouse-pos)
    (value-now mouse-pos))
  
  (define (clip x lo hi)
    (if (< x lo)
        lo
        (if (> x hi)
            hi
            x)))
  
  (define (posn-diff p1 p2)
    (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
             (sqr (- (posn-y p1) (posn-y p2))))))

  (define (posn-derivative p)
    (make-posn (derivative (posn-x p)) (derivative (posn-y p))))
  
  (define (posn-integral p)
    (make-posn (integral (posn-x p)) (integral (posn-y p))))
  
  (provide
   (all-defined-except pixmap window draw-list l d make-circle make-ring make-solid-ellipse
                       make-rect make-line make-polygon make-graph-string make-wave-state wave-state-hz wave-state-offset)
   (lifted make-circle make-ring make-solid-ellipse make-rect make-line make-polygon make-graph-string)
   (all-from "graphics.ss")))
