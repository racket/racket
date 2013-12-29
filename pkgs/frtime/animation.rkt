(module animation frtime
  
  (require (for-syntax racket/base (only-in racket/function identity))
           racket/match
           racket/class
           (except-in frtime/animation/graphics make-posn posn-x posn-y make-rgb)
           (lifted (only-in frtime/animation/graphics posn-x) posn-x)
           (lifted (only-in frtime/animation/graphics posn-y) posn-y)
           (lifted (only-in frtime/animation/graphics make-posn) make-posn)
           (lifted (only-in frtime/animation/graphics make-rgb) make-rgb)
           (as-is:unchecked frtime/lang-ext lift)
           frtime/frlibs/list
           frtime/frlibs/math)

  (module test racket/base)
  
  (open-graphics)
  
  (define fresh-anim
    (let ([first #t])
      (lambda ([x 400] [y 400] [title "Animation - DrRacket"])
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
    (open-viewport "Animation - DrRacket" 400 400))
  
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
  
  (define-syntax (define-shape-struct stx)
    (syntax-case stx ()
      [(_ name (field ...))
       (with-syntax
           ([ctor-name (datum->syntax stx (string->symbol (format "make-~a" (syntax-e #'name))))]
            [(accessor-name ...)
             (map (lambda (fd)
                    (string->symbol (format "~a-~a" (syntax-e #'name) (syntax-e fd))))
                  (syntax-e #'(field ...)))]
            [(index ...)
             (build-list (length (syntax-e #'(field ...))) identity)])
         #'(begin
             (define (ctor-name field ...)
               (vector 'name field ...))
             (define (accessor-name obj)
               (vector-ref obj index))
             ...))]))
  
  (define-struct ring (center radius color))
  (define-struct solid-ellipse (ul w h color))
  (define-struct graph-string (pos text color))
  (define-struct line (p1 p2 color))
  (define-struct rect (ul w h color))
  (define-struct rrect (ur w h color))
  (define-struct curve (xmin xmax ymin ymax fn))
  (define-struct polygon (posn-list posn color))
  (define-struct solid-polygon (posn-list posn color))
  (define-struct arc (pos width height start-radians end-radians color))
  (define-struct solid-arc (pos width height start-radians end-radians color))
  (define-struct image (pos renderer))
  
  (define (prep-image file)
    (draw-pixmap-posn file))
  
  (define (make-circle center r color)
    (make-solid-ellipse (make-posn (- (posn-x center) r)
                                   (- (posn-y center) r))
                        (* 2 r) (* 2 r) color))
  
  (define l (new-cell empty))
  
  (define (display-shapes x)
    (set-cell! l x))
  
  (define (top-level-draw-list a-los)
    (compound-lift
     (lambda (vn)
       ((clear-viewport pixmap))
       (draw-list a-los vn)
       (copy-viewport pixmap window))))
  
  (define (my-for-each proc lst v-n)
    (let ([lst (v-n lst)])
      (if (empty? lst)
          (void)
          (begin
            (proc (v-n (first lst)))
            (my-for-each proc (rest lst) v-n)))))
  
  (define (draw-list a-los v-n)
    (let loop ([a-los a-los])
      (my-for-each
       (lambda (v)
         (match (v-n v)
           [(? undefined?) (void)]
           [(ring center radius color)
            (let ([center (v-n center)]
                  [radius (v-n radius)]
                  [color (v-n color)])
              (unless (or (undefined? center)
                          (undefined? radius))
                ((draw-ellipse pixmap)
                 (make-posn (- (v-n (posn-x center)) radius)
                            (- (v-n (posn-y center)) radius))
                 (* 2 radius)
                 (* 2 radius)
                 (if (undefined? color) "black" color))))]
           [(arc pos width height start-radians end-radians color)
            (let ([pos (v-n pos)]
                  [width (v-n width)]
                  [height (v-n height)]
                  [start-radians (v-n start-radians)]
                  [end-radians (v-n end-radians)])
              ((draw-arc pixmap) pos width height start-radians end-radians color))]
           [(solid-arc pos width height start-radians end-radians color)
            (let ([pos (v-n pos)]
                  [width (v-n width)]
                  [height (v-n height)]
                  [start-radians (v-n start-radians)]
                  [end-radians (v-n end-radians)])
              ((draw-solid-arc pixmap) pos width height start-radians end-radians color))]
           [(image pos renderer)
            (let ([renderer (v-n renderer)]
                  [pos (v-n pos)])
              ((renderer pixmap) pos))]
           [(solid-ellipse ul w h color)
            (let ([ul (v-n ul)]
                  [w (v-n w)]
                  [h (v-n h)]
                  [color (v-n color)])
              (unless (or (undefined? ul)
                          (undefined? w)
                          (undefined? h))
                ((draw-solid-ellipse pixmap) ul w h (if (undefined? color) "black" color))))]
           [(graph-string pos text color) ((draw-string pixmap) (v-n pos) (v-n text) (v-n color))]
           [(line p1 p2 color)
            (let ([p1 (v-n p1)]
                  [p2 (v-n p2)]
                  [color (v-n color)])
              (unless (or (undefined? p1)
                          (undefined? p2))
                ((draw-line pixmap) p1 p2 (if (undefined? color) "black" color))))]
           [(rect ul w h color)
            (let ([ul (v-n ul)]
                  [w (v-n w)]
                  [h (v-n h)]
                  [color (v-n color)])
              (cond
                [(and (>= w 0) (>= h 0)) ((draw-solid-rectangle pixmap) ul w h color)]
                [(>= h 0) ((draw-solid-rectangle pixmap) (make-posn (+ (posn-x ul) w) (posn-y ul)) (- w) h color)]
                [(>= w 0) ((draw-solid-rectangle pixmap) (make-posn (posn-x ul) (+ (posn-y ul) h)) w (- h) color)]
                [else ((draw-solid-rectangle pixmap) (make-posn (+ (posn-x ul) w) (+ (posn-y ul) h)) (- w) (- h) color)]))]
           [(polygon pts offset color) ((draw-polygon pixmap) pts offset color)]
           [(solid-polygon pts offset color) ((draw-solid-polygon pixmap) pts offset color)]
           [(? list? x) (loop (v-n x))]
           [(? void?) (void)]))
       a-los v-n)))
  
  (define d (top-level-draw-list l))
  
  (define-struct graph-color (fn xmin xmax ymin ymax))
  
  (define (draw-graph-color pm gc)
    (let ([dp (draw-pixel pm)])
      (match gc
        [(graph-color fn xmin xmax ymin ymax)
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
    (lambda (up down limit [init 0])
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
                   (snapshot-e (changes hz) milliseconds)
                   (make-wave-state (value-now hz) 0)
                   (lambda (new-freq+time old-state)
                     (match new-freq+time
                       [(list h1 t)
                        (match old-state
                          [(wave-state h0 o0)
                           (make-wave-state
                            h1
                            (+ o0 (* .002 pi t (- h0 h1))))])])))])
      (+ (lift #f wave-state-offset state)
         (* milliseconds pi (lift #f wave-state-hz state) .002))))
  
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
   (except-out (all-defined-out) pixmap window draw-list l d 
               make-wave-state wave-state-hz wave-state-offset)
   (all-from-out frtime/animation/graphics)))
