#lang racket/base

(require "syntax.rkt"
         racket/math
         racket/class
         "../unsafe/cairo.rkt"
         "fmod.rkt"
         "point.rkt"
         "transform.rkt"
         "font.rkt"
         (only-in racket/base [append r:append] [reverse r:reverse]))

(provide dc-path%
         (protect-out do-path
                      set-text-to-path!
                      
                      get-closed-points
                      get-open-points
                      set-closed+open-points))

(define-local-member-name
  get-closed-points
  get-open-points
  set-closed+open-points
  do-path)

(define 2pi (* 2.0 pi))
(define pi/2 (/ pi 2.0))

(define text-to-path #f)
(define (set-text-to-path! proc) (set! text-to-path proc))

(define dc-path%
  (class object%
    ;; A path is a list of pairs and vectors:
    ;;  * The pairs correspond to points on the path
    ;;  * A vector must be between two pairs; it specifies
    ;;    control points for a curve between the two points.

    (define closed-points null)
    (define rev-closed-points null)
    (define open-points null)
    (define rev-open-points null)

    (define/private (flatten-open!)
      (unless (null? rev-open-points)
        (set! open-points (r:append open-points (r:reverse rev-open-points)))
        (set! rev-open-points null)))

    (define/private (flatten-closed!)
      (unless (null? rev-closed-points)
        (set! closed-points (r:append closed-points (r:reverse rev-closed-points)))
        (set! rev-closed-points null)))
    
    (define/public (get-closed-points) (flatten-closed!) closed-points)
    (define/public (get-open-points) (flatten-open!) open-points)

    (define/public (set-closed+open-points c o)
      (define (ok-points? l)
        (let loop ([l l])
          (cond
           [(null? l) #t]
           [(pair? l)
            (let ([p (car l)])
              (if (pair? p)
                  (and (real? (car p))
                       (real? (cdr p))
                       (loop (cdr l)))
                  (and (vector? p)
                       (= (vector-length p) 4)
                       (real? (vector-ref p 0))
                       (real? (vector-ref p 1))
                       (real? (vector-ref p 2))
                       (real? (vector-ref p 3))
                       (pair? (cdr l))
                       (pair? (cadr l))
                       (loop (cdr l)))))])))
      (unless (and (andmap ok-points? c) (ok-points? o))
        (error "invalid path points"))
      (set! closed-points c)
      (set! open-points o))

    (define/private (do-points cr l align-x align-y)
      (let loop ([l l] [first? #t])
        (cond
         [(null? l) (void)]
         [else
          (let ([p (car l)])
            (if (pair? p)
                (begin
                  (if first?
                      (cairo_move_to cr (align-x (car p)) (align-y (cdr p)))
                      (cairo_line_to cr (align-x (car p)) (align-y (cdr p))))
                  (loop (cdr l) #f))
                (let ([p2 (cadr l)])
                  (cairo_curve_to cr 
                                  (align-x (vector-ref p 0)) (align-y (vector-ref p 1))
                                  (align-x (vector-ref p 2)) (align-y (vector-ref p 3))
                                  (align-x (car p2)) (align-y (cdr p2)))
                  (loop (cddr l) #f))))])))

    (define/public (do-path cr align-x align-y)
      (flatten-closed!)
      (flatten-open!)
      (for ([cp (in-list closed-points)])
        (do-points cr cp align-x align-y)
        (cairo_close_path cr))
      (do-points cr open-points align-x align-y))

    (define/public (append path)
      (flatten-closed!)
      (flatten-open!)
      (set! closed-points (r:append closed-points (send path get-closed-points)))
      (set! open-points (r:append open-points (send path get-open-points))))

    (def/public (reset)
      (set! open-points null)
      (set! closed-points null)
      (set! rev-open-points null)
      (set! rev-closed-points null))

    (def/public (reverse)
      (flatten-closed!)
      (flatten-open!)
      (let ([rev-one (lambda (l)
                       (map (lambda (p)
                              (if (pair? p)
                                  p
                                  (vector (vector-ref p 2) (vector-ref p 3)
                                          (vector-ref p 0) (vector-ref p 1))))
                            l))])
        (set! open-points (rev-one (r:reverse open-points)))
        (set! closed-points (map rev-one (map r:reverse closed-points)))))

    (def/public (close)
      (flatten-open!)
      (unless (null? open-points)
        (set! rev-closed-points (cons open-points rev-closed-points))
        (set! open-points null)))

    (def/public (open?)
      (or (pair? open-points)
          (pair? rev-open-points)))

    (def/public (get-bounding-box)
      (flatten-closed!)
      (flatten-open!)
      (if (and (null? closed-points)
               (null? open-points))
          (values 0.0 0.0 0.0 0.0)
          (let-values ([(l t r b)
                        (let loop ([l open-points])
                          (if (null? l)
                              (loop (car closed-points))
                              (let ([p (car l)])
                                (values (car p) (cdr p)
                                        (car p) (cdr p)))))])
            (let-values ([(l t r b)
                          (for*/fold ([l l]
                                      [t t]
                                      [r r]
                                      [b b])
                              ([pts (in-list (cons open-points closed-points))]
                               [p (in-list pts)])
                            (cond
                             [(pair? p) (values (min l (car p))
                                                (min t (cdr p))
                                                (max r (car p))
                                                (max b (cdr p)))]
                             [else (values (min l (vector-ref p 0) (vector-ref p 2))
                                           (min t (vector-ref p 1) (vector-ref p 3))
                                           (max r (vector-ref p 0) (vector-ref p 2))
                                           (max b (vector-ref p 1) (vector-ref p 3)))]))])
              (values l t (- r l) (- b t))))))
    
    (define/public (do-get-path-bounding-box cr type align-x align-y)
      (flatten-closed!)
      (flatten-open!)
      (if (and (null? closed-points) 
               (null? open-points)
               (not cr))
          (values 0. 0. 0. 0.)
          (let ()
            (define cairo_op
              (cond
                [(eq? type 'path)   cairo_path_extents]
                [(eq? type 'fill)   cairo_fill_extents]
                [(eq? type 'stroke) cairo_stroke_extents]
                [else (error 'get-tight-binding-boc "expected 'path, 'fill, or, 'stroke")]))
            (cairo_save cr)
            (do-path cr align-x align-y)
            (define-values (x1 y1 x2 y2) (cairo_op cr))
            (cairo_restore cr)
            (values x1 y1 (- x2 x1) (- y2 y1)))))
      
    (define/public (move-to x y)
      (when (or (pair? open-points)
                (pair? rev-open-points))
        (close))
      (do-move-to x y))

    (define/private (do-move-to x y)
      (set! rev-open-points (list (cons (exact->inexact x) (exact->inexact y)))))

    (define/public (line-to x y)
      (unless (or (pair? open-points)
                  (pair? rev-open-points))
        (error (method-name 'dc-path% 'line-to) "path not yet open"))
      (do-line-to x y))
    
    (define/private (do-line-to x y)
      (set! rev-open-points (cons (cons (exact->inexact x) (exact->inexact y)) rev-open-points)))

    (define/public (lines pts [x 0.0] [y 0.0])
      (unless (or (pair? open-points)
                  (pair? rev-open-points))
        (error (method-name 'dc-path% 'lines) "path not yet open"))
      (for ([p (in-list pts)])
        (if (pair? p)
            (do-line-to (+ x (car p)) (+ y (cdr p)))
            (do-line-to (+ x (point-x p)) (+ y (point-y p))))))

    (define/public (curve-to x1 y1 x2 y2 x3 y3)
      (unless (or (pair? open-points)
                  (pair? rev-open-points))
        (error (method-name 'dc-path% 'curve-to) "path not yet open"))
      (do-curve-to x1 y1 x2 y2 x3 y3))

    (define/private (do-curve-to x1 y1 x2 y2 x3 y3)
      (set! rev-open-points (list* (cons (exact->inexact x3) 
                                         (exact->inexact y3))
                                   (vector (exact->inexact x1) 
                                           (exact->inexact y1)
                                           (exact->inexact x2) 
                                           (exact->inexact y2))
                                   rev-open-points)))

    (define/public (arc x y w h start end [ccw? #t])
      (do-arc x y w h start end ccw?))
    
    (define/private (do-arc x y w h start end ccw?)
      (let-values ([(start end) (if (not ccw?)
                                    (values end start)
                                    (values start end))])
        (let* ([delta (- end start)]
               [delta (cond
                       [(delta . > . 2pi) (fmod delta 2pi)]
                       [(delta . < . 0) (+ (fmod delta 2pi) 2pi)]
                       [else delta])])
          ;; delta is positive and < 2pi
          (let ([start (if (= delta 2pi) 0.0 start)])
            ;; Change top-left to center:
            (let ([x (+ x (/ w 2.0))]
                  [y (+ y (/ h 2.0))]
                  [w (abs w)]
                  [h (abs h)]
                  [pts null])
              ;; make up to 4 curves to represent the arc:
              (let loop ([start start]
                         [delta delta])
                (when (positive? delta)
                  (let ([angle (if (delta . > . pi/2)
                                   pi/2
                                   delta)])
                    ;; First generate points for an arc
                    ;; of `angle' length from -angle/2 to
                    ;; +angle/2:
                    (let* ([x0 (cos (/ angle 2))]
                           [y0 (sin (/ angle 2))]
                           [x1 (/ (- 4 x0) 3)]
                           [y1 (/ (* (- 1 x0) (- 3 x0)) (* 3 y0))]
                           [x2 x1]
                           [y2 (- y1)]
                           [x3 x0]
                           [y3 (- y0)])
                      ;; Rotate to start:
                      (let* ([rotate (+ start (/ angle 2))]
                             [xx (cos rotate)]
                             [xy (sin rotate)]
                             [yy xx]
                             [yx (- xy)]
                             [rotate-xy (lambda (x y)
                                          (values (+ (* xx x) (* xy y))
                                                  (+ (* yy y) (* yx x))))]
                             [w/2 (/ w 2.0)]
                             [h/2 (/ h 2.0)])
                        (let*-values ([(x0 y0) (rotate-xy x0 y0)]
                                      [(x1 y1) (rotate-xy x1 y1)]
                                      [(x2 y2) (rotate-xy x2 y2)]
                                      [(x3 y3) (rotate-xy x3 y3)])
                          ;; Scale and move to match ellipse:
                          (let ([x0 (+ (* x0 w/2) x)]
                                [x1 (+ (* x1 w/2) x)]
                                [x2 (+ (* x2 w/2) x)]
                                [x3 (+ (* x3 w/2) x)]
                                [y0 (+ (* y0 h/2) y)]
                                [y1 (+ (* y1 h/2) y)]
                                [y2 (+ (* y2 h/2) y)]
                                [y3 (+ (* y3 h/2) y)])
                            (set! pts
                                  (cons
                                   (if (positive? angle)
                                       (if ccw?
                                           (vector x0 y0 x1 y1 x2 y2 x3 y3)
                                           (vector x3 y3 x2 y2 x1 y1 x0 y0))
                                       (if ccw?
                                           (vector x0 y0 x3 y3)
                                           (vector x3 y3 x0 y0)))
                                   pts))
                            (loop (+ start angle)
                                  (- delta angle)))))))))
              (for ([v (in-list (if ccw? (r:reverse pts) pts))])
                (if (or (pair? open-points)
                        (pair? rev-open-points))
                    (do-line-to (vector-ref v 0) (vector-ref v 1))
                    (do-move-to (vector-ref v 0) (vector-ref v 1)))
                (if (= (vector-length v) 4)
                    (do-line-to (vector-ref v 2) (vector-ref v 3))
                    (do-curve-to (vector-ref v 2) (vector-ref v 3)
                                 (vector-ref v 4) (vector-ref v 5)
                                 (vector-ref v 6) (vector-ref v 7)))))))))

    (define/public (ellipse x y w h)
      (when (open?) (close))
      (do-arc x y w h 0 2pi #f)
      (close))

    (define/public (text-outline font str x y [combine? #f])
      (when (open?) (close))
      (let ([p (text-to-path font str x y combine?)])
        (for ([a (in-list p)])
          (case (car a)
            [(move) (move-to (cadr a) (caddr a))]
            [(line) (line-to (cadr a) (caddr a))]
            [(curve) (curve-to (cadr a) (caddr a)
                               (list-ref a 3) (list-ref a 4)
                               (list-ref a 5) (list-ref a 6))]
            [(close) (close)])))
      (close))

    (define/public (scale x y)
      (unless (and (= x 1.0) (= y 1.0))
        (flatten-open!)
        (flatten-closed!)
        (set! open-points (scale-points open-points x y))
        (set! closed-points
              (for/list ([pts (in-list closed-points)])
                (scale-points pts x y)))))
    (define/private (scale-points pts x y)
      (for/list ([p (in-list pts)])
        (if (pair? p)
            (cons (* (car p) x)
                  (* (cdr p) y))
            (vector (* (vector-ref p 0) x)
                    (* (vector-ref p 1) y)
                    (* (vector-ref p 2) x)
                    (* (vector-ref p 3) y)))))
  
    (define/public (translate x y)
      (unless (and (zero? x) (zero? y))
        (flatten-open!)
        (flatten-closed!)
        (set! open-points (translate-points open-points x y))
        (set! closed-points
              (for/list ([pts (in-list closed-points)])
                (translate-points pts x y)))))
    (define/private (translate-points pts x y)
      (for/list ([p (in-list pts)])
        (if (pair? p)
            (cons (+ (car p) x)
                  (+ (cdr p) y))
            (vector (+ (vector-ref p 0) x)
                    (+ (vector-ref p 1) y)
                    (+ (vector-ref p 2) x)
                    (+ (vector-ref p 3) y)))))

    (define/public (rotate th)
      (flatten-open!)
      (flatten-closed!)
      (set! open-points (rotate-points open-points th))
      (set! closed-points
            (for/list ([pts (in-list closed-points)])
              (rotate-points pts th))))
    (define/private (rotate-points pts th)
      (if (zero? th)
          pts
          (for/list ([p (in-list pts)])
            (if (pair? p)
                (let-values ([(x y) (rotate-point th (car p) (cdr p))])
                  (cons x y))
                (let-values ([(x2 y2) (rotate-point th (vector-ref p 0) (vector-ref p 1))]
                             [(x3 y3) (rotate-point th (vector-ref p 2) (vector-ref p 3))])
                  (vector x2 y2 x3 y3))))))
    (define/private (rotate-point th x y)
      (let* ([cx (make-rectangular x y)]
             [cx (make-polar (magnitude cx) (+ (angle cx) (- th)))])
        (values (real-part cx) (imag-part cx))))

    (define/public (transform m)
      (flatten-open!)
      (flatten-closed!)
      (set! open-points (transform-points open-points m))
      (set! closed-points
            (for/list ([pts (in-list closed-points)])
              (transform-points pts m))))
    (define/private (transform-points pts m)
      (for/list ([p (in-list pts)])
        (if (pair? p)
            (let-values ([(x y) (transform-point m (car p) (cdr p))])
              (cons x y))
            (let-values ([(x2 y2) (transform-point m (vector-ref p 0) (vector-ref p 1))]
                         [(x3 y3) (transform-point m (vector-ref p 2) (vector-ref p 3))])
              (vector x2 y2 x3 y3)))))
    (define/private (transform-point m x y)
      (values (+ (* x (vector-ref m 0))
                 (* y (vector-ref m 2))
                 (vector-ref m 4))
              (+ (* x (vector-ref m 1))
                 (* y (vector-ref m 3))
                 (vector-ref m 5))))

    (define/public (rectangle x y w h)
      (when (open?) (close))
      (move-to x y)
      (line-to (+ x w) y)
      (line-to (+ x w) (+ y h))
      (line-to x (+ y h))
      (close))

    (define/public (rounded-rectangle x y w h [radius -0.25])
      (when (open?) (close))
      (let ([dx (min (/ w 2)
                     (if (negative? radius)
                         (* (min w h) (- radius))
                         radius))]
            [dy (min (/ h 2)
                     (if (negative? radius)
                         (* (min w h) (- radius))
                         radius))])
        (move-to (+ x (- w dx)) y)
        (arc (+ x w) y (* -2 dx) (* 2 dy) pi/2 0.0 #f)
        (line-to (+ x w) (+ y dy))
        (line-to (+ x w) (+ y (- h dy)))
        (arc (+ x w) (+ y h) (* -2 dx) (* -2 dy) 0 (- pi/2) #f)
        (line-to (+ x (- w dx)) (+ y h))
        (line-to (+ x dx) (+ y h))
        (arc x (+ y h) (* 2 dx) (* -2 dy) (- pi/2) (- pi) #f)
        (line-to x (+ y (- h dy)))
        (line-to x (+ y dy))
        (arc x y (* 2 dx) (* 2 dy) pi pi/2 #f)
        (close)))

    (super-new)))
