#lang racket

(require plot plot/utils)

(define (rgb->hsv rgb)
  (match-define (list r g b) (map (λ (x) (/ x 255)) rgb))
  (define mx (max r g b))
  (define mn (min r g b))
  (define c (- mx mn))
  (define h (* 60 (cond [(zero? c)  0]
                        [(= mx r)   (/ (- g b) c)]
                        [(= mx g)   (+ (/ (- b r) c) 2)]
                        [(= mx b)   (+ (/ (- r g) c) 4)])))
  (list (if (h . < . 0) (+ h 360) h)
        (if (zero? mx) 0 (/ c mx))
        mx))

(define (hsv->rgb hsv)
  (match-define (list h s v) hsv)
  (define c (* v s))
  (let ([h  (/ (real-modulo h 360) 60)])
    (define x (* c (- 1 (abs (- (real-modulo h 2) 1)))))
    (define-values (r g b)
      (cond [(and (0 . <= . h) (h . < . 1))  (values c x 0)]
            [(and (1 . <= . h) (h . < . 2))  (values x c 0)]
            [(and (2 . <= . h) (h . < . 3))  (values 0 c x)]
            [(and (3 . <= . h) (h . < . 4))  (values 0 x c)]
            [(and (4 . <= . h) (h . < . 5))  (values x 0 c)]
            [(and (5 . <= . h) (h . < . 6))  (values c 0 x)]))
    (define m (- v c))
    (list (* 255 (+ r m))
          (* 255 (+ g m))
          (* 255 (+ b m)))))

(define pen-colors (sort (for/list ([i  (in-range 1 7)])
                           (rgb->hsv (->pen-color i)))
                         < #:key first))

(define brush-colors (sort (for/list ([i  (in-range 1 7)])
                             (rgb->hsv (->brush-color i)))
                           < #:key first))

(define (pen-hue-transform h)
  (define x (/ h 60))
  (* (- x (* 5/12 (sin (* (/ x 6) (* 3 pi))))) (/ 360 6)))

(define (pen-saturation-transform h) 1)

(define (pen-value-transform h)
  (define x (/ h 60))
  (+ 1/2 (* 1/6 (sin (* (/ x 6) (* 3 pi))))))

(plot (list (points (map vector '(0 1 2 3 4 5 6) (append (map first pen-colors) (list 360))))
            (function (λ (x) (* x 60)))
            (function (λ (x) (pen-hue-transform (* x 60)))
                      #:color 3))
      #:title "Pen Hue")

(plot (list (points (map vector '(0 1 2 3 4 5 6) (append (map second pen-colors)
                                                         (list (second (first pen-colors))))))
            (function (λ (x) (pen-saturation-transform (* x 60)))))
      #:y-min 0 #:y-max 1
      #:title "Pen Saturation")

(plot (list (points (map vector '(0 1 2 3 4 5 6) (append (map third pen-colors)
                                                         (list (third (first pen-colors))))))
            (function (λ (x) (pen-value-transform (* x 60)))))
      #:y-min 0 #:y-max 1
      #:title "Pen Value")

(define (integer->hue n)
  (let ([n  (abs n)])
    (define i (+ (case (remainder n 6) [(0) 0] [(1) 2] [(2) 4] [(3) 1] [(4) 3] [(5) 5])
                 (* 6 3 (quotient n 6))))
    (remainder (* i 59) 360)))

(define (pen-color n)
  (define h (integer->hue n))
  (hsv->rgb (list (pen-hue-transform h) (pen-saturation-transform h) (pen-value-transform h))))

(plot (for/list ([n  (in-range 60)])
        (lines (list (vector 0 n) (vector 1 n))
               #:color (pen-color n)
               #:width 6)))

(define (brush-hue-transform h)
  (define x (/ h 60))
  (define y (* (/ (- (sqrt (+ x 2)) (sqrt 2))
                  (- (sqrt 8) (sqrt 2)))
               6))
  (* (- x (* 1/4 (sin (* (/ y 6) (* 3 pi))))) (/ 360 6)))

(define (brush-saturation-transform h)
  (define x (/ h 60))
  (+ 3/16 (* 3/32 (sin (* (/ x 6) (* 2 pi))))))

(define (brush-value-transform h) 1)

(plot (list (points (map vector '(0 1 2 3 4 5 6) (append (map first brush-colors) (list 360))))
            (function (λ (x) (* x 60)))
            (function (λ (x) (brush-hue-transform (* x 60)))
                      #:color 3))
      #:title "Brush Hue")

(plot (list (points (map vector '(0 1 2 3 4 5 6) (append (map second brush-colors)
                                                         (list (second (first brush-colors))))))
            (function (λ (x) (brush-saturation-transform (* x 60)))))
      #:y-min 0 #:y-max 1
      #:title "Brush Saturation")

(plot (list (points (map vector '(0 1 2 3 4 5 6) (append (map third brush-colors)
                                                         (list (third (first brush-colors))))))
            (function (λ (x) (brush-value-transform (* x 60)))))
      #:y-min 0 #:y-max 1
      #:title "Brush Value")

(define (brush-color n)
  (define h (integer->hue n))
  (hsv->rgb (list (brush-hue-transform h) (brush-saturation-transform h) (brush-value-transform h))))

(define (integer->value n)
  (* 1/7 (remainder (abs n) 8)))

(define (pen-intensity-transform i)
  (* 128 (expt (integer->value i) 3/4)))

(define (brush-intensity-transform i)
  (+ 127 (* 128 (expt (- 1 (integer->value i)) 3/4))))

(define (gray-pen-color i)
  (make-list 3 (pen-intensity-transform i)))

(define (gray-brush-color i)
  (make-list 3 (brush-intensity-transform i)))

(plot (for/list ([n  (in-range 8)])
        (function-interval sin (λ (x) (+ 1 (sin x))) n (+ 1 n)
                           #:color (gray-brush-color n)
                           #:line1-color (gray-pen-color n)
                           #:line2-color (gray-pen-color n)
                           #:line1-width 2 #:line2-width 2 #:alpha 1)))

(define new-brush-colors
  (append (list (gray-brush-color 0))
          (build-list 120 brush-color)
          (build-list 7 (λ (n) (gray-brush-color (- 7 n))))))

(define new-pen-colors
  (append (list (gray-brush-color 0))
          (build-list 120 brush-color)
          (build-list 7 (λ (n) (gray-brush-color (- 7 n))))))

(plot (for/list ([n  (in-range 60)])
        (lines (list (vector 0 n) (vector 1 n))
               #:color (brush-color n)
               #:width 6)))

(plot (for*/list ([i  (in-range 6)] [j  (in-range 20)])
        (define n (+ i (* j 6)))
        (rectangles (list (vector (ivl (+ i 0.05) (+ i 0.95))
                                  (ivl (+ j 0.05) (+ j 0.95))))
                    #:color (brush-color n)
                    #:line-color (pen-color n)
                    #:line-width 3))
      #:height 1200)

(plot (list (for*/list ([n  (in-range 8)])
              (rectangles (list (vector (ivl (+ n 0.05) (+ n 0.95))
                                        (ivl 0 1)))
                          #:color (->brush-color n)
                          #:line-color (->pen-color n)
                          #:line-width 3))
            (for*/list ([n  (in-range 8)])
              (rectangles (list (vector (ivl (+ n 0.05) (+ n 0.95))
                                        (ivl 1.2 2.2)))
                          #:color (brush-color (- n 1))
                          #:line-color (pen-color (- n 1))
                          #:line-width 3)))
      #:height 200)

(plot (for/list ([n  (in-range 12)])
        (function-interval (λ (x) (* 1/2 (sqr (+ x (* 2 n)))))
                           (λ (x) (+ 1/2 (sqr (+ x (* 2 n)))))
                           0 12 #:color (brush-color n)
                           #:line1-color (pen-color n)
                           #:line2-color (pen-color n)
                           #:line1-width 2 #:line2-width 2)))

(plot (for/list ([n  (in-range 6)])
        (function-interval (λ (x) (* 1/2 (sin (+ x n))))
                           (λ (x) (+ 1/2 (sin (+ x n))))
                           -4 4 #:color (->brush-color (+ n 1))
                           #:line1-color (->pen-color (+ n 1))
                           #:line2-color (->pen-color (+ n 1))
                           #:line1-width 2 #:line2-width 2)))
