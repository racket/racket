#lang scheme/base

#|

The test suite for this code is in
plt/collects/tests/mzscheme/htdp-image.rkt

|#


(require (except-in mred
                    make-color)
         mzlib/class
         mrlib/cache-image-snip
         mzlib/math
         lang/prim
         lang/posn
         lang/private/imageeq
         "color-structs.rkt"
         "error.rkt")

(provide-primitives
         image?
         scene?
         image=?
         image-width
         image-height
         overlay
         overlay/xy
         
         pinhole-x
         pinhole-y
         move-pinhole
         put-pinhole
         
         rectangle
         circle
         ellipse
         triangle
         line
         star
         add-line
         text
         regular-polygon
         
         shrink
         shrink-tl
         shrink-tr
         shrink-bl
         shrink-br
         
         image-inside?
         find-image
         
         image->color-list
         color-list->image
         
         image->alpha-color-list
         alpha-color-list->image
         
         image-color?
         make-color
         color-red
         color-green
         color-blue
         color?
         make-alpha-color
         alpha-color-alpha
         alpha-color-red
         alpha-color-green
         alpha-color-blue
         alpha-color?)

;; ----------------------------------------

(define (color-list? l)
  (and (list? l) (andmap image-color? l)))
(define (alpha-color-list? l)
  (and (list? l) (andmap alpha-color? l)))

;; ----------------------------------------

(define (to-exact-int x) (floor0 (if (exact? x) x (inexact->exact x))))
(define (floor0 n)
  (cond
    [(< n 0) (- (floor (- n)))]
    [else (floor n)]))

;; ----------------------------------------

(define (check name p? v desc arg-posn) (check-arg name (p? v) desc arg-posn v))

(define (check-coordinate name val arg-posn) (check name finite-real? val "finite real number" arg-posn))
(define (check-integer-coordinate name val arg-posn) (check name nii? val "integer" arg-posn))
(define (check-size name val arg-posn) (check name pos-number? val "positive number" arg-posn))
(define (check-posi-size name val arg-posn) (check name pos-integer? val "positive integer" arg-posn))
(define (check-size/0 name val arg-posn) (check name nn-number? val "non-negative number" arg-posn))
(define (check-image name val arg-posn) (check name image? val "image" arg-posn))
(define (check-image-color name val arg-posn) 
  (let ([simple-check (λ (x) (or (string? x) (symbol? x) (color? x)))])
    (check name simple-check val "image-color" arg-posn)
    (unless (image-color? val)
      (error name "~e is not a valid color name" val))))
(define (check-mode name val arg-posn) (check name mode? val mode-str arg-posn))

(define (pos-number? i) (and (number? i) (positive? i)))
(define (pos-integer? i) (and (number? i) (positive? i) (integer? i)))
(define (nn-number? i) (and (number? i) (or (zero? i) (positive? i))))
(define (nii? x) (and (integer? x) (not (= x +inf.0)) (not (= x -inf.0))))
(define (finite-real? x) (and (real? x) (not (= x +inf.0)) (not (= x -inf.0))))

(define (check-sizes who w h)
  (unless (and (< 0 w 10000) (< 0 h 10000))
    (error who (format "cannot make ~a x ~a image" w h))))

(define (mode? x)
  (member x '(solid "solid" outline "outline")))

(define mode-str "'solid \"solid\" 'outline or \"outline\"")

(define (mode->brush-symbol m)
  (cond
    [(member m '(solid "solid"))
     'solid]
    [(member m '(outline "outline"))
     'transparent]))

(define (mode->pen-symbol m)
  (cond
    [(member m '(solid "solid")) 'transparent]
    [(member m '(outline "outline")) 'solid]))

(define (make-color% c)
  (cond
    [(string? c) (send the-color-database find-color c)]
    [(symbol? c) (send the-color-database find-color (symbol->string c))]
    [(color? c) (make-object color%
                  (color-red c)
                  (color-green c)
                  (color-blue c))]
    [else #f]))

(define (image-color? c) 
  (cond
    [(color? c) #t]
    [(string? c) (and (send the-color-database find-color c) #t)]
    [(symbol? c) (and (send the-color-database find-color (symbol->string c)) #t)]
    [else #f]))

(define (image-width a)
  (check-image 'image-width a "first")
  (let-values ([(w h) (snip-size a)])
    (inexact->exact (ceiling w))))

(define (image-height a)
  (check-image 'image-height a "first")
  (let-values ([(w h) (snip-size a)])
    (inexact->exact (ceiling h))))

(define (pinhole-x a)
  (check-image 'pinhole-x a "first")
  (let-values ([(x y) (send (coerce-to-cache-image-snip a) get-pinhole)])
    x))

(define (pinhole-y a)
  (check-image 'pinhole-y a "first")
  (let-values ([(x y) (send (coerce-to-cache-image-snip a) get-pinhole)])
    y))

(define (move-pinhole raw-i dx dy)
  (check-image 'move-pinhole raw-i "first")
  (check-coordinate 'move-pinhole dx "second")
  (check-coordinate 'move-pinhole dy "third")
  (let ([i (coerce-to-cache-image-snip raw-i)])
    (let-values ([(px py) (send i get-pinhole)]
                 [(w h) (send i get-size)])
      (new cache-image-snip%
           (dc-proc (send i get-dc-proc))
           (argb-proc (send i get-argb-proc))
           (width w)
           (height h)
           (argb (send i get-argb/no-compute))
           (px (+ px (floor0 dx)))
           (py (+ py (floor0 dy)))))))

(define (put-pinhole raw-i px py)
  (check-image 'put-pinhole raw-i "first")
  (check-coordinate 'put-pinhole px "second")
  (check-coordinate 'put-pinhole py "third")
  (let ([i (coerce-to-cache-image-snip raw-i)])
    (let-values ([(w h) (send i get-size)])
      (new cache-image-snip%
           (dc-proc (send i get-dc-proc))
           (argb-proc (send i get-argb-proc))
           (width w)
           (height h)
           (argb (send i get-argb/no-compute))
           (px (floor0 px))
           (py (floor0 py))))))

(define (overlay a b . cs)
  (check-image 'overlay a "first")
  (check-image 'overlay b "second")
  (let loop ([cs cs]
             [i 3])
    (unless (null? cs)
      (check-image 'overlay (car cs) (number->ord i))))
  (let ([all-imgs (reverse (list* a b cs))])
    (let loop ([imgs (cdr all-imgs)]
               [acc (car all-imgs)])
      (cond
        [(null? imgs) acc]
        [else (loop (cdr imgs)
                    (real-overlay/xy 'overlay (car imgs) 0 0 acc))]))))

(define (overlay/xy a dx dy b)
  (check-image 'overlay/xy a "first")
  (check-coordinate 'overlay/xy dx "second")
  (check-coordinate 'overlay/xy dy "third")
  (check-image 'overlay/xy b "fourth")
  (real-overlay/xy 'overlay/xy 
                   a
                   (to-exact-int dx)
                   (to-exact-int dy)
                   b))

(define (real-overlay/xy name raw-a raw-delta-x raw-delta-y raw-b)
  (let ([a (coerce-to-cache-image-snip raw-a)]
        [b (coerce-to-cache-image-snip raw-b)])
    (let-values ([(a-w a-h) (snip-size a)]
                 [(b-w b-h) (snip-size b)]
                 [(a-px a-py) (send a get-pinhole)]
                 [(b-px b-py) (send b get-pinhole)])
      (let* ([delta-x (+ raw-delta-x a-px (- b-px))]
             [delta-y (+ raw-delta-y a-py (- b-py))]
             [left (min 0 delta-x)]
             [top (min 0 delta-y)]
             [right (max (+ delta-x b-w) a-w)]
             [bottom (max (+ delta-y b-h) a-h)]
             [new-w (inexact->exact (ceiling (- right left)))]
             [new-h (inexact->exact (ceiling (- bottom top)))]
             [a-dx (inexact->exact (round (- left)))]
             [a-dy (inexact->exact (round (- top)))]
             [b-dx (inexact->exact (round (- delta-x left)))]
             [b-dy (inexact->exact (round (- delta-y top)))]
             [new-px (- a-px left)]
             [new-py (- a-py top)]
             [combine (lambda (a-f b-f)
                        (lambda (dc dx dy)
                          (a-f dc (+ dx a-dx) (+ dy a-dy))
                          (b-f dc (+ dx b-dx) (+ dy b-dy))))])
        (unless (and (<= 0 new-w 10000) (<= 0 new-h 10000))
          (error name (format "cannot make ~a x ~a image" new-w new-h)))
        (new cache-image-snip%
             [dc-proc (combine (send a get-dc-proc)
                               (send b get-dc-proc))]
             [argb-proc (combine (send a get-argb-proc)
                                 (send b get-argb-proc))]
             [width new-w]
             [height new-h]
             [px new-px]
             [py new-py])))))
;; ------------------------------------------------------------

(define (shrink raw-img in-left in-up in-right in-down)
  (check-image 'shrink raw-img "first")
  (check-size/0 'shrink in-left "second")
  (check-size/0 'shrink in-up "third")
  (check-size/0 'shrink in-right "fourth")
  (check-size/0 'shrink in-down "fifth")
  (let ([left (to-exact-int in-left)]
        [up (to-exact-int in-up)]
        [right (to-exact-int in-right)]
        [down (to-exact-int in-down)]
        [img (coerce-to-cache-image-snip raw-img)])
    (let-values ([(i-px i-py) (send img get-pinhole)]
                 [(i-width i-height) (send img get-size)])
      (let* ([dc-proc (send img get-dc-proc)]
             [argb-proc (send img get-argb-proc)]
             [delta-w (- i-px left)]
             [delta-h (- i-py up)]
             [width (+ left right 1)]
             [height (+ up down 1)])
        (new cache-image-snip%
             [px left]
             [py up]
             [dc-proc (lambda (dc dx dy)
                        (let ([clip (send dc get-clipping-region)]
                              [rgn (make-object region% dc)])
                          (send rgn set-rectangle dx dy width height)
                          (when clip
                            (send rgn intersect clip))
                          (send dc set-clipping-region rgn)
                          (dc-proc dc (- dx delta-w) (- dy delta-h))
                          (send dc set-clipping-region clip)))]
             [argb-proc (lambda (argb dx dy) (argb-proc argb (- dx delta-w) (- dy delta-h)))]
             [width width]
             [height height])))))

(define (shrink-tl raw-img in-x in-y)
  (check-image 'shrink-tl raw-img "first")
  (check-size 'shrink-tl in-x "second")
  (check-size 'shrink-tl in-y "third")
  (let ([x (to-exact-int in-x)]
        [y (to-exact-int in-y)])
    (put-pinhole (shrink (put-pinhole raw-img 0 0) 0 0 (- x 1) (- y 1)) (/ x 2) (/ y 2))))

(define (shrink-tr raw-img in-x in-y)
  (check-image 'shrink-tr raw-img "first")
  (check-size 'shrink-tr in-x "second")
  (check-size 'shrink-tr in-y "third")
  (let ([x (to-exact-int in-x)]
        [y (to-exact-int in-y)])
    (put-pinhole (shrink (put-pinhole raw-img (- (image-width raw-img) 1) 0) (- x 1) 0 0 (- y 1))
                 (/ x 2)
                 (/ y 2))))

(define (shrink-bl raw-img in-x in-y)
  (check-image 'shrink-bl raw-img "first")
  (check-size 'shrink-bl in-x "second")
  (check-size 'shrink-bl in-y "third")
  (let ([x (to-exact-int in-x)]
        [y (to-exact-int in-y)])
    (put-pinhole (shrink (put-pinhole raw-img 0 (- (image-height raw-img) 1)) 0 (- y 1) (- x 1) 0) 
                 (/ x 2)
                 (/ y 2))))

(define (shrink-br raw-img in-x in-y)
  (check-image 'shrink-br raw-img "first")
  (check-size 'shrink-br in-x "second")
  (check-size 'shrink-br in-y "third")
  (let ([x (to-exact-int in-x)]
        [y (to-exact-int in-y)])
    (put-pinhole (shrink (put-pinhole raw-img (- (image-width raw-img) 1) (- (image-height raw-img) 1))
                         (- x 1)
                         (- y 1)
                         0
                         0)
                 (/ x 2)
                 (/ y 2))))


;; ------------------------------------------------------------

(define (line in-x in-y color)
  (check-coordinate 'line in-x "first")
  (check-coordinate 'line in-y "second")
  (check-image-color 'line color "third")
  (let* ([x (floor (inexact->exact in-x))]
         [y (floor (inexact->exact in-y))]
         [w (+ (abs x) 1)]
         [h (+ (abs y) 1)]
         [px (abs (min x 0))]
         [py (abs (min y 0))])
    (check-sizes 'line w h)
    (let* ([do-draw
            (λ (dc dx dy)
              (send dc draw-line (+ px dx) (+ py dy) (+ dx px x) (+ dy py y)))]
           [draw-proc (make-color-wrapper color 'transparent 'solid do-draw)]
           [mask-proc (make-color-wrapper 'black 'transparent 'solid do-draw)])
      (make-simple-cache-image-snip w h px py draw-proc mask-proc))))

;; test what happens when the line moves out of the box.
(define (add-line raw-i pre-x1 pre-y1 pre-x2 pre-y2 color-in)
  (check-image 'add-line raw-i "first")
  (check-coordinate 'add-line pre-x1 "second")
  (check-coordinate 'add-line pre-y1 "third")
  (check-coordinate 'add-line pre-x2 "fourth")
  (check-coordinate 'add-line pre-y2 "fifth")
  (check-image-color 'add-line color-in "sixth")
  (let ([i (coerce-to-cache-image-snip raw-i)])
    (let-values ([(px py) (send i get-pinhole)]
                 [(iw ih) (send i get-size)]
                 [(x1 y1 x2 y2)
                  (if (<= pre-x1 pre-x2)
                      (values pre-x1 pre-y1 pre-x2 pre-y2)
                      (values pre-x2 pre-y2 pre-x1 pre-y1))])
      (let* ([line-w (abs (- x2 x1))]
             [line-h (abs (- y2 y1))]
             [build-snip
              (λ (do-draw py-offset)
                (let* ([draw-proc 
                        (make-color-wrapper color-in 'transparent 'solid do-draw)]
                       [mask-proc
                        (make-color-wrapper 'black 'transparent 'solid do-draw)]
                       [line
                        (make-simple-cache-image-snip (+ line-w 1) (+ line-h 1) px py draw-proc mask-proc)])
                  (real-overlay/xy 'add-line i (+ px x1) (+ py py-offset) line)))])
        (if (y1 . <= . y2)
            (build-snip (λ (dc dx dy)
                          (send dc draw-line 
                                dx
                                dy
                                (+ dx (- x2 x1))
                                (+ dy (- y2 y1))))
                        y1)
            (build-snip (λ (dc dx dy)
                          (send dc draw-line 
                                dx
                                (+ dy line-h)
                                (+ dx line-w)
                                dy))
                        y2))))))

(define (text str size color-in)
  (check 'text string? str "string" "first")
  (check 'text (lambda (x) (and (integer? x) (<= 1 x 255))) size "integer between 1 and 255" "second")
  (check-image-color 'text color-in "third")
  (cond
    [(string=? str "")
     (let-values ([(tw th) (get-text-size size "dummyX")])
       (put-pinhole (rectangle 0 th 'solid 'black) 0 0))]
    [else
     (let ([color (make-color% color-in)])
       (let-values ([(tw th) (get-text-size size str)])
         (let ([draw-proc
                (lambda (txt-color mode dc dx dy)
                  (let ([old-mode (send dc get-text-mode)]
                        [old-fore (send dc get-text-foreground)]
                        [old-font (send dc get-font)])
                    (send dc set-text-mode mode)
                    (send dc set-text-foreground txt-color)
                    (send dc set-font (get-font size))
                    (send dc draw-text str dx dy)
                    (send dc set-text-mode old-mode)
                    (send dc set-text-foreground old-fore)
                    (send dc set-font old-font)))])
           (new cache-image-snip%
                [dc-proc (lambda (dc dx dy) (draw-proc color 'transparent dc dx dy))]
                [argb-proc 
                 (lambda (argb dx dy)
                   (let ([bm-color
                          (build-bitmap
                           (lambda (dc)
                             (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
                             (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))
                             (send dc draw-rectangle 0 0 tw th))
                           tw
                           th)]
                         [bm-mask
                          (build-bitmap
                           (lambda (dc)
                             (draw-proc 
                              (send the-color-database find-color "black")
                              'solid dc 0 0))
                           tw
                           th)])
                     (overlay-bitmap argb dx dy bm-color bm-mask)))]
                [width tw]
                [height th]
                [px 0]
                [py 0]))))]))

(define cached-bdc-for-text-size (make-thread-cell #f))
(define (get-text-size size string)
  (unless (thread-cell-ref cached-bdc-for-text-size)
    (let* ([bm (make-object bitmap% 1 1)]
           [dc (make-object bitmap-dc% bm)])
      (thread-cell-set! cached-bdc-for-text-size dc)))
  (let ([dc (thread-cell-ref cached-bdc-for-text-size)])
    (let-values ([(w h _1 _2) (send dc get-text-extent string (get-font size))])
      (values (inexact->exact (ceiling w)) 
              (inexact->exact (ceiling h))))))

(define (get-font size)
  (send the-font-list find-or-create-font size
        'default 'normal 'normal #f
        (case (system-type)
          [(macosx) 'partly-smoothed]
          [else 'smoothed])))

(define (a-rect/circ do-draw w h color brush pen)
  (let* ([dc-proc (make-color-wrapper color brush pen do-draw)]
         [mask-proc (make-color-wrapper 'black brush pen do-draw)])
    (make-simple-cache-image-snip w h (floor (/ w 2)) (floor (/ h 2)) dc-proc mask-proc)))

(define (rectangle in-w in-h mode color)
  (check-size/0 'rectangle in-w "first")
  (check-size/0 'rectangle in-h "second")
  (check-mode 'rectangle mode "third")
  (check-image-color 'rectangle color "fourth")
  (let ([w (inexact->exact (floor in-w))]
        [h (inexact->exact (floor in-h))])
    (a-rect/circ (lambda (dc dx dy) (send dc draw-rectangle dx dy w h))
                 w h color (mode->brush-symbol mode) (mode->pen-symbol mode))))

(define (ellipse in-w in-h mode color)
  (check-size/0 'ellipse in-w "first")
  (check-size/0 'ellipse in-h "second")
  (check-mode 'ellipse mode "third")
  (check-image-color 'ellipse color "fourth")
  (let ([w (inexact->exact (floor in-w))]
        [h (inexact->exact (floor in-h))])
    (a-rect/circ (lambda (dc dx dy) (send dc draw-ellipse dx dy w h))
                 w h color (mode->brush-symbol mode) (mode->pen-symbol mode))))

(define (circle in-r mode color)
  (check-size/0 'circle in-r "first")
  (check-mode 'circle mode "second")
  (check-image-color 'circle color "third")
  (let ([r (inexact->exact (floor in-r))])
    (a-rect/circ (lambda (dc dx dy) (send dc draw-ellipse dx dy (* 2 r) (* 2 r)))
                 (* 2 r) (* 2 r) color (mode->brush-symbol mode) (mode->pen-symbol mode))))

(define (triangle in-size mode color)
  (check 'triangle
         (lambda (x) (and (real? x) (< 2 x 10000)))
         in-size 
         "positive real number bigger than 2"
         "first")
  (check-mode 'triangle mode "second")
  (check-image-color 'triangle color "third")
  (let* ([size (floor (inexact->exact in-size))]
         [right (- size 1)]
         [bottom (inexact->exact (ceiling (* size (sin (* 2/3 pi)))))]
         [points (list (make-object point% 0 bottom)
                       (make-object point% right bottom)
                       (make-object point% (/ size 2) 0))]
         [draw (make-color-wrapper
                color (mode->brush-symbol mode) 'solid
                (lambda (dc dx dy)
                  (send dc draw-polygon points dx dy)))]
         [mask-draw (make-color-wrapper
                     'black (mode->brush-symbol mode) 'solid
                     (lambda (dc dx dy)
                       (send dc draw-polygon points dx dy)))]
         [w size]
         [h (+ bottom 1)])
    (make-simple-cache-image-snip w h (floor (/ w 2)) (floor (/ h 2)) draw mask-draw)))

(define (regular-polygon sides in-radius mode color [angle 0])
  (check 'regular-polygon 
         (λ (x) (and (integer? x) (<= 3 sides 10000)))
         sides
         "positive integer bigger than or equal to 3"
         "first")
  (check-size 'regular-polygon in-radius "second")
  (check-mode 'regular-polygon mode "third")
  (check-image-color 'regular-polygon color "fourth")
  (check 'regular-polygon real? angle "real number" "fifth")
  (let* ([radius (inexact->exact (floor in-radius))]
         [points (regular-polygon-points radius sides angle)]
         [draw 
          (make-color-wrapper
           color (mode->brush-symbol mode) 'solid
           (λ (dc dx dy)
             (send dc draw-polygon points dx dy)))]
         [mask-draw 
          (make-color-wrapper
           'black (mode->brush-symbol mode) 'solid
           (λ (dc dx dy)
             (send dc draw-polygon points dx dy)))])
    (make-simple-cache-image-snip 
     (* radius 2)
     (* radius 2)
     radius
     radius
     draw
     mask-draw)))

(define (regular-polygon-points in-radius points delta-angle)
  (let ([radius (- in-radius 1)])
    (let loop ([n points])
      (cond
        [(zero? n) null]
        [else
         (let ([angle (+ delta-angle (/ (* 2 pi n) points))])
           (cons (make-object point% 
                   (+ radius (* radius (cos angle)))
                   (+ radius (* radius (sin angle))))
                 (loop (- n 1))))]))))
  
(define (star points in-inner-radius in-outer-radius mode color)
  (check 'star
         (lambda (x) (and (integer? x) (<= 3 x 10000)))
         points
         "positive integer bigger than or equal to 3"
         "first")
  (check-size 'star in-inner-radius "second")
  (check-size 'star in-outer-radius "third")
  (check-mode 'star mode "fourth")
  (check-image-color 'star color "fifth")
  (let* ([inner-radius (inexact->exact (floor in-inner-radius))]
         [outer-radius (inexact->exact (floor in-outer-radius))]
         [points (star-points inner-radius outer-radius points)]
         [radial-offset
          (if (<= inner-radius outer-radius)
              0
              (- inner-radius outer-radius))]
         [draw 
          (make-color-wrapper
           color (mode->brush-symbol mode) 'solid
           (λ (dc dx dy)
             (send dc draw-polygon points 
                   (+ dx radial-offset)
                   (+ dy radial-offset))))]
         [mask-draw 
          (make-color-wrapper
           'black (mode->brush-symbol mode) 'solid
           (λ (dc dx dy)
             (send dc draw-polygon points
                   (+ dx radial-offset)
                   (+ dy radial-offset))))]
         
         ;; we want the radius to be this max -- if it draws outside
         ;; this radius, we should change the drawing.
         [size-determining-radius (max inner-radius outer-radius)])
    (make-simple-cache-image-snip 
     (* size-determining-radius 2)
     (* size-determining-radius 2)
     size-determining-radius
     size-determining-radius
     draw
     mask-draw)))

(define (star-points in-small-rad in-large-rad points)
  (let* ([small-rad (- in-small-rad 1)]
         [large-rad (- in-large-rad 1)]
         [roff (floor (/ large-rad 2))])
    (let loop ([i points])
      (cond
        [(zero? i) '()]
        [else 
         (let* ([this-p (- i 1)]
                [theta1 (* 2 pi (/ this-p points))]
                [theta2 (* 2 pi (/ (- this-p 1/2) points))])
           (let-values ([(x1 y1) (find-xy small-rad theta1)]
                        [(x2 y2) (find-xy large-rad theta2)])
             (let ([p1 (make-object point% 
                         (+ large-rad x1)
                         (+ large-rad y1))]
                   [p2 (make-object point%
                         (+ large-rad x2)
                         (+ large-rad y2))])
               (list* p1 p2 (loop (- i 1))))))]))))

(define (find-xy radius theta)
  (values (* radius (cos theta))
          (* radius (sin theta))))

(define (make-simple-cache-image-snip w h px py dc-proc mask-proc)
  (let ([w (inexact->exact (ceiling w))]
        [h (inexact->exact (ceiling h))])
    (let ([argb-proc 
           (if (or (zero? w) (zero? h))
               void
               (lambda (argb-vector dx dy)
                 (let ([c-bm (build-bitmap (lambda (dc) (dc-proc dc 0 0)) w h)]
                       [m-bm (build-bitmap (lambda (dc) (mask-proc dc 0 0)) w h)])
                   (overlay-bitmap argb-vector dx dy c-bm m-bm))))])
      (new cache-image-snip%
           [dc-proc dc-proc]
           [argb-proc argb-proc]
           [width w]
           [height h]
           [px px]
           [py py]))))

(define (make-color-wrapper color-in brush pen rest)
  (let ([color (make-color% color-in)])
    (lambda (dc dx dy)
      (let ([old-brush (send dc get-brush)]
            [old-pen (send dc get-pen)])
        (send dc set-brush (send the-brush-list find-or-create-brush color brush))
        (send dc set-pen (send the-pen-list find-or-create-pen color 1 pen))
        (rest dc dx dy)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)))))


;; ------------------------------------------------------------

(define (image-inside? i a)
  (and (locate-image 'image-inside? 
                     (coerce-to-cache-image-snip i)
                     (coerce-to-cache-image-snip a))
       #t))

(define (find-image i a)
  (or (locate-image 'find-image 
                    (coerce-to-cache-image-snip i)
                    (coerce-to-cache-image-snip a))
      (error 'find-image
             "the second image does not appear within the first image")))

(define (locate-image who i a)
  (check-image who i "first")
  (check-image who a "second")
  (let-values ([(iw ih) (snip-size i)]
               [(ipx ipy) (send i get-pinhole)]
               [(aw ah) (snip-size a)]
               [(apx apy) (send a get-pinhole)])
    (and (iw . >= . aw)
         (ih . >= . ah)
         (let ([i-argb-vector (argb-vector (send i get-argb))]
               [a-argb-vector (argb-vector (send a get-argb))])
           (let ([al (let loop ([offset 0])
                       (cond
                         [(= offset (* ah aw 4)) null]
                         [else (cons (subvector a-argb-vector offset (+ offset (* 4 aw)))
                                     (loop (+ offset (* 4 aw))))]))])
             (let yloop ([dy 0])
               (and (dy . <= . (- ih ah))
                    (let xloop ([dx 0])
                      (if (dx . <= . (- iw aw))
                          (if (let loop ([al al][dd 0])
                                (or (null? al)
                                    (and (first-in-second?
                                          i-argb-vector 
                                          (car al)
                                          (* 4 (+ (* (+ dy dd) iw) dx)))
                                         (loop (cdr al) (add1 dd)))))
                              (make-posn (+ dx (- apx ipx)) (+ dy (- apy ipy)))
                              (xloop (add1 dx)))
                          (yloop (add1 dy)))))))))))

(define (subvector orig i j)
  (let ([v (make-vector (- j i) #f)])
    (let loop ([x i])
      (when (< x j)
        (vector-set! v (- x i) (vector-ref orig x))
        (loop (+ x 1))))
    v))
#|
(initial inequalities thanks to Matthew (thanks!!))

We know that, for a combination:
  m3 = (m1+m2-m1*m2) and 
  b3 = (m1*b1*(1-m2) + m2*b2)/m3

So, we need to figure out what m1 & m2 might have been, 
given the other values.

Check m3:

   m3 = m2 when m1 = 0
   m3 = 1 when m1 = 1

   [deriv of m3 with respect to m1 = 1 - m2, which is positive]

    so check that m3 is between m2 and 1

Then check m3*b3:

   b3*m3 = m2*b2  when m1 = 0 or b1 = 0
   b3*m3 = (1 - m2) + m2*b2 when m1 = b1 = 1

   [deriv with respect to m1 is b1*(1-m2), which is positive]
   [deriv with respect to b1 is m1*(1-m2), which is positive]

    So check that m3*b3 is between m2*b2 and (1 - m2) + m2*b2

This is all in alphas from 0 to 1 and needs to be from 255 to 0.
Converting (but using the same names) for the alpha test, we get:

(<= (- 1 (/ m2 255))
    (- 1 (/ m3 255))
    1)

sub1 to each:

(<= (- (/ m2 255))
    (- (/ m3 255))
    0)

mult by 255:

(<= (- m2)
    (- m3)
    0)

negate and flip ineq:


(>= m2 m3 0)

flip ineq back:

(<= 0 m3 m2)


Here's the original scheme expression for the second check:

(<= (* m2 b2) 
    (* m3 b3)
    (+ (- 1 m2) (* m2 b2))

converting from the computer's coordinates, we get:


(<= (* (- 1 (/ m2 255)) (- 1 (/ b2 255)))
    (* (- 1 (/ m3 255)) (- 1 (/ b3 255)))
    (+ (- 1 (- 1 (/ m2 255)))
       (* (- 1 (/ m2 255)) (- 1 (/ b2 255)))))

;; multiplying out the binomials:

(<= (+ 1
       (- (/ m2 255)) 
       (- (/ b2 255)) 
       (/ (* m2 b2) (* 255 255)))
    (+ 1
       (- (/ m3 255)) 
       (- (/ b3 255)) 
       (/ (* m3 b3) (* 255 255)))
    (+ (- 1 (- 1 (/ m2 255)))
       (+ 1
          (- (/ m2 255)) 
          (- (/ b2 255)) 
          (/ (* m2 b2) (* 255 255)))))

;; simplifying the last term
  
(<= (+ 1
       (- (/ m2 255)) 
       (- (/ b2 255)) 
       (/ (* m2 b2) (* 255 255)))
    (+ 1
       (- (/ m3 255)) 
       (- (/ b3 255)) 
       (/ (* m3 b3) (* 255 255)))
    (+ 1
       (- (/ b2 255)) 
       (/ (* m2 b2) (* 255 255))))
  
;; multiply thru by 255:

(<= (+ 255
       (- m2) 
       (- b2) 
       (* m2 b2 1/255))
    (+ 255
       (- m3)
       (- b3)
       (* m3 b3 1/255))
    (+ 255
       (- b2) 
       (* m2 b2 1/255)))
  
;; subtract out 255 from each:

(<= (+ (- m2) 
       (- b2) 
       (* m2 b2 1/255))
    (+ (- m3)
       (- b3)
       (* m3 b3 1/255))
    (+ (- b2) 
       (* m2 b2 1/255)))

;; negate them all, and reverse the inequality

(>= (+ m2 b2 (* m2 b2 -1/255))
    (+ m3 b3 (* m3 b3 -1/255))
    (+ b2 (* m2 b2 -1/255)))

;; aka

(<= (+ b2 (* m2 b2 -1/255))
    (+ m3 b3 (* m3 b3 -1/255))
    (+ m2 b2 (* m2 b2 -1/255)))

|#

;; in the above, m3 & b3 come from iv
;; and m2 & b2 come from av
(define (first-in-second? iv av xd)
  (let loop ([i (vector-length av)])
    (or (zero? i)
        (let ([a (- i 4)]
              [r (- i 3)]
              [g (- i 2)]
              [b (- i 1)])
          (let* ([m2 (vector-ref av a)]
                 [m3 (vector-ref iv (+ xd a))]
                 [test
                  (lambda (b2 b3)
                    (<= (+ b2 (* m2 b2 -1/255))
                        (+ m3 b3 (* m3 b3 -1/255))
                        (+ m2 b2 (* m2 b2 -1/255))))])
            (and (<= 0 m3 m2)
                 (test (vector-ref av r) (vector-ref iv (+ xd r)))
                 (test (vector-ref av g) (vector-ref iv (+ xd g)))
                 (test (vector-ref av b) (vector-ref iv (+ xd b)))
                 (loop (- i 4))))))))

;; ----------------------------------------

(define (image->color-list i-raw)
  (check-image 'image->color-list i-raw "first")
  (let* ([cis (coerce-to-cache-image-snip i-raw)]
         [i (send cis get-bitmap)])
    (cond
      [(not i) '()]
      [else
       (let* ([iw (send i get-width)]
              [ih (send i get-height)]
              [new-bitmap (make-object bitmap% iw ih)]
              [bdc (make-object bitmap-dc% new-bitmap)])
         (send bdc clear)
         (send bdc draw-bitmap i 0 0 'solid 
               (send the-color-database find-color "black")
               (send i get-loaded-mask))
         (let ([is (make-bytes (* 4 iw ih))]
               [cols (make-vector (* iw ih))])
           (send bdc get-argb-pixels 0 0 iw ih is)
           (let yloop ([y 0][pos 0])
             (unless (= y ih)
               (let xloop ([x 0][pos pos])
                 (if (= x iw)
                     (yloop (add1 y) pos)
                     (begin
                       (vector-set! cols (+ x (* y iw))
                                    (make-color (bytes-ref is (+ 1 pos))
                                                (bytes-ref is (+ 2 pos))
                                                (bytes-ref is (+ 3 pos))))
                       (xloop (add1 x) (+ pos 4)))))))
           (send bdc set-bitmap #f)
           (vector->list cols)))])))

(define (image->alpha-color-list i)
  (check-image 'image->alpha-color-list i "first")
  (let* ([argb (cond
                 [(is-a? i image-snip%) 
                  (send (coerce-to-cache-image-snip i) get-argb)]
                 [(is-a? i cache-image-snip%) (send i get-argb)])]
         [v (argb-vector argb)])
    (let loop ([i (vector-length v)]
               [a null])
      (cond
        [(zero? i) a]
        [else (loop (- i 4)
                    (cons (make-alpha-color
                           (vector-ref v (- i 4))
                           (vector-ref v (- i 3))
                           (vector-ref v (- i 2))
                           (vector-ref v (- i 1)))
                          a))]))))

(define (color-list->image cl in-w in-h px py)
  (check 'color-list->image color-list? cl "list-of-colors" "first")
  (check-size/0 'color-list->image in-w "second")
  (check-size/0 'color-list->image in-h "third")
  (check-coordinate 'color-list->image px "fourth")
  (check-coordinate 'color-list->image py "fifth")
  (let ([w (inexact->exact in-w)]
        [h (inexact->exact in-h)])
    
    (unless (= (* w h) (length cl))
      (error 'color-list->image
             "given width times given height is ~a, but the given color list has ~a items"
             (* w h) 
             (length cl)))
    
    (cond
      [(or (equal? w 0) (equal? h 0))
       (put-pinhole (rectangle w h 'solid 'black) px py)]
      [else
       (unless (and (< 0 w 10000) (< 0 h 10000))
         (error 'color-list->image "cannot make ~a x ~a image" w h))
       
       (let* ([bm (make-object bitmap% w h)]
              [mask-bm (make-object bitmap% w h)]
              [dc (make-object bitmap-dc% bm)]
              [mask-dc (make-object bitmap-dc% mask-bm)])
         (unless (send bm ok?)
           (error (format "cannot make ~a x ~a image" w h)))
         (let ([is (make-bytes (* 4 w h) 0)]
               [mask-is (make-bytes (* 4 w h) 0)]
               [cols (list->vector (map (λ (x) 
                                          (or (make-color% x)
                                              (error 'color-list->image "color ~e is unknown" x)))
                                        cl))])
           (let yloop ([y 0][pos 0])
             (unless (= y h)
               (let xloop ([x 0][pos pos])
                 (if (= x w)
                     (yloop (add1 y) pos)
                     (let* ([col (vector-ref cols (+ x (* y w)))]
                            [r (pk (send col red))]
                            [g (pk (send col green))]
                            [b (pk (send col blue))])
                       (bytes-set! is (+ 1 pos) r)
                       (bytes-set! is (+ 2 pos) g)
                       (bytes-set! is (+ 3 pos) b)
                       (when (= 255 r g b)
                         (bytes-set! mask-is (+ 1 pos) 255)
                         (bytes-set! mask-is (+ 2 pos) 255)
                         (bytes-set! mask-is (+ 3 pos) 255))
                       (xloop (add1 x) (+ pos 4)))))))
           (send dc set-argb-pixels 0 0 w h is)
           (send mask-dc set-argb-pixels 0 0 w h mask-is))
         (send dc set-bitmap #f)
         (send mask-dc set-bitmap #f)
         (bitmaps->cache-image-snip bm mask-bm px py))])))

(define (pk col) (min 255 (max 0 col)))

(define (alpha-color-list->image cl in-w in-h px py)
  (check 'alpha-color-list->image alpha-color-list? cl "list-of-alpha-colors" "first")
  (check-size/0 'alpha-color-list->image in-w "second")
  (check-size/0 'alpha-color-list->image in-h "third")
  (check-coordinate 'alpha-color-list->image px "fourth")
  (check-coordinate 'alpha-color-list->image py "fifth")
  (let ([w (inexact->exact in-w)]
        [h (inexact->exact in-h)])
    (unless (= (* w h) (length cl))
      (error 'alpha-color-list->image
             "given width times given height is ~a, but the given color list has ~a items"
             (* w h) (length cl)))
    (cond
      [(or (equal? w 0) (equal? h 0))
       (put-pinhole (rectangle w h 'solid 'black) px py)]
      [else
       (unless (and (< 0 w 10000) (< 0 h 10000))
         (error 'alpha-color-list->image format "cannot make ~a x ~a image" w h))
       (let ([index-list (alpha-colors->ent-list cl)])
         (argb->cache-image-snip (make-argb (list->vector index-list) w h) px py))])))

;; alpha-colors->ent-list : (listof alpha-color) -> (listof number)
(define (alpha-colors->ent-list cl)
  (let loop ([cl cl])
    (cond
      [(null? cl) null]
      [else 
       (let ([ac (car cl)])
         (list* (alpha-color-alpha ac)
                (alpha-color-red ac)
                (alpha-color-green ac)
                (alpha-color-blue ac)
                (loop (cdr cl))))])))


;                                                                 
;                                                                 
;                                                                 
;                                                                 
;   ;;;                                  ;                        
;   ;                                                             
;   ;; ;;;  ;;   ;;;;;;;     ;  ;  ;;;;  ; ;;  ; ;;;  ;;; ;;; ;;; 
;   ;  ;   ;  ;  ;  ;  ;     ;  ;  ;  ;  ;  ; ;  ;  ; ;   ;   ;  ;
;   ;  ;   ;  ;  ;  ;  ;     ;  ;  ;  ;  ;  ; ;  ;;;; ;   ;;; ;;;;
;   ;  ;   ;  ;  ;  ;  ;     ;  ;  ;  ;  ;   ;   ;    ;     ; ;   
;   ;  ;    ;;   ;  ;  ;     ;;;;  ;  ;  ;   ;   ;;;  ;   ;;; ;;; 
;                                                                 
;                                                                 
;

(provide
 ;; type Scene = Image with pinhole in origin 
 nw:rectangle ;; Number Number Mode Color -> Image
 ;; create a rectangle with pinhole in the upper-left corner 
 place-image  ;; Image Number Number Scene -> Scene
 ;; place image at (x,y) in given scene 
 empty-scene  ;; Number Number -> Scene 
 ;; create an empty scene of size width x height (!= (nw:rectangle width height))
 scene+line   ;; Scene Number Number Number Number Color -> Scene 
 ;; cut all pieces that are outside the given rectangle 
 )

(define (nw:rectangle width height mode color)
  (check-size/0 'nw:rectangle width "first")
  (check-size/0 'nw:rectangle height "second")
  (check-mode 'nw:rectangle mode "third")
  (check-image-color 'nw:rectangle color "fourth")
  (put-pinhole (rectangle width height mode color) 0 0))

(define (place-image image x y scene)
  (check-image 'place-image image "first")
  (check-arg 'place-image (real? x) 'real "second" x)
  (check-arg 'place-image (real? y) 'real "third" y)
  (check-scene 'place-image scene "fourth")
  (let ([x (to-exact-int x)]
        [y (to-exact-int y)])
    (place-image0 image x y scene)))

(define (empty-scene width height)
  (check-size/0 'empty-scene width "first")
  (check-size/0 'empty-scene height "second")    
  (put-pinhole 
   (overlay (rectangle width height 'solid 'white)
            (rectangle width height 'outline 'black))
   0 0))

(define (scene+line img x0 y0 x1 y1 c)
  ;; img and c are checked via calls to add-line from image.rkt
  (check-arg 'scene+line (scene? img) "scene" "first" "plain image")
  (check-arg 'scene+line (real? x0) "number" "second" x0)
  (check-arg 'scene+line (real? y0) "number" "third" y0)
  (check-arg 'scene+line (real? x1) "number" "fourth" x1)
  (check-arg 'scene+line (real? y1) "number" "fifth" y1)
  (check-image-color 'scene+line c "sixth")
  (let ([x0 (to-exact-int x0)]
        [x1 (to-exact-int x1)]
        [y0 (to-exact-int y0)]
        [y1 (to-exact-int y1)])
    (add-line-to-scene0 img x0 y0 x1 y1 c)))

;; Image Number Number Image -> Image 
(define (place-image0 image x y scene)
  (define sw (image-width scene))
  (define sh (image-height scene))
  (define ns (overlay/xy scene x y image))
  (define nw (image-width ns))
  (define nh (image-height ns))
  (if (and (= sw nw) (= sh nh)) ns (shrink ns 0 0 (- sw 1) (- sh 1)))) 

;; Image Number Number Number Number Color -> Image
(define (add-line-to-scene0 img x0 y0 x1 y1 c)
  (define w (image-width img))  
  (define h (image-height img))
  (cond
    [(and (<= 0 x0) (< x0 w) (<= 0 y0) (< y0 w)
          (<= 0 x1) (< x1 w) (<= 0 y1) (< y1 w))
     ;; everything is inside 
     (add-line img x0 y0 x1 y1 c)]
    [(= x0 x1) 
     ;; vertical 
     (if (<= 0 x0 w) (add-line img x0 (app y0 h) x0 (app y1 h) c) img)]
    [(= y0 y1) 
     ;; horizontal 
     (if (<= 0 y0 h) (add-line img (app x0 w) y0 (app x1 w) y0 c) img)]
    [else 
     ;; partial off-screen 
     (let ()
       (define lin (points->line x0 y0 x1 y1))
       (define dir (direction x0 y0 x1 y1))
       (define-values (upp low lft rgt) (intersections lin w h))
       (define (add x y) (add-line img x0 y0 x y c))
       (cond
         [(and (< 0 x0 w) (< 0 y0 h)) ;; (x0,y0) is in the interior
          (case dir
            [(upper-left)  (if (number? upp) (add upp 0) (add 0 lft))]
            [(lower-left)  (if (number? low) (add low h) (add 0 lft))]
            [(upper-right) (if (number? upp) (add upp 0) (add h rgt))]
            [(lower-right) (if (number? low) (add low h) (add w rgt))]
            [else (error 'dir "contract violation: ~e" dir)])]
         [(and (< 0 x1 w) (< 0 y1 h)) ;; (x1,y1) in interior; symmetry!
          (add-line-to-scene0 img x1 y1 x0 y0 c)]
         [else 
          (cond
            [(and (number? upp) (number? low)) (add-line img upp 0 low h c)]
            [(and (number? upp) (number? lft)) (add-line img upp 0 0 lft c)]
            [(and (number? upp) (number? rgt)) (add-line img upp 0 w rgt c)]
            [(and (number? low) (number? lft)) (add-line img low h 0 lft c)]
            [(and (number? low) (number? rgt)) (add-line img low h w rgt c)]
            [(and (number? lft) (number? rgt)) (add-line img 0 lft w rgt c)]
            [else img])]))]))

;; Nat Nat -> Nat 
;; y if in [0,h], otherwise the closest boundary
(define (app y h)
  (cond
    [(and (<= 0 y) (< y h)) y]
    [(< y 0) 0]
    [else (- h 1)]))

;; Nat Nat Nat Nat -> (union 'upper-left 'upper-right 'lower-left 'lower-right)
;; how to get to (x1,y1) from (x0,y0)
(define (direction x0 y0 x1 y1)
  (string->symbol
   (string-append 
    (if (<= y0 y1) "lower" "upper") "-" (if (<= x0 x1) "right" "left"))))

#| TESTS 
'direction 
(equal? (direction 10 10 0 0) 'upper-left)
(equal? (direction 10 10 20 20) 'lower-right)
(equal? (direction 10 10 0 20) 'lower-left)
(equal? (direction 10 10 20 0) 'upper-right)
|#

;; -----------------------------------------------------------------------------
;; LINEs 

;; Number Number -> LINE
;; create a line from a slope and the intersection with the y-axis
(define-struct lyne (slope y0))

;; Nat Nat Nat Nat -> LINE
;; determine the line function from the four points (or the attributes)
;; ASSUME: (not (= x0 x1))
(define (points->line x0 y0 x1 y1)
  (define slope  (/ (- y1 y0) (- x1 x0)))
  (make-lyne slope (- y0 (* slope x0))))

;; LINE Number -> Number 
(define (of ln x) (+ (* (lyne-slope ln) x) (lyne-y0 ln)))

;; LINE Nat Nat -> [Opt Number] [Opt Number] [Opt Number] [Opt Number]
;; where does the line intersect the rectangle [0,w] x [0,h]
;; (values UP LW LF RT) means the line intersects with 
;;  the rectangle [0,w] x [0,h] at (UP,0) or (LW,h) or (0,LF) or (w,RT)
;;  when a field is false, the line doesn't interesect with that side 
(define (intersections l w h)
  (values
   (opt (X l 0) w) (opt (X l h) w) (opt (lyne-y0 l) h) (opt (of l w) h)))

;; Number Number -> [Opt Number]
(define (opt z lft) (if (<= 0 z lft) z #f))

;; LINE Number -> Number 
;; the x0 where LINE crosses y(x) = h
;; assume: LINE is not a horizontal
(define (X ln h) (/ (- h (lyne-y0 ln)) (lyne-slope ln)))

;; --- TESTS --- 
#|
(define line1 (points->line 0 0 100 100))
(= (of line1 0) 0)
(= (of line1 100) 100)
(= (of line1 50) 50)

(= (X (make-lyne 1 0) 0) 0)
(= (X (make-lyne 1 0) 100) 100)

(equal? (call-with-values 
         (lambda () (intersections (points->line -10 -10 110 110) 100 100))
         list)
        (list 0 100 0 100))
(equal? (call-with-values 
         (lambda () (intersections (points->line 0 10 100 80) 100 100))
         list)
        (list #f #f 10 80))
|#

;; Symbol Any String -> Void
(define (check-scene tag i rank)
  (define error "image with pinhole at (~s,~s)")
  (if (image? i)
      (check-arg tag (scene? i) "scene" rank (image-pins i))
      (check-arg tag #f         "scene" rank i)))

;; Symbol Any -> Void 
(define (check-scene-result tname i)
  (if (image? i) 
      (check-result tname scene? "scene" i (image-pins i))
      (check-result tname (lambda (x) (image? x)) "scene" i)))

(define (image-pins i)
  (format "image with pinhole at (~s,~s)" (pinhole-x i) (pinhole-y i)))
