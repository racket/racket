#|

The test suite for this code is in
plt/collects/tests/mzscheme/image-test.ss

|#

(module image mzscheme

  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "cache-image-snip.ss" "mrlib")
           (lib "math.ss")
	   (lib "posn.ss" "lang")
           (lib "imageeq.ss" "lang" "private")
           "error.ss")

  (provide image-width
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
           add-line
           text
           
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
      (and (list? l) (andmap color? l)))
  (define (alpha-color-list? l)
    (and (list? l) (andmap alpha-color? l)))

  (define-struct color (red green blue) (make-inspector))
  (define-struct alpha-color (alpha red green blue) (make-inspector))

  ;; ----------------------------------------

  (define (check name p? v desc arg-posn) (check-arg name (p? v) desc arg-posn v))

  (define (check-coordinate name val arg-posn) (check name number? val "number" arg-posn))
  (define (check-size name val arg-posn) (check name posi? val "positive exact integer" arg-posn))
  (define (check-size/0 name val arg-posn) (check name nnosi? val "non-negative exact integer" arg-posn))
  (define (check-image name val arg-posn) (check name image? val "image" arg-posn))
  (define (check-image-color name val arg-posn) (check name image-color? val "image-color" arg-posn))
  (define (check-mode name val arg-posn) (check name mode? val mode-str arg-posn))
  
  (define (posi? i) (and (number? i) (integer? i) (positive? i) (exact? i)))
  (define (nnosi? i) (and (number? i) (integer? i) (exact? i) (or (zero? i) (positive? i))))

  
  (define (check-sizes who w h)
    (unless (and (< 0 w 10000) (< 0 h 10000))
      (error (format "cannot make ~a x ~a image" w h))))
  
  (define (mode? x)
    (or (eq? x 'solid)
        (eq? x 'outline)))
  
  (define mode-str "'solid or 'outline")
  
  (define (mode->brush-symbol m)
    (case m
      [(solid) 'solid]
      [(outline) 'transparent]))
  
  (define (mode->pen-symbol m)
    (case m
      [(solid) 'transparent]
      [(outline) 'solid]))
  
  
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
             (px (+ px dx))
             (py (+ py dy))))))
  
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
             (px (floor px))
             (py (floor py))))))
        
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
    (real-overlay/xy 'overlay/xy a dx dy b))

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
               [new-w (ceiling (- right left))]
               [new-h (ceiling (- bottom top))]
               [a-dx (- left)]
               [a-dy (- top)]
               [b-dx (- delta-x left)]
               [b-dy (- delta-y top)]
               [new-px (- a-px left)]
               [new-py (- a-py top)]
               [combine (lambda (a-f b-f)
                          (lambda (dc dx dy)
                            (a-f dc (+ dx a-dx) (+ dy a-dy))
                            (b-f dc (+ dx b-dx) (+ dy b-dy))))])
          (check-sizes name new-w new-h)
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
  
  (define (shrink raw-img left up right down)
    (check-image 'shrink raw-img "first")
    (check-size/0 'shrink left "second")
    (check-size/0 'shrink up "third")
    (check-size/0 'shrink right "fourth")
    (check-coordinate 'shrink down "fifth")
    (let ([img (coerce-to-cache-image-snip raw-img)])
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
                          (let ([clip (send dc get-clipping-region)])
                            (send dc set-clipping-rect dx dy width height)
                            (dc-proc dc (- dx delta-w) (- dy delta-h))
                            (send dc set-clipping-region clip)))]
               [argb-proc (lambda (argb dx dy) (argb-proc argb (- dx delta-w) (- dy delta-h)))]
               [width width]
               [height height])))))
  
  (define (shrink-tl raw-img x y)
    (check-image 'shrink-tl raw-img "first")
    (check-size 'shrink-tl x "second")
    (check-size 'shrink-tl y "third")
    (put-pinhole (shrink (put-pinhole raw-img 0 0) 0 0 (- x 1) (- y 1)) (/ x 2) (/ y 2)))
  
  (define (shrink-tr raw-img x y)
    (check-image 'shrink-tr raw-img "first")
    (check-size 'shrink-tr x "second")
    (check-size 'shrink-tr y "third")
    (put-pinhole (shrink (put-pinhole raw-img (- (image-width raw-img) 1) 0) (- x 1) 0 0 (- y 1))
                 (/ x 2)
                 (/ y 2)))
  
  (define (shrink-bl raw-img x y)
    (check-image 'shrink-bl raw-img "first")
    (check-size 'shrink-bl x "second")
    (check-size 'shrink-bl y "third")
    (put-pinhole (shrink (put-pinhole raw-img 0 (- (image-height raw-img) 1)) 0 (- y 1) (- x 1) 0) 
                 (/ x 2)
                 (/ y 2)))
  
  (define (shrink-br raw-img x y)
    (check-image 'shrink-br raw-img "first")
    (check-size 'shrink-br x "second")
    (check-size 'shrink-br y "third")
    (put-pinhole (shrink (put-pinhole raw-img (- (image-width raw-img) 1) (- (image-height raw-img) 1))
                         (- x 1)
                         (- y 1)
                         0
                         0)
                 (/ x 2)
                 (/ y 2)))


  ;; ------------------------------------------------------------

  (define (line x y color)
    (check-coordinate 'line x "first")
    (check-coordinate 'line y "second")
    (check-image-color 'line color "third")
    (check-sizes 'line (+ x 1) (+ y 1))
    (let ([draw-proc 
           (make-color-wrapper
            color 'transparent 'solid
            (lambda (dc dx dy)
              (send dc draw-line dx dy (+ dx x) (+ dy y))))]
          [mask-proc
           (make-color-wrapper
            'black 'transparent 'solid
            (lambda (dc dx dy)
              (send dc draw-line dx dy (+ dx x) (+ dy y))))])
      (make-simple-cache-image-snip (+ x 1) (+ y 1) 0 0 draw-proc mask-proc)))
  
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
        (let* ([line-w (+ (abs (- x2 x1)) 1)]
               [line-h (+ (abs (- y2 y1)) 1)])
          (if (y1 . <= . y2)
              (let* ([do-draw
                      (lambda (dc dx dy)
                        (send dc draw-line 
                              dx
                              dy
                              (+ dx (- x2 x1))
                              (+ dy (- y2 y1))))]
                     [draw-proc 
                      (make-color-wrapper color-in 'transparent 'solid do-draw)]
                     [mask-proc
                      (make-color-wrapper 'black 'transparent 'solid do-draw)]
                     [line
                      (make-simple-cache-image-snip line-w line-h px py draw-proc mask-proc)])
                (real-overlay/xy 'add-line i (+ px x1) (+ py y1) line))
              (let* ([do-draw
                      (lambda (dc dx dy)
                        (send dc draw-line 
                              dx
                              (+ dy (- line-h 1))
                              (+ dx (- line-w 1))
                              dy))]
                     [draw-proc 
                      (make-color-wrapper color-in 'transparent 'solid do-draw)]
                     [mask-proc
                      (make-color-wrapper 'black 'transparent 'solid do-draw)]
                     [line
                      (make-simple-cache-image-snip line-w line-h px py draw-proc mask-proc)])
                (real-overlay/xy 'add-line i (+ px x1) (+ py y2) line)))))))

  (define (text str size color-in)
    (check 'text string? str "string" "first")
    (check 'text (lambda (x) (and (integer? x) (<= 1 x 255))) size "integer between 1 and 255" "second")
    (check-image-color 'text color-in "third")
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
               [py 0])))))
                    
  (define (get-text-size size string)
    (let* ([bm (make-object bitmap% 1 1)]
           [dc (make-object bitmap-dc% bm)])
      (let-values ([(w h _1 _2) (send dc get-text-extent string (get-font size))])
        (values (inexact->exact (ceiling w)) 
                (inexact->exact (ceiling h))))))
  
  (define (get-font size)
    (send the-font-list find-or-create-font size
          'default 'normal 'normal #f
          (case (system-type)
            [(macosx) 'partly-smoothed]
            [else 'smoothed])))

  (define (a-rect/circ who do-draw w h color brush pen)
    (check-sizes who w h)
    (let* ([dc-proc (make-color-wrapper color brush pen do-draw)]
           [mask-proc (make-color-wrapper 'black brush pen do-draw)])
      (make-simple-cache-image-snip w h (floor (/ w 2)) (floor (/ h 2)) dc-proc mask-proc)))
  
  (define (rectangle w h mode color)
    (check-size 'rectangle w "first")
    (check-size 'rectangle h "second")
    (check-mode 'rectangle mode "third")
    (check-image-color 'rectangle color "fourth")
    (a-rect/circ 'rectangle
                 (lambda (dc dx dy) (send dc draw-rectangle dx dy w h))
                 w h color (mode->brush-symbol mode) (mode->pen-symbol mode)))
  
  (define (ellipse w h mode color)
    (check-size 'ellipse w "first")
    (check-size 'ellipse h "second")
    (check-mode 'ellipse mode "third")
    (check-image-color 'ellipse color "fourth")
    (a-rect/circ 'ellipse
                 (lambda (dc dx dy) (send dc draw-ellipse dx dy w h))
                 w h color (mode->brush-symbol mode) (mode->pen-symbol mode)))
  
  (define (circle r mode color)
    (check-size 'circle r "first")
    (check-mode 'circle mode "second")
    (check-image-color 'circle color "third")
    (a-rect/circ 'circle
                 (lambda (dc dx dy) (send dc draw-ellipse dx dy (* 2 r) (* 2 r)))
                 (* 2 r) (* 2 r) color (mode->brush-symbol mode) (mode->pen-symbol mode)))
  
  (define (triangle size mode color)
    (check 'triangle
           (lambda (x) (and (real? x) (< 2 x 10000)))
           size 
           "positive real number bigger than 2"
           "first")
    (check-mode 'triangle mode "second")
    (check-image-color 'triangle color "third")
    (let* ([right (- size 1)]
           [bottom (inexact->exact (ceiling (* size (sin (* 2/3 pi)))))]
           [points (list (make-object point% 0 bottom)
                         (make-object point% right bottom)
                         (make-object point% (/ size 2) 0))])
      (let ([draw (make-color-wrapper
                   color (mode->brush-symbol mode) 'solid
                   (lambda (dc dx dy)
                     (send dc draw-polygon points dx dy)))]
            [mask-draw (make-color-wrapper
                        'black (mode->brush-symbol mode) 'solid
                        (lambda (dc dx dy)
                          (send dc draw-polygon points dx dy)))]
            [w size]
            [h (+ bottom 1)])
        (make-simple-cache-image-snip w h (floor (/ w 2)) (floor (/ h 2)) draw mask-draw))))
        
  (define (make-simple-cache-image-snip w h px py dc-proc mask-proc)
    (let ([argb-proc 
           (lambda (argb-vector dx dy)
             (let ([c-bm (build-bitmap (lambda (dc) (dc-proc dc 0 0)) w h)]
                   [m-bm (build-bitmap (lambda (dc) (mask-proc dc 0 0)) w h)])
               (overlay-bitmap argb-vector dx dy c-bm m-bm)))])
      (new cache-image-snip%
           [dc-proc dc-proc]
           [argb-proc argb-proc]
           [width w]
           [height h]
           [px px]
           [py py])))
  
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
           [i (send cis get-bitmap)]
           [iw (send i get-width)]
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
        (vector->list cols))))
  
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

  (define (color-list->image cl w h px py)
    (check 'color-list->image color-list? cl "list-of-colors" "first")
    (check-size 'color-list->image w "second")
    (check-size 'color-list->image h "third")
    (check-coordinate 'color-list->image px "fourth")
    (check-coordinate 'color-list->image py "fifth")
    (unless (and (< 0 w 10000) (< 0 h 10000))
      (error (format "cannot make ~a x ~a image" w h)))
    (unless (= (* w h) (length cl))
      (error (format "given width times given height is ~a, but the given color list has ~a items"
		     (* w h) (length cl))))
    (let* ([bm (make-object bitmap% w h)]
           [mask-bm (make-object bitmap% w h)]
	   [dc (make-object bitmap-dc% bm)]
           [mask-dc (make-object bitmap-dc% mask-bm)])
      (unless (send bm ok?)
	(error (format "cannot make ~a x ~a image" w h)))
      (let ([is (make-bytes (* 4 w h) 0)]
            [mask-is (make-bytes (* 4 w h) 0)]
	    [cols (list->vector cl)])
	(let yloop ([y 0][pos 0])
	  (unless (= y h)
	    (let xloop ([x 0][pos pos])
	      (if (= x w)
		  (yloop (add1 y) pos)
		  (let* ([col (vector-ref cols (+ x (* y w)))]
                         [r (pk color-red col)]
                         [g (pk color-green col)]
                         [b (pk color-blue col)])
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
      (bitmaps->cache-image-snip bm mask-bm px py)))
  
  (define (pk sel col) (min 255 (max 0 (sel col))))
  
  (define (alpha-color-list->image cl w h px py)
    (check 'alpha-color-list->image alpha-color-list? cl "list-of-alpha-colors" "first")
    (check-size 'alpha-color-list->image w "second")
    (check-size 'alpha-color-list->image h "third")
    (check-coordinate 'alpha-color-list->image px "fourth")
    (check-coordinate 'alpha-color-list->image py "fifth")
    (unless (and (< 0 w 10000) (< 0 h 10000))
      (error (format "cannot make ~a x ~a image" w h)))
    (unless (= (* w h) (length cl))
      (error (format "given width times given height is ~a, but the given color list has ~a items"
		     (* w h) (length cl))))
    (let ([index-list (alpha-colors->ent-list cl)])
      (argb->cache-image-snip (make-argb (list->vector index-list) w) px py)))
  
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
                  (loop (cdr cl))))]))))
