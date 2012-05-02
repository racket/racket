#lang scheme/base

#|

The test suite for this code is in
collects/tests/deinprogramm/image.rkt

|#

(require (except-in mred
                    make-color)
	   mzlib/class
           mrlib/cache-image-snip
           mzlib/math
	   lang/prim
	   lang/posn
           lang/private/imageeq
           htdp/error
	   deinprogramm/signature/signature-syntax
	   (only-in deinprogramm/DMdA integer natural))

(provide ; #### -primitives doesn't work for us
         image?
	 image-width
	 image-height

	 empty-image

	 overlay
	 above
	 beside
	 
	 clip
	 pad

	 rectangle
	 circle
	 ellipse
	 triangle
	 line
	 text
	 
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
	 alpha-color?

	 octet rgb-color alpha-rgb-color mode image image-color
	 h-place v-place h-mode v-mode)

;; ----------------------------------------

(define (color-list? l)
  (and (list? l) (andmap image-color? l)))
(define (alpha-color-list? l)
  (and (list? l) (andmap alpha-color? l)))

(define-struct color (red green blue) #:inspector (make-inspector))
(define-struct alpha-color (alpha red green blue) #:inspector (make-inspector))

;; ----------------------------------------

(define (check name p? v desc arg-posn) (check-arg name (p? v) desc arg-posn v))

(define (check-coordinate name val arg-posn) (check name finite-real? val "real" arg-posn))
(define (check-integer-coordinate name val arg-posn) (check name nii? val "integer" arg-posn))
(define (check-size name val arg-posn) (check name pos-real? val "positive real" arg-posn))
(define (check-posi-size name val arg-posn) (check name pos-integer? val "positive integer" arg-posn))
(define (check-size/0 name val arg-posn) (check name nn-real? val "non-negative real" arg-posn))
(define (check-h-place name val arg-posn)
  (check name h-place? val
	 "non-negative exact integer or horizontal alignment position"
	 arg-posn))
(define (check-v-place name val arg-posn)
  (check name v-place? val
	 "non-negative exact integer or vertical alignment position"
	 arg-posn))
(define (check-image name val arg-posn) (check name image? val "image" arg-posn))
(define (check-image-color name val arg-posn) 
  (let ([simple-check (lambda (x) (or (string? x) (symbol? x) (color? x)))])
    (check name simple-check val "image-color" arg-posn)
    (unless (image-color? val)
      (error name "~e is not a valid color name" val))))
(define (check-mode name val arg-posn) (check name mode? val mode-str arg-posn))

(define (pos-real? i) (and (real? i) (positive? i)))
(define (pos-integer? i) (and (integer? i) (positive? i)))
(define (nn-real? i) (and (real? i) (or (zero? i) (positive? i))))
(define (nii? x) (and (integer? x) (not (= x +inf.0)) (not (= x -inf.0))))

(define (finite-real? x) (and (real? x) (not (= x +inf.0)) (not (= x -inf.0))))

(define (check-sizes who w h)
  (unless (and (< 0 w 10000) (< 0 h 10000))
    (error (format "cannot make ~a x ~a image" w h))))

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

(define (h-place? x)
  (or (nn-real? x)
      (h-mode? x)))

(define (v-place? x)
  (or (nn-real? x)
      (v-mode? x)))

(define (h-mode? x)
  (member x '(left "left" right "right" "center")))

(define (v-mode? x)
  (member x '(top "top" bottom "bottom" center "center")))

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

(define (overlay a b h-place v-place)
  (overlay-helper 'overlay a b h-place v-place))

(define (overlay-helper name a b h-place v-place)
  (check-image name a "first")
  (check-image name b "second")
  (check-h-place name h-place "third")
  (check-v-place name v-place "fourth")
  (let ((dx (h-place->delta-x h-place a b))
	(dy (v-place->delta-y v-place a b)))
    (real-overlay name
		  a
		  (inexact->exact (floor dx))
		  (inexact->exact (floor dy))
		  b)))

(define (h-place->delta-x h-place a b)
  (cond
   ((real? h-place) (inexact->exact (floor h-place)))
   ((member h-place '(left "left")) 0)
   ((member h-place '(right "right"))
    (- (image-width a) (image-width b)))
   ((member h-place '(center "center"))
    (- (quotient (image-width a) 2)
       (quotient (image-width b) 2)))))

(define (v-place->delta-y v-place a b)
  (cond
   ((real? v-place) (inexact->exact (floor v-place)))
   ((member v-place '(top "top")) 0)
   ((member v-place '(bottom "bottom"))
    (- (image-height a) (image-height b)))
   ((member v-place '(center "center"))
    (- (quotient (image-height a) 2)
       (quotient (image-height b) 2)))))

(define (above a b h-mode)
  (overlay-helper 'above a b h-mode (image-height a)))

(define (beside a b v-mode)
  (overlay-helper 'beside a b (image-width a) v-mode))

(define (real-overlay name raw-a delta-x delta-y raw-b)
  (let ([a (coerce-to-cache-image-snip raw-a)]
	[b (coerce-to-cache-image-snip raw-b)])
    (let-values ([(a-w a-h) (snip-size a)]
		 [(b-w b-h) (snip-size b)])
      (let* ([left (min 0 delta-x)]
	     [top (min 0 delta-y)]
	     [right (max (+ delta-x b-w) a-w)]
	     [bottom (max (+ delta-y b-h) a-h)]
	     [new-w (inexact->exact (ceiling (- right left)))]
	     [new-h (inexact->exact (ceiling (- bottom top)))]
	     [a-dx (inexact->exact (round (- left)))]
	     [a-dy (inexact->exact (round (- top)))]
	     [b-dx (inexact->exact (round (- delta-x left)))]
	     [b-dy (inexact->exact (round (- delta-y top)))]
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
	     ;; match what image=? expects, so we don't get false negatives
	     [px (floor (/ new-w 2))]
	     [py (floor (/ new-h 2))])))))

;; ------------------------------------------------------------

(define (clip raw-img delta-w delta-h width height)
  (check-image 'clip raw-img "first")
  (check-size/0 'clip delta-w "second")
  (check-size/0 'clip delta-h "third")
  (check-size/0 'clip width "fourth")
  (check-size/0 'clip height "fifth")
  (let ((delta-w (inexact->exact (floor delta-w)))
	(delta-h (inexact->exact (floor delta-h)))
	(width (inexact->exact (floor width)))
	(height (inexact->exact (floor height))))
    (let ([img (coerce-to-cache-image-snip raw-img)])
      (let-values ([(i-width i-height) (send img get-size)])
	(let* ([dc-proc (send img get-dc-proc)]
	       [argb-proc (send img get-argb-proc)])
	  (new cache-image-snip%
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
	       [height height]
	       ;; match what image=? expects, so we don't get false negatives
	       [px (floor (/ width 2))] [py (floor (/ height 2))]))))))

(define (pad raw-img left right top bottom)
  (check-image 'pad raw-img "first")
  (check-size/0 'pad left "second")
  (check-size/0 'pad right "third")
  (check-size/0 'pad top "fourth")
  (check-size/0 'pad bottom "fifth")
  (let ((left (inexact->exact (floor left)))
	(right (inexact->exact (floor right)))
	(top (inexact->exact (floor top)))
	(bottom (inexact->exact (floor bottom))))
    (let ([img (coerce-to-cache-image-snip raw-img)])
      (let-values ([(i-width i-height) (send img get-size)])
	(let ((width (+ left i-width right))
	      (height (+ top i-height bottom)))
	  (let* ([dc-proc (send img get-dc-proc)]
		 [argb-proc (send img get-argb-proc)])
	    (new cache-image-snip%
		 [dc-proc (lambda (dc dx dy)
			    (let ([clip (send dc get-clipping-region)]
				  [rgn (make-object region% dc)])
			      (send rgn set-rectangle dx dy width height)
			      (when clip
				(send rgn intersect clip))
			      (send dc set-clipping-region rgn)
			      (dc-proc dc (+ dx left) (+ dy top))
			      (send dc set-clipping-region clip)))]
		 [argb-proc (lambda (argb dx dy) (argb-proc argb (+ dx left) (+ dy top)))]
		 [width width]
		 [height height]
		 ;; match what image=? expects, so we don't get false negatives
		 [px (floor (/ width 2))] [py (floor (/ height 2))])))))))


;; ------------------------------------------------------------

;; test what happens when the line moves out of the box.
(define (line width height pre-x1 pre-y1 pre-x2 pre-y2 color-in)
  (check-size/0 'line width "first")
  (check-size/0 'line height "second")
  (check-coordinate 'line pre-x1 "third")
  (check-coordinate 'line pre-y1 "fourth")
  (check-coordinate 'line pre-x2 "fifth")
  (check-coordinate 'line pre-y2 "sixth")
  (check-image-color 'line color-in "seventh")
  (let ((width (inexact->exact (floor width)))
	(height (inexact->exact (floor height))))
    (let-values ([(x1 y1 x2 y2)
		  (if (<= pre-x1 pre-x2)
		      (values pre-x1 pre-y1 pre-x2 pre-y2)
		      (values pre-x2 pre-y2 pre-x1 pre-y1))])
      (define do-draw
	(lambda (dc dx dy)
	  (let ([clip (send dc get-clipping-region)]
		[rgn (make-object region% dc)])
	    (send rgn set-rectangle dx dy width height)
	    (when clip
	      (send rgn intersect clip))
	    (send dc set-clipping-region rgn)
	    (send dc draw-line 
		  (+ x1 dx) (+ y1 dy) (+ x2 dx) (+ y2 dy))
	    (send dc set-clipping-region clip))))

      (let ([draw-proc 
	     (make-color-wrapper color-in 'transparent 'solid do-draw)]
	    [mask-proc
	     (make-color-wrapper 'black 'transparent 'solid do-draw)])
	(make-simple-cache-image-snip width height draw-proc mask-proc)))))

(define (text str size color-in)
  (check 'text string? str "string" "first")
  (check 'text (lambda (x) (and (integer? x) (<= 1 x 255))) size "integer between 1 and 255" "second")
  (check-image-color 'text color-in "third")
  (cond
    [(string=? str "")
     (let-values ([(tw th) (get-text-size size "dummyX")])
       (rectangle 0 th 'solid 'black))]
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
		;; match what image=? expects, so we don't get false negatives
		[px (floor (/ tw 2))] [py (floor (/ th 2))]))))]))

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
    (make-simple-cache-image-snip w h dc-proc mask-proc)))

(define (rectangle w h mode color)
  (check-size/0 'rectangle w "first")
  (check-size/0 'rectangle h "second")
  (check-mode 'rectangle mode "third")
  (check-image-color 'rectangle color "fourth")
  (let ((w (inexact->exact (floor w)))
	(h (inexact->exact (floor h))))
    (a-rect/circ (lambda (dc dx dy) (send dc draw-rectangle dx dy w h))
		 w h color (mode->brush-symbol mode) (mode->pen-symbol mode))))

(define (ellipse w h mode color)
  (check-size/0 'ellipse w "first")
  (check-size/0 'ellipse h "second")
  (check-mode 'ellipse mode "third")
  (check-image-color 'ellipse color "fourth")
  (let ((w (inexact->exact (floor w)))
	(h (inexact->exact (floor h))))
    (a-rect/circ (lambda (dc dx dy) (send dc draw-ellipse dx dy w h))
		 w h color (mode->brush-symbol mode) (mode->pen-symbol mode))))

(define (circle r mode color)
  (check-size/0 'circle r "first")
  (check-mode 'circle mode "second")
  (check-image-color 'circle color "third")
  (let ((r (inexact->exact (floor r))))
    (a-rect/circ (lambda (dc dx dy) (send dc draw-ellipse dx dy (* 2 r) (* 2 r)))
		 (* 2 r) (* 2 r) color (mode->brush-symbol mode) (mode->pen-symbol mode))))

(define (triangle size mode color)
  (check 'triangle
	 (lambda (x) (and (real? x) (< 2 x 10000)))
	 size 
	 "positive real number bigger than 2"
	 "first")
  (check-mode 'triangle mode "second")
  (check-image-color 'triangle color "third")
  (let* ([size (inexact->exact (floor size))]
	 [right (- size 1)]
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
      (make-simple-cache-image-snip w h draw mask-draw))))

(define (make-simple-cache-image-snip w h dc-proc mask-proc)
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
	   ;; match what image=? expects, so we don't get false negatives
	   [px (floor (/ w 2))] [py (floor (/ h 2))]))))

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
	       [(aw ah) (snip-size a)])
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
			      (make-posn dx dy)
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

(define (color-list->image cl in-w in-h)
  (check 'color-list->image color-list? cl "list-of-colors" "first")
  (check-size/0 'color-list->image in-w "second")
  (check-size/0 'color-list->image in-h "third")
  (let ([w (inexact->exact in-w)]
        [h (inexact->exact in-h)])
    (let ([px (floor (/ w 2))] [py (floor (/ h 2))])
    
      (unless (= (* w h) (length cl))
	(error 'color-list->image
	       "given width times given height is ~a, but the given color list has ~a items"
	       (* w h) 
	       (length cl)))
    
      (cond
       [(or (equal? w 0) (equal? h 0))
	(rectangle w h 'solid 'black)]
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
		[cols (list->vector (map (lambda (x) 
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
	  (bitmaps->cache-image-snip bm mask-bm px py))]))))

(define (pk col) (min 255 (max 0 col)))

(define (alpha-color-list->image cl in-w in-h)
  (check 'alpha-color-list->image alpha-color-list? cl "list-of-alpha-colors" "first")
  (check-size/0 'alpha-color-list->image in-w "second")
  (check-size/0 'alpha-color-list->image in-h "third")
  (let ([w (inexact->exact in-w)]
        [h (inexact->exact in-h)])
    (let ([px (floor (/ w 2))] [py (floor (/ h 2))])
      (unless (= (* w h) (length cl))
	(error 'alpha-color-list->image
	       "given width times given height is ~a, but the given color list has ~a items"
	       (* w h) (length cl)))
      (cond
       [(or (equal? w 0) (equal? h 0))
	(rectangle w h 'solid 'black)]
       [else
	(unless (and (< 0 w 10000) (< 0 h 10000))
	  (error 'alpha-color-list->image format "cannot make ~a x ~a image" w h))
	(let ([index-list (alpha-colors->ent-list cl)])
	  (argb->cache-image-snip (make-argb (list->vector index-list) w h) px py))]))))

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

(define empty-image
  (make-simple-cache-image-snip 0 0 void void))

(define octet (signature (combined natural (predicate (lambda (n) (<= n 255))))))
(define rgb-color (signature (predicate color?)))
(define alpha-rgb-color (signature (predicate alpha-color?)))
(define mode (signature (one-of "solid" "outline")))
(define image (signature (predicate image?)))
(define image-color (signature (predicate image-color?)))
(define h-place (signature (mixed integer (one-of "left" "right" "center"))))
(define v-place (signature (mixed integer (one-of "top" "bottom" "center"))))
(define h-mode (signature (one-of "left" "right" "center")))
(define v-mode (signature (one-of "top" "bottom" "center")))
