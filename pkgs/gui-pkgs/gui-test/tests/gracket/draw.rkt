#lang scheme/gui
(require "unsafe-draw.rkt")

(define manual-chinese? #f)

(when manual-chinese?
  (send the-font-name-directory set-post-script-name 
	(send the-font-name-directory find-or-create-font-id "MOESung-Regular" 'default)
	'normal
	'normal
	"MOESung-Regular"))

(define sys-path 
  (lambda (f)
    (collection-file-path f "icons")))

(define (get-icon)
  (make-object bitmap% (sys-path "mred.xbm") 'xbm))

(define get-plt
  (let ([i #f])
    (lambda ()
      (unless i
	(set! i (make-object bitmap% (sys-path "plt.gif"))))
      i)))

(define get-rotated
  (let ([i #f])
    (lambda ()
      (unless i
	(set! i (let* ([icon (get-icon)]
                       [w (send icon get-width)]
                       [h (send icon get-height)])
                  (let ([bm (make-object bitmap% w h #t)])
                    (let ([src (make-object bitmap-dc% icon)]
                          [dest (make-object bitmap-dc% bm)]
                          [c (make-object color%)])
                      (let loop ([i 0])
                        (unless (= i w)
                          (let loop ([j 0])
                            (unless (= j h)
                              (send src get-pixel i j c)
                              (send dest set-pixel i (- h j 1) c)
                              (loop (add1 j))))
                          (loop (add1 i))))
                      (send src set-bitmap #f)
                      (send dest set-bitmap #f)
                      bm)))))
      i)))

(define (show-instructions file)
  (letrec ([f (make-object frame% (path->string file) #f 400 400)]
	   [print (make-object button% "Print" f
			       (lambda (b ev)
				 (send e print)))]
	   [c (make-object editor-canvas% f)]
	   [e (make-object text%)])
    (send e load-file file)
    (send e lock #t)
    (send c set-editor e)
    (send f show #t)))

(define star
  ;; uses pairs instead of point%s
  (list (cons 30 0)
	(cons 48 60)
	(cons 0 20)
	(cons 60 20)
	(cons 12 60)))

(define octagon
  (list (make-object point% 60 60)
	(make-object point% 120 60)
	(make-object point% 180 120)
	(make-object point% 180 180)
	(make-object point% 120 240)
	(make-object point% 60 240)
	(make-object point% 0 180)
	(make-object point% 0 120)
	(make-object point% 60 60)))

(define (get-b&w-light-stipple)
  (make-object bitmap%
	       (list->bytes '(#x88 0 #x22 0 #x88 0 #x22 0))
	       8 8))

(define (get-b&w-half-stipple)
  (make-object bitmap%
	       (list->bytes '(#xcc #x33 #xcc #x33 #xcc #x33 #xcc #x33))
	       8 8))


(define lambda-path
  (let ()
    (define left-lambda-path
      (let ([p (new dc-path%)])
	(send p move-to 148 670)

	;; top corner
	(send p line-to 156.5 654)
	
	;; left edge spline
	(send p curve-to 197.5 665 225 672 240 653)
	(send p curve-to 275.06 608.59 282.5 573 291.5 528)
	(send p curve-to 296.12 504.92 294.11 490.62 288.96 470)
	(send p curve-to 276.34 419.46 254.18 382.39 228.5 339)
	(send p curve-to 193.21 279.37 159.68 208.41 120.5 150)
	
	(send p line-to 130 142)

	p))

    (define bottom-lambda-path
      (let ([p (new dc-path%)])
	(send p move-to 130 142)

	;; bottom left foot
	(send p line-to 183.5 150)
	
	;; bottom middle spline
	(send p curve-to 203.5 197 225.91 248.79 246 294)
	(send p curve-to 262 330 273.5 366 291.5 402)
	(send p curve-to 296.01 411.02 313 456 324 440)
	(send p curve-to 333.89 425.61 346 400 353 382)
	(send p curve-to 372.28 332.42 390.57 284.39 409 237)
	(send p curve-to 423 201 431.5 174 444.5 141)
	
	;; bottom right foot
	(send p line-to 460 134)
	(send p line-to 524 169)

	p))

    (define right-lambda-path
      (let ([p (new dc-path%)])
	(send p move-to 148 670)

	;; right edge spline
	(send p curve-to 187.21 683.31 228.21 699.77 270 694)
	(send p curve-to 323.6 686.6 345.23 610.92 359 563)
	(send p curve-to 373.75 511.68 395.5 470 413 420)
	(send p curve-to 441.56 338.4 489.5 258 525.5 177)

	(send p line-to 524 169)

	(send p reverse)

	p))

    (let ([p (new dc-path%)])
      (send p append left-lambda-path)
      (send p append bottom-lambda-path)
      (send p append right-lambda-path)
      
      (send p translate -5 -86)
      (send p scale 1 -1)
      (send p translate 0 630)
      (send p scale 0.5 0.5)
      p)))


(define fancy-path
  (let ([p (new dc-path%)]
	[p2 (new dc-path%)])
    (send p2 move-to 10 80)
    (send p2 line-to 80 80)
    (send p2 line-to 80 10)
    (send p2 line-to 10 10)
    (send p2 close)
    
    (send p move-to 1 1)
    (send p line-to 90 1)
    (send p line-to 90 90)
    (send p line-to 1 90)
    (send p close)
    (send p append p2)
    (send p arc 50 50 100 120 0 (* pi 1/2) #f)

    p))

(define square-bm
  (let* ([bm (make-object bitmap% 10 10)]
	 [dc (make-object bitmap-dc% bm)])
    (send dc clear)
    (send dc set-brush "white" 'transparent)
    (send dc set-pen "black" 1 'solid)
    (send dc draw-rectangle 0 0 10 10)
    (send dc set-bitmap #f)
    bm))

(define (show-error . args)
  (with-handlers ([exn? (lambda (exn)
                          (printf "~a\n" (exn-message exn)))])
    (apply error args)))

(define DRAW-WIDTH 550)
(define DRAW-HEIGHT 375)

(let* ([f (make-object frame% "Graphics Test" #f 600 550)]
       [vp (make-object vertical-panel% f)]
       [hp0 (make-object horizontal-panel% vp)]
       [hp (make-object horizontal-panel% vp)]
       [hp2.75 (new horizontal-panel% [parent vp]
                    [stretchable-height #f])]
       [hp3 (make-object horizontal-panel% vp)]
       [hp2 hp]
       [hp2.5 hp0]
       [hp4 (new horizontal-panel% [parent vp]
                 [stretchable-height #f])]
       [bb (make-object bitmap% (sys-path "bb.gif") 'gif)]
       [return (let* ([bm (make-object bitmap% (sys-path "return.xbm") 'xbm)]
		      [dc (make-object bitmap-dc% bm)])
		 (send dc draw-line 0 3 20 3)
		 (send dc set-bitmap #f)
		 bm)]
       [clock-start #f]
       [clock-end #f]
       [clock-clip? #f]
       [do-clock #f]
       [use-bitmap? #f]
       [platform-bitmap? #f]
       [screen-bitmap? #f]
       [compat-bitmap? #f]
       [scaled-bitmap? #f]
       [use-record? #f]
       [serialize-record? #f]
       [use-bad? #f]
       [depth-one? #f]
       [cyan? #f]
       [multi-page? #f]
       [smoothing 'unsmoothed]
       [align-scale 1.0]
       [save-filename #f]
       [save-file-format #f]
       [clip 'none]
       [current-alpha 1.0]
       [current-c-alpha 1.0]
       [current-rotation 0.0]
       [current-skew? #f])
  (send hp0 stretchable-height #f)
  (send hp stretchable-height #f)
  (send hp2.5 stretchable-height #f)
  (send hp3 stretchable-height #f)
  (make-object button% "What Should I See?" hp0
	       (lambda (b e)
		 (show-instructions (collection-file-path "draw-info.txt" "tests/gracket"))))
  (let ([canvas
	 (make-object
	  (class canvas%
	    (init parent)
	    (inherit get-dc refresh init-auto-scrollbars)
	    (define no-bitmaps? #f)
	    (define no-stipples? #f)
	    (define pixel-copy? #f)
	    (define kern? #f)
	    (define clip-pre-scale? #f)
            (define c-clip #f)
	    (define mask-ex-mode 'mred)
	    (define xscale 1)
	    (define yscale 1)
	    (define offset 0)
	    (define c-xscale 1)
	    (define c-yscale 1)
	    (define c-offset 0)
	    (define c-rotate 0)
            (define c-gray? #f)
	    (public*
	     [set-bitmaps (lambda (on?) (set! no-bitmaps? (not on?)) (refresh))]
	     [set-stipples (lambda (on?) (set! no-stipples? (not on?)) (refresh))]
	     [set-pixel-copy (lambda (on?) (set! pixel-copy? on?) (refresh))]
	     [set-kern (lambda (on?) (set! kern? on?) (refresh))]
	     [set-clip-pre-scale (lambda (on?) (set! clip-pre-scale? on?) (refresh))]
	     [set-canvas-clip (lambda (mode) (set! c-clip mode) (refresh))]
	     [set-mask-ex-mode (lambda (mode) (set! mask-ex-mode mode) (refresh))]
	     [set-canvas-scale (lambda (xs ys) (set! c-xscale xs) (set! c-yscale ys) (refresh))]
	     [set-scale (lambda (xs ys) (set! xscale xs) (set! yscale ys) (refresh))]
	     [set-offset (lambda (o) (set! offset o) (refresh))]
	     [set-canvas-offset (lambda (o) (set! c-offset o) (refresh))]
	     [set-canvas-rotation (lambda (r) (set! c-rotate r) (refresh))]
	     [set-canvas-gray (lambda (g?) (set! c-gray? g?) (refresh))])
	    (override*
	     [on-paint
	      (case-lambda
	       [() (time (on-paint #f))]
	       [(kind)
		(let* ([can-dc (get-dc)]
		       [pen0s (make-object pen% "BLACK" 0 'solid)]
		       [pen1s (make-object pen% "BLACK" 1 'solid)]
		       [pen2s (make-object pen% "BLACK" 2 'solid)]
		       [pen0t (make-object pen% "BLACK" 0 'transparent)]
		       [pen1t (make-object pen% "BLACK" 1 'transparent)]
		       [pen2t (make-object pen% "BLACK" 2 'transparent)]
		       [pen0x (make-object pen% "BLACK" 0 'xor)]
		       [pen1x (make-object pen% "BLACK" 1 'xor)]
		       [pen2x (make-object pen% "BLACK" 2 'xor)]
		       [brushs (make-object brush% "BLACK" 'solid)]
		       [brusht (make-object brush% "BLACK" 'transparent)]
		       [brushb (make-object brush% "BLUE" 'solid)]
		       [mem-dc (if use-bitmap?
				   (make-object bitmap-dc%)
				   #f)]
		       [bm (if use-bitmap?
			       (if use-bad?
				   (make-object bitmap% "no such file")
                                   (let ([w (ceiling (* xscale DRAW-WIDTH))]
                                         [h (ceiling (* yscale DRAW-HEIGHT))])
                                     (cond
                                      [platform-bitmap?
                                       (make-platform-bitmap w h)]
                                      [screen-bitmap?
                                       (make-screen-bitmap w h)]
                                      [compat-bitmap?
                                       (send this make-bitmap w h)]
                                      [scaled-bitmap?
                                       (make-bitmap w h #:backing-scale 3.0)]
                                      [else
                                       (make-object bitmap% w h depth-one? c-gray?)])))
			       #f)]
		       [draw-series
			(lambda (dc pens pent penx size x y flevel last?)
			  (let* ([ofont (send dc get-font)]
				 [otfg (send dc get-text-foreground)]
				 [otbg (send dc get-text-background)]
				 [obm (send dc get-text-mode)])
			    (when (positive? flevel)
                              (send dc set-font
                                    (make-object font%
                                                 10 'decorative
                                                 'normal 
                                                 (if (> flevel 1)
                                                     'bold
                                                     'normal)
                                                 #t)))
			    (send dc set-pen pens)
			    (send dc set-brush brusht)
			    
			    ; Text should overlay this line (except for 2x2)
			    (send dc draw-line 
				  (+ x 3) (+ y 12)
				  (+ x 40) (+ y 12))

			    (send dc set-text-background (make-object color% "YELLOW"))
			    (when (= flevel 2)
			      (send dc set-text-foreground (make-object color% "RED"))
			      (send dc set-text-mode 'solid))

			    (send dc draw-text (string-append size " P\uE9n") ; \uE9 is e with '
				  (+ x 5) (+ y 8))
			    (send dc set-font ofont)
			    
			    (when (= flevel 2)
			      (send dc set-text-foreground otfg)
			      (send dc set-text-mode obm))
			    (send dc set-text-background otbg)
			    
			    (send dc draw-line
				  (+ x 5) (+ y 27) (+ x 10) (+ 27 y))
			    (send dc draw-rectangle
				  (+ x 5) (+ y 30) 5 5)
			    (send dc draw-line
				  (+ x 12) (+ y 30) (+ x 12) (+ y 35))
			    
			    (send dc draw-line
				  (+ x 5) (+ y 40) (+ x 10) (+ 40 y))
			    (send dc draw-rectangle
				  (+ x 5) (+ y 41) 5 5)
			    (send dc draw-line
				  (+ x 10) (+ y 41) (+ x 10) (+ 46 y))
			    
			    (send dc draw-line
				  (+ x 15) (+ y 25) (+ x 20) (+ 25 y))
			    (send dc draw-line
				  (+ x 20) (+ y 30) (+ x 20) (+ 25 y))
			    
			    (send dc draw-line
				  (+ x 30) (+ y 25) (+ x 25) (+ 25 y))
			    (send dc draw-line
				  (+ x 25) (+ y 30) (+ x 25) (+ 25 y))
			    
			    (send dc draw-line
				  (+ x 35) (+ y 30) (+ x 40) (+ 30 y))
			    (send dc draw-line
				  (+ x 40) (+ y 25) (+ x 40) (+ 30 y))
			    
			    (send dc draw-line
				  (+ x 50) (+ y 30) (+ x 45) (+ 30 y))
			    (send dc draw-line
				  (+ x 45) (+ y 25) (+ x 45) (+ 30 y))

			    ; Check line thickness with "X"
			    (send dc draw-line
				  (+ x 20) (+ y 45) (+ x 40) (+ 39 y))
			    (send dc draw-line
				  (+ x 20) (+ y 39) (+ x 40) (+ 45 y))
			    
			    (send dc draw-rectangle
				  (+ x 5) (+ y 50) 10 10)
			    (send dc draw-rounded-rectangle
				  (+ x 5) (+ y 65) 10 10 3)
			    (send dc draw-ellipse
				  (+ x 5) (+ y 80) 10 10)
			    
			    (send dc set-brush brushs)
			    (send dc draw-rectangle
				  (+ x 17) (+ y 50) 10 10)
			    (send dc draw-rounded-rectangle
				  (+ x 17) (+ y 65) 10 10 3)
			    (send dc draw-ellipse
				  (+ x 17) (+ y 80) 10 10)
			    
			    (send dc set-pen pent)
			    (send dc draw-rectangle
				  (+ x 29) (+ y 50) 10 10)
			    (send dc draw-rounded-rectangle
				  (+ x 29) (+ y 65) 10 10 3)
			    (send dc draw-ellipse
				  (+ x 29) (+ y 80) 10 10)
			    
			    (send dc set-pen penx)
			    (send dc draw-rectangle
				  (+ x 41) (+ y 50) 10 10)
			    (send dc draw-rounded-rectangle
				  (+ x 41) (+ y 65) 10 10 3)
			    (send dc draw-ellipse
				  (+ x 41) (+ y 80) 10 10)
			    
			    (send dc set-pen pens)
			    (send dc draw-rectangle
				  (+ x 17) (+ y 95) 10 10)
			    ; (send dc set-logical-function 'clear)
			    (send dc draw-rectangle
				  (+ x 18) (+ y 96) 8 8)
			    ; (send dc set-logical-function 'copy)
			    
			    (send dc draw-rectangle
				  (+ x 29) (+ y 95) 10 10)
			    ; (send dc set-logical-function 'clear)
			    (send dc set-pen pent)
			    (send dc draw-rectangle
				  (+ x 30) (+ y 96) 8 8)

			    (send dc set-pen pens)
			    (send dc draw-rectangle
				  (+ x 5) (+ y 95) 10 10)
			    ; (send dc set-logical-function 'xor)
			    (send dc draw-rectangle
				  (+ x 5) (+ y 95) 10 10)
			    ; (send dc set-logical-function 'copy)
			    
			    (send dc draw-line
				  (+ x 5) (+ y 110) (+ x 8) (+ y 110))
			    (send dc draw-line
				  (+ x 8) (+ y 110) (+ x 11) (+ y 113))
			    (send dc draw-line
				  (+ x 11) (+ y 113) (+ x 11) (+ y 116))
			    (send dc draw-line
				  (+ x 11) (+ y 116) (+ x 8) (+ y 119))
			    (send dc draw-line
				  (+ x 8) (+ y 119) (+ x 5) (+ y 119))
			    (send dc draw-line
				  (+ x 5) (+ y 119) (+ x 2) (+ y 116))
			    (send dc draw-line
				  (+ x 2) (+ y 116) (+ x 2) (+ y 113))
			    (send dc draw-line
				  (+ x 2) (+ y 113) (+ x 5) (+ y 110))
			    
			    (send dc draw-lines
				  (list
				   (make-object point% 5 95)
				   (make-object point% 8 95)
				   (make-object point% 11 98)
				   (make-object point% 11 101)
				   (make-object point% 8 104)
				   (make-object point% 5 104)
				   (make-object point% 2 101)
				   (make-object point% 2 98)
				   (make-object point% 5 95))
				  (+ x 12) (+ y 15))

			    (send dc draw-point (+ x 35) (+ y 115))
			    (send dc draw-line (+ x 35) (+ y 120) (+ x 35) (+ y 120))
			    
			    (send dc draw-line
				  (+ x 5) (+ y 125) (+ x 10) (+ y 125))
			    (send dc draw-line
				  (+ x 11) (+ y 125) (+ x 16) (+ y 125))

			    (send dc set-brush brusht)
			    (send dc draw-arc 
				  (+ x 5) (+ y 135)
				  30 40
				  0 (/ pi 2))
			    (send dc draw-arc 
				  (+ x 5) (+ y 135)
				  30 40
				  (/ pi 2) pi)
			    (send dc set-brush brushs)
			    (send dc draw-arc 
				  (+ x 45) (+ y 135)
				  30 40
				  (/ pi 2) pi)
			    (send dc set-brush brusht)      

			    
			    (when last?
			      (let ([p (send dc get-pen)])
				(send dc set-pen (make-object pen% "BLACK" 1 'xor))
				(send dc draw-polygon octagon)
				(send dc set-pen p))

			      (when clock-start
				(let ([b (send dc get-brush)])
				  (send dc set-brush (make-object brush% "ORANGE" 'solid))
				  (send dc draw-arc 0. 60. 180. 180. clock-start clock-end)
				  (send dc set-brush b))))

			    (when last?
			      (let ([op (send dc get-pen)])

				; Splines
				(define (draw-ess dx dy)
				  (send dc draw-spline 
					(+ dx 200) (+ dy 10)
					(+ dx 218) (+ dy 12)
					(+ dx 220) (+ dy 20))
				  (send dc draw-spline 
					(+ dx 220) (+ dy 20)
					(+ dx 222) (+ dy 28)
					(+ dx 240) (+ dy 30)))
				(send dc set-pen pen0s)
				(draw-ess 0 0)
				(send dc set-pen (make-object pen% "RED" 0 'solid))
				(draw-ess -2 2)
			      
				; Polygons: odd-even vs. winding
				(let ([polygon
				       (list (make-object point% 12 0)
					     (make-object point% 40 0)
					     (make-object point% 40 28)
					     (make-object point% 0 28)
					     (make-object point% 0 12)
					     (make-object point% 28 12)
					     (make-object point% 28 40)
					     (make-object point% 12 40)
					     (make-object point% 12 0))]
				      [ob (send dc get-brush)]
				      [op (send dc get-pen)])
				  (send dc set-pen pen1s)
				  (send dc set-brush (make-object brush% "BLUE" 'solid))
				  (send dc draw-polygon polygon 200 40 'odd-even)
				  (send dc draw-polygon polygon 200 90 'winding)
				  (send dc set-pen op)
				  (send dc set-brush ob))


				; Brush patterns:
				(let ([pat-list (list 'bdiagonal-hatch
						      'crossdiag-hatch
						      'fdiagonal-hatch
						      'cross-hatch
						      'horizontal-hatch
						      'vertical-hatch)]
				      [b (make-object brush% "BLACK" 'solid)]
				      [ob (send dc get-brush)]
				      [obg (send dc get-background)]
				      [blue (make-object color% "BLUE")])
				  (let loop ([x 245][y 10][l pat-list])
				    (unless (null? l)
				      (send b set-color "BLACK")
				      (send b set-style (car l))
				      (send dc set-brush b)
				      (send dc draw-rectangle x y 20 20)
				      (send dc set-brush ob)
				      (send b set-color "GREEN")
				      (send dc set-brush b)
				      (send dc draw-rectangle (+ x 25) y 20 20)
				      (send dc set-background blue)
				      (send dc draw-rectangle (+ x 50) y 20 20)
				      (send dc set-background obg)
				      (send dc set-brush ob)
				      (loop x (+ y 25) (cdr l))))

				  (send b set-style 'panel)
				  (send b set-color (get-panel-background))
				  (send dc set-brush b)
				  (send dc draw-rectangle 320 10 20 20)
				  (send dc draw-ellipse 320 35 20 20)
				  (send dc draw-arc 320 60 20 20 0 3.14)
				  (send dc draw-rounded-rectangle 320 85 20 20 2)

				  (send dc set-brush ob))
				
				(send dc set-pen op))

			      ; Thick-line centering:
			      (let ([thick (make-object pen% "GREEN" 5 'solid)])
				(define (draw-lines)
				  (send dc draw-line 360 10 400 50)
				  (send dc draw-line 360 50 400 10)
				  (send dc draw-line 360 80 400 80)
				  (send dc draw-line 380 60 380 100)
				  (send dc draw-line 360 120 400 140)
				  (send dc draw-line 370 110 390 150))
				(let ([op (send dc get-pen)])
				  (send dc set-pen thick)
				  (draw-lines)
				  (send dc set-pen pen0s)
				  (draw-lines)
				  (send dc set-pen op)))
				
			      ; B&W 8x8 stipple:
			      (unless no-bitmaps?
				(let ([bml (get-b&w-light-stipple)]
				      [bmh (get-b&w-half-stipple)]
				      [orig-b (send dc get-brush)]
				      [orig-pen (send dc get-pen)])
				  (send dc set-brush brusht)
				  (send dc set-pen pen1s)
				  (send dc draw-rectangle 244 164 18 18)
				  (send dc draw-bitmap bml 245 165)
				  (send dc draw-bitmap bml 245 173)
				  (send dc draw-bitmap bml 253 165)
				  (send dc draw-bitmap bml 253 173)

				  (let ([p (make-object pen% "RED" 1 'solid)])
				    (send p set-stipple bmh)
				    (send dc set-pen p)
				    (send dc draw-rectangle 270 164 18 18))

				  (send dc set-brush orig-b)
				  (send dc set-pen orig-pen))))
			    
			    (unless no-bitmaps?
			      (let ([obg (send dc get-background)]
				    [tan (make-object color% "TAN")])
				(send dc set-background tan)
				(let* ([bits #"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789/+"]
				       [bm (make-object bitmap% bits 64 8)])
				  (send dc draw-bitmap bm 306 164 'opaque))
				(let* ([bits #"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567"]
				       [bm (make-object bitmap% bits 48 10)])
				  (send dc draw-bitmap bm 306 184 'opaque))
				(send dc set-background obg)))

			    (when last?
			      ; Test get-text-extent
			      (let ([save-pen (send dc get-pen)]
				    [save-fnt (send dc get-font)])
				(send dc set-pen (make-object pen% "YELLOW" 1 'solid))
				(let loop ([fam '(default default modern modern decorative roman)]
					   [stl '(normal  slant   slant  italic normal     normal)]
					   [wgt '(normal  bold    normal normal bold       normal)]
					   [sze '(12      12      12     12     12         32)]
					   [x 244]
					   [y 210]
					   [chinese? #t])
				  (unless (null? fam)
				    (let ([fnt (make-object font% (car sze) (car fam) (car stl) (car wgt))]
					  [s "AvgflfiMh"])
				      (send dc set-font fnt)
				      (send dc draw-text s x y kern?)
				      (send dc set-font save-fnt)
				      (let-values ([(w h d a) (send dc get-text-extent s fnt kern?)])
					(send dc draw-rectangle x y w h)
					(send dc draw-line x (+ y (- h d)) (+ x w) (+ y (- h d)))
					(when chinese?
					  (let ([s "\u7238"]
						[x (+ x (* 1.5 w))]
						[cfnt (if (and (dc . is-a? . post-script-dc%)
							       manual-chinese?)
							  (make-object font% 12 "MOESung-Regular" 'default)
							  fnt)])
					    (send dc set-font cfnt)
					    (send dc draw-text s x y kern?)
					    (send dc set-font fnt)
					    (let-values ([(w h d a) (send dc get-text-extent s cfnt kern?)])
					      (send dc draw-rectangle x y w h)
					      (send dc draw-line x (+ y (- h d)) (+ x w) (+ y (- h d)))
                                              ;; Rotated Chinese character:
                                              (send dc draw-text s (+ x h (- d)) (+ y h 2) kern? 0 (* pi -1/2))
					      ;; Mathematical "A" (beyond UCS-2)
					      (let ([s "\U1D670"]
						    [x (+ x (* 1.5 w))])
						(send dc set-font fnt)
						(send dc draw-text s x y kern?)
						(send dc set-font fnt)
						(let-values ([(w h d a) (send dc get-text-extent s cfnt kern?)])
						  (send dc draw-rectangle x y w h)
						  (send dc draw-line x (+ y (- h d)) (+ x w) (+ y (- h d))))))))
					(loop (cdr fam) (cdr stl) (cdr wgt) (cdr sze) x (+ y h) #f)))))
				(send dc set-pen save-pen)))

                            ;; Text paths:
                            (let ([p (make-object dc-path%)]
                                  [old-pen (send dc get-pen)]
                                  [old-brush (send dc get-brush)])
                              (send p text-outline (make-font #:size 32) "A8" 360 190)
                              (send dc set-pen "black" 1 'solid)
                              (send dc set-brush "pink" 'solid)
                              (send dc draw-path p)
                              (send dc set-pen old-pen)
                              (send dc set-brush old-brush))

			    ; Bitmap copying:
			    (when (and (not no-bitmaps?) last?)
			      (let ([x 5] [y 165])
				(let ([bg (send dc get-background)]
                                      [mred-icon (get-icon)])
                                  (send dc set-background "YELLOW")
				  (case mask-ex-mode
				    [(plt plt-mask plt^plt mred^plt)
				     (let* ([plt (get-plt)]
					    [ww (send mred-icon get-width)]
                                            [hh (send mred-icon get-height)]
					    [tmp-bm (make-object bitmap% ww hh #f)]
					    [tmp-dc (make-object bitmap-dc% tmp-bm)]
                                            [mask-bm tmp-bm])
				       (send tmp-dc draw-bitmap plt
					     (/ (- (send mred-icon get-width)
						   (send plt get-width))
						2)
					     (/ (- (send mred-icon get-height)
						   (send plt get-height))
						2))
                                       (when (memq mask-ex-mode '(plt^plt mred^plt))
                                         ;; Convert to grayscale
                                         (let ([s (make-bytes (* 4 ww hh))])
                                           (send tmp-bm get-argb-pixels 0 0 ww hh s)
                                           (for* ([i (in-range 0 ww)]
                                                  [j (in-range 0 hh)])
                                             (let* ([p (* 4 (+ (* j ww) i))]
                                                    [v (quotient (+ (bytes-ref s (+ p 1))
                                                                    (bytes-ref s (+ p 2))
                                                                    (bytes-ref s (+ p 3)))
                                                                 3)])
                                               (bytes-set! s (+ p 1) v)
                                               (bytes-set! s (+ p 2) v)
                                               (bytes-set! s (+ p 3) v)))
                                           (set! mask-bm (make-object bitmap% ww hh #f))
                                           (send tmp-dc set-bitmap mask-bm)
                                           (send tmp-dc set-argb-pixels 0 0 ww hh s)))
				       (if (eq? mask-ex-mode 'mred^plt)
					   (send dc draw-bitmap mred-icon x y
						 'solid
						 (send the-color-database find-color "BLACK")
						 mask-bm)
					   (send dc draw-bitmap tmp-bm x y 'solid 
						 (send the-color-database find-color "BLACK")
						 (cond
						  [(eq? mask-ex-mode 'plt-mask) mred-icon]
						  [(eq? mask-ex-mode 'plt^plt) mask-bm]
						  [else #f]))))]
                                    [(mred^mred)
                                     (send dc draw-bitmap mred-icon x y
                                           'solid
                                           (send the-color-database find-color "BLACK")
                                           mred-icon)]
                                    [(mred~)
                                     (send dc draw-bitmap (get-rotated) x y 'opaque)]
                                    [(mred^mred~ opaque-mred^mred~ red-mred^mred~ opaque-red-mred^mred~)
                                     (send dc draw-bitmap mred-icon x y 
                                           (if (memq mask-ex-mode '(opaque-mred^mred~ opaque-red-mred^mred~))
                                               'opaque
                                               'solid)
                                           (send the-color-database find-color 
                                                 (if (memq mask-ex-mode '(red-mred^mred~ opaque-red-mred^mred~))
                                                     "RED"
                                                     "BLACK"))
                                           (get-rotated))]
                                    [else
                                     ;; simple draw
                                     (send dc draw-bitmap mred-icon x y 'xor)])
                                  (send dc set-background bg))
				(set! x (+ x (send (get-icon) get-width)))
				(let ([black (send the-color-database find-color "BLACK")]
				      [red (send the-color-database find-color "RED")]
				      [do-one
				       (lambda (bm mode color)
					 (if (send bm ok?)
					     (begin
					       (let ([h (send bm get-height)]
						     [w (send bm get-width)])
						 (send dc set-pen (make-object pen% "YELLOW" 1 'solid))
						 (send dc draw-line 3 3 40 40)
						 (send dc draw-bitmap-section
						       bm x y 
						       0 0 w h
						       mode color)
						 (set! x (+ x w 10))))
					     (printf "bad bitmap\n")))])
				  ;; BB icon
				  (do-one bb 'solid black)
				  (let ([start x])
				    ;; First three return icons:
				    (do-one return 'solid black)
				    (do-one return 'solid red)
				    (do-one return 'opaque red)
				    ;; Next three, on a blue background
				    (let ([end x]
					  [b (send dc get-brush)])
				      (send dc set-brush (make-object brush% "BLUE" 'solid))
				      (send dc draw-rounded-rectangle (- start 5) (+ y 15) (- end start) 15 -0.2)
				      (send dc set-brush b)
				      (set! x start)
				      (set! y (+ y 18))
				      (do-one return 'solid black)
				      (do-one return 'solid red)
				      (do-one return 'opaque red)
				      (set! y (- y 18))))
				  ;; Another BB icon, make sure color has no effect
				  (do-one bb 'solid red)
				  ;; Another return, blacnk on red
				  (let ([bg (send dc get-background)])
				    (send dc set-background (send the-color-database find-color "BLACK"))
				    (do-one return 'opaque red)
				    (send dc set-background bg))
				  ;; Return by drawing into color, copying color to monochrome, then
				  ;;  monochrome back oonto canvas:
				  (let* ([w (send return get-width)]
					 [h (send return get-height)]
					 [color (make-object bitmap% w h)]
					 [mono (make-object bitmap% w h #t)]
					 [cdc (make-object bitmap-dc% color)]
					 [mdc (make-object bitmap-dc% mono)])
				    (send cdc clear)
				    (send cdc draw-bitmap return 0 0)
				    (send mdc clear)
				    (send mdc draw-bitmap color 0 0)
				    (send dc draw-bitmap mono
					  (- x w 10) (+ y 18)))
				  (send dc set-pen pens))))

			    (when (and (not no-stipples?) last?)
			      ; Blue box as background:
			      (send dc set-brush brushb)
			      (send dc draw-rectangle 80 200 125 40)
			      (when (send return ok?)
				(let ([b (make-object brush% "GREEN" 'solid)])
				  (send b set-stipple return)
				  (send dc set-brush b)
				  ; First stipple (transparent background)
				  (send dc draw-rectangle 85 205 30 30)
				  (send dc set-brush brushs)
				  (send b set-style 'opaque)
				  (send dc set-brush b)
				  ; Second stipple (opaque)
				  (send dc draw-ellipse 120 205 30 30)
				  (send dc set-brush brushs)
				  (send b set-stipple bb)
				  (send dc set-brush b)
				  ; Third stipple (BB logo)
				  (send dc draw-rectangle 155 205 20 30)
				  (send dc set-brush brushs)
				  (send b set-stipple #f)
				  (send b set-style 'cross-hatch)
				  (send dc set-brush b)
				  ; Green cross hatch (white BG) on blue field
				  (send dc draw-rectangle 180 205 20 20)
				  (send dc set-brush brushs))))
			    
			    (when (and pixel-copy? last? (not (or kind (eq? dc can-dc))))
			      (let* ([x 100]
				     [y 170]
				     [x2 245] [y2 188]
				     [w 40] [h 20]
				     [c (make-object color%)]
				     [bm (make-object bitmap% w h depth-one?)]
				     [mdc (make-object bitmap-dc%)])
				(send mdc set-bitmap bm)
				(let iloop ([i 0])
				  (unless (= i w)
				    (let jloop ([j 0])
				      (if (= j h)
					  (iloop (add1 i))
					  (begin
					    (send dc get-pixel (+ i x) (+ j y) c)
					    (send mdc set-pixel i j c)
					    (jloop (add1 j)))))))
				(send dc draw-bitmap bm x2 y2)
				(let ([p (send dc get-pen)]
				      [b (send dc get-brush)])
				  (send dc set-pen (make-object pen% "BLACK" 0 'xor-dot))
				  (send dc set-brush brusht)
				  (send dc draw-rectangle x y w h)
				  (send dc set-pen p)
				  (send dc set-brush b))))
			    
			    (let ([styles (list 'solid
						'dot
						'long-dash
						'short-dash
						'dot-dash)]
				  [obg (send dc get-background)]
				  [red (make-object color% "RED")])
			      (let loop ([s styles][y 250])
				(unless (null? s)
				  (let ([p (make-object pen% "GREEN" flevel (car s))])
				    (send dc set-pen p)
				    (send dc draw-line (+ x 5) y (+ x 30) y)
				    (send dc set-background red)
				    (send dc draw-line (+ x 5) (+ 4 y) (+ x 30) (+ y 4))
				    (send dc set-background obg)
				    (send pens set-style (car s))
				    (send dc set-pen pens)
				    (send dc draw-line (+ x 30) y (+ x 55) y)
				    (send dc set-background red)
				    (send dc draw-line (+ x 30) (+ y 4) (+ x 55) (+ y 4))
				    (send dc set-background obg)
				    (send dc set-pen pent)
				    (send pens set-style 'solid)
				    (loop (cdr s) (+ y 8))))))

			    (when (= flevel 2)
			      (let ([lens '(0 0.25 0.5 0.75 1.0 1.25 1.5 1.75 2.0)])
				(let loop ([l lens][x 10])
				  (unless (null? l)
				    (let ([p (make-object pen% "BLACK" (car l) 'solid)])
				      (send dc set-pen p)
				      (send dc draw-line x 300 (+ x 19) 300)
				      (send dc set-pen pent)
				      (loop (cdr l) (+ x 20)))))))

			    (when last?
			      (let ()
				(define (pen cap join)
				  (let ([p (make-object pen% "blue" 4 'solid)])
				    (send p set-cap cap)
				    (send p set-join join)
				    (send dc set-pen p)))
				(send dc set-brush (make-object brush% "white" 'transparent))
				(pen 'projecting 'miter)
				(send dc draw-lines star 410 10)
				(send dc draw-polygon star 480 10)
				(pen 'round 'round)
				(send dc draw-lines star 410 80)
				(send dc draw-polygon star 480 80)
				(pen 'butt 'bevel)
				(send dc draw-lines star 410 150)
				(send dc draw-polygon star 480 150))

			      (send dc set-pen (make-object pen% "green" 3 'solid))
			      (send dc set-brush (make-object brush% "yellow" 'solid))
			      (send dc draw-path (let ([p (new dc-path%)])
						   (send p append fancy-path)
						   (send p scale 0.5 0.5)
						   (send p translate 410 230)
						   p))
			      (send dc set-pen (make-object pen% "black" 0 'solid))
			      (send dc set-brush (make-object brush% "red" 'solid))
			      (send dc draw-path (let ([p (new dc-path%)])
						   (send p append lambda-path)
						   (send p scale 0.3 0.3)
						   p)
				    465 230)

			      (send dc draw-path (let ([p (new dc-path%)])
						   (send p rectangle 10 310 20 20)
						   (send p rounded-rectangle 40 310 20 20 5)
						   (send p ellipse 70 310 20 20)
						   (send p move-to 100 310)
						   (send p lines (list (make-object point% 0 0)
								       (make-object point% 0 20)
								       (make-object point% 20 10))
							 100 310)
						   p))

                              (let ([p (send dc get-pen)])
                                (send dc set-pen (make-object color% 0 0 0 0.1) 1 'solid)
                                (send dc set-brush (make-object color% 255 0 200 0.5) 'solid)
                                (send dc draw-rectangle 250 320 20 20)
                                (send dc set-brush (make-object color% 0 255 200 0.5) 'solid)
                                (send dc draw-rectangle 260 330 20 20)
                                (send dc set-pen p))

                              (let ([p (send dc get-pen)])
                                (send dc set-pen "white" 1 'transparent)
                                (send dc set-brush (new brush%
                                                        [gradient
                                                         (make-object linear-gradient%
                                                                      300 0 380 0
                                                                      (list (list 0.0
                                                                                  (make-object color% 255 0 0))
                                                                            (list 0.5
                                                                                  (make-object color% 0 255 0))
                                                                            (list 1.0
                                                                                  (make-object color% 0 0 255 0.0))))]))
                                (send dc draw-rectangle 300 320 80 20)
                                (send dc set-pen p))

                              (let ([p (send dc get-pen)])
                                (send dc set-pen "black" 1 'solid)
                                (send dc set-brush surface-brush)
                                (send dc draw-rectangle 400 320 30 40)
                                (send dc set-pen p))

                              (let ([p (send dc get-pen)])
                                (send dc set-pen "white" 1 'transparent)
                                (send dc set-brush (new brush%
                                                        [gradient
                                                         (make-object radial-gradient%
                                                                      360 250 5
                                                                      365 245 25
                                                                      (list (list 0.0
                                                                                  (make-object color% 255 0 0))
                                                                            (list 0.5
                                                                                  (make-object color% 0 255 0))
                                                                            (list 1.0
                                                                                  (make-object color% 0 0 255 0.0))))]))
                                (send dc draw-rectangle 338 228 44 44)
                                (send dc set-pen p))

			      (send dc draw-line 130 310 150 310)
			      (send dc draw-line 130 312.5 150 312.5)
			      (send dc draw-line 130 314.3 150 314.3)
			      (send dc draw-line 130 316.7 150 316.7)

			      (let-values ([(xs ys) (send dc get-scale)])
				(send dc set-scale (* xs 1.25) (* ys 1.25))
				(let ([x (/ 10 1.25)]
				      [y (/ 340 1.25)])
				  (send dc draw-bitmap square-bm x y)
				  (send dc draw-bitmap square-bm (+ x 10) y)
				  (send dc draw-bitmap square-bm (+ x 20) y)
				  (send dc draw-bitmap square-bm (+ x 30) y))
				(send dc set-scale xs ys)
				(send dc set-pen "black" 0 'solid)
				(send dc draw-line 10 337 59 337))

			      (let ([p (send dc get-pen)])
				(send dc set-pen "blue" 8 'solid)
				(send dc draw-rectangle 160 310 20 20)
				(send dc set-pen "blue" 7 'solid)
				(send dc draw-rectangle 187 310 20 20)
				(send dc set-pen p)))
			      
                            (when (and last? 
                                       (or (and (not (or kind (eq? dc can-dc)))
                                                (send mem-dc get-bitmap))
                                           use-record?))
                              (when c-gray?
                                (let ([b (send can-dc get-brush)]
                                      [p (send can-dc get-pen)])
                                  (send can-dc set-brush "gray" 'solid)
                                  (send can-dc set-pen "black" 1 'transparent)
                                  (send can-dc draw-rectangle 0 0 1024 1024)
                                  (send can-dc set-brush b)
                                  (send can-dc set-pen p)))
                              (send can-dc set-origin c-offset c-offset)
                              (send can-dc set-rotation c-rotate)
                              (send can-dc set-scale c-xscale c-yscale)
                              (send can-dc set-alpha current-c-alpha)
                              (when c-clip
                                (define r (new region%))
                                (case c-clip
                                  [(square) (send r set-rectangle 0 0 200 200)]
                                  [(squares)
                                   (define r2 (new region%))
                                   (send r set-rectangle 0 0 200 200)
                                   (send r2 set-rectangle 210 210 40 40)
                                   (send r union r2)]
                                  [(octagon) (send r set-polygon octagon)])
                                (send can-dc set-clipping-region r))
                              (if use-record?
                                  (if serialize-record?
                                      (let ()
                                        (define-values (i o) (make-pipe))
                                        (write (send dc get-recorded-datum) o)
                                        ((recorded-datum->procedure (read i)) can-dc))
                                      ((send dc get-recorded-procedure) can-dc))
                                  (send can-dc draw-bitmap (send mem-dc get-bitmap) 0 0 'opaque))
                              (send can-dc set-origin 0 0)
                              (send can-dc set-scale 1 1)
                              (send can-dc set-alpha 1.0)
                              (send can-dc set-clipping-region #f)))

			  'done)])

		  (send (get-dc) set-scale 1 1)
		  (send (get-dc) set-origin 0 0)

		  (let ([dc (if kind
				(let ([dc (case kind
                                            [(print) (make-object printer-dc%)]
                                            [(svg)
                                             (let ([fn (put-file)])
                                               (and fn
                                                    (new svg-dc%
                                                         [width (* xscale DRAW-WIDTH)]
                                                         [height (* yscale DRAW-HEIGHT)]
                                                         [output fn]
                                                         [exists 'truncate])))]
                                            [(ps pdf)
                                             (let ([page?
                                                    (eq? 'yes (message-box
                                                               "Bounding Box"
                                                               "Use paper bounding box?"
                                                               #f
                                                               '(yes-no)))])
                                               (new (if (eq? kind 'ps)
                                                        post-script-dc% 
                                                        pdf-dc%)
                                                    [width (* xscale DRAW-WIDTH)]
                                                    [height (* yscale DRAW-HEIGHT)]
                                                    [as-eps (not page?)]
                                                    [use-paper-bbox page?]))])])
				  (and (send dc ok?) dc))
                                (if use-record?
                                    (make-object record-dc% (* xscale DRAW-WIDTH) (* yscale DRAW-HEIGHT))
                                    (if (and use-bitmap?)
                                        (begin
                                          (send mem-dc set-bitmap bm)
                                          mem-dc)
                                        (get-dc))))])
		    (when dc
                      (send dc start-doc "Draw Test")
		      (send dc start-page)

                      (send dc erase)

		      (send dc set-alpha current-alpha)
                      (send dc set-rotation (- current-rotation))
                      (send dc set-initial-matrix (if current-skew?
                                                      (vector 1 0 0.2 1 3 0)
                                                      (vector 1 0 0 1 0 0)))

		      (if clip-pre-scale?
			  (begin
			    (send dc set-scale 1 1)
			    (send dc set-origin 0 0))
			  (begin
			    (send dc set-scale xscale yscale)
			    (send dc set-origin offset offset)))
		      (send dc set-smoothing smoothing)
		      (send dc set-alignment-scale align-scale)
		      
		      (send dc set-background
			    (if cyan?
				(send the-color-database find-color "CYAN")
				(send the-color-database find-color "WHITE")))

		      ;(send dc set-clipping-region #f)
		      (send dc erase)

                      (let ([clip-dc dc])
                        (if clock-clip?
                            (let ([r (make-object  region% clip-dc)])
                              (send r set-arc 0. 60. 180. 180. clock-start clock-end)
                              (send dc set-clipping-region r))
                            (let ([mk-poly (lambda (mode)
                                             (let ([r (make-object region% clip-dc)])
                                               (send r set-polygon octagon 0 0 mode) r))]
                                  [mk-circle (lambda ()
                                               (let ([r (make-object region% clip-dc)])
                                                 (send r set-ellipse 0. 60. 180. 180.) r))]
                                  [mk-rect (lambda ()
                                             (let ([r (make-object region% clip-dc)])
                                               (send r set-rectangle 100 -25 10 400) r))])
                              (case clip
                                [(none) (void)]
                                [(rect) (send dc set-clipping-rect 100 -25 10 400)]
                                [(rect2) (send dc set-clipping-rect 50 -25 10 400)]
                                [(poly) (send dc set-clipping-region (mk-poly 'odd-even))]
                                [(circle) (send dc set-clipping-region (mk-circle))]
                                [(wedge) (let ([r (make-object region% clip-dc)])
                                           (send r set-arc 0. 60. 180. 180. (* 1/4 pi) (* 3/4 pi))
                                           (send dc set-clipping-region r))]
                                [(lam) (let ([r (make-object region% clip-dc)])
                                         (send r set-path lambda-path)
                                         (send dc set-clipping-region r))]
                                [(A) (let ([p (new dc-path%)]
                                           [r (make-object region% clip-dc)])
                                       (send p text-outline (make-font #:size 256) "A" 10 10)
                                       (send r set-path p)
                                       (send dc set-clipping-region r))]
                                [(rect+poly) (let ([r (mk-poly 'winding)])
                                               (send r union (mk-rect))
                                               (send dc set-clipping-region r))]
                                [(rect+circle) (let ([r (mk-circle)])
                                                 (send r union (mk-rect))
                                                 (send dc set-clipping-region r))]
                                [(poly-rect) (let ([r (mk-poly 'odd-even)])
                                               (send r subtract (mk-rect))
                                               (send dc set-clipping-region r))]
                                [(poly&rect) (let ([r (mk-poly 'odd-even)])
                                               (send r intersect (mk-rect))
                                               (send dc set-clipping-region r))]
                                [(poly^rect) (let ([r (mk-poly 'odd-even)])
                                               (send r xor (mk-rect))
                                               (send dc set-clipping-region r))]
                                [(roundrect) (let ([r (make-object region% clip-dc)])
                                               (send r set-rounded-rectangle 80 200 125 40 -0.25)
                                               (send dc set-clipping-region r))]
                                [(empty) (let ([r (make-object region% clip-dc)])
                                           (send dc set-clipping-region r))]
                                [(polka) 
                                 (let ([c (send dc get-background)])
                                   (send dc set-background (send the-color-database find-color "PURPLE"))
                                   (send dc clear)
                                   (send dc set-background c))
                                 (let ([r (make-object region% clip-dc)]
                                       [w 30]
                                       [s 10])
                                   (let xloop ([x 0])
                                     (if (> x 300)
                                         (send dc set-clipping-region r)
                                         (let yloop ([y 0])
                                           (if (> y 500)
                                               (xloop (+ x w s))
                                               (let ([r2 (make-object region% clip-dc)])
                                                 (send r2 set-ellipse x y w w)
                                                 (send r union r2)
                                                 (yloop (+ y w s))))))))
                                 (send dc clear)]))))

		      (when clip-pre-scale?
			(send dc set-scale xscale yscale)
			(send dc set-origin offset offset)

			(let ([r (send dc get-clipping-region)])
			  (send dc set-clipping-rect 0 0 20 20)
			  (if r
			      (let ([r2 (make-object region% dc)])
				(send r2 set-rectangle 0 0 0 0)
				(send r xor r2)
				(send r2 xor r)
				(send dc set-clipping-region r2))
			      (send dc set-clipping-region #f))))

                      (unless clock-clip?
                        (let ([r (send dc get-clipping-region)])
                          (when r
                            (when (send r get-dc)
                              (unless (eq? (send r is-empty?) (eq? clip 'empty))
                                (show-error 'draw-text "region `is-empty?' mismatch"))))))
                      
                      (define (mutate-region)
                        (when (and (not clock-clip?)
                                   (not (eq? clip 'none)))
                          ;; To be uncooperative, mutate the clipping region:
                          (define r (send dc get-clipping-region))
                          (define r2 (make-object region% (send r get-dc)))
                          (send r2 union r)
                          (send dc set-clipping-region #f)
                          (send r set-rectangle 0 0 10 10)
                          (send dc set-clipping-region r2)))

		      ;; check default pen/brush:
		      (send dc draw-rectangle 0 0 5 5)
		      (send dc draw-line 0 0 20 6)

		      (send dc set-font (make-object font% 10 'default))

		      (draw-series dc pen0s pen0t pen0x "0 x 0" 5 0 0 #f)

                      (mutate-region)
		      
		      (draw-series dc pen1s pen1t pen1x "1 x 1" 70 0 1 #f)
		      
		      (draw-series dc pen2s pen2t pen2x "2 x 2" 135 0 2 #t)

		      (unless clock-clip?
			(let ([r (send dc get-clipping-region)])
                          (if (eq? clip 'none)
			      (when r
				(show-error 'draw-test "shouldn't have been a clipping region"))
			      (let*-values ([(x y w h) (send r get-bounding-box)]
					    [(l) (list x y w h)]
					    [(=~) (lambda (x y)
						    (or (not y)
                                                        (<= (- x 2) y (+ x 2))))])
				(unless (andmap =~ l
						(let ([l
						       (case clip
							 [(rect) '(100. -25. 10. 400.)]
							 [(rect2) '(50. -25. 10. 400.)]
							 [(poly circle poly-rect) '(0. 60. 180. 180.)]
							 [(wedge) '(26. 60. 128. 90.)]
							 [(lam) '(58. 10. 202. 281.)]
							 [(A) '(#f #f #f #f)]
							 [(rect+poly rect+circle poly^rect) '(0. -25. 180. 400.)]
							 [(poly&rect) '(100. 60. 10. 180.)]
							 [(roundrect) '(80. 200. 125. 40.)]
							 [(polka) '(0. 0. 310. 510.)]
							 [(empty) '(0. 0. 0. 0.)])])
						  (if clip-pre-scale?
						      (list (- (/ (car l) xscale) offset)
							    (- (/ (cadr l) yscale) offset)
							    (- (/ (caddr l) xscale) offset)
							    (- (/ (cadddr l) yscale) offset))
						      l)))
				  (show-error 'draw-test "clipping region changed badly: ~a" l))))))

		      (let-values ([(w h) (send dc get-size)])
			(unless (cond
				 [kind #t]
				 [use-bad? #t]
				 [use-bitmap? (and (= w (ceiling (* xscale DRAW-WIDTH))) (= h (ceiling (* yscale DRAW-HEIGHT))))]
				 [else (and (= w (* 2 DRAW-WIDTH)) (= h (* 2 DRAW-HEIGHT)))])
			  (show-error 'x "wrong size reported by get-size: ~a ~a (not ~a)" w h
                                      (if use-bitmap?
                                          (list (* xscale DRAW-WIDTH) (* yscale DRAW-HEIGHT))
                                          (list (* 2 DRAW-WIDTH) (* 2 DRAW-HEIGHT))))))

                      (send dc set-clipping-region #f)

                      (send dc end-page)
                      (when (and kind multi-page?)
                        (send dc start-page)
                        (send dc draw-text "Page 2" 0 0)
                        (send dc end-page))
		      (send dc end-doc)))
		  
		  (when save-filename
		    (send bm save-file save-filename save-file-format)
		    (set! save-filename #f))

		  'done)])])
	    (super-new [parent parent][style '(hscroll vscroll)])
	    (init-auto-scrollbars (* 2 DRAW-WIDTH) (* 2 DRAW-HEIGHT) 0 0))
	  vp)])
    (make-object choice% #f '("Canvas" "Pixmap" "Bitmap" "Platform" "Screen" "Compatible" "Backing x3" "Record" "Serialize" "Bad") hp0
		 (lambda (self event)
		   (set! use-bitmap? (< 0 (send self get-selection)))
		   (set! depth-one? (= 2 (send self get-selection)))
		   (set! platform-bitmap? (= 3 (send self get-selection)))
		   (set! screen-bitmap? (= 4 (send self get-selection)))
		   (set! compat-bitmap? (= 5 (send self get-selection)))
		   (set! scaled-bitmap? (= 6 (send self get-selection)))
		   (set! use-record? (<= 7 (send self get-selection) 8))
		   (set! serialize-record? (= 8 (send self get-selection)))
		   (set! use-bad? (< 9 (send self get-selection)))
		   (send canvas refresh)))
    (make-object choice% #f
		 '("MrEd XOR" "PLT Middle" "PLT ^ MrEd" "MrEd ^ PLT" "MrEd ^ MrEd" 
		   "MrEd~ Opaque" "MrEd ^ MrEd~" "M^M~ Opaque" "M^M~ Red" "M^M~ Rd Opq"
		   "PLT^PLT")
		 hp
		 (lambda (self event)
		   (send canvas set-mask-ex-mode 
			 (list-ref '(mred plt plt-mask mred^plt mred^mred 
					  mred~ mred^mred~ opaque-mred^mred~ 
                                          red-mred^mred~ opaque-red-mred^mred~
					  plt^plt)
				   (send self get-selection)))))
    (make-object check-box% "Kern" hp
		 (lambda (self event)
		   (send canvas set-kern (send self get-value))))
    (make-object check-box% "Cyan" hp
		 (lambda (self event)
		   (set! cyan? (send self get-value))
		   (send canvas refresh)))
    (send (make-object check-box% "Icons" hp2
		       (lambda (self event)
			 (send canvas set-bitmaps (send self get-value))))
	  set-value #t)
    (send (make-object check-box% "Stipples" hp2
		       (lambda (self event)
			 (send canvas set-stipples (send self get-value))))
	  set-value #t)
    (make-object check-box% "Pixset" hp2
		 (lambda (self event)
		   (send canvas set-pixel-copy (send self get-value))))
    (make-object button% "Save" hp
		 (lambda (b e)
		   (unless use-bitmap?
		     (error 'save-file "only available for pixmap/bitmap mode"))
		   (let ([f (put-file)])
		     (when f
		       (let ([format
			      (cond 
			       [(regexp-match "[.]xbm$" f) 'xbm]
			       [(regexp-match "[.]xpm$" f) 'xpm]
			       [(regexp-match "[.]jpe?g$" f) 'jpeg]
			       [(regexp-match "[.]png$" f) 'png]
			       [else (error 'save-file "unknown suffix: ~e" f)])])
			 (set! save-filename f)
			 (set! save-file-format format)
			 (send canvas refresh))))))
    (make-object choice% #f '("Unsmoothed" "Smoothed" "Aligned") hp2.5
		 (lambda (self event)
		   (set! smoothing (list-ref '(unsmoothed smoothed aligned)
					     (send self get-selection)))
		   (send canvas refresh)))
    (make-object choice% #f '("Align 1.0" "Align 2.0" "Align 3.0" "Align 0.5") hp2.5
		 (lambda (self event)
		   (set! align-scale (list-ref '(1.0 2.0 3.0 0.5)
                                               (send self get-selection)))
		   (send canvas refresh)))
    (make-object button% "Clock" hp2.5 (lambda (b e) (do-clock #f)))
    (make-object choice% "Clip" 
		 '("None" "Rectangle" "Rectangle2" "Octagon" 
		   "Circle" "Wedge" "Round Rectangle" "Lambda" "A"
		   "Rectangle + Octagon" "Rectangle + Circle" 
		   "Octagon - Rectangle" "Rectangle & Octagon" "Rectangle ^ Octagon" "Polka"
		   "Empty")
		 hp2.75
		 (lambda (self event)
		   (set! clip (list-ref
		               '(none rect rect2 poly circle wedge roundrect lam A 
				      rect+poly rect+circle poly-rect poly&rect poly^rect 
				      polka empty)
		               (send self get-selection)))
		   (send canvas refresh)))
    (make-object check-box% "Clip Pre-Scale" hp2.75
		 (lambda (self event)
		   (send canvas set-clip-pre-scale (send self get-value))))
    (make-object choice% #f '("1" "*2" "/2" "1,*2" "*2,1") hp2.75
		 (lambda (self event)
		   (send canvas set-scale 
			 (list-ref '(1 2 1/2 1 2) (send self get-selection))
			 (list-ref '(1 2 1/2 2 1) (send self get-selection)))))
    (make-object check-box% "+10" hp2.75
		 (lambda (self event)
		   (send canvas set-offset (if (send self get-value) 10 0))))
    (make-object choice% #f '("Cvs 1" "Cvs *2" "Cvs /2" "Cvs 1,*2" "Cvs *2,1") hp3
		 (lambda (self event)
		   (send canvas set-canvas-scale 
			 (list-ref '(1 2 1/2 1 2) (send self get-selection))
			 (list-ref '(1 2 1/2 2 1) (send self get-selection)))))
    (make-object check-box% "Cvs +10" hp3
		 (lambda (self event)
		   (send canvas set-canvas-offset (if (send self get-value) 10 0))))
    (make-object check-box% "Cvs rot" hp3
		 (lambda (self event)
		   (send canvas set-canvas-rotation (if (send self get-value) (* pi -1/5) 0))))
    (make-object choice% "Cvs Clip" '("None" "Empty" "Square" "Squares" "Octagon") hp3
		 (lambda (self event)
		   (send canvas set-canvas-clip (case (send self get-selection)
                                                  [(0) #f]
                                                  [(1) 'empty]
                                                  [(2) 'square]
                                                  [(3) 'squares]
                                                  [(4) 'octagon]))))
    (make-object check-box% "Cvs Gray" hp3
		 (lambda (self event)
		   (send canvas set-canvas-gray (send self get-value))))
    (make-object button% "PS" hp3
		 (lambda (self event)
		   (send canvas on-paint 'ps)))
    (make-object button% "PDF" hp3
		 (lambda (self event)
		   (send canvas on-paint 'pdf)))
    (make-object button% "SVG" hp3
		 (lambda (self event)
		   (send canvas on-paint 'svg)))
    (let ([clock (lambda (clip?)
		   (thread (lambda ()
			     (set! clock-clip? clip?)
			     (let loop ([c 0][swapped? #f][start 0.][end 0.])
			       (if (= c 32)
				   (if swapped?
				       (void)
				       (loop 0 #t 0. 0.))
				   (begin
				     (set! clock-start (if swapped? end start))
				     (set! clock-end (if swapped? start end))
				     (send canvas on-paint)
				     (sleep 0.25)
				     (loop (add1 c) swapped? (+ start (/ pi 8)) (+ end (/ pi 16))))))
			     (set! clock-clip? #f)
			     (set! clock-start #f)
			     (set! clock-end #f)
			     (send canvas refresh))))])
      (set! do-clock clock)
      (make-object button% "Clip Clock" hp2.75 (lambda (b e) (clock #t)))
      (make-object check-box% "Multiple Pages" hp2.75
                   (lambda (self event)
                     (set! multi-page? (send self get-value))))
      (make-object button% "Print" hp4 (lambda (self event) (send canvas on-paint 'print)))
      (make-object button% "Print Setup" hp4 (lambda (b e) (let ([c (get-page-setup-from-user)])
                                                             (when c
                                                               (send (current-ps-setup) copy-from c)))))
      (make-object slider% "Alpha" 0 10 hp4
                   (lambda (s e)
                     (let ([a (/ (send s get-value) 10.0)])
                       (unless (= a current-alpha)
                         (set! current-alpha a)
                         (send canvas refresh))))
                   10 '(horizontal plain))
      (make-object check-box% "Cvs Fade" hp4
                   (lambda (c e)
                     (set! current-c-alpha (if (send c get-value) 0.5 1.0))
                     (send canvas refresh)))
      (make-object slider% "Rotation" 0 100 hp4
                   (lambda (s e)
                     (let ([a (* pi 1/4 (/ (send s get-value) 100.0))])
                       (unless (= a current-rotation)
                         (set! current-rotation a)
                         (send canvas refresh))))
                   0 '(horizontal plain))
      (make-object check-box% "Skew" hp4
                   (lambda (c e)
                     (set! current-skew? (send c get-value))
                     (send canvas refresh)))))

  (send f show #t))

;; For test mode, check that we can at least start,
;; but exit right away:
(module+ test 
  (queue-callback (lambda () (exit)) #f))
