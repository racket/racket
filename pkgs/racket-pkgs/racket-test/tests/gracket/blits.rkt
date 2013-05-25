#lang scheme/gui

(define ok-frame (make-object frame% "Ok"))
(define ok-panel #f)

(define (try path mode color bg-color sx sy)
  (let ([bm (if (is-a? path bitmap%)
		path
		(make-object bitmap% path 'unknown/mask))])
    (let ([w (inexact->exact (ceiling (* sx (send bm get-width))))]
          [h (inexact->exact (ceiling (* sy (send bm get-height))))])
      (let* ([dest1 (make-object bitmap% w h)]
             [dest2 (make-object bitmap% w h)]
             [dc1 (make-object bitmap-dc% dest1)]
             [dc2 (make-object bitmap-dc% dest2)]
             [s1 (make-bytes (* w h 4))]
             [s2 (make-bytes (* w h 4))])
        (send dc1 clear)
        (send dc2 clear)
	(send dc1 set-brush bg-color 'solid)
	(send dc1 draw-rectangle 0 0 w h)
	(send dc2 set-brush bg-color 'solid)
	(send dc2 draw-rectangle 0 0 w h)
	(send dc1 set-scale sx sy)
	(send dc2 set-scale sx sy)
        (send dc1 draw-bitmap bm 0 0 
              mode color (send bm get-loaded-mask))
        (send dc2 draw-bitmap bm 0 0 
              mode color (send bm get-loaded-mask))
        (send dc1 get-argb-pixels 0 0 w h s1)
        (send dc2 get-argb-pixels 0 0 w h s2)
	(send dc1 set-bitmap #f)
	(send dc2 set-bitmap #f)
        (if (bytes=? s1 s2)
	    (make-object message% dest1 ok-panel)
	    (let ([f (make-object frame% "Different!")])
	      (make-object message% dest1 f)
	      (make-object message% dest2 f)
	      (send f show #t)))))))

(define (self-mask path)
  (let ([bm (make-object bitmap% path)])
    (send bm set-loaded-mask bm)
    bm))

(define (plus-mask path mpath)
  (let ([bm (make-object bitmap% path)]
	[xmbm (make-object bitmap% mpath)])
    (let* ([w (send bm get-width)]
	   [h (send bm get-height)]
	   [mbm (make-object bitmap% w h (= 1 (send xmbm get-depth)))]
	   [dc (make-object bitmap-dc% mbm)])
      (send dc clear)
      (send dc draw-bitmap-section xmbm 0 0 0 0 w h)
      (send dc set-bitmap #f)
      (send bm set-loaded-mask mbm)
      bm)))

(define targets
  (list
   (build-path (collection-path "frtime") "tool" "clock.png")
   (self-mask (build-path (collection-path "frtime") "tool" "clock.png"))
   (build-path (collection-path "icons") "foot-up.png")
   (build-path (collection-path "icons") "mred.xbm")
   (self-mask (build-path (collection-path "icons") "mred.xbm"))
   (plus-mask (build-path (collection-path "icons") "mred.xbm")
	      (build-path (collection-path "icons") "PLT-206.png"))
   (plus-mask (build-path (collection-path "frtime") "tool" "clock.png")
	      (build-path (collection-path "icons") "mred.xbm"))
   (build-path (collection-path "icons") "htdp-icon.gif")
   ))

(for-each 
 (lambda (mode)
   (for-each (lambda (sx sy)
	       (set! ok-panel (make-object horizontal-panel% ok-frame))
	       (for-each 
		(lambda (fg)
		  (for-each (lambda (target)
			      (try target
				   mode
				   fg
				   (make-object color% "green")
				   sx sy))
			    targets))
		(list (make-object color% "black")
		      (make-object color% "red"))))
	     '(1 3/2 1/2)
	     '(1 1/2 3/2)))
 '(solid opaque xor))


(send ok-frame show #t)
