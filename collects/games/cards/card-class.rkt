
(module card-class mzscheme
  (require mzlib/class
	   mzlib/class100
           mzlib/shared
	   (prefix mred: mred)
	   "snipclass.rkt"
	   "region.rkt"
           (only scheme/base for in-range))

  (provide card%)

  (define prev-regions #f)
  (define prev-region-dc #f)

  (define (with-card-region dc x y width height thunk)
    (let ([rs (if #f ; (eq? prev-region-dc dc) <- assumes the same xform matrix
                  prev-regions
		  (cons (make-object mred:region% dc)
			(make-object mred:region% dc)))])
      (set! prev-regions rs)
      (set! prev-region-dc dc)
      (send (car rs) set-rectangle x (add1 y) width (- height 2))
      (send (cdr rs) set-rectangle (add1 x) y (- width 2) height)
      (send (car rs) union (cdr rs))
      (let ([r (send dc get-clipping-region)])
	(when r
	  (send (car rs) intersect r))
	(send dc set-clipping-region (car rs))
	(thunk)
	(send dc set-clipping-region r))))

  (define (rotate-bm bm cw?)
    (let ([w (send bm get-width)]
          [h (send bm get-height)])
      (let ([bm2 (make-object mred:bitmap% h w)]
            [s (make-bytes (* w h 4))]
            [s2 (make-bytes (* h w 4))])
        (send bm get-argb-pixels 0 0 w h s)
        (for ([i (in-range w)])
          (for ([j (in-range h)])
            (let ([src-pos (* (+ i (* j w)) 4)])
              (bytes-copy! s2 
                           (if cw?
                               (* (+ (- (- h j) 1) (* i h)) 4) 
                               (* (+ j (* (- (- w i) 1) h)) 4))
                           s src-pos (+ src-pos 4)))))
        (let ([dc (make-object mred:bitmap-dc% bm2)])
          (send dc set-argb-pixels 0 0 h w s2)
          (send dc set-bitmap #f))
        bm2)))

  (define orientations (shared ([o (list* 'n 'e 's 'w o)]) o))
  (define (find-head l s)
    (if (eq? (car l) s)
        l
        (find-head (cdr l) s)))

  (define card%
    (class100 mred:snip% (-suit-id -value -width -height -front -back -mk-dim-front -mk-dim-back -rotated-bms)
      (inherit set-snipclass set-count get-admin)
      (private-field
	[suit-id -suit-id]
	[value -value]
	[width -width]
	[height -height]
        [rotated 'n]
	[front -front]
	[back -back]
	[mk-dim-front -mk-dim-front]
	[mk-dim-back -mk-dim-back]
	[dim-front #f]
	[dim-back #f]
	[is-dim? #f]
	[flipped? #f]
	[semi-flipped? #f]
	[can-flip? #t]
	[can-move? #t]
	[snap-back? #f]
	[stay-region #f]
	[home-reg #f]
        [rotated-bms -rotated-bms])
      (private
	[refresh
	 (lambda ()
	   (let ([a (get-admin)])
	     (when a
	       (send a needs-update this 0 0 width height))))]
	[refresh-size
	 (lambda ()
	   (let ([a (get-admin)])
	     (when a
	       (send a resized this #f)))
           (refresh))]
	[check-dim
	 (lambda ()
	   (when is-dim?
	     (if flipped?
		 (unless dim-back
		   (set! dim-back (mk-dim-back)))
		 (unless dim-front
		   (set! dim-front (mk-dim-front))))))]
        [get-rotated
         (lambda (bm dir)
           (if (eq? dir 'n)
               bm
               (or (hash-table-get rotated-bms (cons dir bm) #f)
                   (let ([rotated-bm (case dir
                                       [(w) (rotate-bm bm #f)]
                                       [(e) (rotate-bm bm #t)]
                                       [(s) (rotate-bm (rotate-bm bm #t) #t)])])
                     (hash-table-put! rotated-bms (cons dir bm) rotated-bm)
                     rotated-bm))))])
      (public
	[face-down? (lambda () flipped?)]
	[flip
	 (lambda ()
	   (set! flipped? (not flipped?))
	   (refresh))]
	[semi-flip
	 (lambda ()
	   (set! semi-flipped? (not semi-flipped?))
	   (refresh))]
	[face-up (lambda () (when flipped? (flip)))]
	[face-down (lambda () (unless flipped? (flip)))]
	[dim (case-lambda 
	      [() is-dim?]
	      [(v)
	       (unless (eq? is-dim? (and v #t))
		 (set! is-dim? (and v #t))
		 (refresh))])]
        [orientation (lambda () (case rotated
                                  [(n) 0]
                                  [(e) 270]
                                  [(w) 90]
                                  [(s) 180]))]
        [rotate (lambda (mode)
                  (let ([delta (case mode
                                 [(0 360) 0]
                                 [(cw -90 270) 1]
                                 [(ccw 90 -270) 3]
                                 [(180 -180) 2]
                                 [else (error 'rotate "bad mode: ~e" mode)])])
                    (set! rotated (list-ref (find-head orientations rotated) delta))
                    (if (odd? delta)
                        (let ([w width])
                          (set! width height)
                          (set! height w)
                          (refresh-size))
                        (refresh))))]
	[get-suit-id
	 (lambda () suit-id)]
	[get-suit
	 (lambda ()
	   (case suit-id
	     [(1) 'clubs]
	     [(2) 'diamonds]
	     [(3) 'hearts]
	     [(4) 'spades]
	     [else 'unknown]))]
	[get-value
	 (lambda () value)]
	[user-can-flip
	 (case-lambda
	  [() can-flip?]
	  [(f) (set! can-flip? (and f #t))])]
	[user-can-move
	 (case-lambda
	  [() can-move?]
	  [(f) (set! can-move? (and f #t))])]
	[snap-back-after-move
	 (case-lambda
	  [() snap-back?]
	  [(f) (set! snap-back? (and f #t))])]
	[stay-in-region
	 (case-lambda
	  [() stay-region]
	  [(r) (set! stay-region r)])]
	[home-region
	 (case-lambda
	  [() home-reg]
	  [(r) (set! home-reg r)])]
	[card-width (lambda () width)]
	[card-height (lambda () height)])
      (override
	[resize
	 (lambda (w h) (void))]
	[get-extent
	 (lambda (dc x y w h descent space lspace rspace)
	   (map
	    (lambda (b)
	      (when b
		(set-box! b 0)))
	    (list descent space lspace rspace))
	   (when w (set-box! w width))
	   (when h (set-box! h height)))]
	[draw
	 (lambda (dc x y left top right bottom dx dy draw-caret)
	   (check-dim)
	   (let ([do-draw
                  (lambda (x y)
                    (with-card-region
                     dc x y width height
                     (lambda ()
                       (send dc draw-bitmap 
                             (let ([bm (if flipped? 
                                           (if is-dim? dim-back back)
                                           (if is-dim? dim-front front))])
                               (get-rotated bm rotated))
                             x y))))])
             (if semi-flipped?
                 (let-values ([(sx sy) (send dc get-scale)])
                   (case rotated
                     [(n s)
                      (send dc set-scale (/ sx 2) sy)
                      (do-draw (+ (* 2 x) (/ width 2)) y)
                      (send dc set-scale sx sy)]
                     [(e w)
                      (send dc set-scale sx (/ sy 2))
                      (do-draw x (+ (* 2 y) (/ height 2)))
                      (send dc set-scale sx sy)]))
                 (do-draw x y))))]
	[copy (lambda () 
                (let ([rotated? (memq rotated '(e w))])
                  (make-object card% suit-id value 
                               (if rotated? height width) 
                               (if rotated? width height )
                               front back
                               (lambda () 
                                 (unless dim-front 
                                   (set! dim-front (mk-dim-front)))
                                 dim-front)
                               (lambda () 
                                 (unless dim-back 
                                   (set! dim-back (mk-dim-back)))
                                 dim-back)
                               rotated-bms)))])
      (private-field
	[save-x (box 0)]
	[save-y (box 0)])
      (public
	[remember-location
	 (lambda (pb)
	   (send pb get-snip-location this save-x save-y))]
	[back-to-original-location
	 (lambda (pb)
	   (when snap-back?
	     (send pb move-to this (unbox save-x) (unbox save-y)))
	   (when home-reg
	     (let ([xbox (box 0)]
		   [ybox (box 0)])
	       (send pb get-snip-location this xbox ybox #f)
	       ;; Completely in the region?
	       (let* ([l (unbox xbox)]
		      [rl (region-x home-reg)]
		      [r (+ l width)]
		      [rr (+ rl (region-w home-reg))]
		      [t (unbox ybox)]
		      [rt (region-y home-reg)]
		      [b (+ t height)]
		      [rb (+ rt (region-h home-reg))])
		 (when (or (< l rl) (> r rr)
			   (< t rt) (> b rb))
		   ;; Out of the region - completely or partly?
		   (if (and (or (<= rl l rr) (<= rl r rr))
			    (or (<= rt t rb) (<= rt b rb)))
		       ;; Just slightly out
		       (send pb move-to this
			     (min (max l rl) (- rr width))
			     (min (max t rt) (- rb height)))
		       ;; Completely out
		       (send pb move-to this (unbox save-x) (unbox save-y))))))))])
      (sequence
	(super-init)
	(set-count 1)
	(set-snipclass sc)
	(flip)))))


