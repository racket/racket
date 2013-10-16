#lang scheme/unit

  (require racket/class)

  (require racket/draw/draw-sig
           racket/gui/dynamic
           (only-in racket/file make-temporary-file)
           (only-in racket/port copy-port))

  (require "mrpict-sig.rkt"
           "common-sig.rkt")
  

  (import draw^
          texpict-common^
          texpict-internal^)
  (export mrpict-extra^
          texpict-common-setup^)

      (define show-pict
        (λ (p [w #f] 
              [h #f] 
              #:frame-style [frame-style '()]
              #:frame-x [frame-x #f]
              #:frame-y [frame-y #f])
          (define the-pict p)
          (define pict-drawer (make-pict-drawer the-pict))
          (define no-redraw? #f)
          (define pict-frame%
            (class (gui-dynamic-require 'frame%)
              (define/public (set-pict p)
                (set! the-pict p)
                (set! pict-drawer (make-pict-drawer the-pict))
                (set! no-redraw? #t)
                (let ([pw (inexact->exact (floor (pict-width the-pict)))]
                      [ph (inexact->exact (floor (pict-height the-pict)))])
                  (send c min-width (if w (max w pw) pw))
                  (send c min-height (if h (max h ph) ph)))
                (set! no-redraw? #f)
                (send c on-paint))
              (super-instantiate ())))
          (define pict-canvas%
            (class (gui-dynamic-require 'canvas%)
              (inherit get-dc)
              (define/override (on-paint)
                (unless no-redraw?
                  (let ([dc (get-dc)])
                    (send dc clear)
                    (let* ([pw (pict-width the-pict)]
                           [ph (pict-height the-pict)]
                           [xo (if (and w
                                        (pw . < . w))
                                   (- (/ w 2) (/ pw 2))
                                   0)]
                           [yo (if (and h
                                        (ph . < . h))
                                   (- (/ h 2) (/ ph 2))
                                   0)])
                      (pict-drawer dc xo yo)))))
              (super-instantiate ())))
          (define f (new pict-frame% 
                         [label "MrPict"] 
                         [style frame-style] 
                         [x frame-x]
                         [y frame-y]))
          (define c (make-object pict-canvas% f))
          (send (send c get-dc) set-smoothing 'aligned)
          (send f set-pict p)
          (send f show #t)))
      
      (define dc-for-text-size (make-parameter 
				(make-object bitmap-dc% (make-bitmap 1 1))
				(lambda (x)
				  (unless (or (not x)
					      (is-a? x dc<%>))
				    (raise-type-error 'dc-for-parameter "dc<%> object or #f" x))
				  x)))

      (define dc 
	(case-lambda
	 [(f w h a d)
	  (make-pict `(prog ,f ,h) w h a d null #f #f)]
	 [(f w h)
	  (dc f w h h 0)]))
      (define prog-picture dc)

      (define current-expected-text-scale (make-parameter (list 1 1)))
      (define (with-text-scale dc thunk)
	(let ([x (current-expected-text-scale)])
	  (if (equal? x '(1 1))
	      (thunk)
	      (let-values ([(xs ys) (send dc get-scale)])
		(send dc set-scale (* xs (car x)) (* ys (cadr x)))
		(let-values ([(w h d s) (thunk)])
		  (send dc set-scale xs ys)
	          (values w h d s))))))

      (define (memq* a l)
	(if (pair? l)
	    (or (eq? (car l) a)
		(memq* a (cdr l)))
	    #f))

      (define (extend-font font size style weight hinting)
	(if (send font get-face)
	    (send the-font-list find-or-create-font
		  size 
		  (send font get-face)
		  (send font get-family)
		  style
		  weight
		  #f
		  'default
		  #t
                  hinting)
	    (send the-font-list find-or-create-font
		  size 
		  (send font get-family)
		  style
		  weight
		  #f
		  'default
		  #t
                  hinting)))

  (define text
    (case-lambda
      [(string) (text string '() 12)]
      [(string style) (text string style 12)]
      [(string style size) (text string style size 0)]
      [(str style size angle)
       (if (il-memq 'caps style)
           (begin
             (unless (zero? angle) 
               (error 'text "the style cannot include 'caps with a non-zero angle"))
             (caps-text str (il-remq 'caps style) size))
           (not-caps-text str style size angle))]))
  
  (define families '(default decorative roman script swiss modern symbol system))

  (define (il-memq sym s)
    (and (pair? s)
         (or (eq? sym (car s))
             (il-memq sym (cdr s)))))
  (define (il-remq sym s)
    (if (pair? s)
        (if (eq? sym (car s))
            (cdr s)
            (cons (car s) (il-remq sym (cdr s))))
        s))
  
      (define (not-caps-text string orig-style size angle)
	  (let ([font
		 (let loop ([style orig-style])
		   (cond
		    [(null? style) 
		     (send the-font-list find-or-create-font
			   size 'default 'normal 'normal #f 'default #t 'unaligned)]
		    [(is-a? style font%)
		     style]
		    [(memq style families)
		     (send the-font-list find-or-create-font
			   size style 'normal 'normal #f 'default #t 'unaligned)]
		    [(string? style)
		     (send the-font-list find-or-create-font
			   size style 'default 'normal 'normal #f 'default #t 'unaligned)]
                    [(and (pair? style)
                          (string? (car style))
                          (memq (cdr style) families))
                     (send the-font-list find-or-create-font
			   size (car style) (cdr style) 'normal 'normal #f 'default #t 'unaligned)]
		    [(and (pair? style)
			  (memq (car style)
				'(superscript 
				  subscript
				  bold italic
                                  aligned unaligned)))
		     (let ([font (loop (cdr style))]
			   [style (car style)])
		       (cond
			[(eq? style 'bold)
			 (extend-font font
				      (send font get-point-size)
				      (send font get-style)
				      'bold
                                      (send font get-hinting))]
			[(eq? style 'italic)
			 (extend-font font
				      (send font get-point-size)
				      'italic
				      (send font get-weight)
                                      (send font get-hinting))]
			[(or (eq? style 'aligned)
                             (eq? style 'unaligned))
			 (extend-font font
				      (send font get-point-size)
				      (send font get-style)
				      (send font get-weight)
                                      style)]
			[else font]))]
		    [(and (pair? style)
			  (memq (car style) '(combine no-combine outline)))
		     (loop (cdr style))]
		    [(and (pair? style)
			  (is-a? (car style) color%))
		     (loop (cdr style))]
		    [else (raise-type-error 'text
					    "style"
					    orig-style)]))]
		[combine? (let loop ([style orig-style])
			    (cond
			     [(eq? style 'modern) #f]
			     [(not (pair? style)) #t]
			     [(eq? (car style) 'combine) #t]
			     [(eq? (car style) 'no-combine) #f]
			     [else (loop (cdr style))]))]
		[sub? (memq* 'subscript orig-style)]
		[sup? (memq* 'superscript orig-style)]
		[outline? (memq* 'outline orig-style)]
                [color (let loop ([style orig-style])
                         (cond
                          [(not (pair? style)) #f]
                          [(is-a? (car style) color%) 
                           (resolve-color (car style))]
                          [else (loop (cdr style))]))])
	    (let ([s-font (if (or sub? sup?)
			      (extend-font font
					   (floor (* 6/10 (send font get-point-size)))
					   (send font get-style)
					   (send font get-weight)
                                           (send font get-hinting))
			      font)]
		  [dc (dc-for-text-size)])
	      (unless dc
		(error 'text "no dc<%> object installed for sizing"))
	      (let-values ([(w h d s) (with-text-scale
				       dc
				       (lambda ()
					 (send dc get-text-extent string s-font combine?)))])
                (define (make-draw adj-x adj-y angle)
                  (define p 
                    (and outline?
                         (let ([p (new dc-path%)])
                           (send p text-outline
                                 s-font string 0 0 combine?)
			   (unless (zero? angle)
			     (send p rotate angle))
                           p)))
                  (lambda (dc x y)
                    (let ([f (send dc get-font)])
                      (define dest-x (adj-x x))
                      (define dest-y (adj-y y))
                      (cond
                       [outline?
                        (define pn (and color (send dc get-pen)))
                        (when color (send dc set-pen color (send pn get-width) (send pn get-style)))
                        (send dc draw-path p dest-x dest-y)
                        (when color (send dc set-pen pn))]
                       [else
                        (define fg (and color (send dc get-text-foreground)))
                        (when color (send dc set-text-foreground color))
                        (send dc set-font s-font)
                        (send dc draw-text string
                              dest-x dest-y
                              combine? 0 angle)
                        (when fg (send dc set-text-foreground fg))
                        (send dc set-font f)]))))
                 (if (or sub? sup?)
                     (let-values ([(ww wh wd ws) (with-text-scale
                                                  dc
                                                  (lambda ()
                                                    (send dc get-text-extent "Wy" font)))])
                       (prog-picture (make-draw
                                      (lambda (x) x)
                                      (lambda (y) (if sub?
                                                      (+ y (- wh h))
                                                      y))
				      0)
                                     w wh (- wh wd) wd))
                     (if (zero? angle)
                         ;; Normal case: no rotation
                         (prog-picture (make-draw (lambda (x) x)
                                                  (lambda (y) y)
						  0)
                                       w h (- h d) d)
                         ;; Rotation case. Need to find the bounding box.
                         ;; Calculate the four corners, relative to top left as origin:
                         (let* ([tlx 0]
                                [tly 0]
                                [ca (cos angle)]
                                [sa (sin angle)]
                                [trx (* w ca)]
                                [try (- (* w sa))]
                                [brx (+ trx (* h sa))]
                                [bry (- try (* h ca))]
                                [blx (* h sa)]
                                [bly (- (* h ca))]
                                ;;min-x and min-y must be non-positive,
                                ;; since tlx and tly are always 0
                                [min-x (min tlx trx blx brx)]
                                [min-y (min tly try bly bry)])
                           (let ([pw (- (max tlx trx blx brx) min-x)]
                                 [ph (- (max tly try bly bry) min-y)]
                                 [dx (cond
                                      [(and (positive? ca) (positive? sa)) 0]
                                      [(positive? ca) (- (* h sa))]
                                      [(positive? sa) (- (* w ca))]
                                      [else (+ (- (* w ca)) (- (* h sa)))])]
                                 [dy (cond
                                      [(and (positive? ca) (negative? sa)) 0]
                                      [(positive? ca) (* w sa)]
                                      [(negative? sa) (- (* h ca))]
                                      [else (+ (- (* h ca)) (* w sa))])])
                             (prog-picture (make-draw (lambda (x) (+ x dx))
                                                      (lambda (y) (+ y dy))
						      angle)
                                           pw ph ph 0)))))))))

      (define caps-text
	(case-lambda
	 [(string) (caps-text string '() 12)]
	 [(string style) (caps-text string style 12)]
	 [(string style size)
	  (let ([strings
		 (let loop ([l (string->list string)] [this null] [results null] [up? #f])
		   (if (null? l)
		       (reverse (cons (reverse this) results))
		       (if (eq? up? (char-upper-case? (car l)))
			   (loop (cdr l) (cons (car l) this) results up?)
			   (loop (cdr l) (list (car l)) (cons (reverse this) results) (not up?)))))]
		[cap-style
		 (let loop ([s style])
		   (cond
		    [(pair? s) (cons (car s) (loop (cdr s)))]
		    [(is-a? s font%) (send the-font-list find-or-create-font
					   (floor (* 8/10 (send s get-point-size)))
					   (send s get-family)
					   (send s get-style)
					   (send s get-weight)
					   (send s get-underlined?)
					   (send s get-smoothing)
					   (send s get-size-in-pixels?))]
		    [else s]))]
		[cap-size (floor (* 8/10 size))])
	    (let ([picts
		   (let loop ([l strings] [up? #f])
		     (if (null? l)
			 null
                         (let* ([first-string (list->string (map char-upcase (car l)))]
                                [first
                                 (not-caps-text first-string
                                                (if up? style cap-style)
                                                (if up? size cap-size)
                                                0)]
                                [rest (loop (cdr l) (not up?))])
                           (if (and up? (pair? (cdr l)))
                               ;; kern capital followed by non-captial
                               (let ([plain-first (not-caps-text first-string
                                                                 cap-style
                                                                 cap-size
                                                                 0)]
                                     [together (not-caps-text (string-append
                                                               first-string
                                                               (list->string (map char-upcase (cadr l))))
                                                              cap-style
                                                              cap-size
                                                              0)])
                                 (cons (hbl-append (- (pict-width together)
                                                      (+ (pict-width plain-first)
                                                         (pict-width (car rest))))
                                                   first
                                                   (car rest))
                                       (cdr rest)))
                               ;; no kerning needed:
                               (cons first rest)))))])
	      (apply hbl-append 0 picts)))]))

      (define (linewidth n p) (line-thickness n p))
      (define (linestyle n p) 
        (unless (memq n '(transparent solid xor hilite 
                                      dot long-dash short-dash dot-dash 
                                      xor-dot xor-long-dash xor-short-dash 
                                      xor-dot-dash))
          (raise-type-error 'linestyle "style symbol" n))
        (line-style n p))

      (define connect
	(case-lambda
	 [(x1 y1 x2 y2) (connect x1 y1 x2 y2 #f)]
	 [(x1 y1 x2 y2 arrow?) (~connect 'r +inf.0 x1 y1 x2 y2 arrow?)]))

      (define ~connect 
	(case-lambda
	 [(exact close-enough x1 y1 x2 y2) (~connect exact close-enough x1 y1 x2 y2 #f)]
	 [(exact close-enough x1 y1 x2 y2 arrow?)
	  `((put ,x1 ,y1 (,(if arrow? 'vector 'line) ,(- x2 x1) ,(- y2 y1) #f)))]))

      (define (resolve-color c)
        (let* ([requested-color (cond
                                 [(is-a? c color%) c]
                                 [(string? c)
                                  (send the-color-database find-color c)]
                                 [(list? c)
                                  (apply make-object color% c)])]
               [color (or requested-color 
                          (send the-color-database find-color "BLACK"))])
          (unless requested-color
            (eprintf "WARNING: couldn't find color: ~s\n" c))
          color))


      (define (render dc h+top l dx dy)
	(define b&w? #f)
	
	(with-method ([draw-line (dc draw-line)]
		      [draw-spline (dc draw-spline)]
		      [draw-ellipse (dc draw-ellipse)]
		      [get-pen (dc get-pen)]
		      [get-brush (dc get-brush)]
		      [get-text-foreground (dc get-text-foreground)]
		      [set-pen (dc set-pen)]
		      [set-brush (dc set-brush)]
		      [set-text-foreground (dc set-text-foreground)]
		      [find-or-create-pen (the-pen-list find-or-create-pen)]
		      [find-or-create-brush (the-brush-list find-or-create-brush)])

	  (let loop ([dx dx][dy dy][l l])
	    (unless (null? l)
	      (let ([x (car l)])
		(if (string? x)
		    (error 'draw-pict "how did a string get here?: ~s" x)
		    (case (car x)
		      [(offset) (loop (+ dx (cadr x))
				      (+ dy (caddr x))
				      (cadddr x))]
		      [(line vector)
		       (let ([xs (cadr x)]
			     [ys (caddr x)]
			     [len (cadddr x)])
			 (let ([mx (if len (abs (if (zero? xs) ys xs)) 1)]
			       [len (or len 1)])
			   (draw-line dx (- h+top dy)
				      (+ dx (* (/ xs mx) len)) (- h+top (+ dy (* (/ ys mx) len))))))]
		      [(circle circle*)
		       (let ([size (cadr x)])
			 (draw-ellipse (- dx (/ size 2)) (- h+top dy (/ size 2))
				       size size))]
		      [(oval)
		       (let ([b (get-brush)]
			     [rx (- dx (/ (cadr x) 2))]
			     [ry (- h+top dy (/ (caddr x) 2))])
			 (set-brush (find-or-create-brush "BLACK" 'transparent))
			 (let ([part (cadddr x)]
			       [cr (send dc get-clipping-region)]
			       [set-rect (lambda (l t r b)
					   (let ([cr (make-object region% dc)])
					     (send cr set-rectangle
						   (+ rx (* l (cadr x)))
						   (+ ry (* t (caddr x)))
						   (* (- r l) (cadr x))
						   (* (- b t) (caddr x)))
					     cr))])
			   (send dc set-clipping-region
				 (cond
				  [(string=? part "[l]")
				   (set-rect 0 0 0.5 1.0)]
				  [(string=? part "[tl]")
				   (set-rect 0 0 0.5 0.5)]
				  [(string=? part "[tr]")
				   (set-rect 0.5 0 1.0 0.5)]
				  [(string=? part "[r]")
				   (set-rect 0.5 0 1.0 1.0)]
				  [(string=? part "[bl]")
				   (set-rect 0 0.5 0.5 1.0)]
				  [(string=? part "[br]")
				   (set-rect 0.5 0.5 1.0 1.0)]
				  [else cr]))
			   (send dc draw-rounded-rectangle
				 rx ry
				 (cadr x) (caddr x)
				 (if (string=? part "") -0.2 -0.5))
			   (send dc set-clipping-region cr)
			   (set-brush b)))]
		      [(bezier)
		       (draw-spline (+ dx (list-ref x 1))
				    (- h+top (+ dy (list-ref x 2)))
				    (+ dx (list-ref x 3))
				    (- h+top (+ dy (list-ref x 4)))
				    (+ dx (list-ref x 5))
				    (- h+top (+ dy (list-ref x 6))))]
		      [(with-color)
		       (if b&w?
			   (loop dx dy (caddr x))
			   (let ([p (get-pen)]
				 [b (get-brush)]
				 [fg (get-text-foreground)])
			     (let ([color (resolve-color (cadr x))])
                          (set-pen (find-or-create-pen color 
                                                       (send p get-width) (send p get-style)
                                                       (send p get-cap) (send p get-join)))
			       (set-brush (find-or-create-brush color 'solid))
			       (set-text-foreground color))
			     (loop dx dy (caddr x))
			     (set-pen p)
			     (set-brush b)
			     (set-text-foreground fg)))]
		      [(with-thickness)
		       (let ([p (get-pen)])
			 (set-pen (find-or-create-pen (send p get-color) 
						      (if (number? (cadr x))
							  (cadr x)
                                                          (case (cadr x)
                                                            [(thicklines) 1]
                                                            [(thinlines) 0]
                                                            [else (send p get-width)]))
                                                      (if (number? (cadr x))
                                                          (send p get-style)
                                                          (case (cadr x)
                                                            [(#f) 'transparent]
                                                            [(thicklines thinlines) (send p get-style)]
                                                            [else (cadr x)]))
                                                      (send p get-cap) 
                                                      (send p get-join)))
			 (loop dx dy (caddr x))
			 (set-pen p))]
		      [(prog)
		       ((cadr x) dc dx (- h+top dy (caddr x)))]
		      [else (error 'render "unknown command: ~a\n" x)])))
	      (loop dx dy (cdr l))))))

      (define (make-pict-drawer p)
	(let ([cmds (pict->command-list p)])
	  (lambda (dc dx dy)
	    (render dc (+ (pict-height p) dy)
		    cmds
		    dx 0))))

      (define (draw-pict p dc dx dy)
	((make-pict-drawer p) dc dx dy))


      (define (convert-pict p format default)
        (if (eq? format 'pdf-bytes+bounds)
            (let ([xscale (box 1.0)]
                  [yscale (box 1.0)])
              (send (current-ps-setup) get-scaling xscale yscale)
              (list (convert-pict/bytes p 'pdf-bytes default)
                    (* (unbox xscale) (pict-width p))
                    (* (unbox yscale) (pict-height p))
                    (* (unbox yscale) (pict-descent p))
                    0))
            (convert-pict/bytes p format default)))
      
      (define (convert-pict/bytes p format default)
        (case format
          [(png-bytes)
           (let* ([bm (make-bitmap
                       (max 1 (inexact->exact (ceiling (pict-width p))))
                       (max 1 (inexact->exact (ceiling (pict-height p)))))]
                  [dc (make-object bitmap-dc% bm)])
             (send dc set-smoothing 'aligned)
             (draw-pict p dc 0 0)
             (send dc set-bitmap #f)
             (let ([s (open-output-bytes)])
               (send bm save-file s 'png)
               (get-output-bytes s)))]
          [(eps-bytes pdf-bytes)
           (let ([s (open-output-bytes)]
                 [xs (box 1)]
                 [ys (box 1)])
             (send (current-ps-setup) get-scaling xs ys)
             (let ([dc (new (if (eq? format 'eps-bytes) post-script-dc% pdf-dc%)
                            [interactive #f]
                            [as-eps #t]
                            [width (* (pict-width p) (unbox xs))]
                            [height (* (pict-height p) (unbox ys))]
                            [output s])])
               (send dc set-smoothing 'smoothed)
               (send dc start-doc "pict")
               (send dc start-page)
               (draw-pict p dc 0 0)
               (send dc end-page)
               (send dc end-doc))
             (get-output-bytes s))]
          [(svg-bytes)
           (let ([s (open-output-bytes)])
             (define dc (new svg-dc% 
                             [width  (pict-width p)]
                             [height (pict-height p)]
                             [output s]))
             (send dc set-smoothing 'smoothed)
             (send dc start-doc "Generating svg")
             (send dc start-page)
             (draw-pict p dc 0 0)
             (send dc end-page)
             (send dc end-doc)
             (regexp-replace "width=\"(.*pt)\" height=\"(.*pt)\"" 
                             (get-output-bytes s)
                             (λ (all w h) 
                               (define (rem x) (bytes->string/utf-8 (regexp-replace "pt" x "")))
                               (string->bytes/utf-8
                                (string-append "width=\"" (rem w) "\" height=\"" (rem h) "\""))))
             (get-output-bytes s))]
          [else default]))

             
