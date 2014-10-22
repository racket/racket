(module face mzscheme
  (require racket/draw
           pict/private/pict
           pict/private/utils
           mzlib/class
           mzlib/math
           mzlib/etc
           mzlib/kw)

  (provide face face* default-face-color)
  
  (define no-brush (find-brush "white" 'transparent))
  (define no-pen (find-pen "white" 1 'transparent))

  (define (series dc steps start-c end-c f pen? brush?)
    (color-series dc steps #e0.5 start-c end-c f pen? brush?))

  (define default-face-color (make-object color% "orange"))

  (define face*
    (lambda/kw (eyebrows-kind
                mouth-kind
                frown?
                #:optional
                [in-face-color default-face-color]
                [eye-inset 0]
                [eyebrow-dy 0]
                [eye-dx 0]
                [eye-dy 0]
                #:key
                (mouth-shading? #t)
                (eye-shading? #t)
                (eyebrow-shading? #t)
                (tongue-shading? #t)
                (face-background-shading? #t)
                (teeth? #t))
               
      (define face-color (if (string? in-face-color)
                             (make-object color% in-face-color)
                             in-face-color))
      (define face-bright-edge-color (scale-color #e1.6 face-color))
      (define face-edge-color (scale-color #e0.8 face-color))
      (define face-dark-edge-color (scale-color #e0.6 face-color))
      (define face-hard-edge-color (scale-color #e0.8 face-edge-color))
      (let ([w 300]
            [h 300])
          (dc (lambda (dc x y)
                (define old-pen (send dc get-pen))
                (define old-brush (send dc get-brush))
                
                (define (one-eye l? p? dd look?)
                  (define s (if p? 1/3 1))
                  (define sdd (if p? 1 1/2))
                  (define dx (+ (if p? (* 1/5 w 1/3) 0) (if look? eye-dx 0)))
                  (define dy (+ (if p? (* 1/4 w 1/3) 0) (if look? eye-dy 0)))
                  (send dc draw-ellipse
                        (+ x (* w (if l? 1/5 3/5)) dx (* dd sdd)) (+ y (* h 1/5) dy (* dd sdd))
                        (- (* w 1/5 s) (* 2 dd)) (- (* h 1/4 s) (* 2 dd))))
                
                (define (one-eye-brow l? dd dy dr)
                  (send dc draw-arc
                        (+ x (* w (if l? 1/5 3/5)))
                        (+ y (* h 3/20) dd dy)
                        (* w 1/5) 
                        (* h 1/4)
                        ((if l? + -) (* pi 1/3) dr) ((if l? + -) (* pi 2/3) dr)))
                
                (define (eye-series steps start-c end-c p? extra-inset look?)
                  (series dc
                          (if eye-shading? steps 0)
                          start-c end-c
                          (lambda (i)
                            (one-eye #t p? (+ extra-inset i) look?)
                            (one-eye #f p? (+ extra-inset i) look?))
                          #f #t))
                
                (define (eyebrows dy dr)
                  (send dc set-brush no-brush)
                  (series dc
                          (if eyebrow-shading? 3 0)
                          face-hard-edge-color
                          face-edge-color
                          (lambda (i)
                            (one-eye-brow #t i dy dr)
                            (one-eye-brow #f i dy dr))
                          #t #f)
                  (send dc set-pen no-pen))
                
                (define (normal-eyebrows dy)
                  (eyebrows dy 0))
                
                (define (worried-eyebrows dy)
                  (eyebrows dy 0.3))
                
                (define (angry-eyebrows dy)
                  (eyebrows dy -0.3))
                
                (define (smile sw sh i da path dy flip?)
                  ;; Either draw or set path.
                  ((if path 
                       (lambda (x y w h s e)
			 (send path arc x y w h s e))
                       (lambda (x y w h s e)
                         (send dc draw-arc x y w h s e)))
                   (+ x (/ (- w sw) 2) (* 1/6 sw)) 
                   (+ y (/ (- sh h) 2) (* 1/8 h) dy (if flip? i 0) (if flip? (- (* h 1/2) (- sh h)) 0))
                   (* sw 2/3) (+ (if flip? 0 i) (* h 2/3))
                   (- (* pi (- 5/4 (if flip? 1 0))) da) (+ (* pi (- 7/4 (if flip? 1 0))) da)))
                
                (define (plain-smile flip? tongue? narrow?)
		  (send dc set-brush no-brush)
		  (series dc 
                          (if mouth-shading? 3 0)
                          (make-object color% "black")
                          face-edge-color
                          (lambda (i)
                            (let ([da (if narrow? (* pi -1/8) 0)])
                              (smile w h i da #f 0 flip?)
                              (smile w h (+ 1 (- i)) da #f 0 flip?)))
                          #t #f)
		  (when tongue?
		    (let ([path (new dc-path%)]
			  [rgn (make-object region% dc)])
		      (smile w h 2 0 path 0 flip?)
		      (send path line-to (+ w x) (+ h y))
		      (send path line-to x (+ h y))
		      (send rgn set-path path)
		      (send dc set-clipping-region rgn)
		      (send dc set-pen no-pen)
		      (let ([dx (+ x (if flip?
					 (* 1/3 w)
					 (* 1/2 w)))]
			    [dy (+ y (if flip?
					 (* 1/2 h)
					 (* 13/20 h)))]
			    [tw (* 1/5 w)]
			    [th (* 1/4 h)])
			(series dc
                                (if tongue-shading? 3 0)
				face-color
				(make-object color% "red")
				(lambda (i)
				  (send dc draw-ellipse dx dy (- tw i) (- th i)))
				#f #t)
			(series dc 
                                (if tongue-shading? 4 0)
				(make-object color% "black")
				(scale-color 0.6 (make-object color% "red"))
				(lambda (i)
				  (send dc draw-line (- (+ dx i) (* tw 1/10)) dy (+ dx (* tw 0.65)) (+ dy (* th 0.75))))
				#t #f)
			(send dc set-clipping-region #f)))))
                
                (define (teeth)
                  ;; Assumes clipping region is set
                  (send dc set-brush (find-brush "white"))
                  (send dc draw-ellipse x y w h)
                  (when teeth?
                    (series dc 
                            5
                            (make-object color% "darkgray")
                            (make-object color% "lightgray")
                            (lambda (i)
                              (let loop ([j 0][delta 0][tw (* w 1/10)])
                                (unless (= j 5)
                                  (send dc draw-rectangle
                                        (+ x (* w 1/2) delta 1) y
                                        (- tw i 1) h)
                                  (send dc draw-rectangle
                                        (+ x (* w 1/2) (- delta) (- tw) 1) y
                                        (- tw i 1) h)
                                  (loop (add1 j) (+ delta tw) (* 8/10 tw)))))
                            #f #t)))
                
                (define (toothy-smile tw th ta bw bh ba flip? ddy)
                  (let-values ([(path) (make-object dc-path%)]
			       [(tmp-rgn1) (make-object region% dc)]
                               [(dy) (+ ddy (/ (- h (if flip? (+ th (abs (- bh th))) th)) 2))])
                    ;; Teeth:
                    (smile tw th 0 ta path dy flip?)
		    (send path reverse)
                    (smile bw bh 0 ba path dy flip?)
		    (send tmp-rgn1 set-path path)
                    (send dc set-clipping-region tmp-rgn1)
                    (teeth)
                    (send dc set-clipping-region #f)
                    
                    ;; Smile edges:
                    (send dc set-brush no-brush)
                    (series dc 
                            (if mouth-shading? 3 0)
                            (if flip? face-bright-edge-color face-hard-edge-color)
                            (if flip? face-color face-edge-color)
                            (lambda (i)
                              (smile bw bh (if flip? i (- i)) ba #f dy flip?))
                            #t #f)
                    (series dc
                            (if mouth-shading? 3 0)
                            (if flip? face-hard-edge-color face-bright-edge-color)
                            (if flip? face-edge-color face-color)
                            (lambda (i)
                              (smile tw th (if flip? (- i) i) ta #f dy flip?))
                            #t #f)))
                
                (define (grimace tw th ta flip?)
                  (let-values ([(path) (make-object dc-path%)]
                               [(tmp-rgn1) (make-object region% dc)]
                               [(dy) (/ (- h th) 2)]
                               [(elx ely) (values (+ x (* w 0.27)) (+ y (* h 0.65) (if flip? 3 1)))])
                    ;; Teeth:
		    (smile tw th 0 ta path (+ (if flip? -30 0) dy) flip?)
		    (send path arc elx ely 30 30 (* 1/2 pi) (* 3/2 pi) #t)
		    (send path reverse)
		    (send path arc (- (+ x w) (- elx x) 30) ely 30 30 (* 1/2 pi) (* 3/2 pi) #f)
		    (smile tw th 0 ta path (+ (if flip? 0 -30) dy) flip?)
                    (send tmp-rgn1 set-path path)
                    (send dc set-clipping-region tmp-rgn1)
		    (teeth)
                    (send dc set-clipping-region #f)
		    
                    ;; Smile edges:
                    (send dc set-brush no-brush)
                    (let ([sides (lambda (top? i)
                                   (send dc draw-arc (- elx (/ i 2)) (- ely (/ i 2)) 30 (+ 30 i) 
                                         (* pi (if top? 1 1/2)) (* pi (if top? 3/2 1)))
                                   (send dc draw-arc (+ (- (+ x w) (- elx x) 30) (/ i 2)) (- ely (/ i 2)) 30 (+ 30 i) 
                                         (* pi (if top? -1/2 0)) (* pi (if top? 0 1/2))))])
                      (series dc 
                              (if mouth-shading? 3 0)
                              (if flip? face-bright-edge-color face-hard-edge-color)
                              (if flip? face-color face-edge-color)
                              (lambda (i)
                                (sides flip? i)
                                (smile tw th (if flip? i (- i)) ta #f (+ (if flip? -2 -30) dy) flip?))
                              #t #f)
                      (series dc 
                              (if mouth-shading? 3 0)
                              (if flip? face-hard-edge-color face-bright-edge-color)
                              (if flip? face-edge-color face-color)
                              (lambda (i)
                                (sides (not flip?) i)
                                (smile tw th (if flip? (- i) i) ta #f (+ (if flip? -30 0) dy) flip?))
                              #t #f))))
                
                (define (medium-grimace flip?)
                  (grimace
                   (* 1.2 w) (* h 0.9) (- (* 0.1 pi)) 
                   flip?))
                
                (define (narrow-grimace flip?)
                  (grimace
                   (* 1.2 w) (* h 0.9) (- (* 0.1 pi))
                   flip?))
                
                (define (large-smile flip?)
                  (toothy-smile
                   w (* 1.05 h) (* 0.10 pi)
                   (* 1.1 w) (* h 0.95) (* 0.05 pi)
                   flip? 0))
                
                (define (largest-smile flip?)            
                  (toothy-smile
                   w (* 1.1 h) (* 0.14 pi)
                   (* 1.2 w) (* h 0.9) (* 0.05 pi)
                   flip? (if flip? (* h 0.1) 0)))
                
                (define (narrow-smile flip?)
                  (toothy-smile
                   (* 0.8 w) (* h 0.7) (- (* 0.00 pi))
                   (* 1.0 w) (* h 0.6) (- (* 0.06 pi))
                   flip? (if flip? (- (* h 0.2)) 0)))
                
                (define (medium-smile flip?)
                  (toothy-smile
                   (* 0.8 w) (* h 0.9) (* 0.08 pi)
                   (* 1.0 w) (* h 0.75) (- (* 0.01 pi))
                   flip? 0))
                
                (define (oh)
                  (let ([do-draw
                         (λ (i)
                           (let ([sw (* w 7/20)]
                                 [sh (* h 8/20)])
                             (send dc draw-ellipse
                                   (+ x i (/ (- w sw) 2))
                                   (+ y (* i .75) (* h 1/4) (* h -1/16) (/ (- h sh) 2))
                                   (- sw (* i 2))
                                   (- sh (* i 2)))))])
                    (series dc 
                            (if mouth-shading? 5 0)
                            face-color
                            face-dark-edge-color
                            do-draw
                            #t #t)
                    (send dc set-brush (find-brush "black"))
                    (send dc set-pen no-pen)
                    (do-draw 9)))
                
                (define (draw-eyes inset)
                  ;; Draw eyes
                  (eye-series 10 
                              (make-object color% "lightgray")
                              (make-object color% "white")
                              #f
                              inset
                              #f)
                  
                  ;; Draw pupils
                  (eye-series 3
                              (make-object color% 220 220 220)
                              (make-object color% "black")
                              #t
                              0
                              #t))
                
                (send dc set-pen no-pen)
                
                ;; Draw face background
                (series dc
                        (if face-background-shading? 3 0)
                        face-edge-color
                        face-color
                        (lambda (i)
                          (send dc draw-ellipse 
                                (+ x (/ i 2)) (+ y (/ i 2))
                                (- w (* 2 i)) (- h (* 2 i))))
                        #f #t)

		(draw-eyes eye-inset)
		(case eyebrows-kind
		  [(normal) (normal-eyebrows eyebrow-dy)]
		  [(worried) (worried-eyebrows eyebrow-dy)]
		  [(angry) (angry-eyebrows eyebrow-dy)]
		  [(none) (void)])
		(case mouth-kind
		  [(plain) (plain-smile frown? #f #f)]
		  [(smaller) (plain-smile frown? #f #t)]
                  [(narrow) (narrow-smile frown?)]
		  [(medium) (medium-smile frown?)]
		  [(large) (large-smile frown?)]
		  [(huge) (largest-smile frown?)]
		  [(grimace) (medium-grimace frown?)]
                  [(oh) (oh)]
		  [(tongue) (plain-smile frown? #t #f)])
                
                (send dc set-brush old-brush)
                (send dc set-pen old-pen))
              w h 0 0))))

  (define-syntax (case/good-error-message stx)
    (syntax-case stx (else)
      [(_ test [(sym ...) e] ... [else x last-e])
       (syntax
        (case test 
          [(sym ...) e]  ...
          [else (let ([x (apply append '((sym ...) ...))]) last-e)]))]))
      
  (define face
    (opt-lambda (mood [face-color default-face-color])
      (case/good-error-message mood
	[(unhappy)
	 (face* 'none 'plain #t face-color 6)]
	[(sortof-happy)
	 (face* 'worried 'medium #f face-color 6)]
	[(sortof-unhappy)
	 (face* 'worried 'grimace #t face-color 6)]
	[(happy)
	 (face* 'none 'plain #f face-color 6)]
	[(happier)
	 (face* 'none 'large #f face-color 3)]
	[(embarrassed embarassed) ; keep older misspelled name
	 (face* 'worried 'medium #f face-color 3)]
	[(badly-embarrassed badly-embarassed) ; here too
	 (face* 'worried 'medium #t face-color 3)]
	[(unhappier)
	 (face* 'normal 'large #t face-color 3)]
	[(happiest)
	 (face* 'normal 'huge #f face-color 0 -3)]
	[(unhappiest)
	 (face* 'normal 'huge #t face-color 0 -3)]
        [(mad)
         (face* 'angry 'grimace #t face-color 0)]
        [(mean)
         (face* 'angry 'narrow #f face-color 0)]
        [(surprised)
         (face* 'worried 'oh #t face-color -4 -3 2)]
	[else all-ids (error 'face "unknown mood: ~e, expected one of ~s" mood all-ids)]))))
