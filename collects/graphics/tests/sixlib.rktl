;; load the graphics
(require graphics/graphics
         mzlib/etc
         mzlib/file)
(open-graphics)

(define (struct-test)
  ;; test rgb selectors
  (let* ([fraction 1/5]
         [test 
          (list
           (rgb-red (make-rgb fraction 0 0))
           (rgb-green (make-rgb 0 fraction 0))
           (rgb-blue (make-rgb 0 0 fraction)))])
    (unless (equal? (list fraction fraction fraction) test)
      (error 'rgb "wrong: ~s" test)))
  
  ;; test posn selectors
  (let* ([test
          (list
           (posn-x (make-posn 1 0))
           (posn-y (make-posn 0 1)))])
    (unless (equal? (list 1 1) test)
      (error 'posn "wrong: ~s" test))))

;; test basic operations
(define (basic-test)
  (let ([v (open-viewport "Tester" 200 200)])
    ((draw-string v) (make-posn 0 20) "Reversed X; click to continue")
    ((draw-string v) (make-posn 0 40) "(busy-waiting right now!)")
    ((draw-line v) (make-posn 0 0) (make-posn 100 100))
    ((draw-line v) (make-posn 100 0) (make-posn 0 100))
    ((flip-viewport v))
    (let loop ()
      (unless (ready-mouse-click v)
        (loop)))
    
    ((clear-viewport v))
    ((draw-string v) (make-posn 0 20) "Cleared; click")
    (get-mouse-click v)
    
    (let ([rect-draw
           (lambda (f)
             (f (make-posn 20 20) 60 60))]
          [poly-draw
           (lambda (f)
             (f (list (make-posn 0 0) (make-posn 40 0) (make-posn 20 40)) (make-posn 20 20)))]
          [string-draw
           (lambda (f)
             (f (make-posn 10 20) "XXXXX"))]
          [shape
           (lambda (do-draw draw clear flip name)
             ((clear-viewport v))
             ((draw-string v) (make-posn 0 20) (format "Centered ~s" name))
             (do-draw (draw v))
             (get-mouse-click v)
             
             ((draw-string v) (make-posn 0 40) (format "Erased ~s" name))
             (do-draw (clear v))
             (get-mouse-click v)
             
             ((clear-viewport v))
             ((draw-string v) (make-posn 0 20) (format "Centered ~s" name))
             (do-draw (draw v))
             (get-mouse-click v)
             
             ((draw-string v) (make-posn 0 40) (format "Flipped ~s" name))
             (do-draw (flip v))
             (get-mouse-click v)
             
             ((draw-string v) (make-posn 0 40) (format "Flipped ~s back" name))
             (do-draw (flip v))
             (get-mouse-click v))])
      (shape rect-draw draw-rectangle clear-rectangle flip-rectangle "box")
      (shape rect-draw draw-solid-rectangle clear-solid-rectangle flip-solid-rectangle "solid box")
      (shape rect-draw draw-ellipse clear-ellipse flip-ellipse "circle")
      (shape rect-draw draw-solid-ellipse clear-solid-ellipse flip-solid-ellipse "solid circle")
      (shape poly-draw draw-polygon clear-polygon flip-polygon "polygon")
      (shape poly-draw draw-solid-polygon clear-solid-polygon flip-solid-polygon "solid polygon")
      (shape string-draw
             draw-string
             clear-string
             flip-string
             "string"))
    
    ((clear-viewport v))
    ((draw-string v) (make-posn 0 20) "Done; click")
    (get-mouse-click v)
    
    (close-viewport v)))

;; test get-pixel
(define (pixel-test)
  (let ([v (open-viewport "test get-pixel" 8 8)]
        [f (lambda (x y) 
             (if (= (modulo (+ x y) 2) 0)
                 (make-rgb 1 1 1)
                 (make-rgb (/ (modulo (+ x y) 3) 2)
                           (/ (modulo (+ x y 1) 3) 2)
                           (/ (modulo (+ x y 2) 3) 2))))]
        
        [unmarshall-color
         (lambda (c)
           (if (is-a? c color%)
               (list (send c red)
                     (send c green)
                     (send c blue))
               c))]

        [for-each-point
         (lambda (f)
           (let loop ([i 8])
             (unless (= i 0)
               (let loop ([j 8])
                 (unless (= j 0)
                   (f (- i 1) (- j 1))
                   (loop (- j 1))))
               (loop (- i 1)))))])
    (for-each-point
     (lambda (i j)
       ;(printf "(~a, ~a) -> ~a\n" i j (unmarshall-color (f i j)))
       ((draw-pixel v) (make-posn i j) (f i j))))
    ;(get-mouse-click v)
    (for-each-point
     (lambda (i j)
       (let* ([cmp
               (lambda (rgb1 rgb2)
                 (and (= (rgb-red rgb1) (rgb-red rgb2))
                      (= (rgb-blue rgb1) (rgb-blue rgb2))
                      (= (rgb-green rgb1) (rgb-green rgb2))))]
              [color-expected ((test-pixel v) (f i j))]
              [bw-expected (if (cmp (make-rgb 1 1 1) color-expected) 0 1)]
              [color-got ((get-color-pixel v) (make-posn i j))]
              [bw-got ((get-pixel v) (make-posn i j))])
         (unless (= bw-got bw-expected)
           (error 'test-get-pixel "wrong answer for (~a,~a); got ~a expectd ~a"
                  i j bw-got bw-expected))
         (unless (cmp color-expected color-got)
           (error 'test-get-color-pixel "wrong answer for (~a,~a); got ~a expected ~a"
                  i j
                  (unmarshall-color color-got)
                  (unmarshall-color color-expected))))))
    (close-viewport v)))

(define (snip-test)
  ;; test snips
  (let ([vp (open-pixmap "snip test" 100 100)])
    ((draw-string vp) (make-posn 20 30) "flipped rect")
    ((flip-solid-rectangle vp) (make-posn 10 10) 80 80)
    (display (viewport->snip vp))))

(define (color-test)
  (let ([v (open-viewport "Color Tester" 100 200)])
    ((draw-solid-rectangle v) (make-posn 10 10) 80 80 (make-rgb 1 0 0))
    ((draw-solid-ellipse v) (make-posn 10 10) 80 80 (make-rgb 0 1 0))
    ((draw-line v) (make-posn 10 10) (make-posn 90 90) (make-rgb 0 0 1))
    ((draw-string v) (make-posn 10 100) "red rectangle")
    ((draw-string v) (make-posn 10 120) "green ellipse")
    ((draw-string v) (make-posn 10 140) "blue line")
    (get-mouse-click v)
    
    ((draw-viewport v) (make-rgb 1 0 0))
    ((draw-string v) (make-posn 10 100) "solid red")
    (get-mouse-click v)
    
    ((draw-viewport v))
    ((clear-string v) (make-posn 10 100) "solid black")
    (get-mouse-click v)
    
    (close-viewport v)))

(define (pixmap-test)
  (local [(define width 500)
          (define height 500)
          (define pixmap-filename (build-path (collection-path "icons") "plt.gif"))
          (define view-port (open-viewport "pixmap tests" width height))
          (define (line)
            ((draw-line view-port) (make-posn 50 50) (make-posn 450 450)))
          (define (next desc)
            ((draw-string view-port) (make-posn 0 (- height 50)) desc)
            ((draw-string view-port) (make-posn 0 (- height 30)) "click to continue")
            (get-mouse-click view-port)
            ((clear-viewport view-port)))]
    
    (line)
    (((draw-pixmap-posn pixmap-filename) view-port) (make-posn 0 0))
    (next "draw line then draw-pixmap-posn")
    
    (line)
    ((draw-pixmap view-port) pixmap-filename (make-posn 0 0))
    (next "pixmap-functions: draw line then draw-pixmap")
    
    (line)
    ((draw-pixmap view-port) pixmap-filename (make-posn 0 0))
    (let ([fn (make-temporary-file)])
      ((save-pixmap view-port) fn)
      ((clear-viewport view-port))
      (next (format "same image; saved to ~a and cleared..." fn))
      ((draw-pixmap view-port) fn (make-posn 0 0))
      (delete-file fn)
      (next "re-loaded"))
    
    (close-viewport view-port)))

(define (copy-viewport-test)
  (let* ([width 100]
         [height 100]
         [vs (open-viewport "viewport source" width height)]
         [vd (open-viewport "viewport dest" width height)])
    
    ((draw-ellipse vs) (make-posn 10 10) 80 80)
    ((draw-string vs) (make-posn 10 30) "Click")
    (get-mouse-click vs)
    (copy-viewport vs vd)
    ((clear-viewport vs))
    ((draw-string vs) (make-posn 10 30) "Cleared")
    (get-mouse-click vd)
    (void)))

(define (keyboard-test)
  (let ([v (open-viewport "keyboard" 300 200)]
        [RED (make-rgb 1 0 0)]
        [BLACK (make-rgb 0 0 0)])
    ((draw-string v) (make-posn 5 15) "Type, end with return (red is from key-ready):")
    (let loop ([x 5])
      (let* ([kv (or (begin
                       ((draw-rectangle v) (make-posn 290 0) 10 10 RED)
                       (ready-key-press v))
                     (begin
                       ((draw-rectangle v) (make-posn 290 0) 10 10 BLACK)
                       (cons 'slow (get-key-press v))))]
             [k (key-value (if (pair? kv) (cdr kv) kv))])
        ((clear-rectangle v) (make-posn 0 290) 10 10)
        (cond
          [(eq? k #\return) 'done]
          [(char? k) (let ([s (string k)])
                       ((draw-string v) (make-posn x 50) s
                        (if (pair? kv)
                            BLACK
                            RED))
                       (sleep 0.05) ; slow down so key-ready takes effect
                       (loop (+ x (car ((get-string-size v) s)))))]
          [else (loop x)])))
    (close-viewport v)))

(define (color-accepting-test)
  (let* ([vp (open-viewport "viewport" 200 200)]
	 [operations
	  (list
	   (draw-viewport vp)
	   (lambda (c) ((draw-pixel vp) (make-posn 0 0) c))
	   (lambda (c) ((draw-line vp) (make-posn 0 0) (make-posn 100 100) c))
	   (lambda (c) ((draw-rectangle vp) (make-posn 0 0) 10 10 c))
	   (lambda (c) ((flip-rectangle vp) (make-posn 0 0) 10 10 c))
	   (lambda (c) ((draw-solid-rectangle vp) (make-posn 0 0) 10 10 c))
	   (lambda (c) ((flip-solid-rectangle vp) (make-posn 0 0) 10 10 c))
	   (lambda (c) ((draw-ellipse vp) (make-posn 0 0) 10 10 c))
	   (lambda (c) ((flip-ellipse vp) (make-posn 0 0) 10 10 c))
	   (lambda (c) ((draw-solid-ellipse vp) (make-posn 0 0) 10 10 c))
	   (lambda (c) ((flip-solid-ellipse vp) (make-posn 0 0) 10 10 c))
	   (lambda (c) ((draw-polygon vp)
			(list (make-posn 0 0)
			      (make-posn 10 10)
			      (make-posn 0 10))
			(make-posn 20 20)
			c))
	   (lambda (c) ((draw-solid-polygon vp)
			(list (make-posn 0 0)
			      (make-posn 10 10)
			      (make-posn 0 10))
			(make-posn 20 20)
			c))
	   (lambda (c) ((draw-string vp) (make-posn 40 40) "abcd" c))
	   (lambda (c) (((draw-pixmap-posn (build-path (collection-path "icons") "bb.gif"))
			 vp)
			(make-posn 20 0)
			c))
	   (lambda (c) ((draw-pixmap vp)
			(build-path (collection-path "icons") "bb.gif")
			(make-posn 0 0)
			c)))])
    (for-each (lambda (c)
		(c "red")
		(c "blue")
		(c "black")
		(c (make-rgb .3 .5 .1)))
	      operations)
    (close-viewport vp)))
	     
(struct-test)
(basic-test)
(pixel-test)
(color-test)
(snip-test)
(pixmap-test)
(copy-viewport-test)
(keyboard-test)
(color-accepting-test)

(close-graphics)
