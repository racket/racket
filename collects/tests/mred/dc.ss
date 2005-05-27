
(load-relative "loadtest.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               DC Tests                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mdc (make-object bitmap-dc%))
(define bm (make-object bitmap% 10 10))
(define bm2 (make-object bitmap% 10 10))

(test #t 'is-color? (send bm is-color?))

(define (bad m . args)
  (with-handlers ([exn:fail:contract?
		   (lambda (x)
		     (test '("ok")
			   `(send <bad-dc> ,m ...)
			   (regexp-match "ok" (exn-message x))))])
    (send-generic mdc (make-generic (object-interface mdc) m) . args)
    (error 'bad-dc "~a shouldn't succeed" `(send <bad-dc> ,m ...))))

(define (test-all mdc try)
  (try 'clear)
  (try 'draw-arc 0 0 10 10 0.1 0.2)
  (try 'draw-bitmap bm2 0 0)
  (try 'draw-bitmap-section bm2 0 0 0 0 5 5)
  (try 'draw-ellipse 0 0 10 10)
  (try 'draw-line 0 0 10 10)
  (try 'draw-lines (list (make-object point% 0 0) (make-object point% 10 10)))
  (try 'draw-point 5 5)
  (try 'draw-polygon (list (make-object point% 0 0) (make-object point% 10 10) (make-object point% 5 10)))
  (try 'draw-rectangle 0 0 10 10)
  (try 'draw-rounded-rectangle 0 0 10 10)
  (try 'draw-spline 0 0 10 10 5 10)
  (try 'draw-text "Hello" 0 0)

  (try 'start-doc "Ok")
  (try 'start-page)
  (try 'end-page)
  (try 'end-doc)

  (try 'get-background)
  (try 'get-brush)
  (try 'get-clipping-region)
  (try 'get-font)
  (try 'get-pen)
  (try 'get-size)
  (try 'get-text-background)
  (try 'get-text-foreground)
  (try 'get-text-mode)

  (try 'set-background (make-object color% "Yellow"))
  (try 'set-brush (make-object brush% "Yellow" 'solid))
  (try 'set-clipping-rect 0 0 10 10)
  (try 'set-clipping-region (make-object region% mdc))
  (try 'set-font (make-object font% 12 'default 'normal 'normal))
  (try 'set-origin 0 0)
  (try 'set-pen (make-object pen% "Yellow" 1 'solid))
  (try 'set-scale 2 2)
  (try 'set-text-background (make-object color% "Yellow"))
  (try 'set-text-foreground (make-object color% "Yellow"))
  (try 'set-text-mode 'transparent)
  (try 'try-color (make-object color% "Yellow") (make-object color%)))

(st #f mdc ok?)
(test-all mdc bad)

(send mdc set-bitmap bm)
(test-all mdc (lambda (m . args)
		(send-generic mdc (make-generic (object-interface mdc) m) . args)))

(send mdc set-bitmap #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get-pixel, set-pixel, get-argb-pixels, etc.

;; New DC to reset background, etc:
(define mdc (make-object bitmap-dc% bm))
(send mdc clear)

(define col (make-object color%))
(define bts (make-bytes 40))

(st #f mdc get-pixel 30 4 col)
(st #t mdc get-pixel 3 4 col)
(st 255 col red)
(st 255 col green)
(st 255 col blue)
(stv mdc get-argb-pixels 0 0 2 5 bts)
(test #t 'same-str (equal? (make-bytes 40 255) bts))

(send col set 30 40 50)
(send mdc try-color col col)
(send mdc set-pixel 3 4 col)

(stv mdc get-argb-pixels 2 1 2 5 bts)
(test #t 'same-str (equal? (bytes-append (make-bytes 28 255)
					 (bytes 255
						(send col red)
						(send col green)
						(send col blue))
					 (make-bytes 8 255))
			   bts))

(define col2 (make-object color% 130 140 150))
(send mdc try-color col2 col2)
(let loop ([i 0])
  (unless (= i 10)
    (bytes-set! bts (+ 0 (* i 4)) 255)
    (bytes-set! bts (+ 1 (* i 4)) (send col2 red))
    (bytes-set! bts (+ 2 (* i 4)) (send col2 green))
    (bytes-set! bts (+ 3 (* i 4)) (send col2 blue))
    (loop (add1 i))))
(stv mdc set-argb-pixels 5 5 5 2 bts)
(let ([col3 (make-object color%)]
      [white (make-object color% 255 255 255)]
      [check-col (lambda (what a b)
		   (test #t `(same red ,what) (= (send a red) (send b red)))
		   (test #t `(same green ,what) (= (send a green) (send b green)))
		   (test #t `(same blue ,what) (= (send a blue) (send b blue))))])
  (let i-loop ([i 0])
    (unless (= i 10)
      (let j-loop ([j 0])
	(if (= j 10)
	    (i-loop (add1 i))
	    (begin
	      (st #t mdc get-pixel i j col3)
	      (cond
	       [(and (= i 3) (= j 4))
		(check-col '(3 4) col col3)]
	       [(and (<= 5 i 9)
		     (<= 5 j 6))
		(check-col `(,i ,j) col2 col3)]
	       [else
		(check-col `(,i ,j) white col3)])
	      (j-loop (add1 j))))))))


(report-errs)
