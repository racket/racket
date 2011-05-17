
(load-relative "loadtest.rktl")

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

(define (good m . args)
  (send-generic mdc (make-generic (object-interface mdc) m) . args))

(define (test-all mdc try try-ok)
  (try 'erase)
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

  (try 'get-size)

  (try-ok 'get-background)
  (try-ok 'get-brush)
  (try-ok 'get-clipping-region)
  (try-ok 'get-font)
  (try-ok 'get-pen)
  (try-ok 'get-text-background)
  (try-ok 'get-text-foreground)
  (try-ok 'get-text-mode)
  (try-ok 'get-alpha)
  (try-ok 'get-scale)
  (try-ok 'get-origin)
  (try-ok 'get-rotation)

  (try-ok 'set-background (make-object color% "Yellow"))
  (try-ok 'set-brush (make-object brush% "Yellow" 'solid))
  (try-ok 'set-clipping-rect 0 0 10 10)
  (try-ok 'set-clipping-region (make-object region% mdc))
  (try-ok 'set-font (make-object font% 12 'default 'normal 'normal))
  (try-ok 'set-origin 0 0)
  (try-ok 'set-pen (make-object pen% "Yellow" 1 'solid))
  (try-ok 'set-scale 2 2)
  (try-ok 'set-alpha 0.75)
  (try-ok 'set-text-background (make-object color% "Yellow"))
  (try-ok 'set-text-foreground (make-object color% "Yellow"))
  (try-ok 'set-text-mode 'transparent)

  (try-ok 'get-char-height)
  (try-ok 'get-char-width)

  (try 'try-color (make-object color% "Yellow") (make-object color%)))

(st #f mdc ok?)
(test-all mdc bad good)

(send mdc set-bitmap bm)

(test-all mdc 
          (lambda (m . args)
            (send-generic mdc (make-generic (object-interface mdc) m) . args))
          (lambda (m . args)
            (send-generic mdc (make-generic (object-interface mdc) m) . args)))

(send mdc set-bitmap #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get-pixel, set-pixel, get-argb-pixels, etc.

(require mzlib/etc)

(define (pixel-tests b&w?)
  (begin-with-definitions

   (define bm3 (make-object bitmap% 10 10 b&w?))

   (define mdc (make-object bitmap-dc% bm3))
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
   (when b&w?
     (st 0 col red)
     (st 0 col green)
     (st 0 col blue))

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
		      (test #t `(same red ,what, (send a red) ,(send b red)) (= (send a red) (send b red)))
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
		 (j-loop (add1 j))))))))))

(pixel-tests #f)
(pixel-tests #t)

;; ----------------------------------------

;; Extra get-argb-pixels on monochrome text (from PR 8821):

(let* ((bm (make-object bitmap% 5 5 #t))
       (bm2 (make-object bitmap% 5 5 #t))
       (dc (new bitmap-dc% (bitmap bm)))
       (pt (lambda (x y) (make-object point% x y)))
       (bs (make-bytes 100))
       (bs2 (make-bytes 100)))
  (send dc clear)
  (send dc set-brush (make-object color% 0 0 0) 'solid)
  (send dc draw-polygon (list (pt 2 0) (pt 2 4)
                              (pt 4 4) (pt 4 0)))
  (send dc set-bitmap #f)
  (send bm get-argb-pixels 0 0 5 5 bs)
  (send dc set-bitmap bm2)
  (send dc set-argb-pixels 0 0 5 5 bs)
  (send dc get-argb-pixels 0 0 5 5 bs2)
  (send dc set-bitmap #f)
  (test #t 'mono-bits
        (equal?
         bs
         (bytes-append 
          #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0\377\0\0\0\377\377\377\377\377"
          #"\377\377\377\377\0\0\0\377\0\0\0\377\0\0\0\377\377\377\377\377\377\377\377\377\0\0\0"
          #"\377\0\0\0\377\0\0\0\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0\377\0\0\0"
          #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0\377\0\0\0")))
  (test #t 'same-bits (equal? bs bs2)))

;; ----------------------------------------
;; Test draw-bitmap-section-smooth

(let* ([bm (make-bitmap 100 100)]
       [dc (make-object bitmap-dc% bm)]
       [bm2 (make-bitmap 70 70)]
       [dc2 (make-object bitmap-dc% bm2)]
       [bm3 (make-bitmap 70 70)]
       [dc3 (make-object bitmap-dc% bm3)])
  (send dc draw-ellipse 0 0 100 100)
  (send dc2 draw-bitmap-section-smooth bm 
        10 10 50 50
        0 0 100 100)
  (send dc3 scale 0.5 0.5)
  (send dc3 draw-bitmap bm 20 20)
  (let ([s2 (make-bytes (* 4 70 70))]
        [s3 (make-bytes (* 4 70 70))])
    (send bm2 get-argb-pixels 0 0 70 70 s2)
    (send bm3 get-argb-pixels 0 0 70 70 s3)
    (test #t 'same-scaled (equal? s2 s3))))

;; ----------------------------------------
;; Test some masking combinations

(let ()
  (define u (make-object bitmap% 2 2))
  (define mu (make-object bitmap% 2 2))
  (send u set-argb-pixels 0 0 2 2
        (bytes 255 100 0 0
               255 0 0 0
               255 100 0 0
               255 255 255 255))
  (send mu set-argb-pixels 0 0 2 2
        (bytes 255 0 0 0
               255 255 255 255
               255 0 0 0
               255 255 255 255))
  (send u set-loaded-mask mu)
  (define (try-draw nonce-color mode expect 
                    #:bottom? [bottom? #f])
    (let* ((b&w? (not (eq? mode 'color)))
           (bm (make-object bitmap% 2 2 b&w?))
           (dc (make-object bitmap-dc% bm)))
      (send dc clear)
      (when (eq? mode 'black)
        (send dc set-brush "black" 'solid)
        (send dc draw-rectangle 0 0 2 2))
      ;; Check that draw-bitmap-section really uses the
      ;; section, even in combination with a mask.
      (send dc draw-bitmap-section u 
            0 (if bottom? 1 0)
            0 (if bottom? 1 0) 2 1
            'solid nonce-color (send u get-loaded-mask))
      (send dc set-bitmap #f)
      (let ([s (make-bytes (* 2 2 4))])
        (send bm get-argb-pixels 0 0 2 2 s)
        (when b&w? (send bm get-argb-pixels 0 0 2 2 s #t))
        (test expect 'masked-draw s))))
  (define usual-expect (bytes 255 100 0 0
                              255 255 255 255
                              255 255 255 255
                              255 255 255 255))
  (try-draw (make-object color% "green") 'color usual-expect)
  (try-draw (make-object color%) 'color usual-expect)
  (try-draw (make-object color%) 'white
            ;; For b&w destination, check that the
            ;;  alpha is consistent with the drawn pixels
            (bytes 255 0 0 0
                   0 255 255 255
                   0 255 255 255
                   0 255 255 255))
  (send mu set-argb-pixels 0 0 2 2
        (bytes 255 255 255 255
               255 255 255 255
               255 0 0 0
               255 0 0 0))
  (try-draw (make-object color%) 'black
            #:bottom? #t
            ;; Another b&w destination test, this time
            ;; with a mask that forces black pixels to
            ;; white:
            (bytes 255 0 0 0
                   255 0 0 0
                   255 0 0 0
                   0 255 255 255))
  (send mu set-argb-pixels 0 0 2 2
        (bytes 255 255 255 255
               255 0 0 0
               255 255 255 255
               255 0 0 0))
  (try-draw (make-object color%) 'color
            (bytes 255 255 255 255
                   255 0 0 0
                   255 255 255 255
                   255 255 255 255))
  (let ([dc (make-object bitmap-dc% mu)])
    (send dc erase)
    (send dc set-pen "white" 1 'transparent)
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle 0 0 1 1)
    (send dc set-bitmap #f))
  (try-draw (make-object color%) 'color usual-expect))

;; ----------------------------------------
;; 0 alpha should make the RGB components irrelevant

(let ()
  (define bm1 (make-bitmap 1 2))
  (define bm2 (make-bitmap 1 2))

  (send bm1 set-argb-pixels 0 0 1 2 (bytes 0   0 0 0
                                           255 0 0 255))
  (send bm2 set-argb-pixels 0 0 1 2 (bytes 255 255 0 0
                                           0   0   0 0))

  (define the-bytes (make-bytes 8 0))

  (define bm3 (make-bitmap 1 2))
  (define bdc (make-object bitmap-dc% bm3))
  (void (send bdc draw-bitmap bm1 0 0))
  (void (send bdc draw-bitmap bm2 0 0))

  (send bdc get-argb-pixels 0 0 1 2 the-bytes)
  (test (bytes 255 255 0 0
               255 0   0 255)
        values
        the-bytes))

;; ----------------------------------------

;; Check B&W drawing to B&W, 'solid vs. 'opaque
(let ([mk
       (lambda (expect style bg-col col mask?)
         (let* ((bm1 (make-object bitmap% 2 2 #t))
                (bm2 (make-object bitmap% 2 2 #t))
                (bm3 (make-object bitmap% 2 2 #t))
                (dc1 (new bitmap-dc% (bitmap bm1)))
                (dc2 (new bitmap-dc% (bitmap bm2)))
                (dc3 (new bitmap-dc% (bitmap bm3)))
                (s (make-bytes 16)))
           (send dc1 clear)
           (send dc1 set-argb-pixels 0 0 2 1 #"\xFF\0\0\0\xFF\0\0\0")
           (send dc2 clear)
           (send dc2 set-argb-pixels 0 1 2 1 #"\xFF\0\0\0\xFF\0\0\0")
           (send dc3 set-argb-pixels 0 0 2 2 (bytes-append #"\xFF\0\0\0\xFF\xFF\xFF\xFF"
                                                           #"\xFF\0\0\0\xFF\xFF\xFF\xFF"))
           (send dc2 set-background bg-col)
           (send dc2 draw-bitmap bm1 0 0 style col (and mask? bm3))
           (send dc2 set-bitmap #f)
           (send bm2 get-argb-pixels 0 0 2 2 s)
           (let ([col->str (lambda (c)
                             (if (zero? (send c red)) "black" "white"))])
             (test expect `(mk ,style ,(col->str bg-col) ,(col->str col), mask?) s))))]
      [black (make-object color%)]
      [white (make-object color% 255 255 255)])
  (mk #"\377\0\0\0\377\0\0\0\377\0\0\0\377\0\0\0" 'solid white black #f)
  (mk #"\377\0\0\0\377\0\0\0\377\0\0\0\377\0\0\0" 'solid black black #f)
  (mk #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0" 'solid black white #f)
  (mk #"\377\0\0\0\377\377\377\377\377\0\0\0\377\0\0\0" 'solid white black #t)
  (mk #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0" 'solid white white #t)
  (mk #"\377\0\0\0\377\0\0\0\377\377\377\377\377\377\377\377" 'opaque white black #f)
  (mk #"\377\0\0\0\377\0\0\0\377\0\0\0\377\0\0\0" 'opaque black black #f)
  (mk #"\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377" 'opaque white white #f)
  (mk #"\377\0\0\0\377\377\377\377\377\377\377\377\377\0\0\0" 'opaque white black #t)
  (mk #"\377\377\377\377\377\377\377\377\377\0\0\0\377\0\0\0" 'opaque black white #t))

;; ----------------------------------------

(let ()
  (define (get-column-alpha bm x y)
    (define bs (make-bytes 4))
    (send bm get-argb-pixels x y 1 1 bs #t)
    bs)
  (define abm (make-object bitmap% 2 2 #f #t))
  (define nbm (make-object bitmap% 2 2 #f #f))
  (define (avg bstr) (- 255
                        (quotient (+ (bytes-ref bstr 0)
                                     (bytes-ref bstr 1)
                                     (bytes-ref bstr 2))
                                  3)))
  (send abm set-argb-pixels 0 0 2 2 #"0123456789abcdef")
  (send nbm set-argb-pixels 0 0 2 2 #"0123456789abcdef")  

  (test (bytes (char->integer #\0) 0 0 0) 'a0+0 (get-column-alpha abm 0 0))
  (test (bytes (char->integer #\4) 0 0 0) 'a1+0 (get-column-alpha abm 1 0))
  (test (bytes (char->integer #\8) 0 0 0) 'a0+1 (get-column-alpha abm 0 1))
  (test (bytes (char->integer #\c) 0 0 0) 'a1+1 (get-column-alpha abm 1 1))

  (test (bytes (avg #"123") 0 0 0) 'n0+0 (get-column-alpha nbm 0 0))
  (test (bytes (avg #"567") 0 0 0) 'n1+0 (get-column-alpha nbm 1 0))
  (test (bytes (avg #"9ab") 0 0 0) 'n0+1 (get-column-alpha nbm 0 1))
  (test (bytes (avg #"def") 0 0 0) 'n1+1 (get-column-alpha nbm 1 1)))


;; ----------------------------------------

(report-errs)
