#lang racket/load

(load-relative (collection-file-path "loadtest.rktl" "tests/racket"))

(require racket/draw
         racket/snip
         teachpack/htdp/image
         htdp/error
         lang/posn
         mzlib/list
         lang/imageeq)

(define-values (image-snip1 image-snip2)
  (let ()
    (define size 2)
    
    (define (do-draw c-bm m-bm)
      (let ([bdc (make-object bitmap-dc% c-bm)])
        (send bdc clear)
        (send bdc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
        (send bdc set-brush (send the-brush-list find-or-create-brush "red" 'solid))
        (send bdc draw-rectangle 0 0 size size)
        (send bdc set-bitmap m-bm)
        (send bdc clear)
        (send bdc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
        (send bdc set-brush (send the-brush-list find-or-create-brush "black" 'solid))
        (send bdc draw-rectangle 0 0 (/ size 2) size)
        (send bdc set-bitmap #f)))
    
    (define image-snip1 
      (let* ([c-bm (make-object bitmap% size size)]
             [m-bm (make-object bitmap% size size #t)])
        (do-draw c-bm m-bm)
        (make-object image-snip% c-bm m-bm)))
    
    (define image-snip2 
      (let* ([c-bm (make-object bitmap% size size)]
             [m-bm (make-object bitmap% size size)])
        (do-draw c-bm m-bm)
        (send c-bm set-loaded-mask m-bm)
        (make-object image-snip% c-bm)))
    
    (values image-snip1 image-snip2)))

(define image-snip3 (make-object image-snip%))

;; check-on-bitmap : symbol snip -> void
;; checks on various aspects of the bitmap snips to make
;; sure that they draw properly
(define (check-on-bitmap name snp)
  (let-values ([(width height) (send snp get-size)])
    (let ([bdc (make-object bitmap-dc%)]
          [max-difference
           (lambda (s1 s2)
             (cond
               [(and (zero? (bytes-length s1))
                     (zero? (bytes-length s2)))
                0]
               [else
                (apply max
                       (map (lambda (x y) (abs (- x y))) 
                            (bytes->list s1)
                            (bytes->list s1)))]))])
      
      ;; test that no drawing is outside the snip's drawing claimed drawing area
      (let* ([extra-space 100]
             [bm-width (+ width extra-space)]
             [bm-height (+ height extra-space)]
             [bm-clip (make-object bitmap% bm-width bm-height)]
             [bm-noclip (make-object bitmap% bm-width bm-height)]
             [s-clip (make-bytes (* bm-width bm-height 4))]
             [s-noclip (make-bytes (* bm-width bm-height 4))]
             [s-trunc (make-bytes (* bm-width bm-height 4))])
        (send bdc set-bitmap bm-clip)
        (send bdc clear)
        (send bdc set-clipping-rect (/ extra-space 2) (/ extra-space 2) width height) 
        (send snp draw bdc (/ extra-space 2) (/ extra-space 2) 0 0 (+ width extra-space) (+ height extra-space) 0 0 #f)
        (send bdc set-clipping-region #f)
        (send bdc get-argb-pixels 0 0 (+ width extra-space) (+ height extra-space) s-clip)
        
        (send bdc set-bitmap bm-noclip)
        (send bdc clear)
        (send snp draw bdc (/ extra-space 2) (/ extra-space 2) 0 0 (+ width extra-space) (+ height extra-space) 0 0 #f)
        (send bdc get-argb-pixels 0 0 (+ width extra-space) (+ height extra-space) s-noclip)
        (send bdc set-bitmap #f)
        
        (test (list 'bmclip name #t) (lambda () (list 'bmclip name (equal? s-clip s-noclip))))
        
        (send bdc set-bitmap bm-noclip)
        (send bdc set-pen "black" 1 'transparent)
        (send bdc set-brush "white" 'solid)
        (send bdc draw-rectangle 0 0 (/ extra-space 2) bm-height)
        (send bdc draw-rectangle (- bm-width (/ extra-space 2)) 0 (/ extra-space 2) bm-height)
        (send bdc draw-rectangle 0 0 bm-width (/ extra-space 2))
        (send bdc draw-rectangle 0 (- bm-height (/ extra-space 2)) bm-width (/ extra-space 2))
        (send bdc get-argb-pixels 0 0 (+ width extra-space) (+ height extra-space) s-trunc)
        
        (test (list 'bmtrunc name #t) (lambda () (list 'bmtrunc name (equal? s-noclip s-trunc)))))
        
      (let ([bm-normal (make-object bitmap% (max 1 width) (max 1 height))]
            [bm-bitmap (make-object bitmap% (max 1 width) (max 1 height))]
            [s-normal (make-bytes (* (max 1 width) (max 1 height) 4))]
            [s-bitmap (make-bytes (* (max 1 width) (max 1 height) 4))])
        
        (send bdc set-bitmap bm-normal)
        (send bdc clear)
        (send snp draw bdc 0 0 0 0 width height 0 0 #f)
        (send bdc get-argb-pixels 0 0 width height s-normal)
        (send bdc set-bitmap bm-bitmap)
        (send bdc clear)
        
        ;; force the snip to switch over to bitmap mode
        (send snp get-argb)
        
        (send snp draw bdc 0 0 0 0 width height 0 0 #f)
        (send bdc get-argb-pixels 0 0 width height s-bitmap)
        (send bdc set-bitmap #f)
        (test (list 'bmsame name #t) 
              (lambda () (list 'bmsame name 
                               (<= (max-difference s-normal s-bitmap) 2))))))))

(test #t 'image? (image? (rectangle 10 10 'solid 'blue)))
(test #t 'image? (image? (rectangle 10 10 "solid" 'blue)))
(test #t 'image? (image? (rectangle 10 10 'outline 'blue)))
(test #t 'image? (image? (rectangle 10 10 "outline" 'blue)))
(test #f 'image? (image? 5))

(define red (make-color 255 0 0))
(define blue (make-color 0 0 255))
(define black (make-color 0 0 0))
(define white (make-color 255 255 255))

(define awhite (make-alpha-color 0 255 255 255))
(define ablack (make-alpha-color 0 0 0 0))
(define ared (make-alpha-color 0 255 0 0))
(define aclr (make-alpha-color 255 0 0 0))

(define (p00 i) (move-pinhole i (- (pinhole-x i)) (- (pinhole-y i))))
(define (ignore x) 'ignore)

(test 3 
      'pinhole-x
      (pinhole-x (rectangle 6 8 'solid 'black)))
(test 4
      'pinhole-y
      (pinhole-y (rectangle 6 8 'solid 'black)))
(test 1
      'move-pinhole1
      (pinhole-x (move-pinhole (rectangle 6 8 'solid 'black) -2 -2)))
(test 2
      'move-pinhole2
      (pinhole-y (move-pinhole (rectangle 6 8 'solid 'black) -2 -2)))

(test (list red)
      'color-list
      (image->color-list (rectangle 1 1 'solid 'red)))

(test (list (list red) (list blue) (list black) (list white))
      'colors-set-up-properly
      (list (image->color-list (rectangle 1 1 'solid 'red))
            (image->color-list (rectangle 1 1 'solid 'blue))
            (image->color-list (rectangle 1 1 'solid 'black))
            (image->color-list (rectangle 1 1 'solid 'white))))

(test (list blue blue blue blue)
      'color-list
      (image->color-list (rectangle 2 2 'solid 'blue)))

(test (list blue blue blue
            blue blue blue
            blue blue blue)
      'color-list2
      (image->color-list (rectangle 3 3 'solid 'blue)))
(test (list blue blue blue
            blue blue blue
            blue blue blue)
      'color-list2
      (image->color-list (rectangle 3 3 "solid" 'blue)))
(test (list blue blue blue
            blue white blue
            blue blue blue)
      'color-list2
      (image->color-list (rectangle 3 3 'outline 'blue)))
(test (list blue blue blue
            blue white blue
            blue blue blue)
      'color-list3
      (image->color-list (rectangle 3 3 "outline" 'blue)))

(test #t
      'color-list4
      (image=? (color-list->image (list blue blue blue blue) 2 2 0 0)
               (p00 (rectangle 2 2 'solid 'blue))))
(test #f
      'color-list5
      (image=? (color-list->image (list blue blue blue blue) 2 2 0 0)
               (rectangle 1 4 'solid 'blue)))
(test #t
      'color-list6
      (image=? (color-list->image (list blue blue blue blue) 1 4 0 0)
               (p00 (rectangle 1 4 'solid 'blue))))
(test #t
      'color-list7
      (image=? (color-list->image (list 'blue 'blue 'blue 'blue) 2 2 0 0)
               (p00 (rectangle 2 2 'solid 'blue))))

(test 10
      'color-list8
      (image-width (color-list->image '() 10 0 0 0)))

(test 0
      'color-list9
      (image-height (color-list->image '() 10 0 0 0)))

(test 0
      'color-list10
      (image-width (color-list->image '() 0 10 0 0)))

(test 10
      'color-list11
      (image-height (color-list->image '() 0 10 0 0)))

(test 3
      'color-list12
      (pinhole-x (color-list->image '() 10 0 3 0)))

(test 3
      'color-list13
      (pinhole-y (color-list->image '() 0 10 0 3)))



(test #t
      'alpha-color-list1
      (equal? (make-alpha-color 0 255 0 0)
              (car (image->alpha-color-list (rectangle 1 1 'solid 'red)))))
(test #t
      'alpha-color-list2
      (equal? (make-alpha-color 0 255 0 0)
              (car (image->alpha-color-list (rectangle 1 1 "solid" 'red)))))

(test #t
      'alpha-color-list3
      (andmap (lambda (x) (equal? x (make-alpha-color 0 255 0 0)))
              (image->alpha-color-list (rectangle 1 1 "solid" 'red))))
(test #t
      'alpha-color-list4
      (andmap (lambda (x) (equal? x (make-alpha-color 0 255 0 0)))
              (image->alpha-color-list (rectangle 1 1 'solid 'red))))

(test #t
      'alpha-color-list5
      (equal? (make-alpha-color 0 0 255 0)
              (car (image->alpha-color-list (rectangle 1 1 'solid 'green)))))
(test #t
      'alpha-color-list6
      (equal? (make-alpha-color 0 0 0 255)
              (car (image->alpha-color-list (rectangle 1 1 'solid 'blue)))))

(test #t
      'alpha-color-list7
      (= (image-width
          (alpha-color-list->image
           (list ared aclr ared
                 aclr aclr aclr)
           3
           2
           0
           0))
         3))

(test #t
      'alpha-color-list8
      (= (image-height
          (alpha-color-list->image
           (list ared aclr ared
                 aclr aclr aclr)
           3
           2
           0
           0))
         2))

(test #t
      'alpha-color-list9
      (equal? (image->color-list
               (alpha-color-list->image
                (list ared aclr ared
                      aclr aclr aclr)
                3 2 0 0))
              (list red   white red
                    white white white)))
(test #t
      'alpha-color-list10
      (equal? (image->color-list
               (overlay
                (p00 (rectangle 3 3 'solid 'blue))
                (p00 (alpha-color-list->image
                      (list ared aclr ared
                            aclr aclr aclr
                            ared aclr ared)
                      3
                      3
                      0
                      0))))
              (list red  blue red
                    blue blue blue
                    red  blue red)))

(test 10
      'alpha-color-list11
      (image-width (alpha-color-list->image '() 10 0 0 0)))

(test 0
      'alpha-color-list12
      (image-height (alpha-color-list->image '() 10 0 0 0)))

(test 0
      'alpha-color-list13
      (image-width (alpha-color-list->image '() 0 10 0 0)))

(test 10
      'alpha-color-list14
      (image-height (alpha-color-list->image '() 0 10 0 0)))


(test 3
      'alpha-color-list15
      (pinhole-x (alpha-color-list->image '() 10 0 3 0)))

(test 3
      'alpha-color-list16
      (pinhole-y (alpha-color-list->image '() 0 10 0 3)))


(test #t
      'image=?1
      (image=? (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1 0 0)
               (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1 0 0)))

;; different pinholes => different images
(test #f
      'image=?1b
      (image=? (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1 1 0)
               (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1 0 0)))

(test #f
      'image=?1c
      (image=? (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1 0 0)
               (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1 0 1)))

(test #t
      'image=?2
      (image=? (alpha-color-list->image (list (make-alpha-color 255 100 100 100)) 1 1 0 0)
               (alpha-color-list->image (list (make-alpha-color 255 200 200 200)) 1 1 0 0)))

(test #f
      'image=?3
      (image=? (alpha-color-list->image (list (make-alpha-color 200 100 100 100)) 1 1 0 0)
               (alpha-color-list->image (list (make-alpha-color 200 200 200 200)) 1 1 0 0)))

(test #f
      'image=?4
      (image=? (alpha-color-list->image (list (make-alpha-color 200 100 150 175)
                                              (make-alpha-color 200 100 150 175))
                                        1
                                        2
                                        0
                                        0)
               (alpha-color-list->image (list (make-alpha-color 200 100 150 175)
                                              (make-alpha-color 200 100 150 175)) 
                                        2
                                        1
                                        0
                                        0)))

(test #f
      'image=?5
      (image=? (rectangle 4 4 'outline 'black) 
               (overlay/xy 
                (rectangle 4 4 'outline 'black)
                -2
                -2
                (circle 2 'solid 'red))))

(test #t
      'overlay
      (image=? (color-list->image (list blue red blue red) 2 2 0 0)
               (overlay (p00 (rectangle 2 2 'solid 'red))
                        (p00 (rectangle 1 2 'solid 'blue)))))

(test #t
      'overlay/multiple
      (image=? (overlay (rectangle 6 6 'solid 'red)
                        (overlay (rectangle 4 4 'solid 'white)
                                 (rectangle 2 2 'solid 'blue)))
               (overlay (rectangle 6 6 'solid 'red)
                        (rectangle 4 4 'solid 'white)
                        (rectangle 2 2 'solid 'blue))))

(test #t
      'overlay/empty-spaces-are-unmasked
      (image=? (color-list->image (list red red red blue) 2 2 0 0)
               (overlay
                (p00 (rectangle 2 2 'solid 'blue))
                (overlay (p00 (rectangle 1 2 'solid 'red))
                         (p00 (rectangle 2 1 'solid 'red))))))

(test #t
      'overlay/xy1
      (image=? (color-list->image (list red blue red blue) 2 2 0 0)
               (overlay/xy (p00 (rectangle 2 2 'solid 'red))
                           1 0
                           (p00 (rectangle 1 2 'solid 'blue)))))

(test #t
      'overlay/xy2
      (image=? (color-list->image (list red red red blue) 2 2 0 0)
               (overlay/xy (p00 (rectangle 2 2 'solid 'red))
                           1 1
                           (p00 (rectangle 1 1 'solid 'blue)))))

(test #t
      'overlay/xy3
      (image=? (color-list->image (list red red blue blue) 2 2 0 0)
               (overlay/xy (p00 (rectangle 2 1 'solid 'red))
                           0 1
                           (p00 (rectangle 2 1 'solid 'blue)))))

(test #t
      'overlay/xy4
      (image=? (color-list->image (list blue blue red red) 2 2 0 0)
               (p00 (overlay/xy (p00 (rectangle 2 1 'solid 'red))
                                0 -1
                                (p00 (rectangle 2 1 'solid 'blue))))))

(test #t
      'overlay/xy/white
      (image=? (alpha-color-list->image (list ablack ablack ablack
                                              ablack awhite ablack
                                              ablack ablack ablack)
                                        3 3 0 0)
               (overlay/xy (p00 (rectangle 3 3 'solid 'black))
                           1 1
                           (p00 (rectangle 1 1 'solid 'white)))))

(test #t
      'color-list->image/white-in-mask
      (image=? (color-list->image (list black red   black
                                        red   red   red
                                        black red   black)
                                  3 3 0 0)
               (overlay (p00 (rectangle 3 3 'solid 'red))
                        (color-list->image (list black white black
                                                 white white white
                                                 black white black)
                                           3 3 0 0))))


(test #t
      'overlay
      (image=? (color-list->image (list red blue red red blue red) 3 2 0 0)
               (overlay/xy (p00 (rectangle 3 2 'solid 'red))
                           1 0
                           (p00 (rectangle 1 2 'solid 'blue)))))

(test #t
      'image=?-zero1
      (image=? (rectangle 0 10 'solid 'red)
               (rectangle 0 10 'solid 'red)))
(test #t
      'image=?-zero2
      (image=? (rectangle 0 10 'solid 'red)
               (rectangle 0 10 'solid 'blue)))
(test #f
      'image=?-zero3
      (image=? (rectangle 0 5 'solid 'red)
               (rectangle 0 4'solid 'blue)))

(test #t
      'image-inside?1
      (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
                                 1 0
                                 (p00 (rectangle 1 2 'solid 'blue)))
                     (rectangle 1 2 'solid 'blue)))

(test #f
      'image-inside?2
      (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
                                 1 0
                                 (p00 (rectangle 1 2 'solid 'blue)))
                     (rectangle 1 2 'solid 'black)))

(test #t
      'image-inside?3
      (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
                                 1 0
                                 (p00 (rectangle 1 2 'solid 'blue)))
                     (rectangle 1 2 'solid 'red)))

(test #f
      'image-inside?4
      (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
                                 1 0
                                 (p00 (rectangle 1 2 'solid 'blue)))
                     (rectangle 2 1 'solid 'red)))

(test #t
      'image-inside?5
      (image-inside? (alpha-color-list->image (list (make-alpha-color 0 255 0 0)) 1 1 0 0)
                     (alpha-color-list->image (list (make-alpha-color 255 0 0 0)) 1 1 0 0)))

(test #f
      'image-inside?6
      (image-inside? (overlay/xy (p00 (rectangle 3 2 'solid 'red))
                                 1 0
                                 (p00 (rectangle 1 2 'solid 'blue)))
                     (color-list->image (list blue white white) 
                                        3 1 0 0)))

(test #t
      'image-inside?7
      (image-inside? (overlay/xy (p00 (rectangle 16 16 'solid 'red))
                                 2 5
                                 (p00 (ellipse 6 6 'outline 'blue)))
                     (ellipse 6 6 'outline 'blue)))

(test #t
      'image-inside?8
      (image-inside?
       (overlay (p00 (rectangle (image-width (text "x" 12 'red))
                                (image-height (text "x" 12 'red))
                                'solid 
                                'white))
                (text "x" 12 'red))
       (text "x" 12 'red)))

(test #t
      'image-inside?9
      (image-inside?
       (text "y x y" 12 'red)
       (text "x" 12 'red)))

(test (make-posn 2 5)
      'find-image1
      (find-image (overlay/xy (p00 (rectangle 16 16 'solid 'red))
                              2 5
                              (p00 (ellipse 6 6 'outline 'blue)))
                  (p00 (ellipse 6 6 'outline 'blue))))

(test (make-posn 0 0)
      'find-image2
      (find-image (p00 (rectangle 16 16 'solid 'blue))
                  (p00 (ellipse 6 6 'outline 'blue))))

(test (make-posn 1 1)
      'find-image3
      (find-image (overlay/xy (rectangle 10 10 'solid 'blue)
                              1
                              1
                              (ellipse 5 5 'solid 'red))
                  (ellipse 5 5 'solid 'red)))

(test 5
      'image-width
      (image-width (rectangle 5 7 'solid 'red)))

(test 7
      'image-height
      (image-height (rectangle 5 7 'solid 'red)))

(test 1 'color-red (color-red (make-color 1 2 3)))
(test 2 'color-green (color-green (make-color 1 2 3)))
(test 3 'color-blue (color-blue (make-color 1 2 3)))
(test #t 'color?1 (color? (make-color 1 2 3)))
(test #f 'color?2 (color? 10))
(test #t 'image-color?1 (image-color? (make-color 1 2 3)))
(test #t 'image-color?2 (image-color? "blue"))
(test #t 'image-color?3 (image-color? 'blue))
(test #f 'image-color?4 (image-color? 10))
(test #f 'image-color?5 (image-color? "not-a-color"))
(test #f 'image-color?6 (image-color? 'not-a-color))

(test #t
      'line 
      (image=? (line 4 0 'red)
               (color-list->image (list red red red red red) 5 1 0 0)))

(test #t
      'line 
      (image=? (line 0 4 'red)
               (color-list->image (list red red red red red) 1 5 0 0)))

(test #t
      'line 
      (image=? (line 0 -4 'red)
               (color-list->image (list red red red red red) 1 5 0 4)))

(test #t
      'line 
      (image=? (line -4 0 'red)
               (color-list->image (list red red red red red) 5 1 4 0)))

;; note: next two tests may be platform-specific... I'm not sure.
;; I developed them under macos x. -robby
(test #t
      'triangle1
      (image=? (p00 (triangle 3 'outline 'red))
               (color-list->image 
                (list white red   white
                      white red   white
                      red   white red
                      red   red   red)
                3
                4
                0
                0)))

(test #t
      'triangle2
      (image=? (p00 (triangle 3 'solid 'red))
               (color-list->image 
                (list white red   white
                      white red   white
                      red   red   red
                      red   red   red)
                3
                4
                0
                0)))

;; make sure star is relatively white
(test #t
      'star1
      (>= 100
          (length (filter (λ (x) (equal? (make-color 255 0 0) x))
                          (image->color-list (star 4 5 10 'outline 'red))))))

;; make solid star is relatively colored
(test #t
      'star2
      (<= 100
          (length (filter (λ (x) (equal? (make-color 255 0 0) x))
                          (image->color-list (star 4 5 10 'solid 'red))))))

;; make sure star is relatively white
(test #t
      'regular-polygon1
      (>= 100
          (length (filter (λ (x) (equal? (make-color 255 0 0) x))
                          (image->color-list (regular-polygon 4 10 'outline 'red))))))

;; make solid star is relatively colored
(test #t
      'regular-polygon2
      (<= 100
          (length (filter (λ (x) (equal? (make-color 255 0 0) x))
                          (image->color-list (regular-polygon 4 10 'solid 'red))))))

(test #t
      'add-line1
      (image=? (overlay (p00 (rectangle 5 4 'solid 'black))
                        (p00 (rectangle 1 4 'solid 'red)))
               (p00 (add-line (p00 (rectangle 4 4 'solid 'black))
                              -1 0
                              -1 3
                              'red))))

(test #t
      'add-line2
      (image=? (overlay (p00 (rectangle 4 5 'solid 'black))
                        (p00 (rectangle 4 1 'solid 'red)))
               (p00 (add-line (p00 (rectangle 4 4 'solid 'black))
                              0 -1
                              3 -1
                              'red))))

(test 7
      'add-line3
      (image-width (add-line (rectangle 7 7 'solid 'black)
                             -3 0
                             2 0
                             'red)))

(test #t
      'add-line4
      (image=? (overlay (rectangle 6 6 'solid 'blue)
                        (rectangle 6 1 'solid 'red))
               (add-line (rectangle 6 6 'solid 'blue)
                         -3 0
                         2 0
                         'red)))

(test 26
      'add-line-w1
      (image-width
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 -20 -20
                 0 0
                 'red)))
(test 26
      'add-line-w2
      (image-width
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 -20 20
                 0 0
                 'red)))
(test 26
      'add-line-w3
      (image-width
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 20 -20
                 0 0
                 'red)))
(test 26
      'add-line-w4
      (image-width
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 20 20
                 0 0
                 'red)))

(test 26
      'add-line-w5
      (image-width
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 0 0
                 -20 -20
                 'red)))
(test 26
      'add-line-w6
      (image-width
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 0 0
                 -20 20
                 'red)))
(test 26
      'add-line-w7
      (image-width
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 0 0
                 20 -20
                 'red)))
(test 26
      'add-line-w8
      (image-width
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 0 0
                 20 20
                 'red)))

(test 26
      'add-line-h1
      (image-height
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 -20 -20
                 0 0
                 'red)))
(test 26
      'add-line-h2
      (image-height
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 -20 20
                 0 0
                 'red)))
(test 26
      'add-line-h3
      (image-height
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 20 -20
                 0 0
                 'red)))
(test 26
      'add-line-h4
      (image-height
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 20 20
                 0 0
                 'red)))

(test 26
      'add-line-h5
      (image-height
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 0 0
                 -20 -20
                 'red)))
(test 26
      'add-line-h6
      (image-height
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 0 0
                 -20 20
                 'red)))
(test 26
      'add-line-h7
      (image-height
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 0 0
                 20 -20
                 'red)))
(test 26
      'add-line-h8
      (image-height
       (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                 0 0
                 20 20
                 'red)))

(test 5
      image-width
      (add-line (line 0 0 'red) 3.141597 3 0 0 'black))

(test 'ignore
      (lambda (x) 'ignore)
      ;; Make sure that inexact coordinate doesn't create inexact
      ;;  width:
      (send (add-line (line 0 0 'red) 3.141597 3 0 0 'black) get-argb))

(test (list 3 4)
      'ph-ellipse
      (list (pinhole-x (ellipse 6 8 'solid 'red))
            (pinhole-y (ellipse 6 8 'solid 'red))))
(test (list 3 3)
      'ph-circle
      (list (pinhole-x (circle 3 'solid 'red))
            (pinhole-y (circle 3 'solid 'red))))
(test (list 0 0)
      'ph-line
      (list (pinhole-x (line 10 10 'red))
            (pinhole-y (line 10 10 'red))))
(test (list 0 0)
      'ph-text
      (list (pinhole-x (text "10" 10 'red))
            (pinhole-y (text "10" 10 'red))))
(test (list 0 0)
      'ph-text
      (list (pinhole-x (text "" 10 'red))
            (pinhole-y (text "" 10 'red))))

(test (list 3 3)
      'ph-add-line
      (list (pinhole-x (add-line (rectangle 6 6 'solid 'red)
                                 0 0
                                 3 3
                                 'black))
            (pinhole-y (add-line (rectangle 6 6 'solid 'red)
                                 0 0
                                 3 3
                                 'black))))
(test (list 3 4)
      'ph-overlay1
      (list (pinhole-x (overlay (rectangle 6 8 'solid 'red) (rectangle 2 4 'solid 'red)))
            (pinhole-y (overlay (rectangle 6 8 'solid 'red) (rectangle 2 4 'solid 'red)))))
(test (list 0 0)
      'ph-overlay2
      (list (pinhole-x (overlay (move-pinhole (rectangle 6 8 'solid 'red) -3 -4)
                                (move-pinhole (rectangle 2 4 'solid 'red) -1 -2)))
            (pinhole-y (overlay (move-pinhole (rectangle 6 8 'solid 'red) -3 -4)
                                (move-pinhole (rectangle 2 4 'solid 'red) -1 -2)))))
(test (list 5 5)
      'ph-overlay/xy1
      (list (pinhole-x (overlay/xy (move-pinhole (rectangle 6 8 'solid 'red) -3 -4)
                                   -5 -5
                                   (move-pinhole (rectangle 2 4 'solid 'red) -1 -2)))
            (pinhole-y (overlay/xy (move-pinhole (rectangle 6 8 'solid 'red) -3 -4)
                                   -5 -5
                                   (move-pinhole (rectangle 2 4 'solid 'red) -1 -2)))))


(test (image->color-list
       (overlay
        (rectangle 11 11 'solid 'green)
        (shrink (rectangle 11 11 'solid 'red)
                1 1 1 1)))
      'shrinking-twice-shrinks-both-times
      (image->color-list
       (overlay
        (rectangle 11 11 'solid 'green)
        (shrink (shrink (rectangle 11 11 'solid 'red)
                        5 5 5 5)
                1 1 1 1))))

(check-on-bitmap 'solid-rect (rectangle 2 2 'solid 'red))
(check-on-bitmap 'outline-rect (rectangle 2 2 'outline 'red))
(check-on-bitmap 'solid-ellipse (ellipse 2 4 'solid 'red))
(check-on-bitmap 'outline-ellipse (ellipse 2 4 'outline 'red))
(check-on-bitmap 'solid-circle (circle 4 'solid 'red))
(check-on-bitmap 'outline-circle (circle 4 'outline 'red))

(check-on-bitmap '0solid-rect1 (rectangle 0 2 'solid 'red))
(check-on-bitmap '0solid-rect2 (rectangle 2 0 'solid 'red))
(check-on-bitmap '0outline-rect1 (rectangle 2 0 'outline 'red))
(check-on-bitmap '0outline-rect2 (rectangle 0 0 'outline 'red))
(check-on-bitmap '0solid-ellipse1 (ellipse 0 3 'solid 'red))
(check-on-bitmap '0solid-ellipse2 (ellipse 3 0 'solid 'red))
(check-on-bitmap '0outline-ellipse1 (ellipse 0 4 'outline 'red))
(check-on-bitmap '0outline-ellipse2 (ellipse 2 0 'outline 'red))
(check-on-bitmap '0solid-circle (circle 0 'solid 'red))
(check-on-bitmap '0outline-circle (circle 0 'outline 'red))

(check-on-bitmap 'solid-triangle (triangle 10 'solid 'red))
(check-on-bitmap 'outline-triangle (triangle 10 'outline 'red))
(check-on-bitmap 'solid-star (star 4 10 20 'solid 'red))
(check-on-bitmap 'solid-star/reverse-args (star 4 20 10 'solid 'red))
(check-on-bitmap 'outline-star (star 4 10 20 'outline 'red))
(check-on-bitmap 'regular-polygon (regular-polygon 4 10 'outline 'red))
(check-on-bitmap 'line (line 10 7 'red))
; (check-on-bitmap 'text (text "XX" 12 'red)) ;; this test fails for reasons I can't control ... -robby
(check-on-bitmap 'overlay1 (overlay (p00 (rectangle 1 4 'solid 'blue))
                                   (p00 (rectangle 4 1 'solid 'green))))
(check-on-bitmap 'overlay2 (overlay/xy (p00 (rectangle 4 4 'solid 'blue))
                                      2 2
                                      (p00 (rectangle 4 4 'solid 'green))))
(check-on-bitmap 'overlay3 (overlay image-snip1
                                    (rectangle (image-width image-snip1) 
                                               (image-height image-snip1)
                                               'outline
                                               'red)))
(check-on-bitmap 'alpha-color-list
                 (overlay
                  (p00 (rectangle 3 3 'solid 'blue))
                  (alpha-color-list->image
                   (list ared aclr ared
                         aclr aclr aclr
                         ared aclr ared)
                   3
                   3
                   0
                   0)))
(check-on-bitmap 'add-line
                 (add-line
                  (p00 (rectangle 100 100 'solid 'black))
                  -10 -10
                  110 110
                  'red))

(check-on-bitmap 'add-line1
                 (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                           -20 -20
                           0 0
                           'red))
(check-on-bitmap 'add-line2
                 (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                           -20 20
                           0 0
                           'red))
(check-on-bitmap 'add-line3
                 (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                           20 -20
                           0 0
                           'red))
(check-on-bitmap 'add-line4
                 (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                           20 20
                           0 0
                           'red)) 
(check-on-bitmap 'add-line5
                 (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                           0 0
                           -20 -20
                           'red))
(check-on-bitmap 'add-line6
                 (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                           0 0
                           -20 20
                           'red))
(check-on-bitmap 'add-line7
                 (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                           0 0
                           20 -20
                           'red))
(check-on-bitmap 'add-line8
                 (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green))
                           0 0
                           20 20
                           'red))

(check-on-bitmap 'shrink
                 (shrink (shrink (rectangle 11 11 'solid 'red)
                                 5 5 5 5)
                         1 1 1 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test images with zero width or zero height 
;;  for various things
;;


(test 10 image-width (rectangle 10 0 'solid 'red))
(test 0 image-height (rectangle 10 0 'solid 'red))
(test 0 image-width (rectangle 0 10 'solid 'red))
(test 10 image-height (rectangle 0 10 'solid 'red))

(test 0 image-width (text "" 12 'black))
(test #t 'not-zero-empty-string-height (not (zero? (image-height (text "" 12 'black)))))

(test '() image->color-list (rectangle 0 10 'solid 'red))
(test '() image->color-list (rectangle 10 0 'solid 'red))
(test '() image->color-list (rectangle 0 0 'solid 'red))

(test '() image->alpha-color-list (rectangle 0 10 'solid 'red))
(test '() image->alpha-color-list (rectangle 10 0 'solid 'red))
(test '() image->alpha-color-list (rectangle 0 0 'solid 'red))

(test 0 image-width (overlay (circle 0 'solid 'red) (circle 0 'solid 'red)))
(test 0 image-height (overlay (circle 0 'solid 'red) (circle 0 'solid 'red)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test that the image construction functions
;;  accept non-integer values (and floor them)
;;

(test (image->color-list (rectangle 1 1 'solid 'blue))
      'shrink-tl-accepting-non-integers
      (image->color-list (shrink-tl (rectangle 10 10 'solid 'blue) 1.5 #e1.5)))

(test (image->color-list (rectangle 1 1 'solid 'blue))
      'shrink-tr-accepting-non-integers
      (image->color-list (shrink-tr (rectangle 10 10 'solid 'blue) 1.5 #e1.5)))

(test (image->color-list (rectangle 1 1 'solid 'blue))
      'shrink-bl-accepting-non-integers
      (image->color-list (shrink-bl (rectangle 10 10 'solid 'blue) 1.5 #e1.5)))

(test (image->color-list (rectangle 1 1 'solid 'blue))
      'shrink-br-accepting-non-integers
      (image->color-list (shrink-br (rectangle 10 10 'solid 'blue) 1.5 #e1.5)))
      
(test (image->color-list (rectangle 2 2 'solid 'blue))
      'rectangle-accepting-non-integers
      (image->color-list (rectangle #e2.5 2.5 'solid 'blue)))

(test (image->color-list (ellipse 2 2 'solid 'blue))
      'ellipse-accepting-non-integers
      (image->color-list (ellipse #e2.5 2.5 'solid 'blue)))

(test (image->color-list (circle 2 'solid 'blue))
      'circle-accepting-non-integers
      (image->color-list (circle #e2.5 'solid 'blue)))

(test (image->color-list (star 10 20 30 'solid 'blue))
      'star-accepting-non-integers
      (image->color-list (star 10 20.5 #e30.2 'solid 'blue)))

(test (image->color-list (triangle 12 'solid 'blue))
      'triangle-accepting-non-integers
      (image->color-list (triangle 12.5 'solid 'blue)))

(test (image->color-list (line 10 12 'blue))
      'line-accepting-non-integer
      (image->color-list (line 10.5 #e12.5 'blue)))

(test (image->color-list (shrink (rectangle 10 10 'solid 'blue) 4 4 4 4))
      'shrink-accepting-non-integers
      (image->color-list
       (shrink (rectangle 10 10 'solid 'blue)
               4.1
               4.2
               #e4.3
               4.4)))

(test (image->color-list (add-line (rectangle 10 10 'solid 'blue)
                                   0 0 2 2 'red))
      'add-line-accepting-non-integers
      (image->color-list (add-line (rectangle 10 10 'solid 'blue)
                                   0.1 #e.2 2.1 2.2 'red)))

(test #t
      'flooring/rounding-is-consistent
      (image=? (overlay (rectangle 10 10 'solid 'black)
                        (move-pinhole
                         (rectangle 5 5 'solid 'red)
                         (- (+ 5 1/10))
                         (- (+ 5 1/10))))
               (overlay/xy (rectangle 10 10 'solid 'black)
                           (+ 5 1/10)
                           (+ 5 1/10)
                           (rectangle 5 5 'solid 'red))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The tests beginning with "bs-" ensure
;; that the operations all can accept bitmap 
;; snips as arguments
;;

(test #t
      'bs-image?
      (image? image-snip1))
(test #t
      'bs-image?
      (image? image-snip2))
(test #t
      'bs-image=?
      (image=? image-snip1 (send image-snip1 copy)))
(test #f
      'bs-image=?
      ;; They have different masks:
      (image=? image-snip1 image-snip2))
(test 2
      'bs-image-width
      (image-width image-snip1))
(test 2
      'bs-image-width
      (image-width image-snip2))
(test 2
      'bs-image-height
      (image-height image-snip1))
(test 2
      'bs-image-height
      (image-height image-snip2))
(test #t
      'bs-overlay
      (image=? image-snip1 (overlay image-snip1 image-snip2)))
(test #t
      'bs-overlay/xy
      (image=? image-snip1 (overlay/xy image-snip1 0 0 image-snip2)))
(test #t
      'bs-add-line
      (image=?
       (add-line image-snip1 0 0 10 10 'green)
       (add-line image-snip2 0 0 10 10 'green)))
(test #t
      'bs-image-inside?1
      (image-inside? image-snip1 image-snip2))
(test #t
      'bs-image-inside?2
      (image-inside? image-snip1 image-snip2))
(test (make-posn 0 0)
      'bs-find-image1
      (find-image image-snip1 image-snip2))
(test (make-posn 0 0)
      'bs-find-image2
      (find-image image-snip2 image-snip1))
(test #t
      'bs-image->color-list
      (equal? (image->color-list image-snip1)
              (image->color-list image-snip2)))
(test #t
      'bs-image->alpha-color-list
      (equal? (image->alpha-color-list image-snip1)
              (image->alpha-color-list image-snip2)))
(test 1
      'bs-pinhole-x
      (pinhole-x image-snip1))
(test 1
      'bs-pinhole-y
      (pinhole-y image-snip2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test image-snip that doesnt' have a bitmap
;;

(test 20
      'image-snip-no-bitmap1
      (image-width image-snip3))

(test 'passed
      'image-snip-no-bitmap2
      (begin (overlay/xy image-snip3 10 10 image-snip3) 'passed))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test color arguments
;;

(test 'ignore
      'clr-rectangle-sym
      (ignore (rectangle 10 10 'solid 'blue)))
(test 'ignore
      'clr-rectangle-str
      (ignore (rectangle 10 10 'solid "blue")))
(test 'ignore
      'clr-rectangle-clr
      (ignore (rectangle 10 10 'solid (make-color 0 0 255))))

(test 'ignore
      'clr-ellipse-sym
      (ignore (ellipse 10 10 'solid 'blue)))
(test 'ignore
      'clr-ellipse-str
      (ignore (ellipse 10 10 'solid "blue")))
(test 'ignore
      'clr-ellipse-clr
      (ignore (ellipse 10 10 'solid (make-color 0 0 255))))

(test 'ignore
      'clr-circle-sym
      (ignore (circle 10 'solid 'blue)))
(test 'ignore
      'clr-circle-str
      (ignore (circle 10 'solid "blue")))
(test 'ignore
      'clr-circle-clr
      (ignore (circle 10 'solid (make-color 0 0 255))))

(test 'ignore
      'clr-triangle-sym
      (ignore (triangle 10 'solid 'blue)))
(test 'ignore
      'clr-triangle-str
      (ignore (triangle 10 'solid "blue")))
(test 'ignore
      'clr-triangle-clr
      (ignore (triangle 10 'solid (make-color 0 0 255))))

(test 'ignore
      'clr-line-sym
      (ignore (line 10 10 'blue)))
(test 'ignore
      'clr-line-str
      (ignore (line 10 10 "blue")))
(test 'ignore
      'clr-line-clr
      (ignore (line 10 10 (make-color 0 0 255))))

(test 'ignore
      'clr-add-line-sym
      (ignore (add-line (rectangle 1 1 'solid 'blue) 0 0 1 1 'blue)))
(test 'ignore
      'clr-add-line-str
      (ignore (add-line (rectangle 1 1 'solid 'blue) 0 0 1 1 "blue")))
(test 'ignore
      'clr-add-line-clr
      (ignore (add-line (rectangle 1 1 'solid 'blue) 0 0 1 1 (make-color 0 0 255))))

(test 'ignore
      'clr-text-sym
      (ignore (text "abc" 12 'blue)))
(test 'ignore
      'clr-text-str
      (ignore (text "abc" 12 "blue")))
(test 'ignore
      'clr-text-clr
      (ignore (text "abc" 12 (make-color 0 0 255))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test scene-based functions
;;

(test #t
      'nw:rectangle1
      (image=? (put-pinhole (rectangle 10 10 'solid 'black) 0 0)
               (nw:rectangle 10 10 'solid 'black)))

(test #t
      'empty-scene1
      (image=? (put-pinhole 
                (overlay (rectangle 10 10 'solid 'white)
                         (rectangle 10 10 'outline 'black))
                0 0)
               (empty-scene 10 10)))

(test #t
      'place-image1
      (image=?
       (overlay/xy (nw:rectangle 50 50 'solid 'orange)
                   10 12
                   (rectangle 10 10 'solid 'purple))
       (place-image (rectangle 10 10 'solid 'purple)
                    10 12
                    (nw:rectangle 50 50 'solid 'orange))))

(define (tp-exn/num re)
  (lambda (exn)
    (and (exn:fail? exn)
         (regexp-match re (exn-message exn)))))

;; tests that the expression
;;  a) raises a teachpack exception record,
;;  b) has the right argument position, and
;;  c) has the right name.
(define-syntax (err/rt-name-test stx)
  (syntax-case stx ()
    [(_ (name . args) position)
     (identifier? (syntax name))
     (syntax 
      (err/rt-test (name . args)                   
                   (lambda (exn)
                     (and (exn:fail? exn)
                          (let* ([msg (exn-message exn)]
                                 [beg (format "~a:" 'name)]
                                 [len (string-length beg)])
                            (and (regexp-match position msg)
                                 ((string-length msg) . > . len)
                                 (string=? (substring msg 0 len) beg)))))))]))

(err/rt-name-test (image-width 1) "first")
(err/rt-name-test (image-height 1) "first")
(err/rt-name-test (overlay 1 2) "first")
(err/rt-name-test (overlay image-snip1 2) "second")
(err/rt-name-test (overlay image-snip1 image-snip2 3) "3")
(err/rt-name-test (overlay/xy #f #f #f #f) "first")
(err/rt-name-test (overlay/xy image-snip1 #f #f #f) "second")
(err/rt-name-test (overlay/xy image-snip1 1 #f #f) "third")
(err/rt-name-test (overlay/xy image-snip1 1 1 #f) "fourth")
(err/rt-name-test (place-image #f #f #f #f) "first")
(err/rt-name-test (place-image image-snip1 #f #f #f) "second")
(err/rt-name-test (place-image image-snip1 1 #f #f) "third")
(err/rt-name-test (place-image image-snip1 1 1 #f) "fourth")
(err/rt-name-test (pinhole-x 1) "first")
(err/rt-name-test (pinhole-y 1) "first")
(err/rt-name-test (move-pinhole #f #f #f) "first")
(err/rt-name-test (move-pinhole image-snip1 #f #f) "second")
(err/rt-name-test (move-pinhole image-snip1 0 #f) "third")
(err/rt-name-test (rectangle #f #f #f #f) "first")
(err/rt-name-test (rectangle 10 #f #f #f) "second")
(err/rt-name-test (rectangle 10 10 #f #f) "third")
(err/rt-name-test (rectangle 10 10 'solid #f) "fourth")
(err/rt-name-test (nw:rectangle #f #f #f #f) "first")
(err/rt-name-test (nw:rectangle 10 #f #f #f) "second")
(err/rt-name-test (nw:rectangle 10 10 #f #f) "third")
(err/rt-name-test (nw:rectangle 10 10 'solid #f) "fourth")
(err/rt-name-test (empty-scene #f #f) "first")
(err/rt-name-test (empty-scene 10 #f) "second")
(err/rt-name-test (circle #f #f #f) "first")
(err/rt-name-test (circle 10 #f #f) "second")
(err/rt-name-test (circle 10 'solid #f) "third")
(err/rt-name-test (ellipse #f #f #f #f) "first")
(err/rt-name-test (ellipse 10 #f #f #f) "second")
(err/rt-name-test (ellipse 10 10 #f #f) "third")
(err/rt-name-test (ellipse 10 10 'solid #f) "fourth")
(err/rt-name-test (triangle #f #f #f) "first")
(err/rt-name-test (triangle 10 #f #f) "second")
(err/rt-name-test (triangle 10 'solid #f) "third")
(err/rt-name-test (line #f #f #f) "first")
(err/rt-name-test (line 10 #f #f) "second")
(err/rt-name-test (line 10 10 #f) "third")
(err/rt-name-test (add-line #f #f #f #f #f #f) "first")
(err/rt-name-test (add-line image-snip1 #f #f #f #f #f) "second")
(err/rt-name-test (add-line image-snip1 10 #f #f #f #f) "third")
(err/rt-name-test (add-line image-snip1 10 10 #f #f #f) "fourth")
(err/rt-name-test (add-line image-snip1 10 10 11 #f #f) "fifth")
(err/rt-name-test (add-line image-snip1 10 10 11 11 #f) "sixth")
(err/rt-name-test (scene+line #f #f #f #f #f #f) "first")
(err/rt-name-test (scene+line (nw:rectangle 10 10 'solid 'blue) #f #f #f #f #f) "second")
(err/rt-name-test (scene+line (nw:rectangle 10 10 'solid 'blue) 10 #f #f #f #f) "third")
(err/rt-name-test (scene+line (nw:rectangle 10 10 'solid 'blue) 10 10 #f #f #f) "fourth")
(err/rt-name-test (scene+line (nw:rectangle 10 10 'solid 'blue) 10 10 11 #f #f) "fifth")
(err/rt-name-test (scene+line (nw:rectangle 10 10 'solid 'blue) 10 10 11 11 #f) "sixth")
(err/rt-name-test (text #f #f #f) "first")
(err/rt-name-test (text "abc" #f #f) "second")
(err/rt-name-test (text "abc" 10 #f) "third")
(err/rt-name-test (image-inside? #f #f) "first")
(err/rt-name-test (image-inside? image-snip1 #f) "second")
(err/rt-name-test (find-image #f #f) "first")
(err/rt-name-test (find-image image-snip1 #f) "second")
(err/rt-name-test (image->color-list 1) "first")
(err/rt-name-test (color-list->image #f #f #f #f #f) "first")
(err/rt-name-test (color-list->image (list (make-color 0 0 0)) #f #f #f #f) "second")
(err/rt-name-test (color-list->image (list (make-color 0 0 0)) 1 #f #f #f) "third")
(err/rt-name-test (color-list->image (list (make-color 0 0 0)) 1 1 #f #f) "fourth")
(err/rt-name-test (color-list->image (list (make-color 0 0 0)) 1 1 0 #f) "fifth")
(err/rt-name-test (image->alpha-color-list #f) "first")
(err/rt-name-test (alpha-color-list->image #f #f #f #f #f) "first")
(err/rt-name-test (alpha-color-list->image (list (make-alpha-color 0 0 0 0)) #f #f #f #f) "second")
(err/rt-name-test (alpha-color-list->image (list (make-alpha-color 0 0 0 0)) 1 #f #f #f) "third")
(err/rt-name-test (alpha-color-list->image (list (make-alpha-color 0 0 0 0)) 1 1 #f #f) "fourth")
(err/rt-name-test (alpha-color-list->image (list (make-alpha-color 0 0 0 0)) 1 1 0 #f) "fifth")

(err/rt-name-test (overlay/xy #f
                              'wrong
                              10
                              (circle 50 'outline 'blue))
                  "first")
(err/rt-name-test (overlay/xy (rectangle 100 200 'outline 'red)
                              'wrong
                              10
                              (circle 50 'outline 'blue))
                  "second")
(err/rt-name-test (overlay/xy (rectangle 100 200 'outline 'red)
                              10
                              'wrong
                              (circle 50 'outline 'blue))
                  "third")
(err/rt-name-test (overlay/xy (rectangle 100 200 'outline 'red) 10 10 #f) "fourth")
(err/rt-name-test (overlay/xy (rectangle 100 200 'outline 'red) 10 +inf.0 #f) "third")
(err/rt-name-test (overlay/xy (rectangle 100 200 'outline 'red) -inf.0 +inf.0 #f) "second")

(parameterize ((current-namespace (make-base-namespace)))
  (err/rt-test
    (eval '(module m lang/htdp-beginner
             (require (lib "image.rkt" "teachpack/htdp"))
             overlay))
    (lambda (exn)
      (regexp-match #rx"no open parenthesis" 
	(exn-message exn)))))

(report-errs)
