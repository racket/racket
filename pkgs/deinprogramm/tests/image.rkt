#lang scheme/base

(provide all-image-tests)

(require rackunit
         deinprogramm/image
         (only-in lang/private/imageeq image=?)
         (except-in mred make-color make-pen)
         mzlib/class
         mrlib/cache-image-snip
         lang/posn
         htdp/error)


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
(define (check-on-bitmap snp)
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
        
        (check-equal? s-clip s-noclip)
        
        (send bdc set-bitmap bm-noclip)
        (send bdc set-pen "black" 1 'transparent)
        (send bdc set-brush "white" 'solid)
        (send bdc draw-rectangle 0 0 (/ extra-space 2) bm-height)
        (send bdc draw-rectangle (- bm-width (/ extra-space 2)) 0 (/ extra-space 2) bm-height)
        (send bdc draw-rectangle 0 0 bm-width (/ extra-space 2))
        (send bdc draw-rectangle 0 (- bm-height (/ extra-space 2)) bm-width (/ extra-space 2))
        (send bdc get-argb-pixels 0 0 (+ width extra-space) (+ height extra-space) s-trunc)
        
        (check-equal? s-noclip s-trunc))
        
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
        (check-true (<= (max-difference s-normal s-bitmap) 2))))))

(define red (make-color 255 0 0))
(define blue (make-color 0 0 255))
(define black (make-color 0 0 0))
(define white (make-color 255 255 255))

(define awhite (make-alpha-color 0 255 255 255))
(define ablack (make-alpha-color 0 0 0 0))
(define ared (make-alpha-color 0 255 0 0))
(define aclr (make-alpha-color 255 0 0 0))

(define-simple-check (check-image=? i1 i2)
  (image=? i1 i2))

(define-simple-check (check-not-image=? i1 i2)
  (not (image=? i1 i2)))

(define-simple-check (check-terminates val1)
  #t)

(define (add-line i x1 y1 x2 y2 color)
  (overlay i
           (line (image-width i)
                 (image-height i) 
                 x1 y1 x2 y2 color)
           "left" "top"))

(define (not-image-inside? i1 i2)
  (not (image-inside? i1 i2)))

;; tests that the expression
;;  a) raises a teachpack exception record,
;;  b) has the right argument position, and
;;  c) has the right name.
(define (tp-exn-pred name position)
  (lambda (exn)
    (and (exn:fail:contract? exn)
         (let* ([msg (exn-message exn)]
                [beg (format "~a:" name)]
                [len (string-length beg)])
           (and (regexp-match position msg)
                ((string-length msg) . > . len)
                (string=? (substring msg 0 len) beg))))))

(define-syntax err/rt-name-test
  (syntax-rules ()
    [(_ (name . args) position)
     (check-exn (tp-exn-pred 'name position)
                (lambda ()
                  (name . args)))]))

(define all-image-tests
 (test-suite
  "Tests for images"

  (test-case
   "image?"
   (check-pred image? (rectangle 10 10 'solid 'blue))
   (check-pred image? (rectangle 10 10 "solid" 'blue))
   (check-pred image? (rectangle 10 10 'outline 'blue))
   (check-pred image? (rectangle 10 10 "outline" 'blue))
   (check-false (image? 5)))
  
  (test-case
   "color-list"
   (check-equal? (list red)
                 (image->color-list (rectangle 1 1 'solid 'red)))
   (check-equal? (list blue blue blue blue)
                 (image->color-list (rectangle 2 2 'solid 'blue))))

  (test-case
   "colors-set-up-properly"
   (check-equal? (list (list red) (list blue) (list black) (list white))
                 (list (image->color-list (rectangle 1 1 'solid 'red))
                       (image->color-list (rectangle 1 1 'solid 'blue))
                       (image->color-list (rectangle 1 1 'solid 'black))
                       (image->color-list (rectangle 1 1 'solid 'white)))))

  (test-case
   "color-list2"
   (check-equal? (list blue blue blue
                       blue blue blue
                       blue blue blue)
                 (image->color-list (rectangle 3 3 'solid 'blue)))
   (check-equal? (list blue blue blue
                       blue blue blue
                       blue blue blue)
                 (image->color-list (rectangle 3 3 "solid" 'blue)))
   (check-equal? (list blue blue blue
                       blue white blue
                       blue blue blue)
                 (image->color-list (rectangle 3 3 'outline 'blue))))

  (test-case
   "color-list3"
   (check-equal? (list blue blue blue
                       blue white blue
                       blue blue blue)
                 (image->color-list (rectangle 3 3 "outline" 'blue))))

  (test-case
   "color-list4"
   (check-image=? (color-list->image (list blue blue blue blue) 2 2)
                  (rectangle 2 2 'solid 'blue)))
  (test-case
   "color-list5"
   (check-not-image=? (color-list->image (list blue blue blue blue) 2 2)
                      (rectangle 1 4 'solid 'blue)))

  (test-case
   "color-list6"
   (check-image=? (color-list->image (list blue blue blue blue) 1 4)
                  (rectangle 1 4 'solid 'blue)))
  (test-case
   "color-list7"
   (check-image=? (color-list->image (list 'blue 'blue 'blue 'blue) 2 2)
                  (rectangle 2 2 'solid 'blue)))

  (test-case
   "color-list8"
   (check-equal? 10
                 (image-width (color-list->image '() 10 0))))

  (test-case
   "color-list9"
   (check-equal? 0
                 (image-height (color-list->image '() 10 0))))

  (test-case
   "color-list10"
   (check-equal?  0
                  (image-width (color-list->image '() 0 10))))

  (test-case
   "color-list11"
   (check-equal? 10
                 (image-height (color-list->image '() 0 10))))

  (test-case
   "alpha-color-list1"
   (check-equal? (make-alpha-color 0 255 0 0)
                 (car (image->alpha-color-list (rectangle 1 1 'solid 'red)))))

  (test-case
   "alpha-color-list2"
   (check-equal? (make-alpha-color 0 255 0 0)
                 (car (image->alpha-color-list (rectangle 1 1 "solid" 'red)))))

  (test-case
   "alpha-color-list3"
   (for-each
    (lambda (x)
      (check-equal? x (make-alpha-color 0 255 0 0)))
    (image->alpha-color-list (rectangle 1 1 "solid" 'red))))

  (test-case
   "alpha-color-list4"
   (for-each
    (lambda (x)
      (check-equal? x (make-alpha-color 0 255 0 0)))
    (image->alpha-color-list (rectangle 1 1 'solid 'red))))

  (test-case
   "alpha-color-list5"
   (check-equal? (make-alpha-color 0 0 255 0)
                 (car (image->alpha-color-list (rectangle 1 1 'solid 'green)))))

  (test-case
   "alpha-color-list6"
   (check-equal? (make-alpha-color 0 0 0 255)
              (car (image->alpha-color-list (rectangle 1 1 'solid 'blue)))))

  (test-case
   "alpha-color-list7"
   (check-equal? (image-width
                  (alpha-color-list->image
                   (list ared aclr ared
                         aclr aclr aclr)
                   3
                   2))
                 3))
  (test-case
   "alpha-color-list8"
   (check-equal? (image-height
                  (alpha-color-list->image
                   (list ared aclr ared
                         aclr aclr aclr)
                   3
                   2))
                 2))

  (test-case
   "alpha-color-list9"
   (check-equal? (image->color-list
                  (alpha-color-list->image
                   (list ared aclr ared
                         aclr aclr aclr)
                   3 2))
                 (list red   white red
                       white white white)))
  (test-case
   "alpha-color-list10"
   (check-equal? (image->color-list
                  (overlay
                   (rectangle 3 3 'solid 'blue)
                   (alpha-color-list->image
                         (list ared aclr ared
                               aclr aclr aclr
                               ared aclr ared)
                         3 3)
                   "left" "top"))
                 (list red  blue red
                       blue blue blue
                       red  blue red)))

  (test-case
   "alpha-color-list11"
   (check-equal? 10 (image-width (alpha-color-list->image '() 10 0))))
  
  (test-case
   "alpha-color-list12"
   (check-equal? 0 (image-height (alpha-color-list->image '() 10 0))))

  (test-case
   "alpha-color-list13"
   (check-equal? 0 (image-width (alpha-color-list->image '() 0 10))))

  (test-case
   "alpha-color-list14"
   (check-equal? 10 (image-height (alpha-color-list->image '() 0 10))))

  (test-case
   "image=?1"
   (check-image=? (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1)
                  (alpha-color-list->image (list (make-alpha-color 200 100 150 175)) 1 1)))

  (test-case 
   "image=?2"
   (check-image=? (alpha-color-list->image (list (make-alpha-color 255 100 100 100)) 1 1)
                  (alpha-color-list->image (list (make-alpha-color 255 200 200 200)) 1 1)))

  (test-case
   "image=?3"
   (check-not-image=? (alpha-color-list->image (list (make-alpha-color 200 100 100 100)) 1 1)
                      (alpha-color-list->image (list (make-alpha-color 200 200 200 200)) 1 1)))

  (test-case
   "image=?4"
   (check-not-image=? (alpha-color-list->image (list (make-alpha-color 200 100 150 175)
                                                     (make-alpha-color 200 100 150 175))
                                               1
                                               2)
                      (alpha-color-list->image (list (make-alpha-color 200 100 150 175)
                                                     (make-alpha-color 200 100 150 175)) 
                                               2
                                               1)))

  ;; This one is broken because of a fundamental problem with the
  ;; image primitives.
  #;(test-case
   "image=?5"
   (check-not-image=? (rectangle 4 4 'outline 'black) 
                      (overlay
                       (rectangle 4 4 'outline 'black)
                       (circle 1 'solid 'red)
                       0 0)))

  (test-case 
   "overlay"
   (check-image=? (color-list->image (list blue red blue red) 2 2)
                  (overlay (rectangle 2 2 'solid 'red)
                           (rectangle 1 2 'solid 'blue)
                           "left" "top")))

  (test-case
   "overlay/multiple"
   (check-image=? (overlay (rectangle 6 6 'solid 'red)
                           (overlay (rectangle 4 4 'solid 'white)
                                    (rectangle 2 2 'solid 'blue)
                                    "center" "center")
                           "center" "center")
                  (overlay (overlay (rectangle 6 6 'solid 'red)
                                    (rectangle 4 4 'solid 'white)
                                    "center" "center")
                           (rectangle 2 2 'solid 'blue)
                           "center" "center")))

  (test-case
   "overlay/empty-spaces-are-unmasked"
   (check-image=? (color-list->image (list red red red blue) 2 2)
                  (overlay
                   (rectangle 2 2 'solid 'blue)
                   (overlay (rectangle 1 2 'solid 'red)
                            (rectangle 2 1 'solid 'red)
                            "left" "top")
                   "left" "top")))
  
  (test-case
   "overlay/xy1"
   (check-image=? (color-list->image (list red blue red blue) 2 2)
                  (overlay (rectangle 2 2 'solid 'red)
                           (rectangle 1 2 'solid 'blue)
                           1 0)))

  (test-case
   "overlay/xy2"
   (check-image=? (color-list->image (list red red red blue) 2 2)
                  (overlay (rectangle 2 2 'solid 'red)
                           (rectangle 1 1 'solid 'blue)
                           1 1)))

  (test-case
   "overlay/xy3"
   (check-image=? (color-list->image (list red red blue blue) 2 2)
                  (overlay (rectangle 2 1 'solid 'red)
                           (rectangle 2 1 'solid 'blue)
                           0 1)))

  (test-case
   "overlay/xy/white"
   (check-image=? (alpha-color-list->image (list ablack ablack ablack
                                                 ablack awhite ablack
                                                 ablack ablack ablack)
                                           3 3)
                  (overlay (rectangle 3 3 'solid 'black)
                           (rectangle 1 1 'solid 'white)
                           1 1)))

  (test-case
   "color-list->image/white-in-mask"
   (check-image=? (color-list->image (list black red   black
                                           red   red   red
                                           black red   black)
                                     3 3)
                  (overlay (rectangle 3 3 'solid 'red)
                           (color-list->image (list black white black
                                                    white white white
                                                    black white black)
                                              3 3)
                           "left" "top")))


  (test-case
   "overlay"
   (check-image=? (color-list->image (list red blue red red blue red) 3 2)
               (overlay (rectangle 3 2 'solid 'red)
                        (rectangle 1 2 'solid 'blue)
                        1 0)))

  (test-case
   "image=?-zero1"
   (check-image=? (rectangle 0 10 'solid 'red)
                  (rectangle 0 10 'solid 'red)))
  (test-case
   "image=?-zero2"
   (check-image=? (rectangle 0 10 'solid 'red)
                  (rectangle 0 10 'solid 'blue)))
  (test-case
   "image=?-zero3"
   (check-not-image=? (rectangle 0 5 'solid 'red)
                      (rectangle 0 4'solid 'blue)))

  (test-case
   "image-inside?1"
   (check image-inside?
          (overlay (rectangle 3 2 'solid 'red)
                   (rectangle 1 2 'solid 'blue)
                   1 0)
          (rectangle 1 2 'solid 'blue)))

  (test-case
   "image-inside?2"
   (check not-image-inside?
          (overlay (rectangle 3 2 'solid 'red)
                   (rectangle 1 2 'solid 'blue)
                   1 0)
          (rectangle 1 2 'solid 'black)))

  (test-case
   "image-inside?3"
   (check image-inside?
          (overlay (rectangle 3 2 'solid 'red)
                   (rectangle 1 2 'solid 'blue)
                   1 0)
          (rectangle 1 2 'solid 'red)))

  (test-case
   "image-inside?4"
   (check not-image-inside?
          (overlay (rectangle 3 2 'solid 'red)
                   (rectangle 1 2 'solid 'blue)
                   1 0)
          (rectangle 2 1 'solid 'red)))

  (test-case
   "image-inside?5"
   (check image-inside?
          (alpha-color-list->image (list (make-alpha-color 0 255 0 0)) 1 1)
          (alpha-color-list->image (list (make-alpha-color 255 0 0 0)) 1 1)))

  (test-case
   "image-inside?6"
   (check not-image-inside?
          (overlay (rectangle 3 2 'solid 'red)
                   (rectangle 1 2 'solid 'blue)
                   1 0)
          (color-list->image (list blue white white) 
                             3 1)))

  (test-case
   "image-inside?7"
   (check image-inside?
          (overlay (rectangle 16 16 'solid 'red)
                   (ellipse 6 6 'outline 'blue)
                   2 5)
          (ellipse 6 6 'outline 'blue)))

  (test-case
   "image-inside?8"
   (check image-inside?
          (overlay (rectangle (image-width (text "x" 12 'red))
                              (image-height (text "x" 12 'red))
                              'solid 
                              'white)
                   (text "x" 12 'red)
                   "center" "center")
          (text "x" 12 'red)))

  (test-case
   "image-inside?9"
   (check image-inside?
          (text "y x y" 12 'red)
          (text "x" 12 'red)))

  (test-case
   "find-image1"
   (check-equal? (make-posn 2 5)
                 (find-image (overlay (rectangle 16 16 'solid 'red)
                                      (ellipse 6 6 'outline 'blue)
                                      2 5)
                             (ellipse 6 6 'outline 'blue))))

  (test-case
   "find-image2"
   (check-equal? (make-posn 0 0)
                 (find-image (rectangle 16 16 'solid 'blue)
                             (ellipse 6 6 'outline 'blue))))

  (test-case
   "find-image3"
   (check-equal? (make-posn 1 1)
                 (find-image (overlay (rectangle 10 10 'solid 'blue)
                                      (ellipse 5 5 'solid 'red)
                                      1 1)
                             (ellipse 5 5 'solid 'red))))

  (test-case
   "image-width"
   (check-equal? 5 (image-width (rectangle 5 7 'solid 'red))))

  (test-case
   "image-height"
   (check-equal? 7 (image-height (rectangle 5 7 'solid 'red))))

  (test-case
   "color-red"
   (check-equal? 1 (color-red (make-color 1 2 3))))
  
  (test-case
   "color-green" 
   (check-equal? 2 (color-green (make-color 1 2 3))))

  (test-case
   "color-blue" 
   (check-equal? 3 (color-blue (make-color 1 2 3))))

  (test-case
   "color?1"
   (check-true (color? (make-color 1 2 3))))

  (test-case
   "color?2" 
   (check-false (color? 10)))

  (test-case
   "image-color?1"
   (check-pred image-color? (make-color 1 2 3)))

  (test-case
   "image-color?2"
   (check-pred image-color? "blue"))

  (test-case 
   "image-color?3"
   (check-pred image-color? 'blue))

  (test-case 
   "image-color?4"
   (check-false (image-color? 10)))

  (test-case
   "image-color?5"
   (check-false (image-color? "not-a-color")))

  (test-case
   "image-color?6" 
   (check-false (image-color? 'not-a-color)))

  (test-case
   "line" 
   (check image=? 
          (line 5 1 0 0 4 0 'red)
          (color-list->image (list red red red red red) 5 1))
   (check image=?
          (line 1 5 0 0 0 4 'red)
          (color-list->image (list red red red red red) 1 5))

   (check image=?
          (line 1 5 0 4 0 0 'red)
          (color-list->image (list red red red red red) 1 5))

   (check image=?
          (line 5 1 4 0 0 0 'red)
          (color-list->image (list red red red red red) 5 1)))


; note: next two tests may be platform-specific... I'm not sure.
  ;; I developed them under macos x. -robby
  (test-case
   "triangle1"
   (check image=?
          (triangle 3 'outline 'red)
          (color-list->image 
           (list white red   white
                 white red   white
                 red   white red
                 red   red   red)
           3
           4)))

  (test-case
   "triangle2"
   (check image=? 
          (triangle 3 'solid 'red)
          (color-list->image 
           (list white red   white
                 white red   white
                 red   red   red
                 red   red   red)
           3
           4)))

  (test-case
   "clipping-twice-clips-both-times" 
   (check image=?
          (overlay
           (rectangle 11 11 'solid 'green)
           (clip (rectangle 11 11 'solid 'red)
                 5 5 1 1)
           "center" "center")
          (overlay
           (rectangle 11 11 'solid 'green)
           (clip (clip (rectangle 11 11 'solid 'red)
                       3 3 2 2)
                 2 2 1 1)
           "center" "center")))

  (test-case
   "solid-rect"
   (check-on-bitmap  (rectangle 2 2 'solid 'red)))

  (test-case
   "outline-rect"
   (check-on-bitmap (rectangle 2 2 'outline 'red)))
  (test-case
   "solid-ellipse"
   (check-on-bitmap (ellipse 2 4 'solid 'red)))
  (test-case
   "outline-ellipse"
   (check-on-bitmap (ellipse 2 4 'outline 'red)))
  (test-case
   "solid-circle"
   (check-on-bitmap (circle 4 'solid 'red)))
  (test-case
   "outline-circle"
   (check-on-bitmap (circle 4 'outline 'red)))

  (test-case
   "0solid-rect1"
   (check-on-bitmap (rectangle 0 2 'solid 'red)))
  (test-case
   "0solid-rect2"
   (check-on-bitmap (rectangle 2 0 'solid 'red)))
  (test-case
   "0outline-rect1"
   (check-on-bitmap (rectangle 2 0 'outline 'red)))
  (test-case
   "0outline-rect2"
   (check-on-bitmap (rectangle 0 0 'outline 'red)))
  (test-case
   "0solid-ellipse1"
   (check-on-bitmap (ellipse 0 3 'solid 'red)))
  (test-case
   "0solid-ellipse2"
   (check-on-bitmap (ellipse 3 0 'solid 'red)))
  (test-case
   "0outline-ellipse1"
   (check-on-bitmap (ellipse 0 4 'outline 'red)))
  (test-case
   "0outline-ellipse2"
   (check-on-bitmap (ellipse 2 0 'outline 'red)))
  (test-case
   "0solid-circle"
   (check-on-bitmap (circle 0 'solid 'red)))
  (test-case
   "0outline-circle"
   (check-on-bitmap (circle 0 'outline 'red)))

  (test-case
   "solid-triangle"
   (check-on-bitmap (triangle 10 'solid 'red)))
  (test-case
   "outline-triangle"
   (check-on-bitmap (triangle 10 'outline 'red)))
  (test-case
   "line"
   (check-on-bitmap (line 10 7 0 0 9 6 'red)))



  ;; (check-on-bitmap 'text (text "XX" 12 'red)) ;; this test fails for reasons I can't control ... -robby
  (test-case
   "overlay1"
   (check-on-bitmap (overlay (rectangle 1 4 'solid 'blue)
                             (rectangle 4 1 'solid 'green)
                             "left" "top")))
  (test-case
   "overlay2"
   (check-on-bitmap (overlay (rectangle 4 4 'solid 'blue)
                             (rectangle 4 4 'solid 'green)
                             2 2)))
  (test-case
   "overlay3"
   (check-on-bitmap (overlay image-snip1
                             (rectangle (image-width image-snip1) 
                                        (image-height image-snip1)
                                        'outline
                                        'red)
                             "center" "center")))
  (test-case
   "alpha-color-list"
   (check-on-bitmap
    (overlay
     (rectangle 3 3 'solid 'blue)
     (alpha-color-list->image
      (list ared aclr ared
            aclr aclr aclr
            ared aclr ared)
      3
      3)
     "center" "center")))
   (test-case
    "add-line"
    (check-on-bitmap
     (overlay
      (rectangle 100 100 'solid 'black)
      (line 100 100 -10 -10 110 110 'red)
      0 0)))

   (test-case
    "add-line1"
    (check-on-bitmap
     (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green) "center" "center")
               -20 -20
               0 0
               'red)))
   (test-case
    "add-line2"
    (check-on-bitmap
     (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green) "center" "center")
               -20 20
               0 0
               'red)))
   (test-case
    "add-line3"
    (check-on-bitmap
     (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green) "center" "center")
               20 -20
               0 0
               'red)))
   
   (test-case
    "add-line4"
    (check-on-bitmap
     (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green) "center" "center")
               20 20
               0 0
               'red))) 

   (test-case
    "add-line5"
    (check-on-bitmap
     (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green) "center" "center")
               0 0
               -20 -20
               'red)))

   (test-case
    "add-line6"
    (check-on-bitmap
     (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green) "center" "center")
               0 0
               -20 20
               'red)))

   (test-case
    "add-line7"
    (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green) "center" "center")
              0 0
              20 -20
              'red))

   (test-case
    "add-line8"
    (check-on-bitmap
     (add-line (overlay (rectangle 11 11 'solid 'black) (rectangle 3 3 'solid 'green) "center" "center")
               0 0
               20 20
               'red)))

   (test-case
    "shrink"
    (check-on-bitmap
     (clip (rectangle 11 11 'solid 'red)
           5 5 1 1)))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;;  test images with zero width or zero height 
   ;;  for various things
   ;;
   
   (test-case
    "zero-width/height"
    (check-equal? 10 (image-width (rectangle 10 0 'solid 'red)))
    (check-equal? 0 (image-height (rectangle 10 0 'solid 'red)))
    (check-equal? 0 (image-width (rectangle 0 10 'solid 'red)))
    (check-equal? 10 (image-height (rectangle 0 10 'solid 'red)))
   
    (check-equal? 0 (image-width (text "" 12 'black)))
    (check > (image-height (text "" 12 'black)) 0)

    (check-equal? '() (image->color-list (rectangle 0 10 'solid 'red)))
    (check-equal? '() (image->color-list (rectangle 10 0 'solid 'red)))
    (check-equal? '() (image->color-list (rectangle 0 0 'solid 'red)))

    (check-equal? '() (image->alpha-color-list (rectangle 0 10 'solid 'red)))
    (check-equal? '() (image->alpha-color-list (rectangle 10 0 'solid 'red)))
    (check-equal? '() (image->alpha-color-list (rectangle 0 0 'solid 'red))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;;  test that the image construction functions
   ;;  accept non-integer values (and floor them)
   ;;
   
   (test-case
    "accept-non-integer"
    (check-equal? (image->color-list (rectangle 2 2 'solid 'blue))
                  (image->color-list (rectangle #e2.5 2.5 'solid 'blue)))
    (check-equal? (image->color-list (ellipse 2 2 'solid 'blue))
                  (image->color-list (ellipse #e2.5 2.5 'solid 'blue)))
    (check-equal? (image->color-list (circle 2 'solid 'blue))
                  (image->color-list (circle #e2.5 'solid 'blue)))
    (check-equal? (image->color-list (triangle 12 'solid 'blue))
                  (image->color-list (triangle 12.5 'solid 'blue)))
    (check-equal? (image->color-list (line 10 12 0 0 9 11 'blue))
                  (image->color-list (line 10 12 0 0 9.5 #e11.5 'blue)))
    (check-equal? (image->color-list (clip (rectangle 10 10 'solid 'blue) 3 3 4 4))
                  (image->color-list
                   (clip (rectangle 10 10 'solid 'blue)
                         3.1
                         3.2
                         #e4.3
                         4.4)))
    (check-equal? (image->color-list (add-line (rectangle 10 10 'solid 'blue)
                                               0 0 2 2 'red))
                  (image->color-list (add-line (rectangle 10 10 'solid 'blue)
                                               0.1 #e.2 2.1 2.2 'red))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; The tests beginning with "bs-" ensure
   ;; that the operations all can accept bitmap 
   ;; snips as arguments
   ;;
   
   (test-case
    "accept-bitmap"
    (check-pred image? image-snip1)
    (check-pred image? image-snip2)
    (check image=? image-snip1 (send image-snip1 copy))
    (check-not-image=?
     ;; They have different masks:
     image-snip1 image-snip2)
    (check-equal? 2 (image-width image-snip1))
    (check-equal? 2 (image-width image-snip2))
    (check-equal? 2 (image-height image-snip1))
    (check-equal? 2 (image-height image-snip2))
    (check image=? image-snip1 (overlay image-snip1 image-snip2 "center" "center"))
    (check image=? image-snip1 (overlay image-snip1 image-snip2 "left" "top"))
    (check image=?
           (add-line image-snip1 0 0 10 10 'green)
           (add-line image-snip2 0 0 10 10 'green))
    (check image-inside? image-snip1 image-snip2)
    (check image-inside? image-snip2 image-snip1)
    (check-equal? (make-posn 0 0)
                  (find-image image-snip1 image-snip2))
    (check-equal? (make-posn 0 0)
                  (find-image image-snip2 image-snip1))
    (check-equal? (image->color-list image-snip1)
                  (image->color-list image-snip2))
    (check-equal? (image->alpha-color-list image-snip1)
                  (image->alpha-color-list image-snip2)))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; test image-snip that doesnt' have a bitmap
   ;;
   
   (test-case
    "image-snip-no-bitmap"
    (check-equal? 20
                  (image-width image-snip3))
    (overlay image-snip3 image-snip3 10 10))


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; test color arguments
   ;;
   (test-case
    "color-arguments"
    (check-terminates (rectangle 10 10 'solid 'blue))
    (check-terminates (rectangle 10 10 'solid "blue"))
    (check-terminates (rectangle 10 10 'solid (make-color 0 0 255)))
    (check-terminates (ellipse 10 10 'solid 'blue))
    (check-terminates (ellipse 10 10 'solid "blue"))
    (check-terminates (ellipse 10 10 'solid (make-color 0 0 255)))
    (check-terminates (circle 10 'solid 'blue))
    (check-terminates (circle 10 'solid "blue"))
    (check-terminates (circle 10 'solid (make-color 0 0 255)))
    (check-terminates (triangle 10 'solid 'blue))
    (check-terminates (triangle 10 'solid "blue"))
    (check-terminates (triangle 10 'solid (make-color 0 0 255)))
    (check-terminates (line 10 10 0 0 9 9 'blue))
    (check-terminates (line 10 10 0 0 9 9 "blue"))
    (check-terminates (line 10 10 0 0 9 9 (make-color 0 0 255)))
    (check-terminates (add-line (rectangle 1 1 'solid 'blue) 0 0 1 1 'blue))
    (check-terminates (add-line (rectangle 1 1 'solid 'blue) 0 0 1 1 "blue"))
    (check-terminates (add-line (rectangle 1 1 'solid 'blue) 0 0 1 1 (make-color 0 0 255)))
    (check-terminates (text "abc" 12 'blue))
    (check-terminates (text "abc" 12 "blue"))
    (check-terminates (text "abc" 12 (make-color 0 0 255))))

   (test-case
    "error-message"
    (err/rt-name-test (image-width 1) "first") 
    (err/rt-name-test (image-height 1) "first")
    (err/rt-name-test (overlay 1 2 "center" "center") "first")
    (err/rt-name-test (overlay image-snip1 2 "center" "center") "second")
    (err/rt-name-test (overlay 1 2 "center" "center") "first")
    (err/rt-name-test (overlay image-snip1 image-snip2 "foo" "center") "third")
    (err/rt-name-test (overlay image-snip1 image-snip2 "center" "foo") "fourth")
    (err/rt-name-test (rectangle #f #f #f #f) "first")
    (err/rt-name-test (rectangle 10 #f #f #f) "second")
    (err/rt-name-test (rectangle 10 10 #f #f) "third")
    (err/rt-name-test (rectangle 10 10 'solid #f) "fourth")
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
    (err/rt-name-test (line #f #f 0 0 0 0 #f) "first")
    (err/rt-name-test (line 10 #f 0 0 0 0 #f) "second")
    (err/rt-name-test (line 10 10 #f 0 0 0 #f) "third")
    (err/rt-name-test (line 10 10 0 #f 0 0 #f) "fourth")
    (err/rt-name-test (line 10 10 0 0 #f 0 #f) "fifth")
    (err/rt-name-test (line 10 10 0 0 0 #f #f) "sixth")
    (err/rt-name-test (line 10 10 0 0 0 0 #f) "seventh")
    (err/rt-name-test (text #f #f #f) "first")
    (err/rt-name-test (text "abc" #f #f) "second")
    (err/rt-name-test (text "abc" 10 #f) "third")
    (err/rt-name-test (image-inside? #f #f) "first")
    (err/rt-name-test (image-inside? image-snip1 #f) "second")
    (err/rt-name-test (find-image #f #f) "first")
    (err/rt-name-test (find-image image-snip1 #f) "second")
    (err/rt-name-test (image->color-list 1) "first")
    (err/rt-name-test (color-list->image #f #f #f) "first")
    (err/rt-name-test (color-list->image (list (make-color 0 0 0)) #f #f) "second")
    (err/rt-name-test (color-list->image (list (make-color 0 0 0)) 1 #f) "third")
    (err/rt-name-test (image->alpha-color-list #f) "first")
    (err/rt-name-test (alpha-color-list->image #f #f #f) "first")
    (err/rt-name-test (alpha-color-list->image (list (make-alpha-color 0 0 0 0)) #f #f) "second")
    (err/rt-name-test (alpha-color-list->image (list (make-alpha-color 0 0 0 0)) 1 #f) "third"))
))
