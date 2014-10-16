#lang racket/base

(require mrlib/image-core
         "img-err.rkt"
         racket/match
         racket/contract
         racket/class
         (except-in racket/draw
                    make-pen make-color)
         ;(only-in racket/gui/base frame% canvas% slider% horizontal-panel% button%)
         htdp/error
         racket/math
         (for-syntax racket/base
                     racket/list)
         lang/posn
         net/url)

;; for testing
; (require racket/gui/base)
#;
(define (show-image arg [extra-space 0])
  (letrec ([g (to-img arg)]
           [f (new frame% [label ""])]
           [c (new canvas% 
                   [parent f]
                   [min-width (+ extra-space (image-width g))]
                   [min-height (+ extra-space (image-height g))]
                   [paint-callback
                    (λ (c dc)
                      (send dc set-smoothing 'aligned)
                      (let-values ([(w h) (send c get-client-size)])
                        (let ([scale (send sl get-value)])
                          (send dc set-scale scale scale)
                          (render-image 
                           g
                           dc
                           (inexact->exact (floor (- (/ w 2 scale) (/ (get-right g) 2))))
                           (inexact->exact (floor (- (/ h 2 scale) (/ (get-bottom g) 2))))))))])]
           [min-scale 1]
           [max-scale 10]
           [sl (new slider% 
                    [label "Scale factor"] 
                    [parent f] 
                    [min-value min-scale]
                    [max-value max-scale]
                    [callback (λ ignore (send c refresh))])]
           [bp (new horizontal-panel% 
                    [parent f]
                    [alignment '(center center)]
                    [stretchable-height #f])]
           [scale-adjust
            (λ (f)
              (send sl set-value 
                    (max min-scale (min max-scale (f (send sl get-value)))))
              (send c refresh))])
    (send (new button% 
               [label "√"] 
               [callback (λ x (scale-adjust sub1))] 
               [parent bp]) min-width 100)
    (send (new button% 
               [label "2"]
               [callback (λ x (scale-adjust add1))]
               [parent bp]) min-width 100)
    (send f show #t)))

;; the obfuscation in the width and height defaults 
;; is so that error checking happens in the right order
(define/chk (save-image image
                        filename 
                        [width (if (image? image) (image-width image) 0)] 
                        [height (if (image? image) (image-height image) 0)])
  (check-dependencies 'save-image
                      (not (zero? width))
                      "the width must not be zero, got ~e"
                      width)
  (check-dependencies 'save-image
                      (not (zero? height))
                      "the width must not be zero, got ~e"
                      height)
  (let* ([bm (make-bitmap (inexact->exact (ceiling width)) 
                          (inexact->exact (ceiling height)))]
         [bdc (make-object bitmap-dc% bm)])
    (send bdc set-smoothing 'aligned)
    (send bdc erase)
    (render-image image bdc 0 0)
    (send bdc set-bitmap #f)
    (send bm save-file filename 'png)))

(define/chk (save-svg-image image
                            filename 
                            [width (if (image? image) (image-width image) 0)] 
                            [height (if (image? image) (image-height image) 0)])
  (call-with-output-file filename
    (λ (port)
      (define sdc (new svg-dc% [width width] [height height] [output port]))
      (send sdc start-doc "")
      (send sdc start-page)
      (send sdc set-smoothing 'aligned)
      (render-image image sdc 0 0)
      (send sdc end-page)
      (send sdc end-doc))
    #:exists 'truncate))

(define (get-right img) (bb-right (send img get-bb)))
(define (get-bottom img) (bb-bottom (send img get-bb)))
(define (get-baseline img) (bb-baseline (send img get-bb)))

;                                              
;                                              
;                                              
;                              ;;              
;                              ;;              
;                              ;;              
;    ;;;;  ;;;  ;;;;;;   ;; ;  ;; ;;;;;  ;;;  ;
;   ;;  ;;  ;;  ;;;; ;;  ;;;;  ;; ;   ;;  ;; ;;
;  ;;;  ;;  ;;;; ;;;;;;  ;;    ;;   ;;;;  ;;;; 
;  ;;;  ;;  ;;;; ;;      ;;    ;; ;;  ;;   ;;; 
;   ;;  ;;   ;;; ;;;  ;  ;;    ;; ;;  ;;   ;;; 
;    ;;;;    ;;   ;;;;   ;;    ;; ;;;;;;;  ;;  
;                                          ;;  
;                                          ;   
;                                         ;;   
                                                                            

;; scale : number image -> image
(define/chk (scale factor image)
  (scale-internal factor factor image))

(define/chk (scale/xy x-factor y-factor image)
  (scale-internal x-factor y-factor image))

(define (scale-internal x-factor y-factor image)
  (let ([ph (send image get-pinhole)])
    (make-image (make-scale x-factor y-factor (image-shape image))
                (make-bb (* x-factor (get-right image))
                         (* y-factor (get-bottom image))
                         (* y-factor (get-baseline image)))
                #f
                (and ph 
                     (make-point (* x-factor (point-x ph))
                                 (* y-factor (point-y ph)))))))

;; overlay : image image image ... -> image
;; places images on top of each other with their upper left corners aligned. 
;; last one goes on the bottom
(define/chk (overlay image image2 . image3)
  (overlay/internal 'middle 'middle image (cons image2 image3) #t))

;; underlay : image image image ... -> image
(define/chk (underlay image image2 . image3)
  (let ([imgs (reverse (list* image image2 image3))])
    (overlay/internal 'middle 'middle (car imgs) (cdr imgs) #f)))

;; overlay/align : string string image image image ... -> image
;; the first string has to be one of "center" "middle" "left" or "right" (or symbols)
;; the second string has to be one of "center" "middle" "top" "bottom" or "baseline" (or symbols)
;; behaves like overlay, but lines up the images in the various places.
;; overlay without string arguments is the same as passing "left" and "top"
;; for the two string arguments. Passing, eg, "center" "center" lines the
;; images up at their centers.

(define/chk (overlay/align x-place y-place image image2 . image3)
  (when (or (eq? x-place 'pinhole) (eq? y-place 'pinhole))
    (check-dependencies
     'overlay/align
     (and (send image get-pinhole)
          (send image2 get-pinhole)
          (andmap (λ (x) (send x get-pinhole))
                  image3))
     "when x-place or y-place is ~e or ~e, then all of the arguments must have pinholes"
     'pinhole "pinhole"))
  (overlay/internal x-place y-place image (cons image2 image3) #t))

(define/chk (underlay/align x-place y-place image image2 . image3)
  (when (or (eq? x-place 'pinhole) (eq? y-place 'pinhole))
    (check-dependencies
     'underlay/align
     (and (send image get-pinhole)
          (send image2 get-pinhole)
          (andmap (λ (x) (send x get-pinhole))
                  image3))
     "when x-place or y-place is ~e or ~e, then all of the arguments must have pinholes"
     'pinhole "pinhole"))
  (let ([imgs (reverse (list* image image2 image3))])
    (overlay/internal x-place y-place (car imgs) (cdr imgs) #f)))

(define/chk (overlay/pinhole image1 image2 . image3)
  (overlay/internal 'pinhole 'pinhole 
                    (maybe-center-pinhole image1)
                    (map maybe-center-pinhole (cons image2 image3))
                    #t))

(define/chk (underlay/pinhole image1 image2 . image3)
  (let ([imgs (map maybe-center-pinhole (reverse (list* image1 image2 image3)))])
    (overlay/internal 'pinhole 'pinhole
                      (car imgs)
                      (cdr imgs)
                      #f)))

(define (maybe-center-pinhole img)
  (if (send img get-pinhole)
      img
      (center-pinhole img)))

(define (overlay/internal x-place y-place fst rst first-pinhole?)
  (let loop ([fst fst]
             [rst rst])
    (cond
      [(null? rst) fst]
      [else 
       (let* ([fst-x-spot (find-x-spot x-place fst)]
              [fst-y-spot (find-y-spot y-place fst)]
              [snd-x-spot (find-x-spot x-place (car rst))]
              [snd-y-spot (find-y-spot y-place (car rst))]
              [dx (- fst-x-spot snd-x-spot)]
              [dy (- fst-y-spot snd-y-spot)])
         (loop (overlay/δ fst
                          (if (< dx 0) (- dx) 0) 
                          (if (< dy 0) (- dy) 0)
                          (car rst)
                          (if (< dx 0) 0 dx)
                          (if (< dy 0) 0 dy)
                          first-pinhole?
                          #f)
               (cdr rst)))])))

(define (find-x-spot x-place image)
  (case x-place
    [(left) 0]
    [(middle) (/ (get-right image) 2)]
    [(right) (get-right image)]
    [(pinhole) (point-x (send image get-pinhole))]
    [else (error 'find-x-spot "~s" x-place)]))

(define (find-y-spot y-place image)
  (case y-place
    [(top) 0]
    [(middle) (/ (get-bottom image) 2)]
    [(bottom) (get-bottom image)]
    [(baseline) (get-baseline image)]
    [(pinhole) (point-y (send image get-pinhole))]
    [else (error 'find-y-spot "~s" y-place)]))

;; overlay/xy : image number number image -> image
;; places images on top of each other with their upper-left corners offset by the two numbers

(define/chk (overlay/xy image dx dy image2) 
  (overlay/δ image
             (if (< dx 0) (- dx) 0)
             (if (< dy 0) (- dy) 0)
             image2
             (if (< dx 0) 0 dx)
             (if (< dy 0) 0 dy)
             #t
             #f))

(define/chk (underlay/xy image dx dy image2)
  (overlay/δ image2
             (if (< dx 0) 0 dx)
             (if (< dy 0) 0 dy)
             image
             (if (< dx 0) (- dx) 0)
             (if (< dy 0) (- dy) 0)
             #f
             #f))

(define (overlay/δ image1 dx1 dy1 image2 dx2 dy2 first-pinhole? beside-baseline?)
    (make-image (make-overlay (make-translate dx1 dy1 (image-shape image1))
                              (make-translate dx2 dy2 (image-shape image2)))
                (make-bb (max (+ (get-right image1) dx1)
                              (+ (get-right image2) dx2))
                         (max (+ (get-bottom image1) dy1)
                              (+ (get-bottom image2) dy2))
                         (if beside-baseline?
                             (let ([δ1 (- (get-bottom image1) (get-baseline image1))]
                                   [δ2 (- (get-bottom image2) (get-baseline image2))]
                                   [b1 (+ (get-baseline image1) dy1)]
                                   [b2 (+ (get-baseline image2) dy2)])
                               (cond
                                 [(= δ1 δ2) (max b1 b2)]
                                 [(< δ1 δ2) b2]
                                 [else b1]))
                             (max (+ (get-baseline image1) dy1)
                                  (+ (get-baseline image2) dy2))))
                #f
                (if first-pinhole?
                    (let ([ph (send image1 get-pinhole)])
                      (and ph
                           (make-point (+ (point-x ph) dx1)
                                       (+ (point-y ph) dy1))))
                    (let ([ph (send image2 get-pinhole)])
                      (and ph
                           (make-point (+ (point-x ph) dx2)
                                       (+ (point-y ph) dy2)))))))

;; beside : image image image ... -> image
;; places images in a single horizontal row, top aligned
(define/chk (beside image1 image2 . image3)
  (beside/internal 'middle image1 (cons image2 image3)))

;; beside/align : string image image image ... -> image
;; places images in a horizontal row where the vertical alignment is
;; covered by the string argument
(define/chk (beside/align y-place image1 image2 . image3)
  (when (eq? y-place 'pinhole)
    (check-dependencies 'beside/align
                        (and (send image1 get-pinhole)
                             (send image2 get-pinhole)
                             (andmap (λ (x) (send x get-pinhole))
                                     image3))
                        "when y-place is ~e or ~e, then all of the arguments must have pinholes"
                        'pinhole "pinhole"))
  (beside/internal y-place image1 (cons image2 image3)))

(define (beside/internal y-place fst rst)
  (let loop ([fst fst]
             [rst rst])
    (cond
      [(null? rst) fst]
      [else
       (let* ([snd (car rst)]
              [fst-y-spot (find-y-spot y-place fst)]
              [snd-y-spot (find-y-spot y-place (car rst))]
              [dy (- fst-y-spot snd-y-spot)])
         (loop (overlay/δ fst
                          0 
                          (if (< dy 0) (- dy) 0)
                          (car rst)
                          (get-right fst)
                          (if (< dy 0) 0 dy)
                          #t
                          #t)
               (cdr rst)))])))

;; above : image image image ... -> image
;; places images in a single vertical row, left aligned
(define/chk (above image1 image2 . image3)
  (above/internal 'middle image1 (cons image2 image3)))

;; beside/align : string image image image ... -> image
;; places images in a horizontal row where the vertical alignment is
;; covered by the string argument
(define/chk (above/align x-place image1 image2 . image3)
  (when (eq? x-place 'pinhole)
    (check-dependencies 'above/align
                        (and (send image1 get-pinhole)
                             (send image2 get-pinhole)
                             (andmap (λ (x) (send x get-pinhole))
                                     image3))
                        "when x-place is ~e or ~e, then all of the arguments must have pinholes"
                        'pinhole "pinhole"))
  (above/internal x-place image1 (cons image2 image3)))

(define (above/internal x-place fst rst)
  (let loop ([fst fst]
             [rst rst])
    (cond
      [(null? rst) fst]
      [else
       (let* ([snd (car rst)]
              [fst-x-spot (find-x-spot x-place fst)]
              [snd-x-spot (find-x-spot x-place (car rst))]
              [dx (- fst-x-spot snd-x-spot)])
         (loop (overlay/δ fst
                          (if (< dx 0) (- dx) 0) 
                          0
                          (car rst)
                          (if (< dx 0) 0 dx)
                          (get-bottom fst)
                          #t
                          #f)
               (cdr rst)))])))

(define/chk (overlay/offset image1 dx dy image2)
  (overlay/offset/internal 'middle 'middle image1 dx dy image2))

(define/chk (overlay/align/offset x-place y-place image1 dx dy image2)
  (overlay/offset/internal x-place y-place image1 dx dy image2))

(define/chk (underlay/offset image1 dx dy image2)
  (overlay/offset/internal 'middle 'middle  image2 (- dx) (- dy) image1))

(define/chk (underlay/align/offset x-place y-place image1 dx dy image2)
  (overlay/offset/internal x-place y-place image2 (- dx) (- dy) image1))

(define (overlay/offset/internal x-place y-place fst orig-dx orig-dy snd)
  (let* ([fst-x-spot (find-x-spot x-place fst)]
         [fst-y-spot (find-y-spot y-place fst)]
         [snd-x-spot (find-x-spot x-place snd)]
         [snd-y-spot (find-y-spot y-place snd)]
         [dx (+ (- fst-x-spot snd-x-spot) orig-dx)]
         [dy (+ (- fst-y-spot snd-y-spot) orig-dy)])
    (overlay/δ fst
               (if (< dx 0) (- dx) 0) 
               (if (< dy 0) (- dy) 0)
               snd
               (if (< dx 0) 0 dx)
               (if (< dy 0) 0 dy)
               #t
               #f)))


;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                 ;;;;        ;;                        ;                          
;                                 ;;;;        ;;                       ;;                          
;    ;;;;;   ;;;;   ;;;;;;; ;;;;  ;;;;;;;        ;;;; ;;;  ;;;;;;;   ;;;;;   ;;;;   ;;; ;;;  ;;;;; 
;   ;;;;;;  ;;;;;;  ;;;;;;;;;;;;; ;;;;;;;;  ;;;; ;;;;;;;;; ;;;;;;;; ;;;;;;  ;;;;;;  ;;;;;;; ;;;;;; 
;  ;;;;;;; ;;;;;;;; ;;;; ;;; ;;;; ;;;;;;;;; ;;;; ;;;; ;;;;     ;;;;  ;;;;  ;;;;;;;; ;;;; ;; ;;;;   
;  ;;;;    ;;;; ;;; ;;;; ;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;;  ;;;;;;;  ;;;;  ;;;; ;;; ;;;;     ;;;;  
;  ;;;;;;; ;;;;;;;; ;;;; ;;; ;;;; ;;;;;;;;; ;;;; ;;;; ;;;; ;;  ;;;;  ;;;;; ;;;;;;;; ;;;;      ;;;; 
;   ;;;;;;  ;;;;;;  ;;;; ;;; ;;;; ;;;;;;;;  ;;;; ;;;; ;;;; ;;;;;;;;  ;;;;;  ;;;;;;  ;;;;    ;;;;;; 
;    ;;;;;   ;;;;   ;;;; ;;; ;;;; ;;;;;;;   ;;;; ;;;; ;;;;  ;; ;;;;   ;;;;   ;;;;   ;;;;    ;;;;;  
;                                                                                                  
;                                                                                                  
;                                                                                                  
    

;; crop : number number number number image -> image
;; crops an image to be w x h from (x,y)
(define/chk (crop x1 y1 width height image)
  (crop/internal x1 y1 width height image))

(define/chk (crop/align x-place y-place width height image)
  (define x-spot (find-x-spot x-place image))
  (define y-spot (find-y-spot y-place image))
  (define crop-rec (rectangle width height "solid" "black"))
  (define w-off (find-x-spot x-place crop-rec))
  (define h-off (find-y-spot y-place crop-rec))
  (crop/internal (- x-spot w-off)
                 (- y-spot h-off)
                 width height image))

(define (crop/internal x1 y1 width height image)
  (let ([points (rectangle-points width height)]
        [ph (send image get-pinhole)])
    (make-image (make-crop points
                           (make-translate (- x1) (- y1) (image-shape image)))
                (make-bb width
                         height
                         (min height (get-baseline image)))
                #f
                (and ph 
                     (make-point (- (point-x ph) x1)
                                 (- (point-y ph) y1))))))

;; place-image : image x y scene -> scene
(define/chk (place-image image1 x1 y1 image2) 
  (place-image/internal image1 x1 y1 image2 'middle 'middle))
(define/chk (place-image/align image1 x1 y1 x-place y-place image2)
  (when (or (eq? x-place 'pinhole) (eq? y-place 'pinhole))
    (check-dependencies
     'place-image/align
     (send image1 get-pinhole)
     "when x-place or y-place is ~e or ~e, the the first image argument must have a pinhole"
     'pinhole "pinhole"))
  (place-image/internal image1 x1 y1 image2 x-place y-place))
(define/chk (place-images images zero-or-more-posns image2)
  (check-place-images-dependency 'place-images images zero-or-more-posns)
  (for/fold ([image2 image2]) ([image1 (in-list (reverse images))] 
                               [posn (in-list (reverse zero-or-more-posns))])
    (place-image/internal
     image1 (posn-x posn) (posn-y posn) image2 'middle 'middle)))
(define/chk (place-images/align images zero-or-more-posns x-place y-place image2)
  (check-place-images-dependency 'place-images/align images zero-or-more-posns)
  (for/fold ([image2 image2]) ([image1 (in-list (reverse images))] 
                               [posn (in-list (reverse zero-or-more-posns))])
    (place-image/internal
     image1 (posn-x posn) (posn-y posn) image2 x-place y-place)))

(define (check-place-images-dependency who images zero-or-more-posns)
  (check-dependencies who
                      (= (length images) (length zero-or-more-posns))
                      "expected images and posns arguments to have the same length"))

(define (place-image/internal image orig-dx orig-dy scene x-place y-place)
  (let ([dx (- orig-dx (find-x-spot x-place image))]
        [dy (- orig-dy (find-y-spot y-place image))])
    (crop/internal
     (if (< dx 0) (- dx) 0)
     (if (< dy 0) (- dy) 0)
     (get-right scene)
     (get-bottom scene)
     (overlay/δ image
                (if (< dx 0) 0 dx)
                (if (< dy 0) 0 dy)
                scene
                (if (< dx 0) (- dx) 0)
                (if (< dy 0) (- dy) 0)
                #f
                #f))))

(define/chk (scene+line image x1 y1 x2 y2 color)
  (let* ([dx (abs (min 0 x1 x2))]
         [dy (abs (min 0 y1 y2))])
    (make-image (make-overlay
                 (make-crop (rectangle-points (get-right image) (get-bottom image))
                            (make-line-segment (make-point x1 y1) (make-point x2 y2) color))
                 (image-shape image))
                (image-bb image)
                #f
                (send image get-pinhole))))

(define/chk (scene+polygon image posns mode color)
  (check-mode/color-combination 'scene+polygon 4 mode color)
  (when (null? posns) (error 'scene+polygon "must have at least one posn"))
  (make-image (make-overlay
               (make-crop (rectangle-points (get-right image) (get-bottom image))
                          (make-polygon (map (lambda (p) (make-point (posn-x p) (posn-y p))) posns)
                                        mode color))
               (image-shape image))
              (image-bb image)
              #f
              (send image get-pinhole)))
    

(define/chk (scene+curve image x1 y1 angle1 pull1 x2 y2 angle2 pull2 color)
  (let* ([dx (abs (min 0 x1 x2))]
         [dy (abs (min 0 y1 y2))])
    (make-image (make-overlay
                 (make-crop (rectangle-points (get-right image) (get-bottom image))
                            (make-curve-segment (make-point x1 y1) angle1 pull1
                                                (make-point x2 y2) angle2 pull2
                                                color))
                 (image-shape image))
                (image-bb image)
                #f
                (send image get-pinhole))))

;; frame : image -> image
;; draws a black frame around a image where the bounding box is
;; (useful for debugging images)

(define/chk (frame image) (real-color-frame "black" image))
(define/chk (color-frame color image) (real-color-frame color image))

(define (real-color-frame color image)
  (make-image (make-overlay (image-shape 
                             (crop 0 0
                                   (get-right image)
                                   (get-bottom image)
                                   (rectangle (get-right image)
                                              (get-bottom image)
                                              'outline
                                              (pen color 2 'solid 'round 'round))))
                            (image-shape image))
              (make-bb (get-right image)
                       (get-bottom image)
                       (get-baseline image))
              #f
              (send image get-pinhole)))

;; scale : I number -> I
;; scales the I by the given factor

;; rotate : number I -> I
;; rotates the I around the top-left corner by the given angle (in degrees)
(define/chk (rotate angle image)
  (let* ([rotated-shape (rotate-normalized-shape 
                         angle
                         (send image get-normalized-shape))]
         [ltrb (normalized-shape-bb rotated-shape)]
         [ph (send image get-pinhole)])
    (make-image (make-translate (- (ltrb-left ltrb)) (- (ltrb-top ltrb)) rotated-shape)
                (make-bb (- (ltrb-right ltrb) (ltrb-left ltrb))
                         (- (ltrb-bottom ltrb) (ltrb-top ltrb))
                         (- (ltrb-bottom ltrb) (ltrb-top ltrb)))
                #f
                (and ph
                     (let ([rp (rotate-point ph angle)])
                       (make-point (- (point-x rp) (ltrb-left ltrb))
                                   (- (point-y rp) (ltrb-top ltrb))))))))

(define/contract (rotate-normalized-shape angle shape)
  (-> number? normalized-shape? normalized-shape?)
  (cond
    [(overlay? shape)
     (let ([top-shape (rotate-normalized-shape angle (overlay-top shape))]
           [bottom-shape (rotate-cn-or-simple-shape angle (overlay-bottom shape))])
       (make-overlay top-shape bottom-shape))]
    [else 
     (rotate-cn-or-simple-shape angle shape)]))

(define/contract (rotate-cn-or-simple-shape angle shape)
  (-> number? cn-or-simple-shape? cn-or-simple-shape?)
  (cond
    [(crop? shape)
     (make-crop (rotate-points (crop-points shape) angle)
                (rotate-normalized-shape angle (crop-shape shape)))]
    [else
     (rotate-simple angle shape)]))

;; rotate-simple : angle simple-shape -> simple-shape
(define/contract (rotate-simple θ simple-shape)
  (-> number? simple-shape? simple-shape?)
  (cond
    [(line-segment? simple-shape)
     (make-line-segment (rotate-point (line-segment-start simple-shape)
                                      θ)
                        (rotate-point (line-segment-end simple-shape)
                                      θ)
                        (line-segment-color simple-shape))]
    [(curve-segment? simple-shape)
     (make-curve-segment (rotate-point (curve-segment-start simple-shape)
                                       θ)
                         (bring-between (+ (curve-segment-s-angle simple-shape) θ) 360)
                         (curve-segment-s-pull simple-shape)
                         (rotate-point (curve-segment-end simple-shape)
                                       θ)
                         (bring-between (+ (curve-segment-e-angle simple-shape) θ) 360)
                         (curve-segment-e-pull simple-shape)
                         (curve-segment-color simple-shape))]
    [(polygon? simple-shape)
     (make-polygon (rotate-points (polygon-points simple-shape) θ)
                   (polygon-mode simple-shape)
                   (polygon-color simple-shape))]
    [else
     (let* ([unrotated (translate-shape simple-shape)]
            [rotated (rotate-atomic θ unrotated)])
       (let-values ([(dx dy) 
                     (c->xy (* (degrees->complex θ)
                               (xy->c (translate-dx simple-shape)
                                      (translate-dy simple-shape))))])
         (make-translate dx dy rotated)))]))

(struct ltrb (left top right bottom) #:transparent)
(define (union-ltrb ltrb1 ltrb2)
  (ltrb (min (ltrb-left ltrb1) (ltrb-left ltrb2))
        (min (ltrb-top ltrb1) (ltrb-top ltrb2))
        (max (ltrb-right ltrb1) (ltrb-right ltrb2))
        (max (ltrb-bottom ltrb1) (ltrb-bottom ltrb2))))

;; only intersection if they already overlap.
(define (intersect-ltrb ltrb1 ltrb2)
  (ltrb (max (ltrb-left ltrb1) (ltrb-left ltrb2))
        (max (ltrb-top ltrb1) (ltrb-top ltrb2))
        (min (ltrb-right ltrb1) (ltrb-right ltrb2))
        (min (ltrb-bottom ltrb1) (ltrb-bottom ltrb2))))

(define/contract (normalized-shape-bb shape)
  (-> normalized-shape? ltrb?)
  (cond
    [(overlay? shape)
     (let ([top-ltrb (normalized-shape-bb (overlay-top shape))]
           [bottom-ltrb (cn-or-simple-shape-bb (overlay-bottom shape))])
       (union-ltrb top-ltrb bottom-ltrb))]
    [else 
     (cn-or-simple-shape-bb shape)]))

(define/contract (cn-or-simple-shape-bb shape)
  (-> cn-or-simple-shape? ltrb?)
  (cond
    [(crop? shape)
     (let ([ltrb (normalized-shape-bb (crop-shape shape))]
           [crop-ltrb (points->ltrb (crop-points shape))])
       (intersect-ltrb crop-ltrb ltrb))]
    [else
     (simple-bb shape)]))

;; simple-bb : simple-shape -> ltrb
;; returns the bounding box of 'shape' 
;; (only called for rotated shapes, so bottom=baseline)
(define/contract (simple-bb simple-shape)
  (-> simple-shape? ltrb?)
  (cond
    [(line-segment? simple-shape)
     (let ([x1 (point-x (line-segment-start simple-shape))]
           [y1 (point-y (line-segment-start simple-shape))]
           [x2 (point-x (line-segment-end simple-shape))]
           [y2 (point-y (line-segment-end simple-shape))])
       (ltrb (min x1 x2)
             (min y1 y2)
             (+ (max x1 x2) 1)
             (+ (max y1 y2) 1)))]
    [(curve-segment? simple-shape)
     (let ([x1 (point-x (curve-segment-start simple-shape))]
           [y1 (point-y (curve-segment-start simple-shape))]
           [x2 (point-x (curve-segment-end simple-shape))]
           [y2 (point-y (curve-segment-end simple-shape))])
       (ltrb (min x1 x2)
             (min y1 y2)
             (+ (max x1 x2) 1)
             (+ (max y1 y2) 1)))]
    [(polygon? simple-shape)
     (points->ltrb (polygon-points simple-shape))]
    [else
     (let ([dx (translate-dx simple-shape)]
           [dy (translate-dy simple-shape)])
       (let-values ([(l t r b) (np-atomic-bb (translate-shape simple-shape))])
         (ltrb (+ l dx)
               (+ t dy)
               (+ r dx)
               (+ b dy))))]))

(define (points->ltrb points)
  (let-values ([(left top right bottom) (points->ltrb-values points)])
    (ltrb left top right bottom)))

(define/contract (np-atomic-bb atomic-shape)
  (-> np-atomic-shape? (values number? number? number? number?))
  (cond
    [(ellipse? atomic-shape)
     (let ([θ (ellipse-angle atomic-shape)])
       (let-values ([(w h) (ellipse-rotated-size (ellipse-width atomic-shape)
                                                 (ellipse-height atomic-shape)
                                                 (degrees->radians θ))])
         
         (values (- (/ w 2))
                 (- (/ h 2))
                 (/ w 2)
                 (/ h 2))))]
    [(text? atomic-shape)
     (let-values ([(w h a d) (send text-sizing-bm get-text-extent 
                                   (text-string atomic-shape) 
                                   (text->font atomic-shape))])
       (rotated-rectangular-bounding-box w h (text-angle atomic-shape)))]
    [(flip? atomic-shape)
     (define bitmap (flip-shape atomic-shape))
     (define bb (ibitmap-raw-bitmap bitmap))
     (define-values (l t r b)
       (rotated-rectangular-bounding-box (* (send bb get-width) (ibitmap-x-scale bitmap))
                                         (* (send bb get-height) (ibitmap-y-scale bitmap))
                                         (ibitmap-angle bitmap)))
     (values l t r b)]
    [else
     (eprintf "using bad bounding box for ~s\n" atomic-shape)
     (values 0 0 100 100)]))

(define (rotated-rectangular-bounding-box w h θ)
  (let*-values ([(ax ay) (rotate-xy (- (/ w 2)) (- (/ h 2)) θ)]
                [(bx by) (rotate-xy (- (/ w 2)) (/ h 2) θ)]
                [(cx cy) (rotate-xy (/ w 2) (- (/ h 2)) θ)]
                [(dx dy) (rotate-xy (/ w 2) (/ h 2) θ)])
    (values (min ax bx cx dx)
            (min ay by cy dy)
            (max ax bx cx dx)
            (max ay by cy dy))))

(define (rotate-points in-points θ)
  (let* ([cs (map point->c in-points)]
         [vectors (points->vectors cs)]
         [rotated-vectors (map (λ (c) (rotate-c c θ)) vectors)]
         [points (vectors->points rotated-vectors)])
    points))

(define (points->vectors orig-points)
  (let loop ([points (cons 0 orig-points)])
    (cond
      [(null? (cdr points)) '()]
      [else
       (cons (- (cadr points) (car points))
             (loop (cdr points)))])))

(define (vectors->points vecs)
  (let loop ([vecs vecs]
             [p 0])
    (cond
      [(null? vecs) '()]
      [else 
       (let ([next-p (+ (car vecs) p)])
         (cons (c->point next-p)
               (loop (cdr vecs)
                     next-p)))])))

(define (center-point np-atomic-shape)
  (let-values ([(l t r b) (np-atomic-bb np-atomic-shape)])
    (xy->c (/ (- r l) 2)
           (/ (- b t) 2))))

;; rotate-atomic : angle np-atomic-shape -> np-atomic-shape
(define (rotate-atomic θ atomic-shape)
  (-> number? np-atomic-shape? np-atomic-shape?)
  (cond
    [(ellipse? atomic-shape)
     (cond
       [(= (ellipse-width atomic-shape)
           (ellipse-height atomic-shape))
        atomic-shape]
       [else
        (let ([new-angle (bring-between (+ θ (ellipse-angle atomic-shape)) 180)])
          (cond
            [(< new-angle 90)
             (make-ellipse (ellipse-width atomic-shape)
                           (ellipse-height atomic-shape)
                           new-angle
                           (ellipse-mode atomic-shape)
                           (ellipse-color atomic-shape))]
            [else
             (make-ellipse (ellipse-height atomic-shape)
                           (ellipse-width atomic-shape)
                           (- new-angle 90)
                           (ellipse-mode atomic-shape)
                           (ellipse-color atomic-shape))]))])]
    [(text? atomic-shape)
     (make-text (text-string atomic-shape)
                (bring-between (+ θ (text-angle atomic-shape)) 360)
                (text-y-scale atomic-shape)
                (text-color atomic-shape)
                (text-size  atomic-shape)
                (text-face  atomic-shape)
                (text-family atomic-shape)
                (text-style atomic-shape)
                (text-weight  atomic-shape)
                (text-underline atomic-shape))]
    [(flip? atomic-shape)
     (let ([bitmap (flip-shape atomic-shape)]
           [flipped? (flip-flipped? atomic-shape)])
       (make-flip flipped? 
                  (make-ibitmap (ibitmap-raw-bitmap bitmap)
                                (bring-between 
                                 (+ (ibitmap-angle bitmap) θ)
                                 360)
                                (ibitmap-x-scale bitmap)
                                (ibitmap-y-scale bitmap)
                                (make-hash))))]))

;; rotate-point : point angle -> point
(define (rotate-point p θ)
  (let-values ([(x y) (rotate-xy (point-x p) (point-y p) θ)])
    (make-point x y)))

(define (rotate-c c θ)
  (* (degrees->complex θ) c))

(define (degrees->complex θ) 
  (unless (and (<= 0 θ)
               (< θ 360))
    (error 'degrees->complex "~s" θ))
  (case (and (integer? θ) (modulo θ 360))
    [(0)    1+0i]
    [(90)   0+1i]
    [(180) -1+0i]
    [(270)  0-1i]
    [else (make-polar 1 (degrees->radians θ))]))

;; rotate-xy : x,y angle -> x,y
(define (rotate-xy x y θ)
  (c->xy (rotate-c (xy->c x y) θ)))

(define (xy->c x y) (make-rectangular x (- y)))
(define (c->xy c) 
  (values (real-part c)
          (- (imag-part c))))
(define (point->c p) (xy->c (point-x p) (point-y p)))
(define (c->point c) 
  (let-values ([(x y) (c->xy c)])
    (make-point x y)))


;; bring-between : rational integer -> rational
;; returns a number that is much like the modulo of 'x' and 'upper-bound',
;; since modulo only works on integers
(define (bring-between x upper-bound)
  (let* ([x-floor (floor x)]
         [fraction (- x x-floor)])
    (+ (modulo x-floor upper-bound) 
       fraction)))

(define/chk (flip-horizontal image)
  (rotate 90 (flip-vertical (rotate -90 image))))

(define/chk (flip-vertical image)
  (let* ([flipped-shape (flip-normalized-shape 
                         (send image get-normalized-shape))]
         [ltrb (normalized-shape-bb flipped-shape)]
         [ph (send image get-pinhole)])
    (make-image (make-translate (- (ltrb-left ltrb)) (- (ltrb-top ltrb)) flipped-shape)
                (make-bb (- (ltrb-right ltrb) (ltrb-left ltrb))
                         (- (ltrb-bottom ltrb) (ltrb-top ltrb))
                         (- (ltrb-bottom ltrb) (ltrb-top ltrb)))
                #f
                (and ph
                     (make-point (+ (point-x ph) (- (ltrb-left ltrb)))
                                 (+ (- (point-y ph)) (- (ltrb-top ltrb))))))))

(define/contract (flip-normalized-shape shape)
  (-> normalized-shape? normalized-shape?)
  (cond
    [(overlay? shape)
     (let ([top-shape (flip-normalized-shape (overlay-top shape))]
           [bottom-shape (flip-cn-or-simple-shape (overlay-bottom shape))])
       (make-overlay top-shape bottom-shape))]
    [else 
     (flip-cn-or-simple-shape shape)]))

(define/contract (flip-cn-or-simple-shape shape)
  (-> cn-or-simple-shape? cn-or-simple-shape?)
  (cond
    [(crop? shape)
     (make-crop (flip-points (crop-points shape))
                (flip-normalized-shape (crop-shape shape)))]
    [else
     (flip-simple shape)]))

(define/contract (flip-simple simple-shape)
  (-> simple-shape? simple-shape?)
  (cond
    [(line-segment? simple-shape)
     (make-line-segment (flip-point (line-segment-start simple-shape))
                        (flip-point (line-segment-end simple-shape))
                        (line-segment-color simple-shape))]
    [(curve-segment? simple-shape)
     (make-curve-segment (flip-point (curve-segment-start simple-shape))
                         (bring-between (- (curve-segment-s-angle simple-shape)) 360)
                         (curve-segment-s-pull simple-shape)
                         (flip-point (curve-segment-end simple-shape))
                         (bring-between (- (curve-segment-e-angle simple-shape)) 360)
                         (curve-segment-e-pull simple-shape)
                         (curve-segment-color simple-shape))]
    [(polygon? simple-shape)
     (make-polygon (flip-points (polygon-points simple-shape))
                   (polygon-mode simple-shape)
                   (polygon-color simple-shape))]
    [else
     (make-translate (translate-dx simple-shape) 
                     (- (translate-dy simple-shape))
                     (flip-atomic (translate-shape simple-shape)))]))

(define/contract (flip-atomic atomic-shape)
  (-> np-atomic-shape? np-atomic-shape?)
  (cond
    [(ellipse? atomic-shape)
     (cond
       [(= (ellipse-width atomic-shape)
           (ellipse-height atomic-shape))
        atomic-shape]
       [else
        (let ([new-angle (bring-between (- 180 (ellipse-angle atomic-shape)) 180)])
          (cond
            [(< new-angle 90)
             (make-ellipse (ellipse-width atomic-shape)
                           (ellipse-height atomic-shape)
                           new-angle
                           (ellipse-mode atomic-shape)
                           (ellipse-color atomic-shape))]
            [else
             (make-ellipse (ellipse-height atomic-shape)
                           (ellipse-width atomic-shape)
                           (- new-angle 90)
                           (ellipse-mode atomic-shape)
                           (ellipse-color atomic-shape))]))])]
    [(text? atomic-shape)
     (error 'flip "cannot flip shapes that contain text")]
    [(flip? atomic-shape)
     (define bitmap (flip-shape atomic-shape))
     (make-flip (not (flip-flipped? atomic-shape))
                (make-ibitmap (ibitmap-raw-bitmap bitmap)
                              (bring-between (- (ibitmap-angle bitmap)) 360)
                              (ibitmap-x-scale bitmap)
                              (ibitmap-y-scale bitmap)
                              (make-hash)))]))

(define (flip-point point) (make-point (point-x point) (- (point-y point))))
(define (flip-points points) (map flip-point points))
;                                                                                                 
;                                                                                                 
;                                                                                                 
;  ;;;;                        ;;               ;;                                                
;  ;;;;                        ;;               ;;                                                
;  ;;;;;;;   ;;;;;;;   ;;;;;        ;;;;;          ;;;;;;; ;;;;  ;;;;;;;   ;;;;;;;   ;;;    ;;;;; 
;  ;;;;;;;;  ;;;;;;;; ;;;;;; ;;;;  ;;;;;;     ;;;; ;;;;;;;;;;;;; ;;;;;;;; ;;;;;;;;  ;;;;;  ;;;;;; 
;  ;;;;;;;;;     ;;;; ;;;;   ;;;; ;;;;;;;     ;;;; ;;;; ;;; ;;;;     ;;;; ;;; ;;;; ;;;; ;; ;;;;   
;  ;;;; ;;;;  ;;;;;;;  ;;;;  ;;;; ;;;;        ;;;; ;;;; ;;; ;;;;  ;;;;;;; ;;;;;;;; ;;;;;;;  ;;;;  
;  ;;;;;;;;; ;;  ;;;;   ;;;; ;;;; ;;;;;;;     ;;;; ;;;; ;;; ;;;; ;;  ;;;;  ;;;;;;; ;;;;;     ;;;; 
;  ;;;;;;;;  ;;;;;;;; ;;;;;; ;;;;  ;;;;;;     ;;;; ;;;; ;;; ;;;; ;;;;;;;; ;   ;;;;  ;;;;;; ;;;;;; 
;  ;;;;;;;    ;; ;;;; ;;;;;  ;;;;   ;;;;;     ;;;; ;;;; ;;; ;;;;  ;; ;;;; ;;;;;;;;   ;;;;  ;;;;;  
;                                                                         ;;;;;;;;                
;                                                                          ;;;;;;                 
;                                                                                                 


(define/chk (polygon posns mode color)
  (check-mode/color-combination 'polygon 3 mode color)
  (make-a-polygon (map (λ (p) (make-point (posn-x p) (posn-y p))) posns)
                  mode
                  color))

(define/chk (rectangle width height mode color)
  (check-mode/color-combination 'rectangle 4 mode color)
  (make-a-polygon (rectangle-points width height) mode color))

(define/chk (square side-length mode color)
  (check-mode/color-combination 'square 3 mode color)
  (make-a-polygon (rectangle-points side-length side-length) mode color))

(define/chk (empty-scene width height [color 'white])
  (crop 0 0 width height
        (overlay (rectangle width height 'outline (pen "black" 2 'solid 'round 'round))
                 (rectangle width height 'solid color))))

(define/chk (rhombus side-length angle mode color)
  (check-mode/color-combination 'rhombus 3 mode color)
  (let* ([left-corner (make-polar side-length (+ (* pi 1/2) (/ (degrees->radians angle) 2)))]
         [right-corner (make-polar side-length (- (* pi 1/2) (/ (degrees->radians angle) 2)))]
         [bottom-corner (+ left-corner right-corner)])
    (make-a-polygon (list (make-point 0 0)
                          (make-point (real-part right-corner) (imag-part right-corner))
                          (make-point (real-part bottom-corner) (imag-part bottom-corner))
                          (make-point (real-part left-corner) (imag-part left-corner)))
                    mode
                    color)))

(define (rectangle-points width height [dx 0] [dy 0])
  (list (make-point dx dy)
        (make-point (+ dx width) dy)
        (make-point (+ dx width) (+ height dy))
        (make-point dx (+ dy height))))
  

(define/chk (line x1 y1 color)
  (let-values ([(shape w h) (line-shape x1 y1 color)])
    (make-image shape
                (make-bb w h h)
                #f)))

(define (line-shape x1 y1 color)
  (let ([dx (- (min x1 0))]
        [dy (- (min y1 0))]
        [w (+ (abs x1) 1)]
        [h (+ (abs y1) 1)])
    (values (make-translate
             dx dy
             (make-line-segment (make-point 0 0)
                                (make-point x1 y1)
                                color))
            w h)))

(define/chk (add-line image x1 y1 x2 y2 color)
  (let* ([dx (abs (min 0 x1 x2))]
         [dy (abs (min 0 y1 y2))]
         [bottom (max (+ y1 dy)
                      (+ y2 dy)
                      (+ dy (get-bottom image)))]
         [right (max (+ x1 dx)
                     (+ x2 dx)
                     (+ dx (get-right image)))]
         [baseline (+ dy (get-baseline image))])
    (make-image (make-translate
                 dx dy
                 (make-overlay
                  (make-line-segment (make-point x1 y1) (make-point x2 y2) color)
                  (image-shape image)))
                (make-bb right bottom baseline)
                #f
                (send image get-pinhole))))

(define/chk (add-polygon image posns mode color)
  (if (null? posns)
      (error 'add-polygon "must have at least one posn")
      (let ((left (apply min (map posn-x posns)))
            (top (apply min (map posn-y posns)))
            (poly (polygon posns mode color)))
        (overlay/xy poly (- left) (- top) image)
        )))

(define/chk (add-curve image x1 y1 angle1 pull1 x2 y2 angle2 pull2 color)
  (define cs (make-curve-segment (make-point x1 y1) angle1 pull1
                                 (make-point x2 y2) angle2 pull2
                                 color))
  (define path (curve-segment->path cs))
  (define rdc (new record-dc%))
  (send rdc set-pen (mode-color->pen 'outline color))
  (send rdc set-brush "black" 'transparent)
  (send rdc set-smoothing 'smoothed)
  (define-values (path-l path-t path-w path-h) (send rdc get-path-bounding-box path 'stroke))
  (define dx (abs (min 0 path-l)))
  (define dy (abs (min 0 path-t)))
  (define bottom (max (+ dy path-t path-h) (+ dy (get-bottom image))))
  (define right (max (+ dx path-l path-w) (+ dx (get-right image))))
  (define baseline (+ dy (get-baseline image)))
  (make-image (make-translate
               dx dy
               (make-overlay
                cs
                (image-shape image)))
              (make-bb right bottom baseline)
              #f
              (send image get-pinhole)))

;; this is just so that 'text' objects can be sized.
(define text-sizing-bm (make-object bitmap-dc% (make-object bitmap% 1 1)))

(define/chk (text string font-size color)
  (mk-text string font-size color #f 'swiss 'normal 'normal #f))

(define/chk (text/font string font-size color face family style weight underline)
  (mk-text string font-size color face family style weight underline))

(define (mk-text str font-size color face family style weight underline)
  (cond
    [(<= (string-length str) 1)
     (mk-single-text str font-size color face family style weight underline)]
    [else
     (let ([letters (string->list str)])
       (beside/internal
        'baseline
        (mk-single-text (string (car letters)) font-size color face family style weight underline)
        (map (λ (letter)
               (mk-single-text (string letter) font-size color face family style weight underline))
             (cdr letters))))]))

(define (mk-single-text letter font-size color face family style weight underline)
  (let ([text (make-text letter 0 1 color font-size face family style weight underline)])
    (let-values ([(w h d a) (send text-sizing-bm get-text-extent letter (text->font text))])
      (make-image (make-translate (/ w 2) (/ h 2) text)
                  (make-bb w h (- h d))
                  #f))))

(define/chk (isosceles-triangle side-length angle mode color)
  (check-mode/color-combination 'isosceles-triangle 4 mode color)
  (let ([left-corner (make-polar side-length (+ (* pi 1/2) (/ (degrees->radians angle) 2)))]
        [right-corner (make-polar side-length (- (* pi 1/2) (/ (degrees->radians angle) 2)))])
    (make-a-polygon (list (make-point 0 0)
                          (make-point (real-part right-corner) (imag-part right-corner))
                          (make-point (real-part left-corner) (imag-part left-corner)))
                    mode
                    color)))

(define/chk (right-triangle side-length1 side-length2 mode color)
  (check-mode/color-combination 'right-triangle 4 mode color)
  (make-a-polygon (list (make-point 0 (- side-length2))
                        (make-point 0 0)
                        (make-point side-length1 0))
                  mode
                  color))

(define/chk (triangle side-length mode color)
  (check-mode/color-combination 'triangle 3 mode color)
  (make-polygon/star side-length 3 mode color values))

; excess : R+ R+ -> R
;  compute the Euclidean excess
;  Note: If the excess is 0, then C is 90 deg.
;        If the excess is negative, then C is obtuse.
;        If the excess is positive, then C is acuse.
(define (excess a b c)
  (+ (sqr a) (sqr b) (- (sqr c))))

; polar->posn : R+ R -> (posn R R)
;  return a position with x and y coordinates
(define (polar->posn radius angle)
  (make-posn (* radius (cos angle))
             (* radius (sin angle))))

; cos-rel : R R R -> R+
;   return c^2 = a^2 + b^2 - 2ab cos(C)
(define (cos-rel a b C)
  (+ (sqr a) (sqr b) (* -2 a b (cos C))))

; sin-rel : R R R -> R
;  return the side b
(define (sin-rel A a B)
  (/ (* a (sin B)) (sin A)))

; last-angle : R R -> R
;   return pi-(A+B)
(define (last-angle A B)
  (- pi A B))

(define (radians degree)
  (* (/ degree 180.0) pi))


(define (triangle/sss side-a side-b side-c mode color)
  (define (triangle-vertices/sss a b c)
    (let ([A (acos (/ (excess b c a) (* 2 b c)))])
      (list (make-posn 0 0)
            (make-posn c 0)
            (polar->posn b A))))
  (check-dependencies 'triangle/sss
                      (and (>= (+ side-a side-b) side-c) 
                           (>= (+ side-a side-c) side-b) 
                           (>= (+ side-b side-c) side-a))
                      "the given side lengths will not form a triangle ~a, ~a, and, ~a."
                      side-a side-b side-c)
  (polygon (triangle-vertices/sss side-a side-b side-c) mode color))

(define/chk (triangle/ass angle-a side-b side-c mode color)
  (define (triangle-vertices/ass A b c)
    (list (make-posn 0 0) (make-posn c 0) (polar->posn b A)))
  (polygon (triangle-vertices/ass (radians angle-a) side-b side-c) mode color))

(define/chk (triangle/sas side-a angle-b side-c mode color)
  (define (triangle-vertices/sas a B c)
    (let ([b^2 (cos-rel a c B)])
      (check-dependencies 'triangle/sas
                          "the given side, angle, and, side will not form a triangle ~a, ~a, and, ~a."
                          side-a angle-b side-c)
      (let* ([b (sqrt b^2)]
             [A (acos (/ (excess b c a) (* 2 b c)))])
        (list (make-posn 0 0) (make-posn c 0) (polar->posn b A)))))
  (polygon (triangle-vertices/sas side-a (radians angle-b) side-c) mode color))
(define/chk (triangle/ssa side-a side-b angle-c mode color)
  (define (triangle-vertices/ssa a b C)
    (let ([c^2 (cos-rel a b C)])
      (check-dependencies 'triangle/ssa 
                          (positive? c^2)
                          "the given side, side, and, angle will not form a triangle ~a, ~a, and, ~a."
                          side-a side-b angle-c)
      (let*([c (sqrt c^2)]
            [A (acos (/ (excess b c a) (* 2 b c)))])
        (list (make-posn 0 0)
              (make-posn c 0)
              (polar->posn b A)))))
  (polygon (triangle-vertices/ssa side-a side-b (radians angle-c)) mode color))

(define/chk (triangle/aas angle-a angle-b side-c mode color)
  (define (triangle-vertices/aas A B c)
    (let* ([C (last-angle A B)]
           [b (sin-rel C c B)])
      (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))))
  (polygon (triangle-vertices/aas (radians angle-a) (radians angle-b) side-c) mode color))

(define/chk (triangle/asa angle-a side-b angle-c mode color)
  (define (triangle-vertices/asa A b C)
    (let* ([B (last-angle A C)]
           [c (sin-rel B b C)])
      (list (make-posn 0 0) (make-posn c 0) (polar->posn b A))))
  (polygon (triangle-vertices/asa (radians angle-a) side-b (radians angle-c)) mode color))

(define/chk (triangle/saa side-a angle-b angle-c mode color)
  (define (triangle-vertices/saa a B C)
    (let* ([A (last-angle B C)]
           [b (sin-rel A a B)]
           [c (sin-rel A a C)])
      (list (make-posn 0 0)
            (make-posn c 0)
            (polar->posn b A))))
  (polygon (triangle-vertices/saa side-a (radians angle-b) (radians angle-c)) mode color))

(define/chk (regular-polygon side-length side-count mode color)
  (check-mode/color-combination 'regular-polygon 4 mode color)
  (make-polygon/star side-length side-count mode color values))

(define/chk (star-polygon side-length side-count step-count mode color)
  (check-mode/color-combination 'star-polygon 5 mode color)
  (check-arg 'star-polygon
             (step-count . < . side-count)
             (format "number that is smaller than the side-count (~a)" side-count)
             3
             step-count)
  (check-arg 'star-polygon
             (= 1 (gcd side-count step-count))
             (format "number that is relatively prime to the side-count (~a)" side-count)
             3
             step-count)
  (make-polygon/star side-length side-count mode color (λ (l) (swizzle l step-count))))

(define/chk (star side-length mode color)
  (check-mode/color-combination 'star 3 mode color)
  (make-polygon/star side-length 5 mode color (λ (l) (swizzle l 2))))

(define (make-polygon/star side-length side-count mode color adjust)
  (make-a-polygon (adjust (regular-polygon-points side-length side-count)) 
                  mode color))

(define/chk (radial-star point-count radius1 radius2 mode color)
  (make-a-polygon (star-points radius1 radius2 point-count) mode color))
  
(define (star-points in-small-rad in-large-rad points)
  (let* ([small-rad (- in-small-rad 1)]
         [large-rad (- in-large-rad 1)]
         [roff (floor (/ large-rad 2))])
    (let loop ([i points])
      (cond
        [(zero? i) '()]
        [else 
         (let* ([this-p (- i 1)]
                [theta1 (* 2 pi (/ this-p points))]
                [theta2 (* 2 pi (/ (- this-p 1/2) points))])
           (let-values ([(x1 y1) (find-xy small-rad theta1)]
                        [(x2 y2) (find-xy large-rad theta2)])
             (let ([p1 (make-point (+ large-rad x1)
                                   (+ large-rad y1))]
                   [p2 (make-point (+ large-rad x2)
                                   (+ large-rad y2))])
               (list* p1 p2 (loop (- i 1))))))]))))

(define (find-xy radius theta)
  (values (* radius (cos theta))
          (* radius (sin theta))))

(define (make-a-polygon points mode color)
  (let* ([poly (make-polygon points mode color)]
         [ltrb (simple-bb poly)]
         [l (ltrb-left ltrb)]
         [t (ltrb-top ltrb)]
         [r (ltrb-right ltrb)]
         [b (ltrb-bottom ltrb)])
    (make-image (make-translate (- l) (- t) poly)
                (make-bb (- r l) (- b t) (- b t))
                #f)))
(define (gcd a b)
  (cond
    [(zero? b) a]
    [else (gcd b (modulo a b))]))



;; swizzle : (listof X)[odd-length] -> (listof X)
;; returns a list with the same elements, 
;; but reordered according to the step. Eg, if the step
;; is 2, we get the even elements and then the odd ones.
(define (swizzle l step)
  (let ([v (list->vector l)])
    (let loop ([i 0])
      (cond
        [(= i (vector-length v)) '()]
        [else
         (cons (vector-ref v (modulo (* i step) (vector-length v)))
               (loop (+ i 1)))]))))

;; regular-polygon-points : number number -> (listof point)
(define (regular-polygon-points side-length side-count)
  (let loop ([p (make-rectangular 0 0)]
             [i 0])
    (cond
      [(= i side-count) '()]
      [else (cons (make-point (real-part p) (imag-part p)) 
                  (loop (+ p (make-polar side-length
                                         (* -1 (* 2 pi) (/ i side-count))))
                        (+ i 1)))])))

(define/chk (ellipse width height mode color)
  (check-mode/color-combination 'ellipse 4 mode color)
  (make-image (make-translate (/ width 2) (/ height 2)
                              (make-ellipse width height 
                                            0
                                            mode
                                            color))
              (make-bb width height height)
              #f))

(define/chk (circle radius mode color)
  (check-mode/color-combination 'circle 3 mode color)
  (let ([w/h (* 2 radius)])
    (make-image (make-translate radius radius (make-ellipse w/h w/h 0 mode color))
                (make-bb w/h w/h w/h)
                #f)))

(define empty-image (rectangle 0 0 'solid 'black))

(define/chk (image-width image) (bb-select/round/exact bb-right image))
(define/chk (image-height image) (bb-select/round/exact bb-bottom image))
(define/chk (image-baseline image) (bb-select/round/exact bb-baseline image))
(define (bb-select/round/exact select image) (inexact->exact (round (select (send image get-bb)))))

(define-syntax (bitmap stx)
  (syntax-case stx ()
    [(_ arg)
     (let* ([arg (syntax->datum #'arg)]
            [path/lst
             (cond
               [(and (pair? arg)
                     (eq? (car arg) 'planet))
                (raise-syntax-error 'bitmap "planet paths not yet supported" stx)]
               [(symbol? arg)
                (define pieces (regexp-split #rx"/" (symbol->string arg)))
                (cond
                  [(or (null? pieces) (null? (cdr pieces)))
                   (raise-syntax-error 'bitmap "expected a path with a / in it" stx)]
                  [else
                   (define fn (last pieces))
                   (define colls (reverse (cdr (reverse pieces))))
                   (define candidate
                     (apply collection-file-path fn colls
                            #:fail
                            (λ (msg) (raise-syntax-error 'bitmap msg stx))))
                   (unless (file-exists? candidate)
                     (raise-syntax-error 'bitmap 
                                         (format "could not find ~s, expected it to be in ~a"
                                                 arg candidate)
                                         stx))
                   (cons fn colls)])]
               [(string? arg)
                (path->complete-path 
                 arg
                 (or (current-load-relative-directory)
                     (current-directory)))]
               [else (raise-syntax-error 
                      'bitmap 
                      (string-append
                       "expected the argument to specify a local path (via a string)"
                       " or a module path (e.g. `icons/b-run.png')")
                      stx)])])
       #`(bitmap/proc '#,path/lst))]))

(define (bitmap/proc arg)
  (define pth (if (path? arg)
                  arg
                  (apply collection-file-path arg
                         #:fail
                         (λ (msg) (error 'bitmap msg)))))
  (when (and (path? pth)
             (not (file-exists? pth)))
    (error 'bitmap "could not find the file ~a" (path->string pth)))
  ;; the rotate does a coercion to a 2htdp/image image
  (rotate 0 (make-object image-snip% (make-object bitmap% pth 'unknown/alpha))))

(define/chk (bitmap/url string)
  ;; the rotate does a coercion to a 2htdp/image image
  (rotate
   0
   (call/input-url (string->url string)
                   get-pure-port
                   (λ (port)
                     (make-object bitmap% port 'unknown/alpha #f #t)))))
                      
(define/chk (bitmap/file filename)
  (unless (file-exists? filename)
    (error 'bitmap/file 
           "could not find the file ~a" 
           filename))
  (rotate
   0
   (read-bitmap filename)))

(define/chk (image->color-list image)
  (define w (image-width image))
  (define h (image-height image))
  (cond
    [(or (= w 0) (= h 0)) '()]
    [else
     (define bm (make-bitmap w h))
     (define bdc (make-object bitmap-dc% bm))
     (define c (make-object color%))
     (define bytes (make-bytes (* w h 4)))
     (send bdc erase)
     (render-image image bdc 0 0)
     (send bdc get-argb-pixels 0 0 w h bytes)
     (for/list ([i (in-range 0 (* w h 4) 4)])
       (color (bytes-ref bytes (+ i 1))
              (bytes-ref bytes (+ i 2))
              (bytes-ref bytes (+ i 3))
              (bytes-ref bytes i)))]))

(define/chk (color-list->bitmap color-list width height)
  (check-dependencies 
   'color-list->bitmap
   (= (* width height) (length color-list))
   (string-append
    "the length of the color list to match the product of the width and the height,"
    " but the list has ~a elements and the width and height are ~a and ~a respectively")
   (length color-list) width height)
  (cond
    [(or (zero? width) (zero? height))
     (rectangle width height "solid" "black")]
    [else
     (define bmp (make-bitmap width height))
     (define bytes (make-bytes (* width height 4) 0))
     (define o (make-object color%))
     (for ([c (in-list color-list)]
           [i (in-naturals)])
       (define j (* i 4))
       (cond
         [(color? c)
          (bytes-set! bytes j (color-alpha c))
          (bytes-set! bytes (+ j 1) (color-red c))
          (bytes-set! bytes (+ j 2) (color-green c))
          (bytes-set! bytes (+ j 3) (color-blue c))]
         [else
          (define str (if (string? c) c (symbol->string c)))
          (define clr (or (send the-color-database find-color str)
                          (send the-color-database find-color "black")))
          (bytes-set! bytes j 255)  ;; this should probably (send clr alpha) when that's possible
          (bytes-set! bytes (+ j 1) (send clr red))
          (bytes-set! bytes (+ j 2) (send clr green))
          (bytes-set! bytes (+ j 3) (send clr blue))]))
     (send bmp set-argb-pixels 0 0 width height bytes)
     (bitmap->image bmp)]))
      
(define build-color/make-color
  (let ([orig-make-color make-color])
    (define/chk make-color
      (case-lambda 
        [(int0-255-1 int0-255-2 int0-255-3) 
         (orig-make-color int0-255-1 int0-255-2 int0-255-3)]
        [(int0-255-1 int0-255-2 int0-255-3 int0-255-4) 
         (orig-make-color int0-255-1 int0-255-2 int0-255-3 int0-255-4)]))
    make-color))

(define/chk (pinhole-x image) (let ([ph (send image get-pinhole)]) (and ph (point-x ph))))
(define/chk (pinhole-y image) (let ([ph (send image get-pinhole)]) (and ph (point-y ph))))
(define/chk (put-pinhole x1 y1 image) 
  (make-image (image-shape image) (image-bb image) (image-normalized? image) (make-point x1 y1)))
(define/chk (center-pinhole image) 
  (let ([bb (send image get-bb)])
    (make-image (image-shape image)
                (image-bb image)
                (image-normalized? image) 
                (make-point (/ (bb-right bb) 2)
                            (/ (bb-baseline bb) 2)))))
(define/chk (clear-pinhole image)
  (make-image (image-shape image) (image-bb image) (image-normalized? image) #f))

(define build-color/color
  (let ([orig-make-color make-color])
    (define/chk color
      (case-lambda
        [(int0-255-1 int0-255-2 int0-255-3) 
         (orig-make-color int0-255-1 int0-255-2 int0-255-3)]
        [(int0-255-1 int0-255-2 int0-255-3 int0-255-4) 
         (orig-make-color int0-255-1 int0-255-2 int0-255-3 int0-255-4)]))
    color))

(define build-pen/make-pen
  (let ([orig-make-pen make-pen])
    (define/chk (make-pen color int-0-255 pen-style pen-cap pen-join)
      (orig-make-pen color int-0-255 pen-style pen-cap pen-join))
    make-pen))

(define build-pen/pen
  (let ([orig-make-pen make-pen])
    (define/chk (pen color int-0-255 pen-style pen-cap pen-join)
      (orig-make-pen color int-0-255 pen-style pen-cap pen-join))
    pen))

(define/chk freeze
  (case-lambda 
    [(image) (freeze/internal 0 0 (image-width image) (image-height image) image)]
    [(width height image) (freeze/internal 0 0 width height image)]
    [(x y width height image) (freeze/internal x y width height image)]))

(define (freeze/internal x y w h image)
  (cond
    [(or (zero? w) (zero? h)) image]
    [else
     (define bm (make-bitmap w h))
     (define bdc (make-object bitmap-dc% bm))
     (render-image image bdc (- x) (- y))
     (send bdc set-bitmap #f)
     (to-img bm)]))

(provide overlay
         overlay/align
         overlay/offset
         overlay/align/offset
         overlay/xy
         
         underlay
         underlay/align
         underlay/align/offset
         underlay/offset
         underlay/xy
         
         beside
         beside/align
         above
         above/align
         
         rotate
         crop
         crop/align
         flip-vertical
         flip-horizontal
         frame
         color-frame

         place-image
         place-image/align
         place-images
         place-images/align
         
         save-image
         save-svg-image
         bring-between
         
         
         scale
         scale/xy
         
         image-width
         image-height
         image-baseline
         
         circle
         ellipse
         rectangle
         empty-scene
         square
         rhombus
         empty-image
         
         polygon
         regular-polygon
         triangle 
         triangle/sss
         triangle/ssa
         triangle/sas
         triangle/ass
         triangle/aas
         triangle/asa
         triangle/saa

         isosceles-triangle
         right-triangle
         star
         star-polygon
         radial-star
         
         line
         add-line
         add-polygon
         add-curve
         scene+line
         scene+polygon
         scene+curve
         text
         text/font
         image->color-list
         color-list->bitmap
         
         bitmap
         bitmap/url
         bitmap/file
         
         swizzle
         
         rotate-xy
         
         put-pinhole
         pinhole-x
         pinhole-y
         clear-pinhole
         center-pinhole
         overlay/pinhole
         underlay/pinhole
         
         build-color/make-color
         build-color/color
         build-pen/make-pen
         build-pen/pen
         
         freeze
         
         render-image)

(provide/contract
 [np-atomic-bb (-> np-atomic-shape? (values real? real? real? real?))]
 [center-point (-> np-atomic-shape? number?)])
