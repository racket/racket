#lang scheme/base

(require "../../mrlib/image-core.ss"
         scheme/contract
         scheme/class
         scheme/gui/base
         htdp/error
         scheme/math
         (for-syntax scheme/base
                     scheme/list)
         lang/posn)

(define (show-image g [extra-space 0])
  (letrec ([f (new frame% [label ""])]
           [c (new canvas% 
                   [parent f]
                   [min-width (+ extra-space (inexact->exact (floor (image-right g))))]
                   [min-height (+ extra-space (inexact->exact (floor (image-bottom g))))]
                   [paint-callback
                    (λ (c dc)
                      (send dc set-smoothing 'aligned)
                      (let-values ([(w h) (send c get-client-size)])
                        (let ([scale (send sl get-value)])
                          (send dc set-scale scale scale)
                          (render-image 
                           g
                           dc
                           (inexact->exact (floor (- (/ w 2 scale) (/ (image-right g) 2))))
                           (inexact->exact (floor (- (/ h 2 scale) (/ (image-bottom g) 2))))))))])]
           [min-scale 1]
           [max-scale 10]
           [sl (new slider% 
                    [label "Scale factor"] 
                    [parent f] 
                    [min-value min-scale]
                    [max-value max-scale]
                    [callback (λ ignore (send c refresh))])]
           [bp (new horizontal-panel% [parent f] [alignment '(center center)] [stretchable-height #f])]
           [scale-adjust
            (λ (f)
              (send sl set-value (max min-scale (min max-scale (f (send sl get-value)))))
              (send c refresh))])
    (send (new button% [label "√"] [callback (λ x (scale-adjust sub1))] [parent bp]) min-width 100)
    (send (new button% [label "2"] [callback (λ x (scale-adjust add1))] [parent bp]) min-width 100)
    (send f show #t)))

(define (save-image pre-image filename)
  (let* ([image (to-img pre-image)]
         [bm (make-object bitmap% 
               (inexact->exact (ceiling (+ 1 (image-width image)))) 
               (inexact->exact (ceiling (+ 1 (image-height image)))))]
         [bdc (make-object bitmap-dc% bm)])
    (send bdc set-smoothing 'aligned)
    (send bdc clear)
    (render-image image bdc 0 0)
    (send bdc set-bitmap #f)
    (send bm save-file filename 'png)))


;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                              ;;                      ;;      ;;                 
;                                              ;;                      ;;      ;;                 
;    ;;;;   ;;;;;;;;;  ;;;;   ;;;;      ;;;;   ;;;;;;   ;;;;    ;;;;   ;; ;;;  ;;  ;; ;;;  ;;;;;; 
;   ;;  ;;  ;;;; ;;;; ;;;;;;  ;;;;     ;;;;;;  ;;;;;;  ;;  ;;  ;;;;;;  ;;;;;   ;;  ;;;;;;  ;;;;;; 
;  ;;;;;;;; ;;   ;;  ;;;  ;;; ;;      ;;;      ;;  ;; ;;;;;;;;;;;      ;;;;;   ;;  ;;  ;; ;;;  ;; 
;  ;;;      ;;   ;;  ;;;  ;;; ;;      ;;;      ;;  ;; ;;;     ;;;      ;;;;;   ;;  ;;  ;; ;;;  ;; 
;   ;;; ;;  ;;   ;;   ;;;;;;  ;;       ;;;;;;  ;;  ;;  ;;; ;;  ;;;;;;  ;;  ;;  ;;  ;;  ;;  ;;;;;; 
;    ;;;;   ;;   ;;    ;;;;   ;;        ;;;;   ;;  ;;   ;;;;    ;;;;   ;;  ;;; ;;  ;;  ;;   ;;;;; 
;                                                                                          ;; ;;; 
;                                                                                          ;;;;;  
;                                                                                                 
;


(define-syntax define/chk
  (λ (stx)
    (syntax-case stx ()
      [(define/chk (fn-name args ... . final-arg) body ...)
       (identifier? #'final-arg)
       (let ([len (length (syntax->list #'(args ...)))])
         (with-syntax ([(i ...) (build-list len values)])
           #`(define (fn-name args ... . final-arg)
               (let ([args (check/normalize 'fn-name 'args args i)] ...
                     [final-arg (map/i (λ (x j) (check/normalize 'fn-name 'final-arg x (+ #,len j)))
                                       final-arg)])
                 body ...))))]
      [(define/chk (fn-name args ...) body ...)
       (with-syntax ([(i ...) (build-list (length (syntax->list #'(args ...))) add1)])
         #'(define (fn-name args ...)
             (let ([args (check/normalize 'fn-name 'args args i)] ...)
               body ...)))])))

(define (map/i f l)
  (let loop ([l l]
             [i 0])
    (cond
      [(null? l) null]
      [else (cons (f (car l) i)
                  (loop (cdr l) (+ i 1)))])))

;; check/normalize : symbol symbol any number -> any
;; based on the name of the argument, checks to see if the input
;; is valid and, if so, transforms it to a specific kind of value
;;   width, height -> number
;;   mode -> 'outline 'solid
;;   color -> (is-a?/c color<%>)
(define (check/normalize fn-name argname arg i)
  (case argname
    [(x-place)
     (check-arg fn-name
                (x-place? arg)
                'x-place
                i
                arg)
     (let ([sym (if (string? arg)
                    (string->symbol arg)
                    arg)])
       (if (eq? sym 'center)
           'middle
           sym))]
    [(y-place) 
     (check-arg fn-name
                (y-place? arg)
                'y-place
                i
                arg)
     (let ([sym (if (string? arg)
                    (string->symbol arg)
                    arg)])
       (if (eq? sym 'center)
           'middle
           sym))]
    [(image image1 image2 image3) 
     (check-arg fn-name
                (image? arg)
                'image
                i
                arg)
     (to-img arg)]
    [(mode)
     (check-arg fn-name
                (mode? arg)
                'mode
                i
                arg)
     (if (string? arg)
         (string->symbol arg)
         arg)]
    [(width height radius side-length side-length1 side-length2)
     (check-arg fn-name
                (and (real? arg)
                     (not (negative? arg)))
                'non-negative-real-number
                i arg)
     arg]
    [(dx dy x1 y1 x2 y2 factor x-factor y-factor)
     (check-arg fn-name
                (real? arg)
                'real\ number
                i arg)
     arg]
    [(side-count)
     (check-arg fn-name
                (side-count? arg)
                'side-count
                i arg)
     arg]
    [(step-count)
     (check-arg fn-name
                (step-count? arg)
                'step-count
                i arg)
     arg]
    [(angle)
     (check-arg fn-name
                (angle? arg)
                'angle\ in\ degrees
                i arg)
     (if (< arg 0)
         (+ arg 360)
         arg)]
    [(color)
     (check-color fn-name i arg)
     (let ([color-str 
            (cond
              [(symbol? arg)
               (symbol->string arg)]
              [else arg])])
       (if (send the-color-database find-color color-str)
           color-str
           "black"))]
    [(string)
     (check-arg fn-name (string? arg) 'string i arg)
     arg]
    [(font-size)
     (check-arg fn-name (and (integer? arg) (<= 1 arg 255)) 'font-size i arg)
     arg]
    [(face)
     (check-arg fn-name (or (not arg) (string? arg)) 'face i arg)
     arg]
    [(family)
     (check-arg fn-name (memq arg '(default decorative roman script swiss modern symbol system)) 'family i arg)
     arg]
    [(style)
     (check-arg fn-name (memq arg '(normal italic slant)) 'style i arg)
     arg]
    [(weight)
     (check-arg fn-name (memq arg '(normal bold light)) 'weight i arg)
     arg]
    [(underline)
     (and arg #t)]
    [(posns)
     (check-arg fn-name
                (and (list? arg)
                     (andmap posn? arg))
                'list-of-posns
                i arg)
     (check-arg fn-name
                (>= (length arg) 3)
                'list-of-at-least-three-posns
                i arg)
     arg]
    [else
     (error 'check "the function ~a has an argument with an unknown name: ~s"
            fn-name
            argname)]))

(define (y-place? arg)
  (member arg '("top" top "bottom" bottom "middle" middle "center" center "baseline" baseline)))
(define (x-place? arg)
  (member arg '("left" left "right" right "middle" middle "center" center)))
(define (mode? arg)
  (member arg '(solid outline "solid" "outline")))
(define (angle? arg)
  (and (real? arg)
       (< -360 arg 360)))
(define (side-count? i)
  (and (integer? i)
       (3 . <= .  i)))
(define (step-count? i)
  (and (integer? i)
       (1 . <= .  i)))
(define (color? c) (or (symbol? c) (string? c)))

(define (to-img arg)
  (cond
    [(is-a? arg image-snip%) (image-snip->image arg)]
    [(is-a? arg bitmap%) (bitmap->image arg)]
    [else arg]))

(define (bitmap->image bm [mask-bm (send bm get-loaded-mask)])
  (let ([w (send bm get-width)]
        [h (send bm get-height)])
    (make-image (make-translate
                 (/ w 2)
                 (/ h 2)
                 (make-bitmap bm mask-bm 0 1 #f))
                (make-bb w h h)
                #f)))

(define (image-snip->image is)
  (bitmap->image (send is get-bitmap)
                 (or (send is get-bitmap-mask)
                     (send (send is get-bitmap) get-loaded-mask))))

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
                                                                            

;; bitmap : string -> image
;; gets one of the bitmaps that comes with drscheme, scales it down by 1/8 or something
;; so that later scaling /translation/whatever will look reasonable.
;; (the error message for a bad argument will list all of the currently installed example images;
;; we may want to have some way teachers can stick new ones in there)

;; scale : number image -> image
(define/chk (scale factor image)
  (scale-internal factor factor image))

(define/chk (scale/xy x-factor y-factor image)
  (scale-internal x-factor y-factor image))

(define (scale-internal x-factor y-factor image)
  (make-image (make-scale x-factor y-factor (image-shape image))
              (make-bb (* x-factor (image-right image))
                       (* y-factor (image-bottom image))
                       (* y-factor (image-baseline image)))
              #f))

;; overlay : image image image ... -> image
;; places images on top of each other with their upper left corners aligned. last one goes on the bottom

(define/chk (overlay image image2 . image3)
  (overlay/internal 'left 'top image (cons image2 image3)))

;; overlay/places : string string image image image ... -> image
;; the first string has to be one of "center" "middle" "left" or "right" (or symbols)
;; the second string has to be one of "center" "middle" "top" "bottom" or "baseline" (or symbols)
;; behaves like overlay, but lines up the images in the various places.
;; overlay without string arguments is the same as passing "left" and "top"
;; for the two string arguments. Passing, eg, "center" "center" lines the
;; images up at their centers.

(define/chk (overlay/places x-place y-place image image2 . image3)
  (overlay/internal x-place y-place image (cons image2 image3)))

(define (overlay/internal x-place y-place fst rst)
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
                          (if (< dy 0) 0 dy))
               (cdr rst)))])))

(define (find-x-spot x-place image)
  (case x-place
    [(left) 0]
    [(middle) (/ (image-right image) 2)]
    [(right) (image-right image)]))

(define (find-y-spot y-place image)
  (case y-place
    [(top) 0]
    [(middle) (/ (image-bottom image) 2)]
    [(bottom) (image-bottom image)]
    [(baseline) (image-baseline image)]))

;; overlay/xy : image number number image -> image
;; places images on top of each other with their upper-left corners offset by the two numbers

(define/chk (overlay/xy image dx dy image2) 
  (overlay/δ image
             (if (< dx 0) (- dx) 0)
             (if (< dy 0) (- dy) 0)
             image2
             (if (< dx 0) 0 dx)
             (if (< dy 0) 0 dy)))

(define (overlay/δ image1 dx1 dy1 image2 dx2 dy2)
  (make-image (make-overlay (make-translate dx1 dy1 (image-shape image1))
                            (make-translate dx2 dy2 (image-shape image2)))
              (make-bb (max (+ (image-right image1) dx1)
                            (+ (image-right image2) dx2))
                       (max (+ (image-bottom image1) dy1)
                            (+ (image-bottom image2) dy2))
                       (max (+ (image-baseline image1) dy1)
                            (+ (image-baseline image2) dy2)))
              #f))

;; beside : image image image ... -> image
;; places images in a single horizontal row, top aligned
(define/chk (beside image1 image2 . image3)
  (beside/internal 'top image1 (cons image2 image3)))

;; beside/places : string image image image ... -> image
;; places images in a horizontal row where the vertical alignment is
;; covered by the string argument
(define/chk (beside/places y-place image1 image2 . image3)
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
                          (image-right fst)
                          (if (< dy 0) 0 dy))
               (cdr rst)))])))

;; above : image image image ... -> image
;; places images in a single vertical row, left aligned
(define/chk (above image1 image2 . image3)
  (above/internal 'left image1 (cons image2 image3)))

;; beside/places : string image image image ... -> image
;; places images in a horizontal row where the vertical alignment is
;; covered by the string argument
(define/chk (above/places x-place image1 image2 . image3)
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
                          (image-bottom fst))
               (cdr rst)))])))


;                                                                                   
;                               ;;      ;;                   ;;                     
;                               ;;      ;;                  ;;;                     
;    ;;;;    ;;;;   ;;;;;; ;;;  ;;;;;   ;;  ;; ;;;   ;;;;  ;;;;;  ;;;;   ;;;; ;;;;; 
;   ;;;;;;  ;;;;;;  ;;;;;;;;;;  ;;;;;;  ;;  ;;;;;;  ;;  ;;  ;;;; ;;;;;;  ;;;; ;; ;; 
;  ;;;     ;;;  ;;; ;;  ;;  ;;  ;;  ;;; ;;  ;;  ;;    ;;;;  ;;; ;;;  ;;; ;;   ;;;;; 
;  ;;;     ;;;  ;;; ;;  ;;  ;;  ;;  ;;; ;;  ;;  ;;  ;;; ;;  ;;; ;;;  ;;; ;;     ;;;;
;   ;;;;;;  ;;;;;;  ;;  ;;  ;;  ;;;;;;  ;;  ;;  ;; ;;;  ;;  ;;;; ;;;;;;  ;;   ;; ;;;
;    ;;;;    ;;;;   ;;  ;;  ;;  ;; ;;   ;;  ;;  ;;  ;;;;;;   ;;;  ;;;;   ;;   ;;;;; 
;                                                                                   
;                                                                                   
;       

;; frame : image -> image
;; draws a black frame around a image where the bounding box is
;; (useful for debugging images)

(define/chk (frame image)
  (make-image (make-overlay (image-shape image)
                            (image-shape 
                             (rectangle (image-right image)
                                        (image-bottom image)
                                        'outline
                                        'black)))
              (make-bb (image-right image)
                       (image-bottom image)
                       (image-baseline image))
              #f))

;; scale : I number -> I
;; scales the I by the given factor

;; rotate : I number -> I
;; rotates the I around the top-left corner by the given angle
;; (in degrees)
;; LINEAR TIME OPERATION (sigh)
(define/chk (rotate angle image)
  (define left +inf.0)
  (define top +inf.0)
  (define right -inf.0)
  (define bottom -inf.0)
  (define (add-to-bounding-box/rotate simple-shape)
    (let ([rotated-shape (rotate-simple angle simple-shape)])
      (let-values ([(this-left this-top this-right this-bottom) (simple-bb rotated-shape)])
        (set! left (min this-left left))
        (set! top (min this-top top))
        (set! right (max this-right right))
        (set! bottom (max this-bottom bottom)))
      rotated-shape))
  (let* ([rotated (normalize-shape (image-shape image) add-to-bounding-box/rotate)])
    (make-image (make-translate (- left) (- top) rotated)
                (make-bb (- right left) (- bottom top) (- bottom top))
                #f)))

;; simple-bb : simple-shape -> (values number number number number)
;; returns the bounding box of 'shape' 
;; (only called for rotated shapes, so bottom=baseline)
(define (simple-bb simple-shape)
  (cond
    [(line-segment? simple-shape)
     (let ([x1 (point-x (line-segment-start simple-shape))]
           [y1 (point-y (line-segment-start simple-shape))]
           [x2 (point-x (line-segment-end simple-shape))]
           [y2 (point-y (line-segment-end simple-shape))])
       (values (min x1 x2)
               (min y1 y2)
               (max x1 x2)
               (max y1 y2)))]
    [(polygon? simple-shape)
     (let ([points (polygon-points simple-shape)])
       (let* ([fx (point-x (car points))]
              [fy (point-y (car points))]
              [left fx]
              [top fy]
              [right fx]
              [bottom fy])
         (for-each (λ (point)
                     (let ([new-x (point-x point)]
                           [new-y (point-y point)])
                       (set! left (min new-x left))
                       (set! top (min new-y top))
                       (set! right (max new-x right))
                       (set! bottom (max new-y bottom))))
                   (cdr points))
         (values left top right bottom)))]
    [else
     (let ([dx (translate-dx simple-shape)]
           [dy (translate-dy simple-shape)])
       (let-values ([(l t r b) (atomic-bb (translate-shape simple-shape))])
         (values (+ l dx)
                 (+ t dy)
                 (+ r dx)
                 (+ b dy))))]))


(define (atomic-bb atomic-shape)
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
     (let*-values ([(w h a d) (send text-sizing-bm get-text-extent 
                                    (text-string atomic-shape) 
                                    (text->font atomic-shape))]
                   [(ax ay) (rotate-xy (- (/ w 2)) (- (/ h 2)) (text-angle atomic-shape))]
                   [(bx by) (rotate-xy (- (/ w 2)) (/ h 2) (text-angle atomic-shape))]
                   [(cx cy) (rotate-xy (/ w 2) (- (/ h 2)) (text-angle atomic-shape))]
                   [(dx dy) (rotate-xy (/ w 2) (/ h 2) (text-angle atomic-shape))])
       (values (min ax bx cx dx)
               (min ay by cy dy)
               (max ax bx cx dx)
               (max ay by cy dy)))]
    [else
     (fprintf (current-error-port) "using bad bounding box for ~s\n" (image-shape atomic-shape))
     (values 0 0 100 100)]))
  
;; rotate-simple : angle simple-shape -> simple-shape
(define (rotate-simple θ simple-shape)
  (cond
    [(line-segment? simple-shape)
     (make-line-segment (rotate-point (line-segment-start simple-shape)
                                       θ)
                        (rotate-point (line-segment-end simple-shape)
                                      θ)
                        (line-segment-color simple-shape))]
    [(polygon? simple-shape)
     (make-polygon (map (λ (p) (rotate-point p θ))
                        (polygon-points simple-shape))
                   (polygon-mode simple-shape)
                   (polygon-color simple-shape))]
    [else
     (let* ([unrotated (translate-shape simple-shape)]
            [rotated (rotate-atomic θ unrotated)])
       (let-values ([(dx dy) 
                     (c->xy (* (make-polar 1 (degrees->radians θ))
                               (xy->c (translate-dx simple-shape)
                                      (translate-dy simple-shape))))])
         (make-translate dx dy rotated)))]))

(define (center-point atomic-shape)
  (let-values ([(l t r b) (atomic-bb atomic-shape)])
    (xy->c (/ (- r l) 2)
           (/ (- b t) 2))))

;; rotate-atomic : angle np-atomic-shape -> np-atomic-shape
(define (rotate-atomic θ atomic-shape)
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
    [(bitmap? atomic-shape)
     (make-bitmap (bitmap-raw-bitmap atomic-shape)
                  (bitmap-raw-mask atomic-shape)
                  (bring-between (+ θ (bitmap-angle atomic-shape)) 360)
                  (bitmap-scale atomic-shape)
                  #f)]))

;; rotate-point : point angle -> point
(define (rotate-point p θ)
  (let-values ([(x y) (rotate-xy (point-x p) (point-y p) θ)])
    (make-point x y)))

;; rotate-xy : x,y angle -> x,y
(define (rotate-xy x y θ)
  (c->xy (* (make-polar 1 (degrees->radians θ)) 
            (xy->c x y))))

(define (xy->c x y) (make-rectangular x (- y)))
(define (c->xy c) 
  (values (real-part c)
          (- (imag-part c))))


;; bring-between : number number -> number
;; returns a number that is much like the modulo of 'x' and 'upper-bound'
;; but does this by repeated subtraction (or addition if it is negative), 
;; since modulo only works on integers
(define (bring-between x upper-bound)
  (let loop ([x x])
    (cond
      [(< x 0)
       (loop (+ x upper-bound))]
      [(< x upper-bound)
       x]
      [else
       (loop (- x upper-bound))])))

;; stamp : I I -> I
;; treats the first I as if it were a mask and uses that mask to
;; mask out parts of the first I (the mask is solid; no alpha stuff
;; here, even if dim were used).
;; only accepts solid black Is

;; see-thru : I number -> I
;; applies an alpha value to the I, making it translucent


;; -- as in the current I library, but they don't actually create
;; bitmaps, but instead just records that are rendered right as they are
;; about to be drawn

;;       rectangle

(define/chk (polygon posns mode color)
  (make-a-polygon (map (λ (p) (make-point (posn-x p) (posn-y p))) posns)
                  mode
                  color))

(define/chk (rectangle width height mode color)
  (make-a-polygon (rectangle-points width height) mode color))

(define/chk (square side-length mode color)
  (make-a-polygon (rectangle-points side-length side-length) mode color))

(define/chk (rhombus side-length angle mode color)
  (let* ([left-corner (make-polar side-length (+ (* pi 1/2) (/ (degrees->radians angle) 2)))]
         [right-corner (make-polar side-length (- (* pi 1/2) (/ (degrees->radians angle) 2)))]
         [bottom-corner (+ left-corner right-corner)])
    (make-a-polygon (list (make-point 0 0)
                          (make-point (real-part right-corner) (imag-part right-corner))
                          (make-point (real-part bottom-corner) (imag-part bottom-corner))
                          (make-point (real-part left-corner) (imag-part left-corner)))
                    mode
                    color)))

(define (rectangle-points width height)
  (list (make-point 0 0)
        (make-point width 0)
        (make-point width height)
        (make-point 0 height)))
  

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
                      (+ dy (image-bottom image)))]
         [right (max (+ x1 dx)
                     (+ x2 dx)
                     (+ dx (image-right image)))]
         [baseline (+ dy (image-baseline image))])
    ;(printf "dx ~s orig-right ~s\n" dx (image-right image))
    (make-image (make-translate
                 dx dy
                 (make-overlay
                  (make-line-segment (make-point x1 y1) (make-point x2 y2) color)
                  (image-shape image)))
                (make-bb right bottom baseline)
                #f)))


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
  (let ([left-corner (make-polar side-length (+ (* pi 1/2) (/ (degrees->radians angle) 2)))]
        [right-corner (make-polar side-length (- (* pi 1/2) (/ (degrees->radians angle) 2)))])
    (make-a-polygon (list (make-point 0 0)
                          (make-point (real-part right-corner) (imag-part right-corner))
                          (make-point (real-part left-corner) (imag-part left-corner)))
                    mode
                    color)))

(define/chk (right-triangle side-length1 side-length2 mode color)
  (make-a-polygon (list (make-point 0 (- side-length2))
                        (make-point 0 0)
                        (make-point side-length1 0))
                  mode
                  color))

(define/chk (triangle side-length mode color)
  (make-polygon/star side-length 3 mode color values))

(define/chk (regular-polygon side-length side-count mode color)
  (make-polygon/star side-length side-count mode color values))

(define/chk (star-polygon side-length side-count step-count mode color)
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
  (make-polygon/star side-length 5 mode color (λ (l) (swizzle l 2))))

(define (make-polygon/star side-length side-count mode color adjust)
  (make-a-polygon (adjust (regular-polygon-points side-length side-count)) 
                  mode color))

(define (make-a-polygon points mode color)
  (let ([poly (make-polygon points mode color)])
    (let-values ([(l t r b) (simple-bb poly)])
      (make-image (make-translate (- l) (- t) poly)
                  (make-bb (- r l) (- b t) (- b t))
                  #f))))
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
  (make-image (make-translate (/ width 2) (/ height 2)
                              (make-ellipse width height 
                                            0
                                            mode
                                            color))
              (make-bb width height height)
              #f))

(define/chk (circle radius mode color)
  (let ([w/h (* 2 radius)])
    (make-image (make-translate radius radius (make-ellipse w/h w/h 0 mode color))
                (make-bb w/h w/h w/h)
                #f)))

(define (mode-color->pen mode color)
  (send the-pen-list find-or-create-pen color 1 
        (case mode
          [(outline) 'solid]
          [(solid) 'transparent])))

(define (mode-color->brush mode color)
  (send the-brush-list find-or-create-brush color 
        (case mode
          [(outline) 'transparent]
          [(solid) 'solid])))

;; add-line : I number number number number -> I
;; add-line : string string I number number number number -> I
;; like add-line, but adapted to use coordinates relative the top-left of the I,
;; or to the user-specified spot

;; add-curve : I posn number number posn number number -> I
;; add-curve : string string I posn number number posn number number -> I
;; the posns are the start and end points of the curve
;; the pair of numbers following each posn are the angle and "pull" of the curve
;; see pin-line in slideshow
;; the initial strings in the second instance of add-curve are like the strings in add-line

(define/chk (image-width image) (image-right image))
(define/chk (image-height image) (image-bottom image))

(define-syntax (bitmap stx)
  (syntax-case stx ()
    [(_ arg)
     (let* ([arg (syntax->datum #'arg)]
            [path
             (cond
               [(and (pair? arg)
                     (eq? (car arg) 'planet))
                (raise-syntax-error 'bitmap "planet paths not yet supported" stx)]
               [(symbol? arg)
                (let ([pieces (regexp-split #rx"/" (symbol->string arg))])
                  (cond
                    [(null? pieces)
                     (raise-syntax-error 'bitmap "expected a path with a / in it" stx)]
                    [else
                     (let loop ([cps (current-library-collection-paths)])
                       (cond
                         [(null? cps)
                          (raise-syntax-error 'bitmap
                                              (format "could not find the ~a collection" (car pieces))
                                              stx)]
                         [else
                          (if (and (directory-exists? (car cps))
                                   (member (build-path (car pieces))
                                           (directory-list (car cps))))
                              (let ([candidate (apply build-path (car cps) pieces)])
                                (if (file-exists? candidate)
                                    candidate
                                    (raise-syntax-error 'bitmap 
                                                        (format "could not find ~a in the ~a collection"
                                                                (apply string-append (add-between (cdr pieces) "/"))
                                                                (car pieces))
                                                        stx)))
                              (loop (cdr cps)))]))]))]
               [(string? arg)
                (path->complete-path 
                 arg
                 (or (current-load-relative-directory)
                     (current-directory)))])])
       #`(make-object image-snip% (make-object bitmap% #,path 'unknown/mask)))]))

(provide overlay
         overlay/places
         overlay/xy
         beside
         beside/places
         above
         above/places
         
         rotate
         
         frame
         
         show-image
         save-image
         bring-between
         
         image-snip->image
         bitmap->image
         
         scale
         scale/xy
         
         x-place?
         y-place?
         mode?
         angle?
         side-count?
         color?
         
         image-width
         image-height
         
         circle
         ellipse
         rectangle
         square
         rhombus
         
         polygon
         regular-polygon
         triangle 
         isosceles-triangle
         right-triangle
         star
         star-polygon
         
         line
         add-line
         
         text
         text/font
         
         bitmap
         
         swizzle
         
         rotate-xy)

(provide/contract
 [atomic-bb (-> atomic-shape? (values real? real? real? real?))]
 [center-point (-> np-atomic-shape? number?)])