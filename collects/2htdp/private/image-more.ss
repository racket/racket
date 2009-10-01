#lang scheme/base

(require "image-core.ss"
         scheme/class
         scheme/gui/base
         htdp/error
         scheme/math
         (for-syntax scheme/base))

(provide overlay
         overlay/places
         overlay/xy
         
         beside
         beside/places
         
         rotate
         
         frame
         
         ellipse
         rectangle
         
         show-image
         bring-between
         
         x-place?
         y-place?
         mode?
         angle?)


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
     arg]
    [(mode)
     (check-arg fn-name
                (mode? arg)
                'mode
                i
                arg)
     (if (string? arg)
         (string->symbol arg)
         arg)]
    [(width height)
     (check-arg fn-name
                (and (number? arg)
                     (not (negative? arg)))
                'non-negative-number
                i arg)
     arg]
    [(dx dy)
     (check-arg fn-name
                (number? arg)
                'number
                i arg)
     arg]
    [(angle)
     (check-arg fn-name
                (angle? arg)
                'angle\ in\ degrees
                i arg)
     arg]
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
  (and (number? arg)
       (real? arg)
       (<= 0 arg)
       (< arg 360)))


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
;; above/places : string I I I ... -> I
;; like beside, but vertically


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
  (define left #f)
  (define top #f)
  (define right #f)
  (define bottom #f)
  (define (add-to-bounding-box/rotate simple-shape)
    (let ([rotated-shape (rotate-simple angle simple-shape)])
      (let-values ([(this-left this-top this-right this-bottom) (simple-bb rotated-shape)])
        (set! left (if left (min this-left left) this-left))
        (set! top (if top (min this-top top) this-top))
        (set! right (if right (max this-right right) this-right))
        (set! bottom (if bottom (max this-bottom bottom) this-bottom)))
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
#|
>>  (rotate theta (ellipse w h _ _))
>> is
>>  (let* {[cos2 (sqr (cos theta))]
>>         [sin2 (sqr (sin theta))]
>>         }
>>    (make-bbox (+ (* w cos2) (* h sin2))
>>               (+ (* w sin2) (* h cos2)))
>>               ... ;; baseline is same as y, for non-text, right?
>>               )
>>
|#
     
     (let ([dx (translate-dx simple-shape)]
           [dy (translate-dy simple-shape)]
           [atomic-shape (translate-shape simple-shape)])
       (cond
         [(ellipse? atomic-shape)
          (let* ([theta (degrees->radians (ellipse-angle atomic-shape))]
                 [w (ellipse-width atomic-shape)]
                 [h (ellipse-height atomic-shape)]
                 [cos2 (sqr (cos theta))]
                 [sin2 (sqr (sin theta))]
                 [new-w (+ (* w cos2) (* h sin2))]
                 [new-h (+ (* w sin2) (* h cos2))])
            (values (+ dx (/ (- new-w w) 2))
                    (+ dy (/ (- new-h h) 2))
                    (+ dx new-w (/ (- new-w w) 2))
                    (+ dy new-h (/ (- new-h h) 2))))]
         [else
          (fprintf (current-error-port) "BAD BOUNDING BOX\n")
          (values 0 0 100 100)]))]))

;; rotate-simple : angle simple-shape -> simple-shape
(define (rotate-simple θ simple-shape)
  (cond
    [(polygon? simple-shape)
     (make-polygon (map (λ (p)
                          (let-values ([(xn yn) (rotate-point (point-x p) (point-y p) θ)])
                            (make-point xn yn)))
                        (polygon-points simple-shape))
                   (polygon-mode simple-shape)
                   (polygon-color simple-shape))]
    [else
     (let-values ([(dx dy) (c->xy (* (make-polar 1 (degrees->radians θ))
                                     (xy->c (translate-dx simple-shape)
                                            (translate-dy simple-shape))))])
       (make-translate
        dx
        dy
        (rotate-atomic θ (translate-shape simple-shape))))]))

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
                (text-font atomic-shape))]
    [(bitmap? atomic-shape)
     (make-bitmap (bitmap-bitmap atomic-shape)
                  (bring-between (+ θ (bitmap-angle atomic-shape)) 360))]))

;; rotate-point : x,y angle -> x,y
(define (rotate-point x y θ)
  (c->xy (* (make-polar 1 (degrees->radians θ)) 
            (xy->c x y))))

(define (xy->c x y) (make-rectangular x (- y)))
(define (c->xy c) 
  (values (real-part c)
          (- (imag-part c))))


;; bring-between : number number -> number
;; returns a number that is much like the modulo of 'x' and 'upper-bound'
;; but does this by repeated subtraction, since modulo only works on integers
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

(define/chk (rectangle width height mode color)
  (make-image (make-polygon (rectangle-points width height)
                            mode
                            color)
              (make-bb width
                       height
                       height)
              #f))

(define (rectangle-points width height)
  (list (make-point 0 0)
        (make-point width 0)
        (make-point width height)
        (make-point 0 height)))
  

;;       circle
;;       ellipse
;;       triangle
;;       line
;;       star
;;       text
;;       regular-polygon

(define/chk (ellipse width height mode color)
  (make-image (make-ellipse width height 
                            0
                            mode
                            color)
              (make-bb width height height)
              #f))

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

