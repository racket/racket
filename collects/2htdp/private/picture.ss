#lang scheme/base
(require scheme/class
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
         
         ;; internal stuff, for the test suite
         
         show-picture
         
         normalize-shape
         rotate-atomic
         rotate-simple
         simple-bb
         make-picture picture-shape
         
         make-bb
         make-overlay
         make-translate
         make-ellipse
         make-text
         make-polygon
         make-point)

(define-struct point (x y) #:transparent)

;; when rendering these things in error messages,
;; they should come out as #<picture: {THE ACTUAL PICTURE}>
;; (automatically scale them down so they fit)

;; redex randomized testing: see if normalization produces normalized shapes.
;;        see if normalization always puts things in the right order

;; need to change error messages to say "the width (second) argument"
;; by passing "width (second)" to the check-arg function

#|

From Matthias: (to use to compare with this library)


You asked about exercises and code snippets for HtDP/2e yesterday. I actually do have a bunch of stuff in

 svn: 2HtDP/SampleCode/

and they all have good sample contracts. (It is amazing what we can do with kids who have just a few weeks of cs down; I would have never dared to write an editor after six weeks in Algol.)


|#


;                                                                
;                                                                
;                                                                
;                                                                
;                                                                
;                                                                
;       ;;           ;;                  ;;           ;;;        
;       ;;          ;;;                  ;;          ;;;         
;    ;;;;;   ;;;;  ;;;;;  ;;;;        ;;;;;   ;;;;  ;;;;; ;; ;;; 
;   ;;;;;;  ;;  ;;  ;;;; ;;  ;;      ;;;;;;  ;;  ;; ;;;;  ;;;;;; 
;  ;;;  ;;    ;;;;  ;;;    ;;;;     ;;;  ;; ;;;;;;;; ;;   ;;  ;; 
;  ;;;  ;;  ;;; ;;  ;;;  ;;; ;;     ;;;  ;; ;;;      ;;   ;;  ;; 
;   ;;;;;; ;;;  ;;  ;;;;;;;  ;;      ;;;;;;  ;;; ;;  ;;   ;;  ;; 
;    ;;;;;  ;;;;;;   ;;; ;;;;;;       ;;;;;   ;;;;   ;;   ;;  ;; 
;                                                                
;                                                                
;                                                                
;                                                         


;; a picture is 
;;  (make-picture shape bb boolean)
;; NOTE: the shape field is mutated when normalized, as
;;       is the normalized? field.
(define-struct picture (shape bb normalized?)  #:mutable #:transparent)

;; a bb is  (bounding box)
;;  (make-bb number number number)
(define-struct bb (right bottom baseline) #:transparent)

;; a shape is either:
;;
;;  - (make-overlay shape shape)
;;    the shapes are in the order passed to the overlay or beside,
;;    which means the bottom one should be drawn first so as to appear
;;    underneath the top one.
(define-struct overlay (top bottom) #:transparent #:omit-define-syntaxes) 
;;
;;  - (make-translate dx dy shape)
(define-struct translate (dx dy shape) #:transparent #:omit-define-syntaxes)
;;
;;  - atomic-shape

;; an atomic-shape is either:
;;
;;  - (make-ellipse width height angle pen brush)
(define-struct ellipse (width height angle pen brush) #:transparent #:omit-define-syntaxes)
;;
;;  - (make-text string angle font)
(define-struct text (string angle font) #:omit-define-syntaxes #:transparent)
;;
;;  - (make-polygon (listof points) angle pen brush)
(define-struct polygon (points angle pen brush) #:transparent)
;;
;;  - (make-bitmap (is-a?/c bitmap%) angle)
(define-struct bitmap (bitmap angle))

;; a normalized-shape (subtype of shape) is either
;;  - (make-overlay normalized-shape simple-shape)
;;  - simple-shape

;; a simple-shape (subtype of shape) is
;;  - (make-translate dx dy atomic-shape)

;; picture-normalized-shape : picture -> normalized-shape
(define (picture-normalized-shape picture)
  (unless (picture-normalized? picture)
    (set-picture-shape! picture (normalize-shape (picture-shape picture) values))
    (set-picture-normalized?! picture #t))
  (picture-shape picture))

;; normalize-shape : shape (atomic-shape -> void) -> normalized-shape
;; normalizes 'shape', calling 'f' on each atomic shape in the normalized shape.
(define (normalize-shape shape [f values])
  (let loop ([shape shape]
             [dx 0]
             [dy 0]
             [bottom #f])
    (cond
      [(translate? shape)
       (loop (translate-shape shape)
             (+ dx (translate-dx shape))
             (+ dy (translate-dy shape))
             bottom)]
      [(overlay? shape)
       (loop (overlay-bottom shape)
             dx dy
             (loop (overlay-top shape)
                   dx dy bottom))]
      [(atomic-shape? shape)
       (let ([this-one (make-translate dx dy shape)]) 
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))])))

(define (atomic-shape? shape)
  (or (ellipse? shape)
      (text? shape)
      (polygon? shape)
      (bitmap? shape)))


;; rotate-point : x,y theta -> x,y
(define (rotate-point x y θ)
  (c->xy (* (make-polar 1 θ) 
            (xy->c x y))))

(define (xy->c x y) (make-rectangular x (- y)))
(define (c->xy c) 
  (values (real-part c)
          (- (imag-part c))))

(define (picture-right picture) (bb-right (picture-bb picture)))
(define (picture-bottom picture) (bb-bottom (picture-bb picture)))
(define (picture-baseline picture) (bb-baseline (picture-bb picture)))


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
                (member arg '("left" left "right" right "middle" middle "center" center))
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
                (member arg '("top" top "bottom" bottom "middle" middle "center" center "baseline" baseline))
                'y-place
                i
                arg)
     (let ([sym (if (string? arg)
                    (string->symbol arg)
                    arg)])
       (if (eq? sym 'center)
           'middle
           sym))]
    [(picture picture1 picture2 picture3) 
     (check-arg fn-name
                (picture? arg)
                'picture
                i
                arg)
     arg]
    [(mode)
     (check-arg fn-name
                (member arg '(solid outline "solid" "outline"))
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
    [(dx dy angle)
     (check-arg fn-name
                (number? arg)
                'number
                i arg)
     arg]
    [(color)
     (check-color fn-name i arg)
     (cond
       [(symbol? arg)
        (send the-color-database find-color (symbol->string arg))]
       [(string? arg)
        (send the-color-database find-color arg)]
       [else arg])]
    [else
     (error 'check "the function ~a has an argument with an unknown name: ~s"
            fn-name
            argname)]))



;                                                                
;                                                                
;                                                                
;                                                                
;                                                                
;                                                                
;                            ;;               ;;                 
;                            ;;               ;;                 
;   ;;;;  ;;;;   ;; ;;;   ;;;;;   ;;;;   ;;;;;;;  ;; ;;;  ;;;;;; 
;   ;;;; ;;  ;;  ;;;;;;  ;;;;;;  ;;  ;;  ;;;; ;;  ;;;;;;  ;;;;;; 
;   ;;  ;;;;;;;; ;;  ;; ;;;  ;; ;;;;;;;; ;;   ;;  ;;  ;; ;;;  ;; 
;   ;;  ;;;      ;;  ;; ;;;  ;; ;;;      ;;   ;;  ;;  ;; ;;;  ;; 
;   ;;   ;;; ;;  ;;  ;;  ;;;;;;  ;;; ;;  ;;   ;;  ;;  ;;  ;;;;;; 
;   ;;    ;;;;   ;;  ;;   ;;;;;   ;;;;   ;;   ;;  ;;  ;;   ;;;;; 
;                                                         ;; ;;; 
;                                                         ;;;;;  
;                                                                
;                                                         

(define (show-picture g [extra-space 0])
  (letrec ([f (new frame% [label ""])]
           [c (new canvas% 
                   [parent f]
                   [min-width (+ extra-space (inexact->exact (floor (picture-right g))))]
                   [min-height (+ extra-space (inexact->exact (floor (picture-bottom g))))]
                   [paint-callback
                    (λ (c dc)
                      (send dc set-smoothing 'aligned)
                      (let-values ([(w h) (send c get-client-size)])
                        (let ([scale (send sl get-value)])
                          (send dc set-scale scale scale)
                          (render-picture 
                           g
                           dc
                           (inexact->exact (floor (- (/ w 2 scale) (/ (picture-right g) 2))))
                           (inexact->exact (floor (- (/ h 2 scale) (/ (picture-bottom g) 2))))))))])]
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
    (send (new button% [label "²"] [callback (λ x (scale-adjust add1))] [parent bp]) min-width 100)
    (send f show #t)))

;; render-picture : normalized-shape dc dx dy -> void
(define (render-picture picture dc dx dy)
  (let loop ([shape (picture-normalized-shape picture)])
    (cond
      [(overlay? shape)
       (render-simple-shape (overlay-bottom shape) dc dx dy)
       (loop (overlay-top shape))]
      [else
       (render-simple-shape shape dc dx dy)])))

(define (render-simple-shape shape dc dx dy)
  (let ([dx (+ dx (translate-dx shape))]
        [dy (+ dy (translate-dy shape))]
        [atomic-shape (translate-shape shape)])
    (cond
      [(ellipse? atomic-shape)
       (let ([path (new dc-path%)]
             [θ (ellipse-angle atomic-shape)])
         (send path ellipse 0 0 (ellipse-width atomic-shape) (ellipse-height atomic-shape))
         (send path rotate θ)
         (send dc set-pen (ellipse-pen atomic-shape))
         (send dc set-brush (ellipse-brush atomic-shape))
         (send dc draw-path path dx dy))]
      [(polygon? atomic-shape)
       (let ([path (new dc-path%)]
             [points (polygon-points atomic-shape)]
             [θ (polygon-angle atomic-shape)])
         (send path move-to (point-x (car points)) (point-y (car points)))
         (let loop ([points (cdr points)])
           (unless (null? points)
             (send path line-to (point-x (car points)) (point-y (car points)))
             (loop (cdr points))))
         (send path line-to (point-x (car points)) (point-y (car points)))
         (send path rotate θ)
         (send dc set-pen (polygon-pen atomic-shape))
         (send dc set-brush (polygon-brush atomic-shape))
         (send dc draw-path path dx dy))]
      [(text? atomic-shape)
       (let ([θ (text-angle atomic-shape)])
         (send dc set-font (text-font atomic-shape))
         (send dc draw-text (text-string atomic-shape) dx dy #f 0 angle))])))


;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   
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
;                                                                           ;  ;   ;

;; bitmap : string -> picture
;; gets one of the bitmaps that comes with drscheme, scales it down by 1/8 or something
;; so that later scaling /translation/whatever will look reasonable.
;; (the error message for a bad argument will list all of the currently installed example pictures;
;; we may want to have some way teachers can stick new ones in there)

;; overlay : picture picture picture ... -> picture
;; places pictures on top of each other with their upper left corners aligned. last one goes on the bottom

(define/chk (overlay picture picture2 . picture3)
  (overlay/internal 'left 'top picture (cons picture2 picture3)))

;; overlay/places : string string picture picture picture ... -> picture
;; the first string has to be one of "center" "middle" "left" or "right" (or symbols)
;; the second string has to be one of "center" "middle" "top" "bottom" or "baseline" (or symbols)
;; behaves like overlay, but lines up the pictures in the various places.
;; overlay without string arguments is the same as passing "left" and "top"
;; for the two string arguments. Passing, eg, "center" "center" lines the
;; pictures up at their centers.

(define/chk (overlay/places x-place y-place picture picture2 . picture3)
  (overlay/internal x-place y-place picture (cons picture2 picture3)))

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

(define (find-x-spot x-place picture)
  (case x-place
    [(left) 0]
    [(middle) (/ (picture-right picture) 2)]
    [(right) (picture-right picture)]))

(define (find-y-spot y-place picture)
  (case y-place
    [(top) 0]
    [(middle) (/ (picture-bottom picture) 2)]
    [(bottom) (picture-bottom picture)]
    [(baseline) (picture-baseline picture)]))

;; overlay/xy : picture number number picture -> picture
;; places pictures on top of each other with their upper-left corners offset by the two numbers

(define/chk (overlay/xy picture dx dy picture2) 
  (overlay/δ picture
             (if (< dx 0) (- dx) 0)
             (if (< dy 0) (- dy) 0)
             picture2
             (if (< dx 0) 0 dx)
             (if (< dy 0) 0 dy)))

(define (overlay/δ picture1 dx1 dy1 picture2 dx2 dy2)
  (make-picture (make-overlay (make-translate dx1 dy1 (picture-shape picture1))
                              (make-translate dx2 dy2 (picture-shape picture2)))
                (make-bb (max (+ (picture-right picture1) dx1)
                              (+ (picture-right picture2) dx2))
                         (max (+ (picture-bottom picture1) dy1)
                              (+ (picture-bottom picture2) dy2))
                         (max (+ (picture-baseline picture1) dy1)
                              (+ (picture-baseline picture2) dy2)))
                #f))

;; beside : picture picture picture ... -> picture
;; places pictures in a single horizontal row, top aligned
(define/chk (beside picture1 picture2 . picture3)
  (beside/internal 'top picture1 (cons picture2 picture3)))

;; beside/places : string picture picture picture ... -> picture
;; places pictures in a horizontal row where the vertical alignment is
;; covered by the string argument
(define/chk (beside/places y-place picture1 picture2 . picture3)
  (beside/internal y-place picture1 (cons picture2 picture3)))

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
                          (picture-right fst)
                          (if (< dy 0) 0 dy))
               (cdr rst)))])))

;; above : picture picture picture ... -> picture
;; above/places : string I I I ... -> I
;; like beside, but vertically


;; frame : picture -> picture
;; draws a black frame around a picture where the bounding box is
;; (useful for debugging pictures)

(define/chk (frame picture)
  (make-picture (make-overlay (picture-shape picture)
                              (picture-shape 
                               (rectangle (picture-right picture)
                                          (picture-bottom picture)
                                          'outline
                                          'black)))
                (make-bb (picture-right picture)
                         (picture-bottom picture)
                         (picture-baseline picture))
                #f))

;; scale : I number -> I
;; scales the I by the given factor

;; rotate : I number -> I
;; rotates the I around the top-left corner by the given angle
;; (in degrees)
;; LINEAR TIME OPERATION (sigh)
(define/chk (rotate angle picture)
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
  (let* ([rotated (normalize-shape (picture-shape picture) add-to-bounding-box/rotate)])
    (make-picture (make-translate (- left) (- top) rotated)
                  (make-bb (- right left) (- bottom top) (- bottom top))
                  #f)))

;; simple-bb : simple-shape -> (values number number number number)
;; returns the bounding box of 'shape' 
;; (only called for rotated shapes, so bottom=baseline)
(define (simple-bb simple-shape)
  (let ([dx (translate-dx simple-shape)]
        [dy (translate-dy simple-shape)]
        [atomic-shape (translate-shape simple-shape)])
    (cond
      [(polygon? atomic-shape)
       (let ([θ (polygon-angle atomic-shape)]
             [points (polygon-points atomic-shape)])
         (let-values ([(x y) (rotate-point (point-x (car points)) (point-y (car points)) θ)])
           (let ([left x]
                 [top y]
                 [right x]
                 [bottom y])
             (for-each (λ (point)
                         (let-values ([(new-x new-y)
                                       (rotate-point (point-x point) (point-y point) θ)])
                           (set! left (min new-x left))
                           (set! top (min new-y top))
                           (set! right (max new-x right))
                           (set! bottom (max new-y bottom))))
                       (cdr points))
             (values (+ dx left) (+ dy top) (+ dx right) (+ dy bottom)))))]
      [else
       (fprintf (current-error-port) "BAD\n")
       (values 0 0 100 100)])))


;; rotate-simple : angle simple-shape -> simple-shape
(define (rotate-simple θ simple-shape)
  (let-values ([(dx dy) (c->xy (* (make-polar 1 θ) 
                                  (xy->c (translate-dx simple-shape)
                                         (translate-dy simple-shape))))])
    (make-translate
     dx
     dy
     (rotate-atomic θ (translate-shape simple-shape)))))

;; rotate-atomic : angle atomic-shape -> atomic-shape
(define (rotate-atomic θ atomic-shape)
  (cond
    [(ellipse? atomic-shape)
     (make-ellipse (ellipse-width atomic-shape)
                   (ellipse-height atomic-shape)
                   (+ θ (ellipse-angle atomic-shape))
                   (ellipse-pen atomic-shape)
                   (ellipse-brush atomic-shape))]
    [(text? atomic-shape)
     (make-text (text-string atomic-shape)
                (+ θ (text-angle atomic-shape))
                (text-font atomic-shape))]
    [(polygon? atomic-shape)
     (make-polygon (polygon-points atomic-shape)
                   (+ θ (polygon-angle atomic-shape))
                   (polygon-pen atomic-shape)
                   (polygon-brush atomic-shape))]
    [(bitmap? atomic-shape)
     (make-bitmap (bitmap-bitmap atomic-shape)
                  (+ θ (bitmap-angle atomic-shape)))]))
  
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
  (make-picture (make-polygon (list (make-point 0 0)
                                    (make-point width 0)
                                    (make-point width height)
                                    (make-point 0 height))
                              0
                              (mode-color->pen mode color)
                              (mode-color->brush mode color))
                (make-bb width
                         height
                         height)
                #f))

;;       circle
;;       ellipse
;;       triangle
;;       line
;;       star
;;       text
;;       regular-polygon

(define/chk (ellipse width height mode color)
  (make-picture (make-ellipse width height 
                              0
                              (mode-color->pen mode color)
                              (mode-color->brush mode color))
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

