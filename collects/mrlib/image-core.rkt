#lang racket/base
#|

This library is the part of the 2htdp/image 
teachpack that has to be shared between drracket
and the user's program to make copy and paste
work right.

Most of the exports are just for use in 2htdp/image
(technically, 2htdp/private/image-more). The main
use of this library is the snip class addition it
does (and any code that does not depend on
has been moved out).


-- in the middle of text:

  - bounding boxes
  - rotating (and bounding boxes)
  - hbl append(?)
  - this doesn't work (how to test?)
(beside/places "baseline"
                 (text "ijy" 12 'black)
                 (text "ijy" 24 'black))
  - /places => /align

|#

(require racket/class
         racket/gui/base
         racket/math
         "private/image-core-bitmap.ss"
         (for-syntax racket/base))

(define-for-syntax id-constructor-pairs '())
(define-for-syntax (add-id-constructor-pair a b)
  (set! id-constructor-pairs (cons (list a b) id-constructor-pairs)))

(define-syntax (define-struct/reg-mk stx)
  (syntax-case stx ()
    [(_ id . rest)
     (let ([build-name
            (λ (fmt)
              (datum->syntax #'id (string->symbol (format fmt (syntax->datum #'id)))))])
       (add-id-constructor-pair (build-name "struct:~a")
                                (build-name "make-~a"))
       #'(define-struct id . rest))]))

(define-syntax (define-id->constructor stx)
  (syntax-case stx ()
    [(_ fn)
     #`(define (fn x)
         (case x
           #,@(map (λ (x) 
                     (with-syntax ([(struct: maker) x])
                       #`[(struct:) maker]))
                   id-constructor-pairs)))]))

(define-struct/reg-mk point (x y) #:transparent)


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


;; a image is 
;;  (make-image shape bb boolean)
;; NOTE: the shape field is mutated when normalized, as
;;       is the normalized? field.
(define (make-image shape bb normalized?) (new image% [shape shape] [bb bb] [normalized? normalized?]))
(define (image-shape p) (send p get-shape))
(define (image-bb p) (send p get-bb))
(define (image-normalized? p) (send p get-normalized?))
(define (set-image-shape! p s) (send p set-shape s))
(define (set-image-normalized?! p n?) (send p set-normalized? n?))
(define (image? p) 
  (or (is-a? p image%)
      (is-a? p image-snip%)
      (is-a? p bitmap%)))


;; a bb is  (bounding box)
;;  (make-bb number number number)
(define-struct/reg-mk bb (right bottom baseline) #:transparent)

;; a shape is either:
;;
;;  - (make-overlay shape shape)
;;    the shapes are in the order passed to the overlay or beside,
;;    which means the bottom one should be drawn first so as to appear
;;    underneath the top one.
(define-struct/reg-mk overlay (top bottom) #:transparent #:omit-define-syntaxes) 
;;
;;  - (make-translate dx dy shape)
(define-struct/reg-mk translate (dx dy shape) #:transparent #:omit-define-syntaxes)
;;
;;  - (make-scale x-factor y-factor shape)
(define-struct/reg-mk scale (x y shape) #:transparent #:omit-define-syntaxes)
;;
;;  - (make-crop (listof vector) shape)
(define-struct/reg-mk crop (points shape) #:transparent #:omit-define-syntaxes)
;;
;;  - atomic-shape

;; an atomic-shape is either:
;;  - polygon
;;  - line-segment
;;  - curve-segment
;;  - np-atomic-shape

;; a np-atomic-shape is:
;;
;;  - (make-ellipse width height angle mode color)
(define-struct/reg-mk ellipse (width height angle mode color) #:transparent #:omit-define-syntaxes)
;;
;;  - (make-text string angle number color
;;               number (or/c #f string) family (or/c 'normal 'italic) (or/c 'normal 'light 'bold) boolean)
;;    NOTE: font can't be the raw mred font or else copy & paste won't work
(define-struct/reg-mk text (string angle y-scale color size face family style weight underline) 
  #:omit-define-syntaxes #:transparent)
;;
;;  - (make-bitmap (is-a?/c bitmap%) angle positive-real (or/c #f (is-a?/c bitmap%)))
;;    NOTE: bitmap copying needs to happen in 'write' and 'read' methods
(define-struct/reg-mk bitmap (raw-bitmap raw-mask angle x-scale y-scale [rendered-bitmap #:mutable] [rendered-mask #:mutable])
  #:omit-define-syntaxes #:transparent)

;; a polygon is:
;;
;;  - (make-polygon (listof vector) mode color)
(define-struct/reg-mk polygon (points mode color) #:transparent #:omit-define-syntaxes)

;; a line-segment is
;;
;;  - (make-line-segment point point color)
(define-struct/reg-mk line-segment (start end color) #:transparent #:omit-define-syntaxes)

;; a curve-segment is
;;
;;  - (make-curve-segment point real real point real real color)
(define-struct/reg-mk curve-segment (start s-angle s-pull end e-angle e-pull color) #:transparent #:omit-define-syntaxes)

;; a normalized-shape (subtype of shape) is either
;;  - (make-overlay normalized-shape cn-or-simple-shape)
;;  - cn-or-simple-shape

;; an cn-or-simple-shape is either:
;;  - simple-shape
;;  - (make-crop (listof points) normalized-shape)

;; a simple-shape (subtype of shape) is
;;  - (make-translate dx dy np-atomic-shape))
;;  - polygon
;;  - line-segment
;;  - curve-segment

;; an angle is a number between 0 and 360 (degrees)

;; a mode is either 'solid or 'outline (indicating a pen width for outline mode)

;; a pen is
;;  - (make-pen color?  ;; <- the struct, not a string
;;              (<=/c 0 255)
;;              (or/c 'solid 'dot 'long-dash 'short-dash 'dot-dash)
;;              (or/c 'round 'projecting 'butt)
;;              (or/c 'round 'bevel 'miter))
(define-struct/reg-mk pen (color width style cap join) #:transparent)

;; an color is
;;  - (make-color (<=/c 0 255) (<=/c 0 255) (<=/c 0 255))
;;  - string
(define-struct/reg-mk color (red green blue) #:transparent)

;                                                   
;                                                   
;                                                   
;   ;;                                    ;;   ;;   
;   ;;                                   ;;;;  ;    
;   ;                                    ; ;; ;     
;   ;;  ;;;;;;;;; ;;;;;   ;;;;;;  ;;;;   ; ;; ;     
;   ;;  ;; ;;; ;;;;   ;;  ;; ;;  ;;; ;;   ;; ;      
;   ;;  ;; ;;; ;;;  ;;;; ;;; ;;  ;;;;;;      ; ;;;  
;   ;;  ;; ;;; ;;;;;  ;;   ;;;   ;;          ;;; ;; 
;   ;;  ;; ;;; ;;;;;  ;;  ;;;;;  ;;;  ;     ; ;; ;; 
;   ;;  ;; ;;; ;;;;;;;;;; ;;;;;;  ;;;;     ;;  ;;;  
;                        ;;   ;;                    
;                        ;;   ;                     
;                         ;;;;                      

(define-local-member-name
  get-shape set-shape get-bb 
  get-normalized? set-normalized get-normalized-shape)

(define skip-image-equality-fast-path (make-parameter #f))

(define image%
  (class* snip% (equal<%>)
    (init-field shape bb normalized?)
    (define/public (equal-to? that eq-recur)
      (or (eq? this that)
          (and (is-a? that image%)
               (same-bb? bb (send that get-bb))
               (or (and (not (skip-image-equality-fast-path))  ;; this is here to make testing more effective
                        (equal? (get-normalized-shape) (send that get-normalized-shape)))
                   (let ([w (+ 1 (round (inexact->exact (bb-right bb))))]    ;; some shapes (ie, rectangles) draw 1 outside the bounding box
                         [h (+ 1 (round (inexact->exact (bb-bottom bb))))])  ;; so we make the bitmap slightly bigger to accomodate that.
                     (or (zero? w)
                         (zero? h)
                         (let ([bm1 (make-object bitmap% w h)]
                               [bm2 (make-object bitmap% w h)]
                               [bytes1 (make-bytes (* w h 4) 0)]
                               [bytes2 (make-bytes (* w h 4) 0)]
                               [bdc (make-object bitmap-dc%)])
                           (and (check-same? bm1 bm2 bytes1 bytes2 bdc "red" that)
                                (check-same? bm1 bm2 bytes1 bytes2 bdc "green" that)))))))))
    
    (define/private (check-same? bm1 bm2 bytes1 bytes2 bdc color that)
      (clear-bitmap/draw/bytes bm1 bdc bytes1 this color)
      (clear-bitmap/draw/bytes bm2 bdc bytes2 that color)
      (equal? bytes1 bytes2))
    
    (define/private (clear-bitmap/draw/bytes bm bdc bytes obj color)
      (send bdc set-bitmap bm)
      (send bdc set-pen "black" 1 'transparent)
      (send bdc set-brush color 'solid)
      (send bdc draw-rectangle 0 0 (send bm get-width) (send bm get-height))
      (render-image obj bdc 0 0)
      (send bdc get-argb-pixels 0 0 (send bm get-width) (send bm get-height) bytes))
    
    (define/public (equal-hash-code-of y) 42)
    (define/public (equal-secondary-hash-code-of y) 3)

    (define/public (get-shape) shape)
    (define/public (set-shape s) (set! shape s))
    (define/public (get-bb) bb)
    (define/public (get-normalized?) normalized?)
    (define/public (set-normalized? n?) (set! normalized? n?))
    
    (define/public (get-normalized-shape)
      (unless normalized?
        (set! shape (normalize-shape shape))
        (set! normalized? #t))
      shape)
    
    (inherit get-admin)
    (define scroll-step #f)
    (define/private (calc-scroll-step)
      (unless scroll-step
        ;; try to set scroll step by font size of the standard style
        (let ([admin (get-admin)])
          (when admin
            (let* ([ed (send admin get-editor)]
                   [sl (send ed get-style-list)]
                   [standard (send sl find-named-style "Standard")])
              (when standard
                (let ([dc (make-object bitmap-dc% (make-object bitmap% 1 1))])
                  (let-values ([(w h d a) (send dc get-text-extent "X" (send standard get-font))])
                    (set! scroll-step (+ h
                                         (if (is-a? ed text%)
                                             (send ed get-line-spacing)
                                             0)))))))))
        ;; if that didn't happen, set it to 12.
        (unless scroll-step (set! scroll-step 12))))
    
    (define/override (get-num-scroll-steps)
      (calc-scroll-step)
      (inexact->exact (ceiling (/ (bb-bottom bb) scroll-step))))
    (define/override (get-scroll-step-offset offset)
      (calc-scroll-step)
      (min (inexact->exact (ceiling (* offset scroll-step))) 
           (bb-bottom bb)))
    (define/override (find-scroll-step y)
      (calc-scroll-step)
      (inexact->exact (ceiling (/ y scroll-step))))
               
    (define/override (copy) (make-image shape bb normalized?))
    (define/override (draw dc x y left top right bottom dx dy draw-caret?)
      (let ([smoothing (send dc get-smoothing)])
        (render-image this dc x y)))
    
    (define/override (get-extent dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (send (get-the-snip-class-list) add snip-class)
      (let ([bottom (round (bb-bottom bb))]
            [right (round (bb-right bb))])
        (set-box/f! w right)
        (set-box/f! h bottom)
        (set-box/f! descent (- bottom (round (bb-baseline bb))))
        (set-box/f! space 0)
        (set-box/f! lspace 0)
        (set-box/f! rspace 0)))

    (define/override (write f) 
      (let ([bytes (string->bytes/utf-8 (format "~s" (list shape bb)))])
        (send f put (bytes-length bytes) bytes)))
    
    (super-new)
    
    (inherit set-snipclass)
    (set-snipclass snip-class)))

(define (same-bb? bb1 bb2)
  (and (= (round (bb-right bb1)) (round (bb-right bb2)))
       (= (round (bb-bottom bb1)) (round (bb-bottom bb2)))
       (= (round (bb-baseline bb1)) (round (bb-baseline bb2)))))
(define racket/base:read read)

(define image-snipclass% 
  (class snip-class%
    (define/override (read f)
      (let* ([bytes (send f get-unterminated-bytes)]
             [str 
              (and bytes
                   (with-handlers ((exn:fail? (λ (x) #f)))
                     (bytes->string/utf-8 bytes)))]
             [lst 
              (and str
                   (with-handlers ((exn:fail:read? (λ (x) #f)))
                     (parse 
                      (racket/base:read
                       (open-input-string
                        str)))))])
        (if lst
            (make-image (list-ref lst 0)
                        (list-ref lst 1)
                        #f)
            (make-image (make-ellipse 100 100 0 'solid "black")
                        (make-bb 100 100 100)
                        #f))))
    (super-new)))

(provide snip-class) 
(define snip-class (new image-snipclass%))
(send snip-class set-classname (format "~s" '(lib "image-core.ss" "2htdp" "private")))
(send snip-class set-version 1)
(send (get-the-snip-class-list) add snip-class)

(define (set-box/f! b v) (when (box? b) (set-box! b v)))
 
(define (parse sexp)
  (let/ec k
    (let loop ([sexp sexp])
      (cond
        [(pair? sexp) (cons (loop (car sexp)) (loop (cdr sexp)))]
        [(vector? sexp)
         (if (= (vector-length sexp) 0)
             (k #f)
             (let ([constructor (id->constructor (vector-ref sexp 0))]
                   [args (cdr (vector->list sexp))])
               (if (and constructor
                        (procedure-arity-includes? constructor (length args)))
                   (apply constructor (map loop args))
                   (k #f))))]
        [else sexp]))))

(define-id->constructor id->constructor)

;; normalize-shape : shape (atomic-shape -> atomic-shape) -> normalized-shape
;; normalizes 'shape', calling 'f' on each atomic shape in the normalized shape.
(define (normalize-shape shape [f values])
  (let loop ([shape shape]
             [dx 0]
             [dy 0]
             [x-scale 1]
             [y-scale 1]
             [bottom #f])
    (define (scale-point p)
      (make-point (+ dx (* x-scale (point-x p)))
                  (+ dy (* y-scale (point-y p)))))
    (cond
      [(translate? shape)
       (loop (translate-shape shape)
             (+ dx (* x-scale (translate-dx shape)))
             (+ dy (* y-scale (translate-dy shape)))
             x-scale
             y-scale
             bottom)]
      [(scale? shape)
       (loop (scale-shape shape)
             dx
             dy
             (* x-scale (scale-x shape))
             (* y-scale (scale-y shape))
             bottom)]
      [(overlay? shape)
       (loop (overlay-bottom shape)
             dx dy x-scale y-scale 
             (loop (overlay-top shape)
                   dx dy x-scale y-scale 
                   bottom))]
      [(crop? shape)
       (let* ([inside (loop (crop-shape shape)
                            dx dy x-scale y-scale 
                            #f)]
              [this-one
               (make-crop (map scale-point (crop-points shape))
                          inside)])
         (if bottom
             (make-overlay bottom this-one)
             this-one))]
      [(polygon? shape)
       (let* ([this-one 
               (make-polygon (map scale-point (polygon-points shape))
                             (polygon-mode shape)
                             (scale-color (polygon-color shape) x-scale y-scale))])
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))]
      [(line-segment? shape)
       (let ([this-one 
              (make-line-segment (scale-point (line-segment-start shape))
                                 (scale-point (line-segment-end shape))
                                 (scale-color (line-segment-color shape) x-scale y-scale))])
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))]
      [(curve-segment? shape)
       ;; the pull is multiplied by the distance 
       ;; between the two points when it is drawn,
       ;; so we don't need to scale it here
       (let ([this-one 
              (make-curve-segment (scale-point (curve-segment-start shape))
                                  (curve-segment-s-angle shape)
                                  (curve-segment-s-pull shape)
                                  (scale-point (curve-segment-end shape))
                                  (curve-segment-e-angle shape)
                                  (curve-segment-e-pull shape)
                                  (scale-color (curve-segment-color shape) x-scale y-scale))])
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))]
      [(np-atomic-shape? shape)
       (let ([this-one 
              (make-translate dx dy (scale-np-atomic x-scale y-scale shape))])
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))]
      [else 
       (error 'normalize-shape "unknown shape ~s\n" shape)])))

(define (normalized-shape? s)
  (cond
    [(overlay? s)
     (and (normalized-shape? (overlay-top s))
          (cn-or-simple-shape? (overlay-bottom s)))]
    [else
     (cn-or-simple-shape? s)]))

(define (cn-or-simple-shape? s)
  (cond
    [(crop? s)
     (normalized-shape? (crop-shape s))]
    [else
     (simple-shape? s)]))

(define (simple-shape? shape)
  (or (and (translate? shape)
           (np-atomic-shape? (translate-shape shape)))
      (polygon? shape)
      (line-segment? shape)
      (curve-segment? shape)))

(define (atomic-shape? shape)
  (or (polygon? shape)
      (line-segment? shape)
      (curve-segment? shape)
      (np-atomic-shape? shape)))

(define (np-atomic-shape? shape)
  (or (ellipse? shape)
      (text? shape)
      (bitmap? shape)
      (point? shape)))

(define (scale-np-atomic x-scale y-scale shape)
  (cond
    [(ellipse? shape)
     (make-ellipse (* x-scale (ellipse-width shape))
                   (* y-scale (ellipse-height shape))
                   (ellipse-angle shape)
                   (ellipse-mode shape)
                   (scale-color (ellipse-color shape) x-scale y-scale))]
    [(text? shape)
     ;; should probably do something different here so that
     ;; the y-scale is always greater than 1
     ;; (or else always smaller than 1)
     (make-text (text-string shape)
                (text-angle shape)
                (* (text-y-scale shape) (/ y-scale x-scale))
                (text-color shape)
                (* (text-size shape) x-scale)
                (text-face shape)
                (text-family shape)
                (text-style shape)
                (text-weight shape)
                (text-underline shape))]
    [(bitmap? shape) 
     (make-bitmap (bitmap-raw-bitmap shape)
                  (bitmap-raw-mask shape)
                  (bitmap-angle shape)
                  (* x-scale (bitmap-x-scale shape))
                  (* y-scale (bitmap-y-scale shape))
                  #f #f)]))

(define (scale-color color x-scale y-scale)
  (cond
    [(pen? color)
     (make-pen (pen-color color)
               (* (pen-width color) (/ (+ x-scale y-scale) 2))
               (pen-style color)
               (pen-cap color)
               (pen-join color))]
    [else color]))

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

;; render-image : image dc dx dy -> void
(define (render-image image dc dx dy)
  (let ([pen (send dc get-pen)]
        [brush (send dc get-brush)]
        [font (send dc get-font)]
        [fg (send dc get-text-foreground)]
        [smoothing (send dc get-smoothing)])
    (render-normalized-shape (send image get-normalized-shape) dc dx dy)
    (send dc set-pen pen)
    (send dc set-brush brush)
    (send dc set-font font)
    (send dc set-text-foreground fg)
    (send dc set-smoothing smoothing)))

(define (save-image-as-bitmap image filename kind)
  (let* ([bb (send image get-bb)]
         [bm (make-object bitmap% 
               (+ 1 (ceiling (inexact->exact (bb-right bb))))
               (+ 1 (ceiling (inexact->exact (bb-bottom bb)))))]
         [bdc (make-object bitmap-dc% bm)])
    (send bdc clear)
    (render-image image bdc 0 0)
    (send bdc set-bitmap #f)
    (send bm save-file filename kind)))

(define (render-normalized-shape shape dc dx dy)
  (cond
    [(overlay? shape)
     (render-cn-or-simple-shape (overlay-bottom shape) dc dx dy)
     (render-normalized-shape (overlay-top shape) dc dx dy)]
    [else
     (render-cn-or-simple-shape shape dc dx dy)]))

(define last-cropped-points (make-parameter #f))

(define (render-cn-or-simple-shape shape dc dx dy)
  (cond
    [(crop? shape)
     (let ([points (crop-points shape)])
       (cond
         [(equal? points (last-cropped-points))
          (render-normalized-shape (crop-shape shape) dc dx dy)]
         [else
          (let ([old-region (send dc get-clipping-region)]
                [new-region (new region% [dc dc])]
                [path (polygon-points->path points)])
            (send new-region set-path path dx dy)
            (when old-region (send new-region intersect old-region))
            (send dc set-clipping-region new-region)
            (parameterize ([last-cropped-points points])
              (render-normalized-shape (crop-shape shape) dc dx dy))
            (send dc set-clipping-region old-region))]))]
    [else
     (render-simple-shape shape dc dx dy)]))

(define (render-simple-shape simple-shape dc dx dy)
  (cond
    [(polygon? simple-shape)
     (let ([mode (polygon-mode simple-shape)]
           [color (polygon-color simple-shape)]
           [path (polygon-points->path (polygon-points simple-shape))])
       (send dc set-pen (mode-color->pen mode color))
       (send dc set-brush (mode-color->brush mode color))
       (send dc set-smoothing (mode-color->smoothing mode color))
       (send dc draw-path path dx dy 'winding))]
    [(line-segment? simple-shape)
     (let* ([start (line-segment-start simple-shape)]
            [end (line-segment-end simple-shape)]
            [path (new dc-path%)]
            [sx (point-x start)]
            [sy (point-y start)]
            [ex (point-x end)]
            [ey (point-y end)])
       (send path move-to sx sy)
       (send path line-to ex ey)
       (send dc set-pen (mode-color->pen 'outline (line-segment-color simple-shape)))
       (send dc set-brush "black" 'transparent)
       (send dc set-smoothing 'smoothed)
       (send dc draw-path path dx dy))]
    [(curve-segment? simple-shape)
     (let* ([path (new dc-path%)]
            [start (curve-segment-start simple-shape)]
            [end (curve-segment-end simple-shape)]
            [sx (point-x start)]
            [sy (point-y start)]
            [ex (point-x end)]
            [ey (point-y end)]
            [sa (degrees->radians (curve-segment-s-angle simple-shape))]
            [ea (degrees->radians (curve-segment-e-angle simple-shape))]
            [d (sqrt (+ (sqr (- ey sy)) (sqr (- ex sx))))]
            [sp (* (curve-segment-s-pull simple-shape) d)]
            [ep (* (curve-segment-e-pull simple-shape) d)])
       (send path move-to sx sy)
       (send path curve-to
             (+ sx (* sp (cos sa)))
             (- sy (* sp (sin sa)))
             (- ex (* ep (cos ea)))
             (+ ey (* ep (sin ea)))
             ex
             ey)
       (send dc set-pen (mode-color->pen 'outline (curve-segment-color simple-shape)))
       (send dc set-brush "black" 'transparent)
       (send dc set-smoothing 'smoothed)
       (send dc draw-path path dx dy))]
    [else
     (let ([dx (+ dx (translate-dx simple-shape))]
           [dy (+ dy (translate-dy simple-shape))]
           [atomic-shape (translate-shape simple-shape)])
       (cond
         [(ellipse? atomic-shape)
          (let* ([path (new dc-path%)]
                 [ew (ellipse-width atomic-shape)]
                 [eh (ellipse-height atomic-shape)]
                 [θ (degrees->radians (ellipse-angle atomic-shape))]
                 [color (ellipse-color atomic-shape)]
                 [mode (ellipse-mode atomic-shape)])
            (let-values ([(rotated-width rotated-height) (ellipse-rotated-size ew eh θ)])
              (send path ellipse 0 0 ew eh)
              (send path translate (- (/ ew 2)) (- (/ eh 2)))
              (send path rotate θ)
              (send dc set-pen (mode-color->pen mode color))
              (send dc set-brush (mode-color->brush mode color))
              (send dc set-smoothing (mode-color->smoothing mode color))
              (send dc draw-path path dx dy)))]
         [(bitmap? atomic-shape)
          (let ([bm (get-rendered-bitmap atomic-shape)]) 
            (send dc draw-bitmap 
                  bm
                  (- dx (/ (send bm get-width) 2))
                  (- dy (/ (send bm get-height) 2))
                  'solid
                  (send the-color-database find-color "black")
                  (get-rendered-mask atomic-shape)))]
         [(text? atomic-shape)
          (let ([θ (degrees->radians (text-angle atomic-shape))]
                [font (send dc get-font)])
            (send dc set-font (text->font atomic-shape))
            (send dc set-text-foreground 
                  (or (send the-color-database find-color (text-color atomic-shape))
                      (send the-color-database find-color "black")))
            (let-values ([(w h _1 _2) (send dc get-text-extent (text-string atomic-shape))])
              (let ([p (- (make-rectangular dx dy)
                          (* (make-polar 1 (- θ)) (make-rectangular (/ w 2) (/ h 2))))])
                (send dc draw-text (text-string atomic-shape) 
                      (real-part p)
                      (imag-part p) 
                      #f 0 θ))))]))]))

(define (polygon-points->path points)
  (let ([path (new dc-path%)])
    (send path move-to (round (point-x (car points))) (round (point-y (car points))))
    (let loop ([points (cdr points)])
      (unless (null? points)
        (send path line-to 
              (round (point-x (car points)))
              (round (point-y (car points))))
        (loop (cdr points))))
    (send path close)
    ;(send path line-to (round (point-x (car points))) (round (point-y (car points))))
    path))

(define (points->bb-path points)
  (let ([path (new dc-path%)])
    (let-values ([(left top right bottom) (points->ltrb-values points)])
      (send path move-to left top)
      (send path line-to right top)
      (send path line-to right bottom)
      (send path line-to left bottom)
      (send path line-to left top)
      path)))

;; points->ltrb-values : (cons point (listof points)) -> (values number number number number)
(define (points->ltrb-values points)
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
    (values left top right bottom)))
  
#|

the mask bitmap and the original bitmap are all together in a single bytes!

|#


(define (get-rendered-bitmap bitmap)
  (calc-renered-bitmap bitmap)
  (bitmap-rendered-bitmap bitmap))

(define (get-rendered-mask bitmap)
  (calc-renered-bitmap bitmap)
  (bitmap-rendered-mask bitmap))

(define (calc-renered-bitmap bitmap)
  (unless (bitmap-rendered-bitmap bitmap)
    ;; fill in the rendered bitmap with the raw bitmaps.
    (set-bitmap-rendered-bitmap! bitmap (bitmap-raw-bitmap bitmap))
    (set-bitmap-rendered-mask! bitmap (bitmap-raw-mask bitmap))
    (cond
      [(and (= 1 (bitmap-x-scale bitmap))
            (= 1 (bitmap-y-scale bitmap))
            (= 0 (bitmap-angle bitmap)))
       ;; if there's no scaling or rotation, we can just keep that bitmap.
       (void)]
      [(<= (* (bitmap-x-scale bitmap) 
              (bitmap-y-scale bitmap))
           1)
       ;; since we prefer to rotate big things, we rotate first
       (do-rotate bitmap)
       (do-scale bitmap)]
      [else
       ;; since we prefer to rotate big things, we scale first
       (do-scale bitmap)
       (do-rotate bitmap)])))

(define (do-rotate bitmap)
  (let ([θ (degrees->radians (bitmap-angle bitmap))])
    (let-values ([(bytes w h) (bitmap->bytes (bitmap-rendered-bitmap bitmap) 
                                             (bitmap-rendered-mask bitmap))])
      (let-values ([(rotated-bytes rotated-w rotated-h)
                    (rotate-bytes bytes w h θ)])
        (let* ([bm (bytes->bitmap rotated-bytes rotated-w rotated-h)]
               [mask (send bm get-loaded-mask)])
          (set-bitmap-rendered-bitmap! bitmap bm)
          (set-bitmap-rendered-mask! bitmap mask))))))

(define (do-scale bitmap)
  (let* ([bdc (make-object bitmap-dc%)]
         [orig-bm (bitmap-rendered-bitmap bitmap)]
         [orig-mask (bitmap-rendered-mask bitmap)]
         [orig-w (send orig-bm get-width)]
         [orig-h (send orig-bm get-height)]
         [x-scale (bitmap-x-scale bitmap)]
         [y-scale (bitmap-y-scale bitmap)]
         [scale-w (ceiling (inexact->exact (* x-scale (send orig-bm get-width))))]
         [scale-h (ceiling (inexact->exact (* y-scale (send orig-bm get-height))))]
         [new-bm (make-object bitmap% scale-w scale-h)]
         [new-mask (and orig-mask (make-object bitmap% scale-w scale-h))])
    (when new-mask
      (send new-bm set-loaded-mask new-mask))
    
    (send bdc set-bitmap new-bm)
    (send bdc set-scale x-scale y-scale)
    (send bdc clear)
    (send bdc draw-bitmap orig-bm 0 0)

    (when new-mask
      (send bdc set-bitmap new-mask)
      (send bdc set-scale x-scale y-scale)
      (send bdc clear)
      (send bdc draw-bitmap orig-mask 0 0))
    
    (send bdc set-bitmap #f)
    
    (set-bitmap-rendered-bitmap! bitmap new-bm)
    (set-bitmap-rendered-mask! bitmap new-mask)))

(define (text->font text)
  (cond
    [(text-face text)
     (send the-font-list find-or-create-font
           (text-size text)
           (text-face text)
           (text-family text)
           (text-style text) 
           (text-weight text)
           (text-underline text))]
    [else
     (send the-font-list find-or-create-font
           (text-size text)
           (text-family text)
           (text-style text) 
           (text-weight text)
           (text-underline text))]))

(define (ellipse-rotated-size ew eh θ)
  (cond
    [(and (zero? ew) (zero? eh))
     (values 0 0)]
    [(zero? eh)
     (values (* (cos θ) ew)
             (* (sin θ) ew))]
    [(zero? ew)
     (values (* (sin θ) eh)
             (* (cos θ) eh))]
    [else
     (let* ([t1 (atan (/ eh ew (exact->inexact (tan θ))))]
            ; a*cos(t1),b*sin(t1) is the point on *original* ellipse which gets rotated to top.
            [t2 (atan (/ (* (- eh) (tan θ)) ew))] ; the original point rotated to right side.
            [rotated-height (+ (* ew (sin θ) (cos t1)) (* eh (cos θ) (sin t1)))]
            [rotated-width  (- (* ew (cos θ) (cos t2)) (* eh (sin θ) (sin t2)))])
       (values (abs rotated-width)
               (abs rotated-height)))]))

(define (degrees->radians θ)
  (* θ 2 pi (/ 360)))

(define (mode-color->smoothing mode color)
  (cond
    [(and (eq? mode 'outline)
          (not (pen? color)))
     'aligned]
    [else 'smoothed]))

(define (mode-color->pen mode color)
  (case mode
    [(outline) 
     (cond
       [(pen? color)
        (pen->pen-obj/cache color)]
       [else
        (send the-pen-list find-or-create-pen (get-color-arg color) 0 'solid)])]
    [(solid)
     (send the-pen-list find-or-create-pen "black" 1 'transparent)]))

(define (mode-color->brush mode color)
  (case mode
    [(outline)
     (send the-brush-list find-or-create-brush "black" 'transparent)]
    [(solid)
     (send the-brush-list find-or-create-brush (get-color-arg color) 'solid)]))

(define (get-color-arg color)
  (if (string? color) 
      color
      (make-object color% 
        (color-red color)
        (color-green color)
        (color-blue color))))


(define pen-ht (make-hash))

(define (pen->pen-obj/cache pen)
  (cond
    [(and (equal? 'round (pen-join pen))
          (equal? 'round (pen-cap pen)))
     (send the-pen-list find-or-create-pen 
           (pen-color pen)
           (pen-width pen)
           (pen-style pen))]
    [else
     (let* ([wb/f (hash-ref pen-ht pen #f)]
            [pen-obj/f (and (weak-box? wb/f) (weak-box-value wb/f))])
       (or pen-obj/f
           (let ([pen-obj (pen->pen-obj pen)])
             (hash-set! pen-ht pen (make-weak-box pen-obj))
             pen-obj)))]))

(define (pen->pen-obj pen)
  (let ([ans (make-object pen% 
               (pen-color pen)
               (pen-width pen)
               (pen-style pen))])
    (send ans set-cap (pen-cap pen))
    (send ans set-join (pen-join pen))
    ans))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide make-image image-shape image-bb image-normalized? image%
         
         (struct-out bb)
         (struct-out point)
         make-overlay overlay? overlay-top overlay-bottom
         make-translate translate? translate-dx translate-dy translate-shape
         make-scale scale? scale-x scale-y scale-shape
	 make-crop crop? crop-points crop-shape
         make-ellipse ellipse? ellipse-width ellipse-height ellipse-angle ellipse-mode ellipse-color
         make-text text? text-string text-angle text-y-scale text-color
         text-angle text-size text-face text-family text-style text-weight text-underline
         make-polygon polygon? polygon-points polygon-mode polygon-color
         make-line-segment line-segment? line-segment-start line-segment-end line-segment-color
         make-curve-segment curve-segment? 
         curve-segment-start curve-segment-s-angle curve-segment-s-pull
         curve-segment-end curve-segment-e-angle curve-segment-e-pull 
         curve-segment-color
         make-pen pen? pen-color pen-width pen-style pen-cap pen-join pen
         
         make-bitmap bitmap? bitmap-raw-bitmap bitmap-raw-mask bitmap-angle bitmap-x-scale bitmap-y-scale 
         bitmap-rendered-bitmap bitmap-rendered-mask

         (struct-out color)
         
         degrees->radians
         normalize-shape
         ellipse-rotated-size
         points->ltrb-values

         image?
         
         text->font
         render-image
         save-image-as-bitmap
         
         skip-image-equality-fast-path
         
         scale-np-atomic)

;; method names
(provide get-shape get-bb get-normalized? get-normalized-shape)

(provide np-atomic-shape? atomic-shape? simple-shape? cn-or-simple-shape? normalized-shape?)
