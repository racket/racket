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
         racket/contract
         "private/image-core-bitmap.ss"
         "image-core-wxme.ss"
         "private/image-core-snipclass.rkt"
         "private/regmk.rkt"
         (prefix-in cis: "cache-image-snip.ss")
         (for-syntax racket/base))




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
;;  (make-image shape bb boolean (or/c point #f))
;; NOTE: the shape field is mutated when normalized, as
;;       is the normalized? field.
(define (make-image shape bb normalized? [pinhole #f]) (new image% [shape shape] [bb bb] [normalized? normalized?] [pinhole pinhole]))
(define (image-shape p) (send p get-shape))
(define (image-bb p) (send p get-bb))
(define (image-normalized? p) (send p get-normalized?))
(define (set-image-shape! p s) (send p set-shape s))
(define (set-image-normalized?! p n?) (send p set-normalized? n?))
(define (image? p) 
  (or (is-a? p image<%>)
      (is-a? p image-snip%)
      (is-a? p bitmap%)))


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
;;  - bitmap
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
;;  - flip

;; a bitmap is:
;;  - (make-bitmap (is-a?/c bitmap%) angle positive-real 
;;                 hash[(list boolean[flip] number[x-scale] number[y-scale] number[angle]) -o> (cons (is-a?/c bitmap%) (is-a?/c bitmap%)])
;;    NOTE: bitmap copying needs to happen in 'write' and 'read' methods
(define-struct/reg-mk bitmap (raw-bitmap raw-mask angle x-scale y-scale cache)
  #:omit-define-syntaxes #:transparent
  #:property prop:custom-write (λ (x y z) (bitmap-write x y z)))

;; a flip is:
;;   - (make-flip boolean bitmap)
;; * the boolean is #t if the bitmap should be flipped vertically (after applying whatever rotation is in there)
;; * this struct is here to avoid adding a field to bitmaps, so that old save files
;;   from when the library did not support flipping still load
;;   (since normalization will add a flip structure if necessary)
(define-struct/reg-mk flip (flipped? shape) #:transparent)

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
;;  - (make-translate dx dy np-atomic-shape)
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

(define skip-image-equality-fast-path (make-parameter #f))

(define image%
  (class* snip% (equal<%> image<%>)
    (init-field shape bb normalized? pinhole)
    (define/public (equal-to? that eq-recur)
      (or (eq? this that)
          (let ([that 
                 (cond
                   [(is-a? that image-snip%) (image-snip->image that)]
                   [(is-a? that bitmap%) (bitmap->image that)]
                   [else that])])
            (and (is-a? that image%)
                 (same-bb? bb (send that get-bb))
                 (equal? pinhole (send that get-pinhole))
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
                                  (check-same? bm1 bm2 bytes1 bytes2 bdc "green" that))))))))))
    
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
    (define/public (get-pinhole) pinhole)
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

    (define/override (copy) (make-image shape bb normalized? pinhole))
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
      (let ([bytes (string->bytes/utf-8 (format "~s" (list shape bb pinhole)))])
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
      (let ([lst (parse (fetch (send f get-unterminated-bytes)))])
        (cond
          [(not lst)
           (make-image (make-ellipse 100 100 0 'solid "black")
                       (make-bb 100 100 100)
                       #f
                       #f)]
          [else
           (make-image (list-ref lst 0)
                       (list-ref lst 1)
                       #f
                       (list-ref lst 2))])))
    (super-new)))

(provide snip-class) 
(define snip-class (new image-snipclass%))
(send snip-class set-classname (format "~s" (list '(lib "image-core.ss" "mrlib")
                                                  '(lib "image-core-wxme.rkt" "mrlib"))))
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
             (cond
               [(bytes? (vector-ref sexp 0))
                ;; bitmaps are vectors with a bytes in the first field
                (apply bytes->bitmap (vector->list sexp))]
               [else
                (let ([constructor (id->constructor (vector-ref sexp 0))]
                      [args (cdr (vector->list sexp))])
                  (if (and constructor
                           (procedure-arity-includes? constructor (length args)))
                      (apply constructor (map loop args))
                      (k #f)))]))]
        [else sexp]))))

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
      (bitmap? shape)
      (np-atomic-shape? shape)))

(define (np-atomic-shape? shape)
  (or (ellipse? shape)
      (text? shape)
      (and (flip? shape)
           (boolean? (flip-flipped? shape))
           (bitmap? (flip-shape shape)))))

;; normalize-shape : shape (atomic-shape -> atomic-shape) -> normalized-shape
;; normalizes 'shape', calling 'f' on each atomic shape in the normalized shape.
(define/contract (normalize-shape shape [f values])
  (->* (any/c) ;; should be shape?
       ((-> any/c any/c))
       normalized-shape?)
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
      [(or (bitmap? shape) (np-atomic-shape? shape))
       (let ([shape (if (bitmap? shape) 
                        (make-flip #f shape)
                        shape)])
         (let ([this-one 
                (make-translate dx dy (scale-np-atomic x-scale y-scale shape))])
           (if bottom
               (make-overlay bottom (f this-one))
               (f this-one))))]
      [else 
       (error 'normalize-shape "unknown shape ~s\n" shape)])))

(define/contract (scale-np-atomic x-scale y-scale shape)
  (-> number? number? np-atomic-shape? np-atomic-shape?)
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
    [(flip? shape)
     (cond
       [(and (= 1 x-scale) (= 1 y-scale))
        shape]
       [else
        (let ([bitmap (flip-shape shape)])
          (make-flip (flip-flipped? shape)
                     (make-bitmap (bitmap-raw-bitmap bitmap)
                                  (bitmap-raw-mask bitmap)
                                  (bitmap-angle bitmap)
                                  (* x-scale (bitmap-x-scale bitmap))
                                  (* y-scale (bitmap-y-scale bitmap))
                                  (bitmap-cache bitmap))))])]))

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
        [smoothing (send dc get-smoothing)]
        [alpha (send dc get-alpha)])
    (cond
      [(is-a? image bitmap%)
       (send dc draw-bitmap image dx dy)]
      [(is-a? image image-snip%)
       (send dc draw-bitmap (send image get-bitmap) dx dy)]
      [else
       (render-normalized-shape (send image get-normalized-shape) dc dx dy)
       (let ([ph (send image get-pinhole)])
         (when ph
           (let* ([px (point-x ph)]
                  [py (point-y ph)]
                  [bb (image-bb image)]
                  [w (bb-right bb)]
                  [h (bb-bottom bb)])
             (send dc set-alpha (* alpha .5))
             (send dc set-smoothing 'smoothed)
             
             (send dc set-pen "white" 1 'solid)
             (send dc draw-line (+ dx px .5) (+ dy .5) (+ dx px .5) (+ dy h -.5))
             (send dc draw-line (+ dx .5) (+ dy py .5) (+ dx w -.5) (+ dy py .5))
             
             (send dc set-pen "black" 1 'solid)
             (send dc draw-line (+ dx px -.5) (+ dy .5) (+ dx px -.5) (+ dy h -.5))
             (send dc draw-line (+ dx .5) (+ dy py -.5) (+ dx w -.5) (+ dy py -.5)))))])
    (send dc set-pen pen)
    (send dc set-brush brush)
    (send dc set-font font)
    (send dc set-text-foreground fg)
    (send dc set-smoothing smoothing)
    (send dc set-alpha alpha)))

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
           [np-atomic-shape (translate-shape simple-shape)])
       (cond
         [(ellipse? np-atomic-shape)
          (let* ([path (new dc-path%)]
                 [ew (ellipse-width np-atomic-shape)]
                 [eh (ellipse-height np-atomic-shape)]
                 [θ (degrees->radians (ellipse-angle np-atomic-shape))]
                 [color (ellipse-color np-atomic-shape)]
                 [mode (ellipse-mode np-atomic-shape)])
            (let-values ([(rotated-width rotated-height) (ellipse-rotated-size ew eh θ)])
              (send path ellipse 0 0 ew eh)
              (send path translate (- (/ ew 2)) (- (/ eh 2)))
              (send path rotate θ)
              (send dc set-pen (mode-color->pen mode color))
              (send dc set-brush (mode-color->brush mode color))
              (send dc set-smoothing (mode-color->smoothing mode color))
              (send dc draw-path path dx dy)))]
         [(flip? np-atomic-shape) 
          (let ([bm (get-rendered-bitmap np-atomic-shape)]) 
            (send dc draw-bitmap 
                  bm
                  (- dx (/ (send bm get-width) 2))
                  (- dy (/ (send bm get-height) 2))
                  'solid
                  (send the-color-database find-color "black")
                  (get-rendered-mask np-atomic-shape)))]
         [(text? np-atomic-shape)
          (let ([θ (degrees->radians (text-angle np-atomic-shape))]
                [font (send dc get-font)])
            (send dc set-font (text->font np-atomic-shape))
            (let ([color (get-color-arg (text-color np-atomic-shape))])
              (send dc set-text-foreground 
                    (cond
                      [(string? color)
                       (or (send the-color-database find-color color)
                           (send the-color-database find-color "black"))]
                      [else color])))
            (let-values ([(w h _1 _2) (send dc get-text-extent (text-string np-atomic-shape))])
              (let ([p (- (make-rectangular dx dy)
                          (* (make-polar 1 (- θ)) (make-rectangular (/ w 2) (/ h 2))))])
                (send dc draw-text (text-string np-atomic-shape) 
                      (real-part p)
                      (imag-part p) 
                      #f 0 θ))))]))]))

(define (polygon-points->path points)
  (let ([path (new dc-path%)])
    (send path move-to (point-x (car points)) (point-y (car points)))
    (let loop ([points (cdr points)])
      (unless (null? points)
        (send path line-to 
              (point-x (car points))
              (point-y (car points)))
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


(define (get-rendered-bitmap flip-bitmap)
  (let ([key (get-bitmap-cache-key flip-bitmap)])
    (calc-rendered-bitmap flip-bitmap key)
    (car (hash-ref (bitmap-cache (flip-shape flip-bitmap))
                   key))))

(define (get-rendered-mask flip-bitmap)
  (let ([key (get-bitmap-cache-key flip-bitmap)])
    (calc-rendered-bitmap flip-bitmap key)
    (cdr (hash-ref (bitmap-cache (flip-shape flip-bitmap))
                   key))))

(define (get-bitmap-cache-key flip-bitmap)
  (let ([bm (flip-shape flip-bitmap)])
    (list (flip-flipped? flip-bitmap)
          (bitmap-x-scale bm)
          (bitmap-y-scale bm)
          (bitmap-angle bm))))

(define (calc-rendered-bitmap flip-bitmap key)
  (let ([bitmap (flip-shape flip-bitmap)])
    (cond
      [(hash-ref (bitmap-cache bitmap) key #f) => (λ (x) x)]
      [else
       (let ([flipped? (flip-flipped? flip-bitmap)])
         (define-values (orig-bitmap-obj orig-mask-obj) (values (bitmap-raw-bitmap bitmap)
                                                                (bitmap-raw-mask bitmap)))
         (define-values (bitmap-obj mask-obj)
           (cond
             [(<= (* (bitmap-x-scale bitmap) 
                     (bitmap-y-scale bitmap))
                  1)
              ;; since we prefer to rotate big things, we rotate first
              (let-values ([(bitmap-obj mask-obj) (do-rotate bitmap orig-bitmap-obj orig-mask-obj flipped?)])
                (do-scale bitmap bitmap-obj mask-obj))]
             [else
              ;; since we prefer to rotate big things, we scale first
              (let-values ([(bitmap-obj mask-obj) (do-scale bitmap orig-bitmap-obj orig-mask-obj)])
                (do-rotate bitmap bitmap-obj mask-obj flipped?))]))
         (define pair (cons bitmap-obj mask-obj))
         (hash-set! (bitmap-cache bitmap) key pair)
         pair)])))

(define (do-rotate bitmap bitmap-obj mask-obj flip?)
  (cond
    [(and (not flip?) (zero? (bitmap-angle bitmap)))
     ;; don't rotate anything in this case.
     (values bitmap-obj mask-obj)]
    [else
     (let ([θ (degrees->radians (bitmap-angle bitmap))])
       (let-values ([(bytes w h) (bitmap->bytes bitmap-obj mask-obj)])
         (let-values ([(rotated-bytes rotated-w rotated-h)
                       (rotate-bytes bytes w h θ)])
           (let* ([flipped-bytes (if flip?
                                     (flip-bytes rotated-bytes rotated-w rotated-h)
                                     rotated-bytes)]
                  [bm (bytes->bitmap flipped-bytes rotated-w rotated-h)]
                  [mask (send bm get-loaded-mask)])
             (values bm mask)))))]))

(define (do-scale bitmap orig-bm orig-mask)
  (let ([x-scale (bitmap-x-scale bitmap)]
        [y-scale (bitmap-y-scale bitmap)])
    (cond
      [(and (= 1 x-scale) (= 1 y-scale))
       ;; no need to scale in this case
       (values orig-bm orig-mask)]
      [else
       (let* ([bdc (make-object bitmap-dc%)]
              [orig-w (send orig-bm get-width)]
              [orig-h (send orig-bm get-height)]
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
         
         (values new-bm new-mask))])))

(define (text->font text)
  (define adjusted-size (min (max (inexact->exact (round (text-size text))) 1) 255))
  (cond
    [(text-face text)
     (send the-font-list find-or-create-font
           adjusted-size
           (text-face text)
           (text-family text)
           (text-style text) 
           (text-weight text)
           (text-underline text))]
    [else
     (send the-font-list find-or-create-font
           adjusted-size
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


(define (pen->pen-obj/cache pen)
  (send the-pen-list find-or-create-pen 
        (pen-color pen)
        (pen-width pen)
        (pen-style pen)
        (pen-cap pen)
        (pen-join pen)))
       
(define (to-img arg)
  (cond
    [(is-a? arg image-snip%) (image-snip->image arg)]
    [(is-a? arg bitmap%) (bitmap->image arg)]
    [else arg]))

(define (image-snip->image is)
  (let ([bm (send is get-bitmap)])
    (cond
      [(not bm)
       ;; this might mean we have a cache-image-snip% 
       ;; or it might mean we have a useless snip.
       (let-values ([(w h) (if (is-a? is cis:cache-image-snip%)
                               (send is get-size)
                               (values 0 0))])
         (make-image (make-polygon
                      (list (make-point 0 0)
                            (make-point w 0)
                            (make-point w h)
                            (make-point 0 h))
                      'solid "black")
                     (make-bb w h h)
                     #f))]
      [else
       (bitmap->image bm
                      (or (send is get-bitmap-mask)
                          (send bm get-loaded-mask)))])))

(define (bitmap->image bm [mask-bm (send bm get-loaded-mask)])
  (let ([w (send bm get-width)]
        [h (send bm get-height)])
    (make-image (make-translate (/ w 2)
                                (/ h 2)
                                (make-bitmap bm mask-bm 0 1 1 (make-hash)))
                (make-bb w h h)
                #f)))

(define (bitmap-write bitmap port mode)
  (let* ([v (struct->vector bitmap)]
         [recur (case mode
                  [(#t) write]
                  [(#f) display]
                  [else (lambda (p port) (print p port mode))])]
         [update 
          (λ (i)
            (let ([o (vector-ref v i)])
              (let ([nv (and o 
                             (call-with-values (λ () (bitmap->bytes o)) vector))])
                (vector-set! v i nv))))])
    (update 1)
    (update 2)
    ;; don't save the cache
    (vector-set! v 6 #f)
    (recur v port)))

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
         bitmap-cache

         make-flip flip? flip-flipped? flip-shape
         
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
         
         scale-np-atomic
         
         to-img
         bitmap->image
         image-snip->image)

;; method names
(provide get-shape get-bb get-pinhole get-normalized? get-normalized-shape)

(provide np-atomic-shape? atomic-shape? simple-shape? cn-or-simple-shape? normalized-shape?)
