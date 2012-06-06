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
         (except-in racket/draw 
                    make-pen make-color)
         (for-syntax racket/base)
         file/convertible
         racket/math
         racket/contract
         "private/image-core-bitmap.rkt"
         "image-core-wxme.rkt"
         "private/image-core-snipclass.rkt"
         "private/regmk.rkt"
         racket/snip
         (prefix-in cis: "cache-image-snip.rkt"))




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
(define (make-image shape bb normalized? [pinhole #f])
  (new image% [shape shape] [bb bb] [normalized? normalized?] [pinhole pinhole]))
(define (image-shape p) (send p get-shape))
(define (image-bb p) (send p get-bb))
(define (image-normalized? p) (send p get-normalized?))
(define (set-image-shape! p s) (send p set-shape s))
(define (set-image-normalized?! p n?) (send p set-normalized? n?))
(define (image? p) 
  (or (is-a? p image<%>)
      (is-a? p image-snip%)
      (is-a? p bitmap%)))

(define (un/cache-image img bitmap-cache?)
  (unless (image? img)
    (error 'un/cache-image "expected an image as the first argument, got ~e" img))
  (define res (send img copy))
  (send res set-use-bitmap-cache?! (and bitmap-cache? #t))
  res)

(define (compute-image-cache img)
  (unless (image? img)
    (error 'compute-cached-bitmap "expected an image as the first argument, got ~e" img))
  (when (is-a? img image<%>)
    (send img compute-cached-bitmap))
  (void))
  
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
;;  - (make-ibitmap (and/c (is-a?/c bitmap%) (lambda (x) (send x has-alpha-channel?)))
;;                  angle positive-real 
;;                  hash[(list boolean[flip] number[x-scale] number[y-scale] number[angle]) -o> (is-a?/c bitmap%)])
;;    NOTE: bitmap copying needs to happen in 'write' and 'read' methods
(define-struct/reg-mk ibitmap #:reflect-id bitmap (raw-bitmap angle x-scale y-scale cache)
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
(define-struct/reg-mk color (red green blue alpha) #:transparent)
(define -make-color
  ;; this let is here just for the name
  (let ([make-color
         (λ (r g b [a 255])
           (make-color r g b a))])
    make-color))
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
(define render-normalized (make-parameter #f))

(define png-convertible<%>
  (interface* ()
              ([prop:convertible
                (lambda (img format default)
                  (case format
                    [(png-bytes)
                     (let ([s (open-output-bytes)])
                       (send (to-bitmap (to-img img)) save-file s 'png)
                       (get-output-bytes s))]
                    [else default]))])))

;; these are used when building a bitmap to render the final image
;; they are probably smaller than the allowed maximum, but they are
;; still huge
(define maximum-width 5000)
(define maximum-height 5000)

(define (to-bitmap img)
  (define bb (send img get-bb))
  (define w (min (inexact->exact (ceiling (bb-right bb))) maximum-width))
  (define h (min (inexact->exact (ceiling (bb-bottom bb))) maximum-height)) 
  (define bm (make-bitmap w h))
  (define bdc (new bitmap-dc% [bitmap bm]))
  (render-image img bdc 0 0)
  (send bdc set-bitmap #f)
  bm)

(define-local-member-name 
  set-use-bitmap-cache?!
  set-cached-bitmap
  compute-cached-bitmap)

(define image%
  (class* snip% (png-convertible<%> image<%>)
    (init-field shape bb normalized? pinhole)
    
    (define/override (equal-to? that eq-recur) (compare-em that eq-recur))
    (define/override (other-equal-to? that eq-recur) (compare-em that eq-recur))
    
    (define/private (compare-em that eq-recur)
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
                     
                     ;; some shapes (ie, rectangles) draw 1 outside the bounding box
                     ;; so we make the bitmap slightly bigger to accommodate that.
                     (let ([w (+ 1 (round (inexact->exact (bb-right bb))))]
                           [h (+ 1 (round (inexact->exact (bb-bottom bb))))])
                       (or ;(zero? w)
                           ;(zero? h)
                           (let ([bm1 (make-bitmap w h #t)]
                                 [bm2 (make-bitmap w h #t)]
                                 [bytes1 (make-bytes (* w h 4) 0)]
                                 [bytes2 (make-bytes (* w h 4) 0)]
                                 [bdc (make-object bitmap-dc%)])
                             (draw-into bm1 bdc bytes1 this)
                             (draw-into bm2 bdc bytes2 that)
                             (equal? bytes1 bytes2)))))))))
    
    (define/private (draw-into bm bdc bytes obj)
      (send bdc set-bitmap bm)
      (send bdc erase)
      (render-image obj bdc 0 0)
      (send bdc get-argb-pixels 0 0 (send bm get-width) (send bm get-height) bytes #f #t))
    
    ;; this could render the image into a bitmap and then get the hash code of the bytes
    ;; cannot render the tree into a string and then get the hash code of that string
    ;;   b/c that might make equal things have the same code.
    (define/override (equal-hash-code-of y) 42)
    (define/override (equal-secondary-hash-code-of y) 3)

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
                    (set! scroll-step (+ h (send admin get-line-spacing)))))))))
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

    (define/override (copy) 
      (define res (make-image shape bb normalized? pinhole))
      (when cached-bitmap
        (send res set-cached-bitmap cached-bitmap))
      res)
    
    (define cached-bitmap #f)
    (define use-cached-bitmap? #t)
    
    ;; this method is only used by the 'copy' method
    (define/public (set-cached-bitmap bm) (set! cached-bitmap bm))
    
    (define/public (compute-cached-bitmap)
      (when use-cached-bitmap?
        (unless cached-bitmap
          (set! cached-bitmap (make-bitmap (min (+ (inexact->exact (round (bb-right bb))) 1) maximum-width) 
                                           (min (+ (inexact->exact (round (bb-bottom bb))) 1) maximum-height)))
          (define bdc (make-object bitmap-dc% cached-bitmap))
          (send bdc erase)
          (render-image this bdc 0 0)
          (send bdc set-bitmap #f))))
    
    (define/public (set-use-bitmap-cache?! u-b-c?) 
      (set! use-cached-bitmap? u-b-c?)
      (unless use-cached-bitmap?
        (set! cached-bitmap #f)))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (compute-cached-bitmap)
      
      (let ([alpha (send dc get-alpha)])
        (when (pair? draw-caret)
          (send dc set-alpha (* alpha .5)))
        (if use-cached-bitmap?
            (send dc draw-bitmap cached-bitmap x y)
            (render-image this dc x y))
        (send dc set-alpha alpha)))
    
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
           (make-image (make-translate 50 50 (make-ellipse 100 100 0 'solid "black"))
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
        [(and (immutable? sexp) (hash? sexp))
         (hash-copy sexp)]
        [(vector? sexp)
         (if (= (vector-length sexp) 0)
             (k #f)
             (cond
               [(bytes? (vector-ref sexp 0))
                ;; bitmaps are vectors with a bytes in the first field
                (apply bytes->bitmap (vector->list sexp))]
               [else
                (let* ([tag (vector-ref sexp 0)]
                       [args (cdr (vector->list sexp))]
                       [constructor (id->constructor tag)]
                       [arg-count (length args)]
                       [parsed-args (map loop args)])
                  (cond
                    [(and constructor (procedure-arity-includes? constructor arg-count))
                     (apply constructor parsed-args)]
                    [(and (eq? tag 'struct:bitmap)
                          (= arg-count 7))
                     ;; we changed the arity of the bitmap constructor from old versions,
                     ;; so fix it up here.
                     (make-ibitmap (list-ref parsed-args 0)
                                   (list-ref parsed-args 2)
                                   (list-ref parsed-args 3)
                                   (list-ref parsed-args 4)
                                   (make-hash))]
                    [(and (eq? tag 'struct:bitmap)
                          (= arg-count 6))
                     ;; we changed the arity of the bitmap constructor from old versions,
                     ;; so fix it up here.
                     ;; it used to have these fields: (raw-bitmap raw-mask angle x-scale y-scale cache)
                     ;; and the mask field was dropped in favor of always having an alpha bitmap in the 
                     ;; raw-bitmap field. The bytes that were written out always had the mask
                     ;; factored in, tho (which led to a bug) so we can just ignore the mask here
                     (make-ibitmap (list-ref parsed-args 0)
                                   (list-ref parsed-args 2)
                                   (list-ref parsed-args 3)
                                   (list-ref parsed-args 4)
                                   (make-hash))]
                    [(and (eq? tag 'struct:color)
                          (= arg-count 3))
                     ;; we changed the arity of the color constructor from old versions,
                     ;; so fix it up here.
                     (make-color (list-ref parsed-args 0)
                                 (list-ref parsed-args 1)
                                 (list-ref parsed-args 2)
                                 255)]
                    [else
                     (k #f)]))]))]
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
      (ibitmap? shape)
      (np-atomic-shape? shape)))

(define (np-atomic-shape? shape)
  (or (ellipse? shape)
      (text? shape)
      (and (flip? shape)
           (boolean? (flip-flipped? shape))
           (ibitmap? (flip-shape shape)))))

;; normalize-shape : shape -> normalized-shape
;; normalizes 'shape', calling 'f' on each atomic shape in the normalized shape.
(define/contract (normalize-shape shape)
  (-> any/c ;; should be shape?
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
             (make-overlay bottom this-one)
             this-one))]
      [(line-segment? shape)
       (let ([this-one 
              (make-line-segment (scale-point (line-segment-start shape))
                                 (scale-point (line-segment-end shape))
                                 (scale-color (line-segment-color shape) x-scale y-scale))])
         (if bottom
             (make-overlay bottom this-one)
             this-one))]
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
             (make-overlay bottom this-one)
             this-one))]
      [(or (ibitmap? shape) (np-atomic-shape? shape))
       (let ([shape (if (ibitmap? shape) 
                        (make-flip #f shape)
                        shape)])
         (let ([this-one 
                (make-translate dx dy (scale-np-atomic x-scale y-scale shape))])
           (if bottom
               (make-overlay bottom this-one)
               this-one)))]
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
                     (make-ibitmap (ibitmap-raw-bitmap bitmap)
                                   (ibitmap-angle bitmap)
                                   (* x-scale (ibitmap-x-scale bitmap))
                                   (* y-scale (ibitmap-y-scale bitmap))
                                   (ibitmap-cache bitmap))))])]))

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
       (if (render-normalized)
           (render-normalized-shape (send image get-normalized-shape) dc dx dy)
           (render-arbitrary-shape (send image get-shape) dc dx dy))
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
    (send bdc erase)
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
     (render-cropped-shape (crop-points shape)
                           (crop-shape shape)
                           (λ (s) (render-normalized-shape s dc dx dy))
                           dc dx dy)]
    [else
     (render-simple-shape shape dc dx dy)]))

(define (render-cropped-shape points inner-shape continue dc dx dy)
  (cond
    [(equal? points (last-cropped-points))
     (continue inner-shape)]
    [else
     (let ([old-region (send dc get-clipping-region)]
           [new-region (new region% [dc dc])]
           [path (polygon-points->path points)])
       (send new-region set-path path dx dy)
       (when old-region (send new-region intersect old-region))
       (send dc set-clipping-region new-region)
       (parameterize ([last-cropped-points points])
         (continue inner-shape))
       (send dc set-clipping-region old-region))]))

(define (render-simple-shape simple-shape dc dx dy)
  (cond
    [(translate? simple-shape)
     (let ([dx (+ dx (translate-dx simple-shape))]
           [dy (+ dy (translate-dy simple-shape))]
           [np-atomic-shape (translate-shape simple-shape)])
       (render-np-atomic-shape np-atomic-shape
                               dc
                               dx dy))]
    [else
     (render-poly/line-segment/curve-segment simple-shape dc dx dy)]))

(define (render-arbitrary-shape shape dc dx dy)
  (let loop ([shape shape]
             [dx dx]
             [dy dy]
             [x-scale 1]
             [y-scale 1])
    (define (scale-point p)
      (make-point (* x-scale (point-x p))
                  (* y-scale (point-y p))))
    (cond
      [(translate? shape)
       (loop (translate-shape shape)
             (+ dx (* x-scale (translate-dx shape)))
             (+ dy (* y-scale (translate-dy shape)))
             x-scale
             y-scale)]
      [(scale? shape)
       (loop (scale-shape shape)
             dx
             dy
             (* x-scale (scale-x shape))
             (* y-scale (scale-y shape)))]
      [(overlay? shape)
       (loop (overlay-bottom shape) dx dy x-scale y-scale)
       (loop (overlay-top shape) dx dy x-scale y-scale)]
      [(crop? shape)
       (render-cropped-shape
        (map scale-point (crop-points shape))
        (crop-shape shape)
        (λ (s) (loop s dx dy x-scale y-scale)) dc dx dy)]
      [(polygon? shape)
       (let* ([this-one 
               (make-polygon (map scale-point (polygon-points shape))
                             (polygon-mode shape)
                             (scale-color (polygon-color shape) x-scale y-scale))])
         (render-poly/line-segment/curve-segment this-one dc dx dy))]
      [(line-segment? shape)
       (let ([this-one 
              (make-line-segment (scale-point (line-segment-start shape))
                                 (scale-point (line-segment-end shape))
                                 (scale-color (line-segment-color shape) x-scale y-scale))])
         (render-poly/line-segment/curve-segment this-one dc dx dy))]
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
         (render-poly/line-segment/curve-segment this-one dc dx dy))]
      [(or (ibitmap? shape) (np-atomic-shape? shape))
       (let* ([shape (if (ibitmap? shape) 
                         (make-flip #f shape)
                         shape)]
              [this-one (scale-np-atomic x-scale y-scale shape)])
         (render-np-atomic-shape this-one dc dx dy))]
      [else 
       (error 'normalize-shape "unknown shape ~s\n" shape)])))

(define/contract (render-poly/line-segment/curve-segment simple-shape dc dx dy)
  (-> (or/c polygon? line-segment? curve-segment?) any/c any/c any/c void?)
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
       (send dc draw-path path dx dy))]))

(define (render-np-atomic-shape np-atomic-shape dc dx dy)
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
       (send dc set-smoothing 'smoothed)
       (send dc draw-bitmap 
             bm
             (- dx (/ (send bm get-width) 2))
             (- dy (/ (send bm get-height) 2))))]
    [(text? np-atomic-shape)
     (let ([θ (degrees->radians (text-angle np-atomic-shape))]
           [font (send dc get-font)])
       (send dc set-font (text->font np-atomic-shape))
       (send dc set-smoothing 'aligned) ;; should this be smoothed?
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
                 #f 0 θ))))]))

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
    (lookup/calc-rendered-bitmap flip-bitmap key)))

(define (get-bitmap-cache-key flip-bitmap)
  (let ([bm (flip-shape flip-bitmap)])
    (list (flip-flipped? flip-bitmap)
          (ibitmap-x-scale bm)
          (ibitmap-y-scale bm)
          (ibitmap-angle bm))))

(define (lookup/calc-rendered-bitmap flip-bitmap key)
  (let ([bitmap (flip-shape flip-bitmap)])
    (cond
      [(hash-ref (ibitmap-cache bitmap) key #f) => values]
      [else
       (let ([flipped? (flip-flipped? flip-bitmap)])
         (define orig-bitmap-obj (ibitmap-raw-bitmap bitmap))
         (define bitmap-obj
           (cond
             [(<= (* (ibitmap-x-scale bitmap) 
                     (ibitmap-y-scale bitmap))
                  1)
              ;; since we prefer to rotate big things, we rotate first
              (do-scale bitmap (do-rotate bitmap orig-bitmap-obj flipped?))]
             [else
              ;; since we prefer to rotate big things, we scale first
              (do-rotate bitmap (do-scale bitmap orig-bitmap-obj) flipped?)]))
         (hash-set! (ibitmap-cache bitmap) key bitmap-obj)
         bitmap-obj)])))

(define (do-rotate bitmap bitmap-obj flip?)
  (cond
    [(and (not flip?) (zero? (ibitmap-angle bitmap)))
     ;; don't rotate anything in this case.
     bitmap-obj]
    ;; speed up rotated (but not flipped) bitmaps
    [(not flip?)
     (define θ (degrees->radians (ibitmap-angle bitmap)))
     (define ow (send bitmap-obj get-width))
     (define oh (send bitmap-obj get-height))
     (define unrotated-pts
       (list (make-rectangular 0 0)
             (make-rectangular ow 0)
             (make-rectangular ow oh)
             (make-rectangular 0 oh)))
     (define pts (map (λ (p) (* p (make-polar 1 θ))) unrotated-pts))
     (define longitudes (map real-part pts))
     (define latitudes (map imag-part pts))
     (define east (apply max longitudes))
     (define west (apply min longitudes))
     (define nrth (apply min latitudes))
     (define sth  (apply max latitudes))
     (define new-w (ceiling (inexact->exact (- east west))))
     (define new-h (ceiling (inexact->exact (- sth nrth))))

     (define new-bm (make-bitmap new-w new-h))
     (define bdc (make-object bitmap-dc% new-bm))
     (send bdc set-smoothing 'smoothed)
     (send bdc rotate (- θ))
     
     ;; would like to just translate by 'tp', but
     ;; the dc applies the translations before applying
     ;; the rotation, so we have to unrotate the translation
     ;; before telling the dc about it
     (define tp (make-rectangular (- west) (- nrth)))
     (define tp-translated (* tp (make-polar 1 (- θ))))
     
     (send bdc translate (real-part tp-translated) (imag-part tp-translated))
     
     (send bdc draw-bitmap bitmap-obj 0 0)
     (send bdc set-bitmap #f)
     new-bm]
    
    [else
     (define θ (degrees->radians (ibitmap-angle bitmap)))
     (define-values (bytes w h) (bitmap->bytes bitmap-obj #f))
     (define-values (rotated-bytes rotated-w rotated-h) (rotate-bytes bytes w h θ))
     (define flipped-bytes (if flip?
                               (flip-bytes rotated-bytes rotated-w rotated-h)
                               rotated-bytes))
     (define bm (bytes->bitmap flipped-bytes rotated-w rotated-h))
     bm]))

(define (do-scale bitmap orig-bm)
  (define x-scale (ibitmap-x-scale bitmap))
  (define y-scale (ibitmap-y-scale bitmap))
  (cond
    [(and (= 1 x-scale) (= 1 y-scale))
     ;; no need to scale in this case
     orig-bm]
    [else
     (define bdc (make-object bitmap-dc%))
     (define orig-w (send orig-bm get-width))
     (define orig-h (send orig-bm get-height))
     (define scale-w (ceiling (inexact->exact (* x-scale (send orig-bm get-width)))))
     (define scale-h (ceiling (inexact->exact (* y-scale (send orig-bm get-height)))))
     (define new-bm (make-bitmap scale-w scale-h))
     
     (send bdc set-bitmap new-bm)
     (send bdc set-scale x-scale y-scale)
     (send bdc erase)
     (send bdc draw-bitmap orig-bm 0 0)
     
     (send bdc set-bitmap #f)
     
     new-bm]))

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
           (text-underline text)
           'default
           #t)]
    [else
     (send the-font-list find-or-create-font
           adjusted-size
           (text-family text)
           (text-style text) 
           (text-weight text)
           (text-underline text)
           'default
           #t)]))

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

(define (mode-color->smoothing mode color)
  (cond
    [(and (eq? mode 'outline)
          (not (pen? color)))
     'aligned]
    [else 'smoothed]))

(define (mode-color->pen mode color)
  (cond
    [(eq? mode 'outline)
     (cond
       [(pen? color)
        (pen->pen-obj/cache color)]
       [else
        (send the-pen-list find-or-create-pen (get-color-arg color) 0 'solid 'round 'miter)])]
    [else
     (send the-pen-list find-or-create-pen "black" 1 'transparent)]))

(define (mode-color->brush mode color)
  (cond
    [(eq? mode 'outline)
     (send the-brush-list find-or-create-brush "black" 'transparent)]
    [else
     ;; this should only be 'solid if we have an old image from a save file somewhere
     (define extra-alpha (if (eq? mode 'solid)
                             255
                             mode))
     (send the-brush-list find-or-create-brush (get-color-arg color extra-alpha) 'solid)]))

(define (get-color-arg color [extra-alpha 255])
  (cond
    [(string? color) 
     (define color-obj (or (send the-color-database find-color color)
                           (send the-color-database find-color "black")))
     (make-object color%
       (send color-obj red)
       (send color-obj green)
       (send color-obj blue)
       (/ extra-alpha 255))]
    [else
     (make-object color% 
       (color-red color)
       (color-green color)
       (color-blue color)
       (* (/ (color-alpha color) 255)
          (/ extra-alpha 255)))]))


(define (pen->pen-obj/cache pen)
  (send the-pen-list find-or-create-pen 
        (get-color-arg (pen-color pen))
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
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define alpha-bm
    (cond
      [(send bm has-alpha-channel?)
       bm]
      [else
       (define new-bm (make-bitmap w h))
       (define bdc (make-object bitmap-dc% new-bm))
       (send bdc draw-bitmap bm 0 0 'solid
             (send the-color-database find-color "black")
             mask-bm)
       (send bdc set-bitmap #f)
       new-bm]))
  (make-image (make-translate (/ w 2)
                              (/ h 2)
                              (make-ibitmap alpha-bm 0 1 1 (make-hash)))
              (make-bb w h h)
              #f))

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
                             (call-with-values (λ () (bitmap->bytes o #f)) vector))])
                (vector-set! v i nv))))])
    (update 1)
    ;; don't save the cache
    (vector-set! v 5 (make-hash))
    (recur v port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide make-image image-shape image-bb image-normalized? image%
        
         un/cache-image compute-image-cache
         
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
         
         make-ibitmap ibitmap? ibitmap-raw-bitmap ibitmap-angle ibitmap-x-scale ibitmap-y-scale 
         ibitmap-cache

         make-flip flip? flip-flipped? flip-shape
         
         (except-out (struct-out color) make-color)
         (rename-out [-make-color make-color]) 
         
         degrees->radians
         normalize-shape
         ellipse-rotated-size
         points->ltrb-values

         image?
         
         text->font
         render-image
         save-image-as-bitmap
         
         skip-image-equality-fast-path
         render-normalized
         
         scale-np-atomic
         
         to-img
         bitmap->image
         image-snip->image
         image-snip%)

;; method names
(provide get-shape get-bb get-pinhole get-normalized? get-normalized-shape)

(provide np-atomic-shape? atomic-shape? simple-shape? cn-or-simple-shape? normalized-shape?)
