#lang scheme/base

#|

This library is the part of the 2htdp/image 
teachpack that has to be shared between drscheme
and the user's program to make copy and paste
work right.

Most of the exports are just for use in 2htdp/image
(technically, 2htdp/private/image-more). The main
use of this library is the snip class addition it
does (and any code that that does not depend on
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

(require scheme/class
         scheme/gui/base
         scheme/math
         "private/image-core-bitmap.ss"
         (for-syntax scheme/base))

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
(define (image-right image) (bb-right (image-bb image)))
(define (image-bottom image) (bb-bottom (image-bb image)))
(define (image-baseline image) (bb-baseline (image-bb image)))
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
;;  - atomic-shape

;; an atomic-shape is either:
;;  - polygon
;;  - line-segment
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
(define-struct/reg-mk polygon (points mode color) #:transparent #:omit-define-syntaxes
  #:property prop:equal+hash 
  (list (λ (a b rec) (polygon-equal? a b rec)) (λ (x y) 42) (λ (x y) 3)))

;; a line-segment is
;;
;;  - (make-line-segment point point color)
(define-struct/reg-mk line-segment (start end color) #:transparent #:omit-define-syntaxes
  #:property prop:equal+hash 
  (list (λ (a b rec) (and (or (and (rec (line-segment-start a) (line-segment-start b))
                                   (rec (line-segment-end a) (line-segment-end b)))
                              (and (rec (line-segment-start a) (line-segment-end b))
                                   (rec (line-segment-end a) (line-segment-start b))))
                          (rec (line-segment-color a) (line-segment-color b))))
        (λ (x y) 42) 
        (λ (x y) 3)))
;; a normalized-shape (subtype of shape) is either
;;  - (make-overlay normalized-shape simple-shape)
;;  - simple-shape

;; a simple-shape (subtype of shape) is
;;  - (make-translate dx dy np-atomic-shape)
;;  - polygon
;;  - line-segment

;; an angle is a number between 0 and 360 (degrees)

;; a mode is either 'solid or 'outline (indicating a pen width for outline mode)

(define (polygon-equal? p1 p2 eq-recur)
  (and (eq-recur (polygon-mode p1) (polygon-mode p2))
       (eq-recur (polygon-color p1) (polygon-color p2))
       (let ([p1-points (polygon-points p1)]
             [p2-points (polygon-points p2)])
         (or (and (null? p1-points)
                  (null? p2-points))
             (and (not (or (null? p1-points)
                           (null? p2-points)))
                  (or (compare-all-rotations p1-points p2-points eq-recur)
                      (compare-all-rotations p1-points (reverse p2-points) eq-recur)))))))


;; returns #t when there is some rotation of l1 that is equal to l2
(define (compare-all-rotations l1 l2 compare)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [else
     (let ([v1 (list->vector l1)]
           [v2 (list->vector l2)])
       (and (= (vector-length v1) 
               (vector-length v2))
            (let o-loop ([init 0])
              (cond
                [(= init (vector-length v1)) #f]
                [else
                 (or (let i-loop ([i 0])
                       (cond
                         [(= i (vector-length v2)) 
                          #t]
                         [else
                          (let ([j (modulo (+ init i) (vector-length v1))])
                            (and (compare (vector-ref v1 j)
                                          (vector-ref v2 i))
                                 (i-loop (+ i 1))))]))
                     (o-loop (+ init 1)))]))))]))
        

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

(define-local-member-name get-shape set-shape get-bb get-normalized? set-normalized get-normalized-shape)

(define image%
  (class* snip% (equal<%>)
    (init-field shape bb normalized?)
    (define/public (equal-to? that eq-recur) 
      (or (eq? this that)
          (and (eq-recur bb (send that get-bb))
               (let* ([w (ceiling (max (inexact->exact (bb-right bb))
                                       (inexact->exact (bb-right (send that get-bb)))))]
                      [h (ceiling (max (inexact->exact (bb-bottom bb))
                                       (inexact->exact (bb-bottom (send that get-bb)))))]
                      [bm1 (make-object bitmap% w h)]
                      [bm2 (make-object bitmap% w h)]
                      [bytes1 (make-bytes (* w h 4) 0)]
                      [bytes2 (make-bytes (* w h 4) 0)]
                      [bdc (make-object bitmap-dc%)])
                 (send bdc set-smoothing 'aligned)
                 (and (check-same? bm1 bm2 bytes1 bytes2 bdc "red" that)
                      (check-same? bm1 bm2 bytes1 bytes2 bdc "green" that)))))
      
      #;
      (eq-recur (get-normalized-shape)
                (send that get-normalized-shape)))
    
    (define/private (check-same? bm1 bm2 bytes1 bytes2 bdc color that)
      (clear-bitmap/draw/bytes bm1 bdc bytes1 this color)
      (clear-bitmap/draw/bytes bm2 bdc bytes2 that color)
      (equal? bytes1 bytes2))
    
    (define/private (clear-bitmap/draw/bytes bm bdc bytes obj color)
      (send bdc set-bitmap bm)
      (send bdc set-pen "black" 1 'transparent)
      (send bdc set-brush color 'solid)
      (send bdc draw-rectangle 0 0 (send bm get-width) (send bm get-height))
      (render-image this bdc 0 0)
      (send bm get-argb-pixels 0 0 (send bm get-width) (send bm get-height) bytes))
    
    (define/public (equal-hash-code-of y) 42)
    (define/public (equal-secondary-hash-code-of y) 3)

    (define/public (get-shape) shape)
    (define/public (set-shape s) (set! shape s))
    (define/public (get-bb) bb)
    (define/public (get-normalized?) normalized?)
    (define/public (set-normalized? n?) (set! normalized? n?))
    
    (define/public (get-normalized-shape)
      (unless normalized?
        (set! shape (normalize-shape shape values))
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
        (send dc set-smoothing 'aligned)
        (render-image this dc x y)
        (send dc set-smoothing smoothing)))
    (define/override (get-extent dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (send (get-the-snip-class-list) add snip-class)
      (let ([bottom (round (bb-bottom bb))])
        (set-box/f! w (round (bb-right bb)))
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

(define scheme/base:read read)

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
                      (scheme/base:read
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
                   dx dy x-scale y-scale bottom))]
      [(polygon? shape)
       (let* ([this-one 
               (make-polygon (map scale-point (polygon-points shape))
                             (polygon-mode shape)
                             (polygon-color shape))])
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))]
      [(line-segment? shape)
       (let ([this-one 
              (make-line-segment (scale-point (line-segment-start shape))
                                 (scale-point (line-segment-end shape))
                                 (line-segment-color shape))])
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))]
      [(np-atomic-shape? shape)
       (let ([this-one (make-translate dx dy (scale-np-atomic x-scale y-scale shape))])
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))]
      [else 
       (error 'normalize-shape "unknown shape ~s\n" shape)])))

(define (simple-shape? shape)
  (or (and (translate? shape)
           (np-atomic-shape? (translate-shape shape)))
      (polygon? shape)
      (line-segment? shape)))

(define (atomic-shape? shape)
  (or (polygon? shape)
      (line-segment? shape)
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
                   (ellipse-color shape))]
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

;; render-image : normalized-shape dc dx dy -> void
(define (render-image image dc dx dy)
  (let ([pen (send dc get-pen)]
        [brush (send dc get-brush)]
        [font (send dc get-font)]
        [fg (send dc get-text-foreground)])
    (let loop ([shape (send image get-normalized-shape)])
      (cond
        [(overlay? shape)
         (render-simple-shape (overlay-bottom shape) dc dx dy)
         (loop (overlay-top shape))]
        [else
         (render-simple-shape shape dc dx dy)]))
    (send dc set-pen pen)
    (send dc set-brush brush)
    (send dc set-font font)
    (send dc set-text-foreground fg)))

(define (render-simple-shape simple-shape dc dx dy)
  (cond
    [(polygon? simple-shape)
     (let ([path (new dc-path%)]
           [points (polygon-points simple-shape)])
       (send path move-to (point-x (car points)) (point-y (car points)))
       (let loop ([point (make-rectangular (point-x (car points)) (point-y (car points)))]
                  [last-point (car points)]
                  [points (cdr points)])
         (unless (null? points)
           (let* ([vec (make-rectangular (- (point-x (car points))
                                            (point-x last-point))
                                         (- (point-y (car points))
                                            (point-y last-point)))]
                  [endpoint (+ point vec (make-polar -1 (angle vec)))])
             (send path line-to (real-part endpoint) (imag-part endpoint))
             (loop endpoint (car points) (cdr points)))))
       (send path line-to (point-x (car points)) (point-y (car points)))
       (send dc set-pen (mode-color->pen (polygon-mode simple-shape) (polygon-color simple-shape)))
       (send dc set-brush (mode-color->brush (polygon-mode simple-shape) (polygon-color simple-shape)))
       (send dc draw-path path dx dy 'winding))]
    [(line-segment? simple-shape)
     (let ([path (new dc-path%)]
           [start (line-segment-start simple-shape)]
           [end (line-segment-end simple-shape)])
       (send dc set-pen (line-segment-color simple-shape) 1 'solid)
       (send dc set-brush "black" 'transparent)
       (send dc draw-line
             (+ dx (point-x start)) (+ dy (point-y start))
             (+ dx (point-x end)) (+ dy (point-y end))))]
    [else
     (let ([dx (+ dx (translate-dx simple-shape))]
           [dy (+ dy (translate-dy simple-shape))]
           [atomic-shape (translate-shape simple-shape)])
       (cond
         [(ellipse? atomic-shape)
          (let* ([path (new dc-path%)]
                 [ew (ellipse-width atomic-shape)]
                 [eh (ellipse-height atomic-shape)]
                 [θ (degrees->radians (ellipse-angle atomic-shape))])
            (let-values ([(rotated-width rotated-height) (ellipse-rotated-size ew eh θ)])
              (send path ellipse 0 0 ew eh)
              (send path translate (- (/ ew 2)) (- (/ eh 2)))
              (send path rotate θ)
              (send dc set-pen (mode-color->pen (ellipse-mode atomic-shape) (ellipse-color atomic-shape)))
              (send dc set-brush (mode-color->brush (ellipse-mode atomic-shape) (ellipse-color atomic-shape)))
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
    (let-values ([(bytes w h) (bitmap->bytes (bitmap-rendered-bitmap bitmap) (bitmap-rendered-mask bitmap))])
      (let-values ([(rotated-bytes rotated-w rotated-h)
                    (rotate-bytes bytes w h θ)])
        (set-bitmap-rendered-bitmap!
         bitmap
         (bytes->bitmap rotated-bytes rotated-w rotated-h))
        (set-bitmap-rendered-mask!
         bitmap
         (send (bitmap-rendered-bitmap bitmap) get-loaded-mask))))))

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

(define (mode-color->pen mode color)
  (cond
    [(eq? mode 'solid)
     (send the-pen-list find-or-create-pen "black" 1 'transparent)]
    [else
     (send the-pen-list find-or-create-pen color 1 'solid)]))

(define (mode-color->brush mode color)
  (cond
    [(eq? mode 'solid)
     (send the-brush-list find-or-create-brush color 'solid)]
    [else
     (send the-brush-list find-or-create-brush "black" 'transparent)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide make-image image-shape image-bb image-normalized? image%
         
         (struct-out bb)
         (struct-out point)
         make-overlay overlay? overlay-top overlay-bottom
         make-translate translate? translate-dx translate-dy translate-shape
         make-scale scale-x scale-y scale-shape
         make-ellipse ellipse? ellipse-width ellipse-height ellipse-angle ellipse-mode ellipse-color
         make-text text? text-string text-angle text-y-scale text-color
         text-angle text-size text-face text-family text-style text-weight text-underline
         make-polygon polygon? polygon-points polygon-mode polygon-color
         make-line-segment line-segment? line-segment-start line-segment-end line-segment-color

         make-bitmap bitmap? bitmap-raw-bitmap bitmap-raw-mask bitmap-angle bitmap-x-scale bitmap-y-scale 
         bitmap-rendered-bitmap bitmap-rendered-mask
         
         degrees->radians
         normalize-shape
         ellipse-rotated-size
         
         image?
         image-right
         image-bottom
         image-baseline
         
         text->font
         compare-all-rotations
         render-image)

;; method names
(provide get-shape get-bb get-normalized? get-normalized-shape)

(provide np-atomic-shape? atomic-shape? simple-shape?)
