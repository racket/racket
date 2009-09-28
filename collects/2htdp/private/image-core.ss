#lang scheme/base

#|

improvments/changes wrt to htdp/image:

  - copying and pasting does not introduce jaggies
  - equal comparisions are more efficient
  - added rotation & scaling
  - got rid of pinholes (see the new overlay, beside, and above functions)

Equality change: equality is now based on the structure of the construction of the picture. 
This means that some equalities that were there before are no longer true. For example,
in the old library, these two images are the same:

  (overlay/xy (rectangle 100 10 'solid 'red)
               0
               10
               (rectangle 100 10 'solid 'red))

  (rectangle 100 20 'solid 'red)

... and why aren't they the same again....?!

todo: sort out wxme library support (loading in text mode).

;; when rendering these things in error messages,
;; they should come out as #<image: {THE ACTUAL PICTURE}>
;; (automatically scale them down so they fit)
;; or should it be just the image directly?

;; redex randomized testing: see if normalization produces normalized shapes.
;;        see if normalization always puts things in the right order

;; need to change error messages to say "the width (second) argument"
;; by passing "width (second)" to the check-arg function


From Matthias: (to use to compare with this library)


You asked about exercises and code snippets for HtDP/2e yesterday. I actually do have a bunch of stuff in

 svn: 2HtDP/SampleCode/

and they all have good sample contracts. (It is amazing what we can do with kids who have just a few weeks of cs down; I would have never dared to write an editor after six weeks in Algol.)


|#


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
         
         show-image
         
         normalize-shape
         rotate-atomic
         rotate-simple
         simple-bb
         make-image image-shape
         
         make-bb
         make-overlay
         make-translate
         make-ellipse
         make-text
         make-polygon
         make-point
         bring-between)


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
(define (image? p) (is-a? p image%))


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
;;  - atomic-shape

;; an atomic-shape is either:
;;  - polygon
;;  - np-atomic-shape

;; a np-atomic-shape is:
;;
;;  - (make-ellipse width height angle mode color)
(define-struct/reg-mk ellipse (width height angle mode color) #:transparent #:omit-define-syntaxes)
;;
;;  - (make-text string angle font)
(define-struct/reg-mk text (string angle font) #:omit-define-syntaxes #:transparent)
;;
;;  - (make-bitmap (is-a?/c bitmap%) angle)
(define-struct/reg-mk bitmap (bitmap angle))

;; a polygon is:
;;
;;  - (make-polygon (listof points) angle pen brush)
(define-struct/reg-mk polygon (points mode color) #:transparent #:omit-define-syntaxes
  #:property prop:equal+hash 
  (list (λ (a b rec) (polygon-equal? a b rec)) (λ (x y) 42) (λ (x y) 3)))

;; a normalized-shape (subtype of shape) is either
;;  - (make-overlay normalized-shape simple-shape)
;;  - simple-shape

;; a simple-shape (subtype of shape) is
;;  - (make-translate dx dy np-atomic-shape)
;;  - polygon

;; an angle is a number between 0 and 360 (degrees)

(define (polygon-equal? p1 p2 eq-recur)
  (and (eq-recur (polygon-mode p1) (polygon-mode p2))
       (eq-recur (polygon-color p1) (polygon-color p2))
       (let ([p1-points (polygon-points p1)]
             [p2-points (polygon-points p2)])
         (or (and (null? p1-points)
                  (null? p2-points))
             (and (not (or (null? p1-points)
                           (null? p2-points)))
                  (eq-recur (rotate-to-zero (closest-to-zero p1-points) p1-points)
                            (rotate-to-zero (closest-to-zero p2-points) p2-points)))))))

(define (rotate-to-zero zero-p points)
  (let loop ([points points]
             [acc null])
    (cond
      [(equal? (car points) zero-p)
       (append points (reverse acc))]
      [else
       (loop (cdr points)
             (cons (car points) acc))])))

(define (closest-to-zero points)
  (car (sort points < #:key (λ (p) (+ (point-x p) (point-y p))))))


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
      (eq-recur (get-normalized-shape)
                (send that get-normalized-shape)))
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
    
    (define/override (copy) (make-image shape bb normalized?))
    (define/override (draw dc x y left top right bottom dx dy draw-caret?)
      (render-image this dc x y))
    (define/override (get-extent dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (send (get-the-snip-class-list) add snip-class)
      (let ([bottom (bb-bottom bb)])
        (set-box/f! w (bb-right bb))
        (set-box/f! h bottom)
        (set-box/f! descent (- bottom (bb-baseline bb)))
        (set-box/f! space 0)
        (set-box/f! lspace 0)
        (set-box/f! rspace 0)))

    (define/override (write f) 
      (send f put (string->bytes/utf-8 (format "~s" (list shape bb)))))
    
    (super-new)
    
    (inherit set-snipclass)
    (set-snipclass snip-class)))

(define scheme/base:read read)

(define image-snipclass% 
  (class snip-class%
    (define/override (read f)
      (let* ([str (bytes->string/utf-8 (send f get-unterminated-bytes))]
             [lst (parse 
                   (scheme/base:read
                    (open-input-string
                     str)))])
        (if lst
            (make-image (list-ref lst 0)
                        (list-ref lst 1)
                        #f)
            (rectangle 20 20 'solid 'black))))
    (super-new)))

(provide snip-class) 
(define snip-class (new image-snipclass%))
(send snip-class set-classname (format "~s" '(lib "image-core.ss" "2htdp/private")))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; normalize-shape : shape (atomic-shape -> atomic-shape) -> normalized-shape
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
      [(polygon? shape)
       (let ([this-one (make-polygon (map (λ (p)
                                            (make-point (+ dx (point-x p))
                                                        (+ dy (point-y p))))
                                          (polygon-points shape))
                                     (polygon-mode shape)
                                     (polygon-color shape))])
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))]
      [(np-atomic-shape? shape)
       (let ([this-one (make-translate dx dy shape)])
         (if bottom
             (make-overlay bottom (f this-one))
             (f this-one)))]
      [else 
       (error 'normalize-shape "unknown shape ~s\n" shape)])))

(define (atomic-shape? shape)
  (or (ellipse? shape)
      (text? shape)
      (polygon? shape)
      (bitmap? shape)))

(define (np-atomic-shape? shape)
  (or (ellipse? shape)
      (text? shape)
      (bitmap? shape)))

;; rotate-point : x,y angle -> x,y
(define (rotate-point x y θ)
  (c->xy (* (make-polar 1 (degrees->radians θ)) 
            (xy->c x y))))

(define (xy->c x y) (make-rectangular x (- y)))
(define (c->xy c) 
  (values (real-part c)
          (- (imag-part c))))


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
    [(image image1 image2 image3) 
     (check-arg fn-name
                (image? arg)
                'image
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
    [(dx dy)
     (check-arg fn-name
                (number? arg)
                'number
                i arg)
     arg]
    [(angle)
     (check-arg fn-name
                (and (number? arg)
                     (<= 0 arg)
                     (< arg 360))
                'angle\ in\ degrees
                i arg)
     arg]
    [(color)
     (check-color fn-name i arg)
     (let ([color-str 
            (cond
              [(symbol? arg)
               (symbol->string arg)]
              [(string? arg)
               (symbol->string arg)]
              [else arg])])
       (if (send the-color-database find-color color-str)
           color-str
           "black"))]
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
    (send (new button% [label "²"] [callback (λ x (scale-adjust add1))] [parent bp]) min-width 100)
    (send f show #t)))

;; render-image : normalized-shape dc dx dy -> void
(define (render-image image dc dx dy)
  (let loop ([shape (send image get-normalized-shape)])
    (cond
      [(overlay? shape)
       (render-simple-shape (overlay-bottom shape) dc dx dy)
       (loop (overlay-top shape))]
      [else
       (render-simple-shape shape dc dx dy)])))

(define (render-simple-shape simple-shape dc dx dy)
  (cond
    [(polygon? simple-shape)
     (let ([path (new dc-path%)]
           [points (polygon-points simple-shape)])
       (send path move-to (point-x (car points)) (point-y (car points)))
       (let loop ([points (cdr points)])
         (unless (null? points)
           (send path line-to (point-x (car points)) (point-y (car points)))
           (loop (cdr points))))
       (send path line-to (point-x (car points)) (point-y (car points)))
       (send dc set-pen (mode-color->pen (polygon-mode simple-shape) (polygon-color simple-shape)))
       (send dc set-brush (mode-color->brush (polygon-mode simple-shape) (polygon-color simple-shape)))
       (send dc draw-path path dx dy))]
    [else
     (let ([dx (+ dx (translate-dx simple-shape))]
           [dy (+ dy (translate-dy simple-shape))]
           [atomic-shape (translate-shape simple-shape)])
       (cond
         [(ellipse? atomic-shape)
          (let ([path (new dc-path%)]
                [θ (degrees->radians (ellipse-angle atomic-shape))])
            (send path ellipse 0 0 (ellipse-width atomic-shape) (ellipse-height atomic-shape))
            (send path rotate θ)
            (send dc set-pen (mode-color->pen (ellipse-mode atomic-shape) (ellipse-color atomic-shape)))
            (send dc set-brush (mode-color->brush (ellipse-mode atomic-shape) (ellipse-color atomic-shape)))
            (send dc draw-path path dx dy))]
         
         [(text? atomic-shape)
          (let ([θ (degrees->radians (text-angle atomic-shape))])
            (send dc set-font (text-font atomic-shape))
            (send dc draw-text (text-string atomic-shape) dx dy #f 0 angle))]))]))

(define (degrees->radians θ)
  (* θ 2 pi (/ 360)))


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
     (let ([dx (translate-dx simple-shape)]
           [dy (translate-dy simple-shape)]
           [atomic-shape (translate-shape simple-shape)])
       (fprintf (current-error-port) "BAD bounding box\n")
       (values 0 0 100 100))]))


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

