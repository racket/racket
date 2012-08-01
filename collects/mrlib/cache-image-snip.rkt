#lang racket/base
  (require racket/draw
           racket/snip
           racket/class
           racket/contract
           racket/promise
           (for-syntax racket/base)
           mzlib/string)
  
  (provide cache-image-snip%
           cache-image-snip-class%
           snip-class

           coerce-to-cache-image-snip
           snip-size
           bitmaps->cache-image-snip)
           
  ;; type argb = (make-argb (vectorof rational[between 0 & 255]) int int)
  (define-struct argb (vector width height))
  
  #|

    The true meaning of an image is a vector of rationals,
    between 0 & 255, representing color and alpha channel
    information. The vector's contents are analogous to
    the last argument to the get-argb-pixels method. That is,
    there are (* 4 w h) entries in the vector for an image
    of width w and height h, and the entries represent the
    alpha, red, green, & blue channels, resp.

    When drawn to the screen, the rationals are rounded to
    their nearest integer, but the true meaning is kept inside
    the image.

  note to self:
    mask of zero means this image dominates
    mask of 255 means this image contributes nothing

    black is 0
    white is 255

    a cleared out bitmap is full of 255s (white)

    an alpha of 1 means the pixel value is 0
    an alpha of 0 means the pixel value is 255
 |#

  (define cache-image-snip%
    (class image-snip%
      
      ;; dc-proc : (union #f ((is-a?/c dc<%>) int[dx] int[dy] -> void))
      ;; used for direct drawing
      (init-field dc-proc)
      (define/public (get-dc-proc) dc-proc)
      
      ;; argb-proc : ((vectorof rational[0 <= x <= 255]) int[dx] int[dy] -> void)
      ;; used for drawing into a bitmap
      (init-field argb-proc)
      (define/public (get-argb-proc) argb-proc)
      
      ;; the pinhole's coordinates
      (init-field px py)
      (when (inexact? px)
        (set! px (floor (inexact->exact px))))
      (when (inexact? py)
        (set! py (floor (inexact->exact py))))
      (define/public (get-pinhole) (values px py))

      (init-field (width #f)
                  (height #f))
      (define/public (get-size)
        (values width height))
      
      ;; argb : (union #f argb)
      (init-field [argb #f])

      ;; bitmap : (union #f (is-a?/c bitmap%))
      ;; the way that this image is be drawn, on its own
      (define bitmap #f)
      
      (define/override (copy)
        (new cache-image-snip% 
             (dc-proc dc-proc)
             (argb-proc argb-proc)
             (width width)
             (height height)
             (argb argb)
             (px px)
             (py py)))
      
      ;; get-bitmap : -> bitmap or false
      ;; returns a bitmap showing what the image would look like, 
      ;; if it were drawn
      (define/override (get-bitmap)
        (cond
          [(or (zero? width) (zero? height))
           #f]
          [else
           (unless bitmap
             (set! bitmap (argb->bitmap (get-argb))))
           bitmap]))
      
      ;; get-argb : -> argb
      (define/public (get-argb)
        (unless argb
          (set! argb (make-argb (make-vector (* 4 width height) 255) width height))
          (argb-proc argb 0 0))
        argb)
      
      ;; get-argb/no-compute : -> (union #f argb)
      (define/public (get-argb/no-compute)
        argb)
      
      (define/override (get-extent dc x y w h descent space lspace rspace)
        (set-box/f! w width)
        (set-box/f! h height)
        (set-box/f! descent 0)
        (set-box/f! space 0)
        (set-box/f! lspace 0)
        (set-box/f! rspace 0))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (cond
          [argb 
           (let ([bitmap (get-bitmap)])
             (when bitmap
               (send dc draw-bitmap bitmap x y 'solid 
                     (send the-color-database find-color "black")
                     (send bitmap get-loaded-mask))))]
          [dc-proc 
           (let ([smoothing (send dc get-smoothing)])
             (send dc set-smoothing 'aligned)
             (dc-proc dc x y)
             (send dc set-smoothing smoothing))]
          [else (void)]))
      
      (define/override (write f)
        (let ([str (string->bytes/utf-8
                    (format "~s"
                            (list (argb-vector (get-argb))
                                  width
                                  height
                                  px 
                                  py)))])
          (send f put str)))
      
      (define/override (get-num-scroll-steps) (inexact->exact (+ (floor (/ height 20)) 1)))
      (define/override (find-scroll-step y) (inexact->exact (floor (/ y 20))))
      (define/override (get-scroll-step-offset offset) (* offset 20))

      (define/override (equal-to? snip recur)
        (if (snip . is-a? . cache-image-snip%)
            ;; Support extensions of cache-image-snip%:
            (send snip other-equal-to? this recur)
            ;; Use ths object's extension:
            (other-equal-to? snip recur)))
      (define/override (other-equal-to? snip recur)
        (image=? this snip))
      
      (super-new)
      (inherit set-snipclass)
      (set-snipclass snip-class)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; snip-class
  ;;

  (define cache-image-snip-class%
    (class snip-class%
      (define/override (read f)
        (data->snip (read-from-string (send f get-bytes) (lambda () #f))))
      (define/public (data->snip data)
        (cond
          [(not (list? data)) (make-null-cache-image-snip)]
          [(= (length data) 4)
           ;; this is the case for old save files
           ;; if the width is zero, the height 
           ;; will automatically also be zero
           (let ([argb-vec (list-ref data 0)]
                 [width (list-ref data 1)]
                 [px (list-ref data 2)]
                 [py (list-ref data 3)])
             (argb->cache-image-snip (make-argb argb-vec
                                                width 
                                                (if (zero? width)
                                                    0
                                                    (/ (vector-length argb-vec) width 4)))
                                     px 
                                     py))]
          [(= (length data) 5)
           ;; this is the new saved data and it has the width and the height separately.
           (let ([argb-vec (list-ref data 0)]
                 [width (list-ref data 1)]
                 [height (list-ref data 2)]
                 [px (list-ref data 3)]
                 [py (list-ref data 4)])
             (argb->cache-image-snip (make-argb argb-vec width height) px py))]))
      (super-new)))

  (define snip-class (new cache-image-snip-class%))
  (send snip-class set-version 1)
  (send snip-class set-classname (format "~s" `(lib "cache-image-snip.ss" "mrlib")))
  
  ;; ***** WARNING: illegal activities **** -- MF 
  (define the-drscheme-snip-class  (get-the-snip-class-list))
  (send the-drscheme-snip-class add snip-class)
  (provide the-drscheme-snip-class)
  ;; ***** WARNING: illegal activities ****
  
  (define (make-null-cache-image-snip)
    (define size 10)
    (define (draw dc dx dy)
      (with-pen/brush
       dc
       "black" 'solid
       "black" 'transparent
       (send dc draw-ellipse dx dy size size)
       (send dc draw-line dx (+ dy size -1) (+ dx size -1) dy)))
    (define bm (build-bitmap (lambda (dc) (draw dc 0 0))
                             size
                             size))
    (new cache-image-snip%
         (width size)
         (height size)
         (draw-proc draw)
         (px (/ size 2))
         (py (/ size 2))
         (argb-proc 
          (lambda (argb dx dy)
            (overlay-bitmap argb size size dx dy bm bm)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; image equality
  ;;

  (define size-dc (delay (make-object bitmap-dc% (make-object bitmap% 1 1))))
  
  (define (snip-size a)
    (cond
      [(is-a? a cache-image-snip%)
       (send a get-size)]
      [else
       (let* ([dc (force size-dc)]
              [wb (box 0)]
              [hb (box 0)])
         (send a get-extent dc 0 0 wb hb #f #f #f #f)
         (values (unbox wb) 
                 (unbox hb)))]))
         
  (define (image=? a-raw b-raw)
    (let ([a (coerce-to-cache-image-snip a-raw)]
          [b (coerce-to-cache-image-snip b-raw)])
      (let-values ([(aw ah) (snip-size a)]
                   [(bw bh) (snip-size b)]
                   [(apx apy) (send a get-pinhole)]
                   [(bpx bpy) (send b get-pinhole)])
        (and (= aw bw)
             (= ah bh)
             (= apx bpx)
             (= apy bpy)
             (same/alpha? (argb-vector (send a get-argb))
                          (argb-vector (send b get-argb)))))))
  
  (define (same/alpha? v1 v2)
    (let loop ([i (vector-length v1)])
      (or (zero? i)
          (let ([a1 (vector-ref v1 (- i 4))]
                [a2 (vector-ref v2 (- i 4))])
            (and (or (= a1 a2 255)
                     (and (= a1 a2)
                          (= (vector-ref v1 (- i 3)) (vector-ref v2 (- i 3)))
                          (= (vector-ref v1 (- i 2)) (vector-ref v2 (- i 2)))
                          (= (vector-ref v1 (- i 1)) (vector-ref v2 (- i 1)))))
                 (loop (- i 4)))))))
  
  (define image-snip-cache (make-weak-hasheq))
  ;; coerce-to-cache-image-snip : image -> (is-a?/c cache-image-snip%)
  (define (coerce-to-cache-image-snip snp)
    (cond
      [(is-a? snp cache-image-snip%) snp]
      [(hash-ref image-snip-cache snp (λ () #f)) => values]
      [(is-a? snp image-snip%)
       (let* ([bmp (send snp get-bitmap)]
              [cis
               (if bmp
                   (let ([bmp-mask (or (send bmp get-loaded-mask)
                                       (send snp get-bitmap-mask)
                                       (bitmap->mask bmp))])
                     (bitmaps->cache-image-snip (copy-bitmap bmp)
                                                (copy-bitmap bmp-mask)
                                                (floor (/ (send bmp get-width) 2))
                                                (floor (/ (send bmp get-height) 2))))
                   (let-values ([(w h) (snip-size snp)])
                     (let* ([bmp (make-object bitmap% 
                                   (inexact->exact (floor w))
                                   (inexact->exact (floor h)))]
                            [bdc (make-object bitmap-dc% bmp)])
                       (send bdc clear)
                       (send snp draw bdc 0 0 0 0 w h 0 0 'no-caret)
                       (send bdc set-bitmap #f)
                       (bitmaps->cache-image-snip bmp 
                                                  (bitmap->mask bmp)
                                                  (floor (/ w 2))
                                                  (floor (/ h 2))))))])
         (hash-set! image-snip-cache snp cis)
         cis)]
      [else snp]))
    
  ;; copy-bitmap : bitmap -> bitmap
  ;; does not copy the mask.
  (define (copy-bitmap bitmap)
    (let* ([w (send bitmap get-width)]
           [h (send bitmap get-height)]
           [copy (make-object bitmap% w h)]
           [a-dc (make-object bitmap-dc% copy)])
      (send a-dc clear)
      (send a-dc draw-bitmap bitmap 0 0)
      (send a-dc set-bitmap #f)
      copy))
  
  ;; bitmap->mask : bitmap -> bitmap
  (define (bitmap->mask bitmap)
    (let* ([w (send bitmap get-width)]
           [h (send bitmap get-height)]
           [s (make-bytes (* 4 w h))]
           [new-bitmap (make-object bitmap% w h)]
           [dc (make-object bitmap-dc% new-bitmap)])
      (send dc clear)
      (send dc draw-bitmap bitmap 0 0)
      (send dc get-argb-pixels 0 0 w h s)
      (let loop ([i (* 4 w h)])
        (unless (zero? i)
          (let ([r (- i 3)]
                [g (- i 2)]
                [b (- i 1)])
            (unless (and (eq? 255 (bytes-ref s r))
                         (eq? 255 (bytes-ref s g))
                         (eq? 255 (bytes-ref s b)))
              (bytes-set! s r 0)
              (bytes-set! s g 0)
              (bytes-set! s b 0))
            (loop (- i 4)))))
      (send dc set-argb-pixels 0 0 w h s)
      (begin0
        (send dc get-bitmap)
        (send dc set-bitmap #f))))
  
  (define (bitmaps->cache-image-snip color mask px py)
    (let ([w (send color get-width)]
          [h (send color get-height)])
      (new cache-image-snip%
           [width w]
           [height h]
           [dc-proc
            (lambda (dc dx dy)
              (send dc draw-bitmap color dx dy 'solid 
                    (send the-color-database find-color "black")
                    mask))]
           [argb-proc
            (lambda (argb-vector dx dy)
              (overlay-bitmap argb-vector dx dy color mask))]
           [px px]
           [py py])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; misc. utilities
  ;;
  
  ;; takes a bitmap with a mask and flattens the colors and the mask
  ;; drawing them as they would appear on the screen.
  (define (flatten-bitmap bm)
    (let* ([w (send bm get-width)]
           [h (send bm get-height)]
           [new-bm (make-object bitmap% w h)]
           [bdc (make-object bitmap-dc% new-bm)])
      (send bdc clear)
      (send bdc draw-bitmap bm 0 0 'solid 
            (send the-color-database find-color "black")
            (send bm get-loaded-mask))
      (send bdc set-bitmap #f)
      new-bm))
  
  ;; build-bitmap : (dc -> void) number number -> bitmap
  (define (build-bitmap draw w h)
    (let* ([bm (make-object bitmap% w h)]
           [bdc (make-object bitmap-dc% bm)])
      (send bdc clear)
      ; (send bdc set-smoothing 'aligned) ; causes image-inside? to fail in test suite.
      (draw bdc)
      (send bdc set-bitmap #f)
      bm))

  (define-syntax (with-pen/brush stx)
    (syntax-case stx ()
      [(_ dc pen-color pen-style brush-color brush-style code ...)
       (syntax
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)])
          (send dc set-pen (send the-pen-list find-or-create-pen pen-color 1 pen-style))
          (send dc set-brush (send the-brush-list find-or-create-brush brush-color brush-style))
          code ...
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)))]))
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; argb vector utilties
  ;;
  
  ;; argb->cache-image-snip : argb number number -> cache-image-snip
  (define (argb->cache-image-snip argb px py)
    (let* ([width (argb-width argb)]
           [height (argb-height argb)]
           [argb-vector (argb-vector argb)]
           [bitmap (argb->bitmap argb)]
           [mask (and bitmap (send bitmap get-loaded-mask))])
      (new cache-image-snip%
           (width width)
           (height height)
           (argb argb)
           (px px)
           (py py)
           (argb-proc (if (or (zero? width) (zero? height))
                          void
                          (lambda (argb dx dy) (overlay-bitmap argb dx dy bitmap mask))))
           (dc-proc (if (or (zero? width) (zero? height))
                        void
                        (lambda (dc dx dy)
                          (send dc draw-bitmap bitmap dx dy 'solid 
                                (send the-color-database find-color "black")
                                mask)))))))
  
  ;; argb-vector->bitmap : argb -> bitmap or false
  ;; flattens the argb vector into a bitmap
  (define (argb->bitmap argb)
    (let* ([argb-vector (argb-vector argb)]
           [w (argb-width argb)]
           [h (argb-height argb)])
      (cond
        [(or (zero? w) (zero? h)) #f]
        [else
         (let* ([bm (make-object bitmap% w h)]
                [mask-bm (make-object bitmap% w h)]
                [bdc (new bitmap-dc% (bitmap bm))]
                [bytes (make-bytes (vector-length argb-vector) 255)]
                [mask-bytes (make-bytes (vector-length argb-vector) 255)])
           (let loop ([i (- (vector-length argb-vector) 1)])
             (cond
               [(zero? (modulo i 4))
                (let ([av (round (vector-ref argb-vector i))])
                  (bytes-set! mask-bytes (+ i 1) av)
                  (bytes-set! mask-bytes (+ i 2) av)
                  (bytes-set! mask-bytes (+ i 3) av))]
               [else
                (bytes-set! bytes i (round (vector-ref argb-vector i)))])
             (unless (zero? i)
               (loop (- i 1))))
           (send bdc set-argb-pixels 0 0 w h bytes)
           (send bdc set-bitmap mask-bm)
           (send bdc set-argb-pixels 0 0 w h mask-bytes)
           (send bdc set-bitmap #f)
           (send bm set-loaded-mask mask-bm)
           bm)])))
  
  ;; overlay-bitmap : argb int int bitmap bitmap -> void
  ;; assumes that the mask bitmap only has greyscale in it
  ;; (ie, that looking at the red component of the mask is enough)
  (define (overlay-bitmap argb dx dy color mask)
    (let* ([argb-vector (argb-vector argb)]
           [argb-w (argb-width argb)]
           [w (send color get-width)]
           [h (send color get-height)]
           [color-bytes (make-bytes (* w h 4) 0)]
           [mask-bytes (make-bytes (* w h 4) 0)]
           [dc (make-object bitmap-dc%)])
      (send dc set-bitmap color)
      (send dc get-argb-pixels 0 0 w h color-bytes)
      (send dc set-bitmap #f) ;; in case mask and color are the same bitmap....
      (send dc set-bitmap mask)
      (send dc get-argb-pixels 0 0 w h mask-bytes)
      (send dc set-bitmap #f)
      (let yloop ([y 0]
                  [str-i 0])
        (unless (= y h)
          (let xloop ([x 0]
                      [str-i str-i])
            (if (= x w)
                (yloop (add1 y) str-i)
                (begin
                  (when (and (<= 0 (+ x dx))
                             (< (+ x dx) argb-w))
                    (let ([argb-i (* 4 (+ (+ dx x) (* (+ dy y) argb-w)))])
                      (when (and (<= 0 argb-i)
                                 (< argb-i (vector-length argb-vector)))
                        (let* ([m1 (vector-ref argb-vector argb-i)]
                               [m2 (bytes-ref mask-bytes (+ str-i 1))] ;; get red coordinate
                               [m3 (build-m3 m1 m2)]
			       [bang (lambda (i v) (vector-set! argb-vector i (floor v)))]
                               [do-b
                                (lambda (off)
                                  (bang (+ argb-i off)
					(build-b3 m1
						  (vector-ref argb-vector (+ argb-i off))
						  m2
						  (bytes-ref color-bytes (+ str-i off))
						  m3)))])
                          (bang argb-i m3)
                          (do-b 1)
                          (do-b 2)
                          (do-b 3)))))
                  (xloop (+ x 1) (+ str-i 4)))))))))
  
#|
From Matthew's computation in PR 6930:
> m3 is (m1+m2-m1*m2) and 
> b3 is (m1*b1*(1-m2) + m2*b2)/m3

but that's for values between 0 and 1 and we
need values between 0 and 255. Worse, the values
sense are reversed. That is,
1 above corresponds to 0 in pixel values and
0 above corresponds to 255.

;; the spec
(define (build-m3-0 big-m1 big-m2)
  (let ([m1 (- 1 (/ big-m1 255))]
        [m2 (- 1 (/ big-m2 255))])
    (let ([m3 (+ m1 m2 (- (* m1 m2)))])
      (* 255 (- 1 m3)))))

; = substitute in lets
(define (build-m3-1 m1 m2)
  (* 255 (- 1 (+ (- 1 (/ m1 255))
                 (- 1 (/ m2 255))
                 (- (* (- 1 (/ m1 255))
                       (- 1 (/ m2 255))))))))

;= multiply out last product
(define (build-m3-2 m1 m2)
  (* 255 (- 1 (+ (- 1 (/ m1 255))
                 (- 1 (/ m2 255))
                 (- (+ 1 
                       (- (/ m1 255))
                       (- (/ m2 255))
                       (* (- (/ m1 255)) (- (/ m2 255)))))))))

; = lift out the neagtives into topmost sum
(define (build-m3-3 m1 m2)
  (* 255 (- 1 (+ (- (/ m1 255))
                 1
                 (- (/ m2 255))
                 1
                 -1
                 (/ m1 255)
                 (/ m2 255)
                 (- (* (/ m1 255) (/ m2 255)))))))

; = push in topmost subtraction
(define (build-m3-4 m1 m2)
  (* 255 (+ 1
            (/ m1 255)
            -1
            (/ m2 255)
            -1
            1
            (- (/ m1 255))
            (- (/ m2 255))
            (* (/ m1 255) (/ m2 255)))))

; = simplify sum:

(define (build-m3-5 m1 m2)
  (* 255 (* (/ m1 255) (/ m2 255))))

; = distribute 255

(define (build-m3-6 m1 m2) (* m1 m2 1/255))

(define (test-m3 m1 m2)
  (values (build-m3-0 m1 m2)
          (build-m3-1 m1 m2)
          (build-m3-2 m1 m2)
          (build-m3-3 m1 m2)
          (build-m3-4 m1 m2)
          (build-m3-5 m1 m2)
          (build-m3-6 m1 m2)))

(test-m3 0 0)
(test-m3 255 255)
(test-m3 100 200)

for b3, we have:

(define (build-m3-6 m1 m2) (* m1 m2 1/255))

;; the spec
(define (build-b3-0 big-m1 big-b1 big-m2 big-b2 big-m3)
  (let ([m1 (- 1 (/ big-m1 255))]
        [b1 (- 1 (/ big-b1 255))]
        [m2 (- 1 (/ big-m2 255))]
        [b2 (- 1 (/ big-b2 255))]
        [m3 (- 1 (/ big-m3 255))])
    (let ([ans (/ (+ (* m1 b1 (- 1 m2)) (* m2 b2)) m3)])
      (* 255 (- 1 ans)))))

;; = substitute in for let.
(define (build-b3-1 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- 1 (/ m1 255)) (- 1 (/ b1 255)) (- 1 (- 1 (/ m2 255)))) 
                    (* (- 1 (/ m2 255)) (- 1 (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = simple substitution
(define (build-b3-2 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- 1 (/ m1 255)) (- 1 (/ b1 255)) (/ m2 255))
                    (* (- 1 (/ m2 255)) (- 1 (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = multiply out first part of first *
(define (build-b3-3 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (+ 1
                          (- (/ m1 255))
                          (- (/ b1 255))
                          (* (/ m1 255) (/ b1 255))) 
                       (/ m2 255))
                    (* (- 1 (/ m2 255)) (- 1 (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = distribute out newly created product 
(define (build-b3-4 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (/ m2 255)
                    (* (- (/ m1 255)) (/ m2 255))
                    (* (- (/ b1 255)) (/ m2 255))
                    (* (/ m1 255) (/ b1 255) (/ m2 255))
                    (* (- 1 (/ m2 255)) (- 1 (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = multiply out product of sum
(define (build-b3-5 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (/ m2 255)
                    (* (- (/ m1 255)) (/ m2 255))
                    (* (- (/ b1 255)) (/ m2 255))
                    (* (/ m1 255) (/ b1 255) (/ m2 255))
                    (+ 1
                       (- (/ m2 255))
                       (- (/ b2 255))
                       (* (/ m2 255) (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = flatten out sum of sum & simplify
(define (build-b3-6 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- (/ m1 255)) (/ m2 255))
                    (* (- (/ b1 255)) (/ m2 255))
                    (* (/ m1 255) (/ b1 255) (/ m2 255))
                    1
                    (- (/ b2 255))
                    (* (/ m2 255) (/ b2 255)))
                 (- 1 (/ m3 255))))))

;; = rearrange denom
(define (build-b3-7 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- (/ m1 255)) (/ m2 255))
                    (* (- (/ b1 255)) (/ m2 255))
                    (* (/ m1 255) (/ b1 255) (/ m2 255))
                    1
                    (- (/ b2 255))
                    (* (/ m2 255) (/ b2 255)))
                 (/ (- 255 m3) 255)))))

;; = move 255 to numerator
(define (build-b3-8 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (* 255 (+ (* (- (/ m1 255)) (/ m2 255))
                           (* (- (/ b1 255)) (/ m2 255))
                           (* (/ m1 255) (/ b1 255) (/ m2 255))
                           1
                           (- (/ b2 255))
                           (* (/ m2 255) (/ b2 255))))
                 (- 255 m3)))))

;; cancel out 255s in numerator
(define (build-b3-9 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- m1) (/ m2 255))
                    (* (- b1) (/ m2 255))
                    (* m1 (/ b1 255) (/ m2 255))
                    255
                    (- b2)
                    (* m2 (/ b2 255)))
                 (- 255 m3)))))

;; rearrange numerator
(define (build-b3-10 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (/ (* (- m1) m2) 255)
                    (/ (* (- b1) m2) 255)
                    (/ (* m1 b1 (/ m2 255)) 255)
                    (/ (* 255 255) 255)
                    (/ (* 255 (- b2)) 255)
                    (/ (* m2 b2) 255))
                 (- 255 m3)))))

;; pull out 255 in num
(define (build-b3-11 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (/ (+ (* (- m1) m2)
                       (* (- b1) m2)
                       (* m1 b1 (/ m2 255))
                       (* 255 255)
                       (* 255 (- b2))
                       (* m2 b2))
                    255)
                 (- 255 m3)))))

;; push 255 into denom
(define (build-b3-12 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- m1) m2)
                    (* (- b1) m2)
                    (* m1 b1 (/ m2 255))
                    (* 255 255)
                    (* 255 (- b2))
                    (* m2 b2))
                 (* 255 (- 255 m3))))))

;; turn 1 into (/ (* 255 (- 255 m3)) (* 255 (- 255 m3)))
;; and add into numerator
(define (build-b3-13 m1 b1 m2 b2 m3)
  (* 255 (/ (- (* 255 (- 255 m3))
               (+ (* (- m1) m2)
                  (* (- b1) m2)
                  (* m1 b1 (/ m2 255))
                  (* 255 255)
                  (* 255 (- b2))
                  (* m2 b2)))
            (* 255 (- 255 m3)))))

;; cancel out outer 255
(define (build-b3-14 m1 b1 m2 b2 m3)
  (/ (- (* 255 (- 255 m3))
        (+ (* (- m1) m2)
           (* (- b1) m2)
           (* m1 b1 (/ m2 255))
           (* 255 255)
           (* 255 (- b2))
           (* m2 b2)))
     (- 255 m3)))

;; push negative thru to make big sum in numerator
(define (build-b3-15 m1 b1 m2 b2 m3)
  (/ (+ (* 255 (- 255 m3))
        (* m1 m2)
        (* b1 m2)
        (- (* m1 b1 (/ m2 255)))
        (- (* 255 255))
        (* 255 b2)
        (- (* m2 b2)))
     (- 255 m3)))

;; distribute 255 in first num term
(define (build-b3-16 m1 b1 m2 b2 m3)
  (/ (+ (* 255 255)
        (- (* 255 m3))
        (* m1 m2)
        (* b1 m2)
        (- (* m1 b1 (/ m2 255)))
        (- (* 255 255))
        (* 255 b2)
        (- (* m2 b2)))
     (- 255 m3)))

;; simplify num
(define (build-b3-17 m1 b1 m2 b2 m3)
  (/ (+ (* m1 m2)
        (* b1 m2)
        (- (* m2 b2))
        (- (* m1 b1 m2 1/255))
        (* 255 b2)
        (- (* 255 m3)))
     (- 255 m3)))

;; simplify num, some more
(define (build-b3-18 m1 b1 m2 b2 m3)
  (/ (+ (* (+ m1 b1 (- b2)) m2) 
        (* m1 b1 m2 -1/255)
        (* 255 b2)
        (* -255 m3))
     (- 255 m3)))

(define (test-b3 m1 b1 m2 b2)
  (let ([m3 (build-m3-6 m1 m2)])
    (values (build-b3-0 m1 b1 m2 b2 m3)
            (build-b3-1 m1 b1 m2 b2 m3)
            (build-b3-2 m1 b1 m2 b2 m3)
            (build-b3-3 m1 b1 m2 b2 m3)
            (build-b3-4 m1 b1 m2 b2 m3)
            (build-b3-5 m1 b1 m2 b2 m3)
            (build-b3-6 m1 b1 m2 b2 m3)
            (build-b3-7 m1 b1 m2 b2 m3)
            (build-b3-8 m1 b1 m2 b2 m3)
            (build-b3-9 m1 b1 m2 b2 m3)
            (build-b3-10 m1 b1 m2 b2 m3)
            (build-b3-11 m1 b1 m2 b2 m3)
            (build-b3-12 m1 b1 m2 b2 m3)
            (build-b3-13 m1 b1 m2 b2 m3)
            (build-b3-14 m1 b1 m2 b2 m3)
            (build-b3-15 m1 b1 m2 b2 m3)
            (build-b3-16 m1 b1 m2 b2 m3)
            (build-b3-17 m1 b1 m2 b2 m3)
            (build-b3-18 m1 b1 m2 b2 m3)
            )))

(test-b3 255 100 0 250)
(test-b3 0 150 255 100)
(test-b3 100 200 75 150)


|#
  
  (define (build-m3 m1 m2) (* m1 m2 1/255))
  
  (define (build-b3 m1 b1 m2 b2 m3)
    (if (= m3 255)
        0
        (/ (+ (* (+ m1 b1 (- b2)) m2) 
              (* m1 b1 m2 -1/255)
              (* 255 b2)
              (* -255 m3))
           (- 255 m3))))

  (define bitmap-size/c (and/c integer? exact? (between/c 1 10000)))
  
  (provide/contract
   [overlay-bitmap (argb? exact-integer?
                          exact-integer?
                          (is-a?/c bitmap%)
                          (is-a?/c bitmap%)
                          . -> .
                          any)]
   [build-bitmap (((is-a?/c dc<%>) . -> . any) bitmap-size/c bitmap-size/c . -> . (is-a?/c bitmap%))]
   [flatten-bitmap ((is-a?/c bitmap%) . -> . (is-a?/c bitmap%))]

   [argb->cache-image-snip (argb? number? number? . -> . (is-a?/c cache-image-snip%))]
   [argb->bitmap (argb? . -> . (or/c #f (is-a?/c bitmap%)))]
           
   [argb? (any/c . -> . boolean?)]
   [make-argb ((vectorof byte?) exact-nonnegative-integer? exact-nonnegative-integer? . -> . argb?)]
   [argb-vector (argb? . -> . (vectorof byte?))]
   [argb-width (argb? . -> . exact-nonnegative-integer?)]
   [argb-height (argb? . -> . exact-nonnegative-integer?)])
