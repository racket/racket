#lang scheme/base
(require racket/draw
         scheme/class)


(provide rotate-bytes ;; : bytes int[width] int[height] radians[radians] -> bytes
         flip-bytes ;; : bytes int[width] int[height] -> bytes
         bitmap->bytes
         bytes->bitmap
         linear-transform)
;; rotate-bitmap : (-> bytes? natural-number/c natural-number/c real? bytes?)
;; avoid a dependency on scheme/contract, which pulls in too much

;; for the test suite:
(provide clamp-1 build-bytes bmbytes-ref/safe interpolate)

(define pi (atan 0 -1))
(define i 0+1i)

#|

instead of this scaling code, we use the dc<%>'s scaling code.

(provide/contract [scale-bitmap
                   (-> bytes? natural-number/c natural-number/c (and/c real? (not/c negative?)) 
                       bytes?)])


; bmbytes: a bytes which represents an image -- 
; its size is a multiple of 4, and each
; four consecutive bytes represent alpha,r,g,b.


; scale: given a bmbytes,
; return a new bmbytes scaled by k in each direction.
;
; TODO: this code is meant for scaling by (>= k 1);
; if (< k 1) then the result will ignore ~ (1-k) of the original's pixels.
; We should really do a proper averaging for that case.
;
(define (scale-bitmap bmbytes w h k)
  (let* {[new-w (round/e (* w k))]
         [new-h (round/e (* h k))]
         }
         (values (build-bmbytes new-w
                                new-h
                                (λ (x y) (interpolate bmbytes w h
                                                      ; You'd think x would map to (x/(kw))*w,
                                                      ; but we actually use (x/(kw-1))*(w-1).
                                                      ; It's because the distance between left- and right-most samples
                                                      ; is one less than the number of samples,
                                                      ; and we want the end-samples at the far ends of the new bitmap.
                                                      (* (/ x (sub1 (* k w))) (sub1 w))
                                                      (* (/ y (sub1 (* k h))) (sub1 h)))))
                 new-w
                 new-h)))
|#

(define (bitmap->bytes bm [mask (send bm get-loaded-mask)])
  (let* ([w (send bm get-width)]
         [h (send bm get-height)]
         [bytes (make-bytes (* w h NUM-CHANNELS) 0)])
    (send bm get-argb-pixels 0 0 w h bytes #f)
    (when mask
      (send mask get-argb-pixels 0 0 w h bytes #t))
    (values bytes w h)))

(define (bytes->bitmap bytes w h)
  (unless (= (bytes-length bytes) (* w h NUM-CHANNELS))
    (error 'bytes->bitmap "wrong sizes, got ~a bytes, w ~a h ~a (which should be ~a bytes)"
           (bytes-length bytes)
           w
           h
           (* w h NUM-CHANNELS)))
  (let* ([bm (make-bitmap w h)])
    (send bm set-argb-pixels 0 0 w h bytes)
    bm))

(define (flip-bytes bmbytes w h)
  (build-bmbytes 
   w h 
   (λ (x y)
     (let ([new-x x]
           [new-y (- h y 1)])
       (bmbytes-ref/safe bmbytes w h new-x new-y)))))

#;
(define (rotate-bytes bmbytes w h theta)
  (let* {[theta-rotation (exp (* i theta))]
         [theta-unrotation (make-rectangular (real-part theta-rotation)
                                             (- (imag-part theta-rotation)))]
         [ne (* theta-rotation w)]
         [sw (* theta-rotation (* i (- h)))]
         [se (* theta-rotation (make-rectangular w (- h)))]
         [nw 0]
         [pts (list ne sw se nw)]
         [longitudes (map real-part pts)]
         [latitudes  (map imag-part pts)]
         [east (apply max longitudes)]
         [west (apply min longitudes)]
         [nrth (apply max latitudes)]
         [sth  (apply min latitudes)]
         [new-w (round/e (- east west))]
         [new-h (round/e (- nrth sth))]
         }
    (values (build-bmbytes new-w
                           new-h
                           (λ (x y)
                             (let* {[pre-image (* (make-rectangular (+ west x 1/2) (- nrth y 1/2))
                                                  theta-unrotation)]
                                    }
                               (interpolate bmbytes w h
                                            (real-part pre-image)
                                            (- (imag-part pre-image))))))
            new-w
            new-h)))

;; linear transform: bytes width height <matrix coodinates> -> (values bytes width height)
;; The matrix is read like this:
;;  +-   -+
;;  | a b |
;;  | c d |
;;  +-   -+
;; The ai, bi, ci, and di are the coordinates of the inverse matrix
(define (linear-transform bmbytes w h a b c d)
  (let-values ([(ai bi ci di)
                (let ([k (/ (- (* a d) (* b c)))])
                  (values (* k d) (* k (- b))
                          (* k (- c)) (* k a)))])
    ;; mapp : <matrix> complex -> complex
    ;; applies the matrix represented by abcd(as in the picture above) to p
    (define (mapp a b c d p)
      (let ([x (real-part p)]
            [y (imag-part p)])
        (make-rectangular (+ (* a x) (* b y))
                          (+ (* c x) (* d y)))))
    (let* {[f-rotation (λ (p) (mapp a b c d p))]
           [f-unrotation (λ (p) (mapp ai bi ci di p))]
           [ne (f-rotation w)]
           [sw (f-rotation (* i (- h)))]
           [se (f-rotation (make-rectangular w (- h)))]
           [nw 0]
           [pts (list ne sw se nw)]
           [longitudes (map real-part pts)]
           [latitudes  (map imag-part pts)]
           [east (apply max longitudes)]
           [west (apply min longitudes)]
           [nrth (apply max latitudes)]
           [sth  (apply min latitudes)]
           [new-w (round/e (- east west))]
           [new-h (round/e (- nrth sth))]}
      (values (build-bmbytes new-w
                             new-h
                             (λ (x y)
                               (let* {[pre-image (f-unrotation (make-rectangular (+ west x 1/2) (- nrth y 1/2)))]}
                                 (interpolate bmbytes w h
                                              (real-part pre-image)
                                              (- (imag-part pre-image))))))
              new-w
              new-h))))

(define (rotate-bytes bmbytes w h theta)
  (let* ([theta-rotation (exp (* i theta -1))]
         [x (real-part theta-rotation)]
         [y (imag-part theta-rotation)])
    (linear-transform 
     bmbytes w h 
     x (- y) y x)))

#;
(define (rotate-bytes bmbytes w h theta)
  (let* {[theta-rotation (exp (* i theta))]
         [theta-unrotation (make-rectangular (real-part theta-rotation)
                                             (- (imag-part theta-rotation)))]
         [f-rotation (λ (p) (* theta-rotation p))]
         [f-unrotation (λ (p) (* theta-unrotation p))]
         [ne (f-rotation w)]
         [sw (f-rotation (* i (- h)))]
         [se (f-rotation (make-rectangular w (- h)))]
         [nw 0]
         [pts (list ne sw se nw)]
         [longitudes (map real-part pts)]
         [latitudes  (map imag-part pts)]
         [east (apply max longitudes)]
         [west (apply min longitudes)]
         [nrth (apply max latitudes)]
         [sth  (apply min latitudes)]
         [new-w (round/e (- east west))]
         [new-h (round/e (- nrth sth))]}
    (values (build-bmbytes new-w
                           new-h
                           (λ (x y)
                             (let* {[pre-image (f-unrotation (make-rectangular (+ west x 1/2) (- nrth y 1/2)))]}
                               (interpolate bmbytes w h
                                            (real-part pre-image)
                                            (- (imag-part pre-image))))))
            new-w
            new-h)))

;; Why the offsets of 1/2 in `rotate-bytes` and `interpolate`?
;; We consider a pixel's RGB as a point-sample taken from the 'true' image,
;; where the RGB is the sample at the *center* of the square covered by the pixel.
;; (When we assume the sample had been from the NW corner instead of the center,
;; we got weird artifacts upon rotation:
;; Consider a 1x1 bitmap rotated by 90 degrees.
;; The NW corner of our new value would be derived from the *NE* corner of
;; the original bitmap, which is a full pixel-width away from the original sample.
;; So a 1x1 bitmap being rotated would counterintuitively give a different bitmap.)



; interpolate: bytes natnum natum real real -> bytes
;
; Given a bitmap (bytes of size (* w h NUM-CHANNELS)), return a pixel (bytes of size NUM-CHANNELS)
; corresponding to the interpolated color at x,y
; where x,y are *real-valued* coordinates in [0,w), [0,h).
;
(define (interpolate bmbytes w h x y)
  (let* {[x0 (floor/e (- x 1/2))]
         [y0 (floor/e (- y 1/2))]
         [dx (- (- x 1/2) x0)]
         [dy (- (- y 1/2) y0)]
         [1-dx (- 1 dx)]
         [1-dy (- 1 dy)]
         [nw (bmbytes-ref/safe bmbytes w h       x0        y0 )]
         [ne (bmbytes-ref/safe bmbytes w h (add1 x0)       y0 )]
         [sw (bmbytes-ref/safe bmbytes w h       x0  (add1 y0))]
         [se (bmbytes-ref/safe bmbytes w h (add1 x0) (add1 y0))]
         }
    (build-bytes 
     NUM-CHANNELS
     (λ (i) (inexact->exact (round/e (+ (* (bytes-ref nw i) 1-dx 1-dy)
                                        (* (bytes-ref ne i) dx   1-dy)
                                        (* (bytes-ref sw i) 1-dx dy)
                                        (* (bytes-ref se i) dx   dy))))))))
    




; Return pixel (i,j) from a bytes representation.
; However, if i,j refers to an off-board location,
; return the nearest board location where alpha has been set to 0.
; (That is, conceptually extend the picture's edge colors
; infinitely, but make them transparent.)
; This is helpful when trying to interpolate just beyond
; an edge pixel.
;
(define (bmbytes-ref/safe bytes w h i j)
  (let* {[i/safe (clamp-1 0 i w)]
         [j/safe (clamp-1 0 j h)]
         [offset (flatten-bm-coord w h i/safe j/safe)]
         [pixel (subbytes bytes offset (+ offset NUM-CHANNELS))]
         }
    (if (and (= i i/safe) (= j j/safe))
        pixel
        (let {[new-pixel (bytes-copy pixel)]}
          (begin (bytes-set! new-pixel 0 0)
                 new-pixel)))))

; Create a bytes representation from
; a function f mapping locations to pixels.
;
; f : [0,w), [0,h) -> (bytes a r g b)
;
(define (build-bmbytes w h f)
  (do {[bm (make-bytes (* NUM-CHANNELS w h))]
       [y 0 (add1 y)]
       }
    [(>= y h) bm]
    (do {[x 0 (add1 x)]
         }
      [(>= x w)]
      (bytes-copy! bm (flatten-bm-coord w h x y) (f x y)))))

; build-bytes, analogous to build-list.
;
(define (build-bytes sz f)
  (do {[b (make-bytes sz)]
       [i 0 (add1 i)]
       }
    [(>= i sz) b]
    (bytes-set! b i (f i))))

;;;; Some utility functions


(define round/e (compose inexact->exact round))
(define floor/e (compose inexact->exact floor))
(define ceiling/e (compose inexact->exact ceiling))

(define NUM-CHANNELS 4) ; alpha, r, g, b

; Return n, clamped to the range [a,b).
; (note the open interval; for integers.)
;
(define (clamp-1 a n b)
  (min (max a n) (sub1 b)))


; Convert an x,y pixel coordinate into its offset into a bytes.
;
(define (flatten-bm-coord w h x y) (* (+ (* y w) x) NUM-CHANNELS))

