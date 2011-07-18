#lang racket/base
(require (for-syntax racket/base)
         racket/unsafe/ops
         racket/contract
         racket/class
         racket/draw
         racket/pretty
         racket/math
         unstable/future
         slideshow/pict)

;; TODO: use clipping regions to avoid computing unused pixels

(define nneg-real/c (and/c real? (not/c negative?)))

(provide/contract
 [blur
  (->* (pict? nneg-real/c)
       (nneg-real/c)
       pict?)]
 [shadow
  (->* (pict? nneg-real/c)
       (real? real?
        #:color (or/c #f string? (is-a?/c color%))
        #:shadow-color (or/c #f string? (is-a?/c color%)))
       pict?)]
 [blur-bitmap!
  (->* ((is-a?/c bitmap%) exact-nonnegative-integer?)
       (exact-nonnegative-integer?)
       void?)])

;; ----

(define (blur p hbr [vbr hbr] #:auto-inset? [auto-inset? #f])
  (let ([blurred (*blur (inset p hbr vbr) hbr vbr)])
    (cond [auto-inset? blurred]
          [else (inset blurred (- hbr) (- vbr))])))

(define (shadow p br [dx 0] [dy dx]
                #:color [c #f]
                #:shadow-color [shc #f]
                #:auto-inset? [auto-inset? #f])
  ;; FIXME: should auto-inset also use dx, dy?
  (define (colorize* p c)
    (if c (colorize p c) p))
  (let ([result
         (pin-under (colorize* p c)
                    dx dy
                    (blur (colorize* p shc) br))])
    (cond [auto-inset? (inset result br)]
          [else result])))

(define (blur-bitmap! bmp hbr [vbr hbr])
  (let ([h-cvec (gaussian-cvec hbr)]
        [v-cvec (gaussian-cvec vbr)])
    (blur! bmp h-cvec v-cvec)))

;; ----

(define MAX-RADIUS (expt 2 10))
(define MAX-WEIGHT (expt 2 5))

(define (*blur p hbr vbr)
  (let* ([w (pict-width p)]
         [h (pict-height p)]
         [drawer (make-pict-drawer p)])
    (dc (lambda (dc x y)
          (let-values ([(sx sy) (send dc get-scale)])
            (let* ([pxw (ceil/e (* w sx))]
                   [pxh (ceil/e (* h sy))]
                   [h-cvec (gaussian-cvec (min (ceil/e (* hbr sx)) pxw MAX-RADIUS))]
                   [v-cvec (gaussian-cvec (min (ceil/e (* vbr sy)) pxh MAX-RADIUS))]
                   [bmp (make-object bitmap% pxw pxh #f #t)]
                   [bdc (new bitmap-dc% (bitmap bmp))])
              (send bdc set-scale sx sy)
              (send bdc set-font (send dc get-font))
              (send bdc set-pen (send dc get-pen))
              (send bdc set-brush (send dc get-brush))
              (send bdc set-text-foreground (send dc get-text-foreground))
              (drawer bdc 0 0)
              (blur! bmp h-cvec v-cvec)
              (send dc set-scale 1.0 1.0)
              (send dc draw-bitmap bmp (* x sx) (* y sy))
              (send dc set-scale sx sy))))
        w h)))

(define (blur! bmp h-cvec v-cvec)
  (let* ([w (send bmp get-width)]
         [h (send bmp get-height)]
         [pix (make-bytes (* w h 4))]
         [out (make-bytes (* w h 4))])
    (send bmp get-argb-pixels 0 0 w h pix #f #t)
    (convolve-h pix out h-cvec w h)
    (convolve-v out pix v-cvec w h)
    (send bmp set-argb-pixels 0 0 w h pix #f #t)
    (void)))

;; ----

(define (ceil/e x) (inexact->exact (ceiling x)))
(define (round/e x) (inexact->exact (round x)))

(define (convolve-line* cvec start end get-val set-val)
  (define CVEC-LEN (vector-length cvec))
  (define CVEC-HALF (unsafe-fxquotient CVEC-LEN 2))
  (let ([non-zero-alpha?
         (for/or ([outi (in-range start end)])
           (positive? (get-val outi 0)))])
    (cond [non-zero-alpha?
           (for ([outi (in-range start end)])
             (define lo-ci (unsafe-fx+ start (unsafe-fx- CVEC-HALF outi)))
             (define hi-ci (unsafe-fx+ end (unsafe-fx- CVEC-HALF outi)))
             (define-values (na nr ng nb sumw)
               (for/fold ([na 0] [nr 0] [ng 0] [nb 0] [sumw 0])
                   ([ci (in-range (unsafe-fxmax 0 lo-ci) (unsafe-fxmin hi-ci CVEC-LEN))])
                 (let ([ini (unsafe-fx+ outi (unsafe-fx- ci CVEC-HALF))]
                       [w (unsafe-vector-ref cvec ci)])
                   (values (unsafe-fx+ na (unsafe-fx* w (get-val ini 0)))
                           (unsafe-fx+ nr (unsafe-fx* w (get-val ini 1)))
                           (unsafe-fx+ ng (unsafe-fx* w (get-val ini 2)))
                           (unsafe-fx+ nb (unsafe-fx* w (get-val ini 3)))
                           (unsafe-fx+ sumw w)))))
             (set-val outi 0 (unsafe-fxquotient na sumw))
             (set-val outi 1 (unsafe-fxquotient nr sumw))
             (set-val outi 2 (unsafe-fxquotient ng sumw))
             (set-val outi 3 (unsafe-fxquotient nb sumw)))]
          [else
           (for ([outi (in-range start end)])
             (set-val outi 0 0)
             (set-val outi 1 0)
             (set-val outi 2 0)
             (set-val outi 3 0))])))

(define (convolve-h in out cvec w h)
  (define (get-val i offset) (bytes-ref in (unsafe-fx+ offset (unsafe-fx* 4 i))))
  (define (set-val i offset v) (bytes-set! out (unsafe-fx+ offset (unsafe-fx* 4 i)) v))
  (for/async ([row (in-range h)])
    (convolve-line* cvec (unsafe-fx* row w) (unsafe-fx* (add1 row) w) get-val set-val)))

(define (convolve-v in out cvec w h)
  (define ((mkget col) i offset)
    (bytes-ref in (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset)))
  (define ((mkset col) i offset v)
    (bytes-set! out (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset) v))
  (for/async ([col (in-range w)])
    (convolve-line* cvec 0 h (mkget col) (mkset col))))
 
(define (gaussian-cvec radius [bias 'none]) ;; .84
  (define sigma 1)
  (define (G x)
    (/ (exp (- (/ (sqr x) (* 2 sigma sigma))))
       (sqrt (* 2 pi sigma sigma))))
  (cond [(zero? radius)
         (vector 1)]
        [else
         (build-vector
          (+ 1 radius radius)
          (lambda (x)
            (cond [(and (< x radius) (eq? bias 'left))
                   0]
                  [(and (> x radius) (eq? bias 'right))
                   0]
                  [else
                   (ceil/e (* MAX-WEIGHT (G (/ (* 3 sigma (- x radius)) radius))))])))]))

#|
(define (linear-cvec w0 hi lo)
  (let ([hw (quotient w0 2)])
    (build-vector (+ 1 hw hw)
                  (lambda (x)
                    (ceil/e (+ lo (* (- 1 (/ (abs (- x hw)) hw)) (- hi lo))))))))
|#

;; ----

;; used for benchmarking to force effectively lazy dc pict constructor
(define (p->bmp p)
  (let* ([bmp (make-object bitmap% (ceil/e (pict-width p)) (ceil/e (pict-height p)))]
         [bdc (new bitmap-dc% (bitmap bmp))])
    (draw-pict p bdc 0 0)
    bmp))
