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
;; TODO: tweak parameters so that gaussian and iterated-box modes are closer

(define nneg-real/c (and/c real? (not/c negative?)))

(provide/contract
 [blur
  (->* (pict? nneg-real/c)
       (nneg-real/c
        #:mode (or/c 'gaussian 'iterated-box 'single-box))
       pict?)]
 [shadow
  (->* (pict? nneg-real/c)
       (real? real?
        #:color (or/c #f string? (is-a?/c color%))
        #:shadow-color (or/c #f string? (is-a?/c color%))
        #:mode (or/c 'gaussian 'iterated-box 'single-box))
       pict?)]
 [blur-bitmap!
  (->* ((is-a?/c bitmap%) exact-nonnegative-integer?)
       (exact-nonnegative-integer?
        #:mode (or/c 'gaussian 'iterated-box 'single-box))
       void?)])

;; ----

(define (blur p hbr [vbr hbr] #:mode [mode 'iterated-box] #:auto-inset? [auto-inset? #f])
  (let ([blurred (*blur (inset p hbr vbr) hbr vbr mode)])
    (cond [auto-inset? blurred]
          [else (inset blurred (- hbr) (- vbr))])))

(define (shadow p br [dx 0] [dy dx]
                #:color [c #f]
                #:shadow-color [shc #f]
                #:mode [mode 'iterated-box]
                #:auto-inset? [auto-inset? #f])
  ;; FIXME: should auto-inset also use dx, dy?
  (define (colorize* p c)
    (if c (colorize p c) p))
  (let ([result
         (pin-under (colorize* p c)
                    dx dy
                    (blur (colorize* p shc) br #:mode mode))])
    (cond [auto-inset? (inset result br)]
          [else result])))

(define (blur-bitmap! bmp hbr [vbr hbr] #:mode [mode 'iterated-box])
  (blur! bmp hbr vbr mode))

;; ----

(define MAX-RADIUS (expt 2 10))
(define MAX-WEIGHT (expt 2 5))
(define BOX-ITERATIONS 3)

(define (*blur p hbr vbr mode)
  (let* ([w (pict-width p)]
         [h (pict-height p)]
         [drawer (make-pict-drawer p)])
    (dc (lambda (dc x y)
          (let-values ([(sx sy) (send dc get-scale)])
            (let* ([pxw (ceil/e (* w sx))]
                   [pxh (ceil/e (* h sy))]
                   [hbr* (min (ceil/e (* hbr sx)) pxw MAX-RADIUS)]
                   [vbr* (min (ceil/e (* vbr sy)) pxh MAX-RADIUS)]
                   [bmp (make-object bitmap% pxw pxh #f #t)]
                   [bdc (new bitmap-dc% (bitmap bmp))])
              (send bdc set-scale sx sy)
              (send bdc set-font (send dc get-font))
              (send bdc set-pen (send dc get-pen))
              (send bdc set-brush (send dc get-brush))
              (send bdc set-text-foreground (send dc get-text-foreground))
              (drawer bdc 0 0)
              (blur! bmp hbr* vbr* mode)
              (send dc set-scale 1.0 1.0)
              (send dc draw-bitmap bmp (* x sx) (* y sy))
              (send dc set-scale sx sy))))
        w h)))

(define (blur! bmp hbr vbr mode)
  (let* ([w (send bmp get-width)]
         [h (send bmp get-height)]
         [pix (make-bytes (* w h 4))]
         [out (make-bytes (* w h 4))])
    (send bmp get-argb-pixels 0 0 w h pix #f #t)
    (case mode
      ((gaussian)
       (let* ([h-cvec (gaussian-cvec hbr)]
              [v-cvec (gaussian-cvec vbr)])
         (convolve-h pix out h-cvec w h)
         (convolve-v out pix v-cvec w h)))
      ((single-box)
       (box-h pix out hbr w h 1)
       (box-v out pix vbr w h 1))
      ((iterated-box)
       (let ([hbr (ceil/e (/ hbr BOX-ITERATIONS))]
             [vbr (ceil/e (/ vbr BOX-ITERATIONS))])
         (box-h pix out hbr w h BOX-ITERATIONS)
         (let-values ([(pix* out*)
                       (cond [(even? BOX-ITERATIONS) (values out pix)]
                             [else (values pix out)])])
           (box-v pix* out* vbr w h BOX-ITERATIONS))))
      (else (error 'blur! "bad mode")))
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

;; ----

;; iterated box blur

(define (box-line* radius start end get-val set-val)
  (let ([non-zero-alpha?
         (for/or ([outi (in-range start end)])
           (positive? (get-val outi 0)))])
    (cond [non-zero-alpha?
           (for/fold ([wA 0] [wR 0] [wG 0] [wB 0] [wW 0])
             ([leadI (in-range start (+ end radius))])
             ;; (eprintf "leadI = ~s, wA = ~s, wW = ~s\n" leadI wA wW)
             (let*-values ([(outI) (unsafe-fx- leadI radius)]
                           [(tailI) (unsafe-fx- leadI (unsafe-fx+ radius radius))]
                           [(addA addR addG addB addW)
                            (cond [(unsafe-fx< leadI end)
                                   (values (get-val leadI 0)
                                           (get-val leadI 1)
                                           (get-val leadI 2)
                                           (get-val leadI 3)
                                           1)]
                                  [else (values 0 0 0 0 0)])]
                           [(dropA dropR dropG dropB dropW)
                            (cond [(unsafe-fx>= tailI start)
                                   (values (get-val tailI 0)
                                           (get-val tailI 1)
                                           (get-val tailI 2)
                                           (get-val tailI 3)
                                           1)]
                                  [else (values 0 0 0 0 0)])]
                           [(nwA) (unsafe-fx+ wA addA)]
                           [(nwR) (unsafe-fx+ wR addR)]
                           [(nwG) (unsafe-fx+ wG addG)]
                           [(nwB) (unsafe-fx+ wB addB)]
                           [(nwW) (unsafe-fx+ wW addW)])
               (when (and (unsafe-fx>= outI start) (unsafe-fx< outI end))
                 ;; (eprintf "setting ~a = (~a,...)\n" outI (quotient nwA nwW))
                 (set-val outI 0 (unsafe-fxquotient nwA nwW))
                 (set-val outI 1 (unsafe-fxquotient nwR nwW))
                 (set-val outI 2 (unsafe-fxquotient nwG nwW))
                 (set-val outI 3 (unsafe-fxquotient nwB nwW)))
               (values (unsafe-fx- nwA dropA)
                       (unsafe-fx- nwR dropR)
                       (unsafe-fx- nwG dropG)
                       (unsafe-fx- nwB dropB)
                       (unsafe-fx- nwW dropW))))]
          [else
           (for ([outI (in-range start end)])
             (set-val outI 0 0)
             (set-val outI 1 0)
             (set-val outI 2 0)
             (set-val outI 3 0))])))

(define (box-h in out radius w h iterations)
  (define (get-val i offset) (bytes-ref in (unsafe-fx+ offset (unsafe-fx* 4 i))))
  (define (set-val i offset v) (bytes-set! out (unsafe-fx+ offset (unsafe-fx* 4 i)) v))
  (define (get-val* i offset) (bytes-ref out (unsafe-fx+ offset (unsafe-fx* 4 i))))
  (define (set-val* i offset v) (bytes-set! in (unsafe-fx+ offset (unsafe-fx* 4 i)) v))
  (for/async ([row (in-range h)])
    (for ([iter (in-range iterations)])
      (box-line* radius (* row w) (* (add1 row) w) 
                 (if (even? iter) get-val get-val*)
                 (if (even? iter) set-val set-val*)))))

(define (box-v in out radius w h iterations)
  (define ((mkget col) i offset)
    (bytes-ref in (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset)))
  (define ((mkset col) i offset v)
    (bytes-set! out (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset) v))
  (define ((mkget* col) i offset)
    (bytes-ref out (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset)))
  (define ((mkset* col) i offset v)
    (bytes-set! in (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset) v))
  (for/async ([col (in-range w)])
    (let ([get (mkget col)]
          [get* (mkget* col)]
          [set (mkset col)]
          [set* (mkset* col)])
      (for ([iter (in-range iterations)])
        (box-line* radius 0 h 
                   (if (even? iter) get get*)
                   (if (even? iter) set set*))))))

;; ----

;; used for benchmarking to force effectively lazy dc pict constructor
(define (p->bmp p)
  (let* ([bmp (make-object bitmap% (ceil/e (pict-width p)) (ceil/e (pict-height p)))]
         [bdc (new bitmap-dc% (bitmap bmp))])
    (draw-pict p bdc 0 0)
    bmp))
