#lang racket/base
(require (for-syntax racket/base)
         racket/unsafe/ops
         racket/contract/base
         racket/class
         racket/draw
         unstable/future
         slideshow/pict)

(define nneg-real/c (and/c real? (not/c negative?)))

(provide/contract
 [blur
  (->* (pict? nneg-real/c)
       (nneg-real/c
        #:pre-inset? any/c)
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

(define (blur p hbr [vbr hbr]
              #:pre-inset? [pre-inset? #t])
  (let* ([p
          (cond [pre-inset? (inset p hbr vbr)]
                [else p])]
         [blurred (*blur p hbr vbr)])
    (cond [pre-inset? (inset blurred (- hbr) (- vbr))]
          [else blurred])))

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
  (blur! bmp hbr vbr))

;; ----

(define MAX-RADIUS (expt 2 10))
(define MAX-WEIGHT (expt 2 5))
(define BOX-ITERATIONS 3)

(define (*blur p hbr vbr)
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
              (when (or (zero? hbr*) (zero? vbr*))
                ;; probably not worth smoothing when about to blur
                ;; except when blurring by zero
                (send bdc set-smoothing (send dc get-smoothing)))
              (drawer bdc 0 0)
              (blur! bmp hbr* vbr*)
              (send dc set-scale 1.0 1.0)
              (send dc draw-bitmap bmp (* x sx) (* y sy))
              (send dc set-scale sx sy))))
        w h)))

(define (blur! bmp hbr vbr)
  (let* ([w (send bmp get-width)]
         [h (send bmp get-height)]
         [pix (make-bytes (* w h 4))]
         [out (make-bytes (* w h 4))])
    (send bmp get-argb-pixels 0 0 w h pix #f #t)
    (let ([hbr (ceil/e (/ hbr BOX-ITERATIONS))]
          [vbr (ceil/e (/ vbr BOX-ITERATIONS))])
      (box-h pix out hbr w h BOX-ITERATIONS)
      (let-values ([(pix* out*)
                    (cond [(even? BOX-ITERATIONS) (values out pix)]
                          [else (values pix out)])])
        (box-v pix* out* vbr w h BOX-ITERATIONS)))
    (send bmp set-argb-pixels 0 0 w h pix #f #t)
    (void)))

;; ----

;; iterated box blur

(define-syntax-rule (box-line* radius start end get-val set-val)
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
  (for/async ([row (in-range h)])
    (for ([iter (in-range iterations)])
      (let ([start (* row w)]
            [end (* (add1 row) w)]
            [in (if (even? iter) in out)]
            [out (if (even? iter) out in)])
        (define-syntax-rule (get-val i offset)
          (bytes-ref in (unsafe-fx+ offset (unsafe-fx* 4 i))))
        (define-syntax-rule (set-val i offset v)
          (bytes-set! out (unsafe-fx+ offset (unsafe-fx* 4 i)) v))
        (box-line* radius start end get-val set-val)))))

(define (box-v in out radius w h iterations)
  (for/async ([col (in-range w)])
    (for ([iter (in-range iterations)])
      (let ([start 0]
            [end h]
            [in (if (even? iter) in out)]
            [out (if (even? iter) out in)])
        (define-syntax-rule (get-val i offset)
          (bytes-ref in (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset)))
        (define-syntax-rule (set-val i offset v)
          (bytes-set! out (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset) v))
        (box-line* radius start end get-val set-val)))))

(define (ceil/e x) (inexact->exact (ceiling x)))

;; ----

;; used for benchmarking to force effectively lazy dc pict constructor
(define (p->bmp p)
  (let* ([bmp (make-object bitmap% (ceil/e (pict-width p)) (ceil/e (pict-height p)))]
         [bdc (new bitmap-dc% (bitmap bmp))])
    (draw-pict p bdc 0 0)
    bmp))
