#lang racket/base
(require racket/class
         ffi/unsafe/atomic
         "syntax.rkt"
         "../unsafe/cairo.rkt"
         "color.rkt"
         "bitmap.rkt"
	 "dc.rkt"
         "local.rkt")

(provide bitmap-dc%
         bitmap-dc-backend%)

(define bitmap-dc-backend%
  (class default-dc-backend%
    (init [(_bm bitmap) #f])
    (inherit reset-cr)

    (define c #f)
    (define bm #f)
    (define b&w? #f)
    
    (when _bm
      (do-set-bitmap _bm #f)
      ;; Needed if the bitmap has a device scale:
      (when c (init-cr-matrix c)))
    
    (define/override (init-cr-matrix cr)
      (when bm
        (define s (send bm get-cairo-device-scale))
        (unless (= s 1)
          (cairo_scale cr s s)))
      (super init-cr-matrix cr))

    (define/override (ok?) (and c #t))

    (define/private (do-set-bitmap v reset?)
      (when c
        (cairo_destroy c)
        (set! c #f))
      (set! bm v)
      (when (and bm (send bm ok?))
        (set! c (cairo_create (send bm get-cairo-target-surface)))
        (set! b&w? (not (send bm is-color?)))))

    (define/public (internal-set-bitmap v [direct? #f])
      (if direct?
          (do-set-bitmap v #t)
          (call-as-atomic
           (lambda ()
             (do-set-bitmap v #t)
             (when c (reset-cr c))))))

    (define/public (internal-get-bitmap) bm)

    (def/override (get-size)
      (let ([bm bm])
        (values (exact->inexact (send bm get-width))
                (exact->inexact (send bm get-height)))))

    (define/override (get-cr) c)
    (define/override (release-cr cr) (when bm (send bm drop-alpha-s)))

    (define/override (end-cr) (void))

    (define/override (dc-adjust-smoothing s) 
      (if b&w?
          'unsmoothed
          s))

    (define/override (install-color cr c a bg?)
      (if b&w?
          (begin
            (cairo_set_operator cr CAIRO_OPERATOR_SOURCE)
            (if (or (zero? a)
                    (zero? (color-alpha c)))
                (super install-color cr c a bg?)
                (if (if bg?
                        ;; Background: all non-black to white
                        (not (and (= (color-red c) 0)
                                  (= (color-green c) 0)
                                  (= (color-blue c) 0)
                                  (= (color-alpha c) 1.0)))
                        ;; Foreground: all non-white to black:
                        (and (= (color-red c) 255)
                             (= (color-green c) 255)
                             (= (color-blue c) 255)
                             (= (color-alpha c) 1.0)))
                    (cairo_set_source_rgba cr 1.0 1.0 1.0 0.0)
                    (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0))))
          (super install-color cr c a bg?)))

    (define/override (collapse-bitmap-b&w?) b&w?)

    (define/override (get-clear-operator)
      (if (or b&w? (send bm has-alpha-channel?))
          CAIRO_OPERATOR_CLEAR
          CAIRO_OPERATOR_OVER))

    (super-new)))

(define black (send the-color-database find-color "black"))

(define bitmap-dc%
  (class (dc-mixin bitmap-dc-backend%)
    (inherit draw-bitmap-section
             internal-set-bitmap
             internal-get-bitmap
             get-size
             get-transformation
             set-transformation
             get-smoothing
             set-smoothing
             scale
             get-font)
    
    (super-new)

    (def/override (get-gl-context)
      (let ([bm (internal-get-bitmap)])
        (and bm
             (send bm get-bitmap-gl-context))))

    (define/public (set-bitmap v)
      (internal-set-bitmap v))

    (def/public (get-bitmap)
      (internal-get-bitmap))
    
    (define/public (set-pixel x y c)
      (let ([s (bytes (inexact->exact (round (* 255 (color-alpha c))))
                      (color-red c)
                      (color-green c)
                      (color-blue c))])
        (set-argb-pixels x y 1 1 s)))

    (define/public (get-pixel x y c)
      (let-values ([(w h) (get-size)])
        (let ([b (make-bytes 4)])
          (get-argb-pixels x y 1 1 b)
          (send c set (bytes-ref b 1) (bytes-ref b 2) (bytes-ref b 3)
                (/ (bytes-ref b 0) 255))
          (and (<= 0 x w) (<= 0 y h)))))

    (define/public (set-argb-pixels x y w h bstr
                                    [set-alpha? #f]
                                    [pre-mult? #f])
      (let ([bm (internal-get-bitmap)])
        (when bm
          (send bm set-argb-pixels x y w h bstr set-alpha? pre-mult?))))

    (define/public (get-argb-pixels x y w h bstr
                                    [get-alpha? #f]
                                    [pre-mult? #f])
      (let ([bm (internal-get-bitmap)])
        (when bm
          (send bm get-argb-pixels x y w h bstr get-alpha? pre-mult?))))

    (define/public (draw-bitmap-section-smooth src dest-x dest-y
                                               dest-w dest-h
                                               src-x src-y
                                               src-w src-h
                                               [style 'solid]
                                               [color black]
                                               [mask #f])
      (let ([sx (if (zero? src-w) 1.0 (/ dest-w src-w))]
            [sy (if (zero? src-h) 1.0 (/ dest-h src-h))])
        (let ([t (get-transformation)]
              [s (get-smoothing)])
          (scale sx sy)
          (when (eq? s 'unsmoothed) (set-smoothing 'aligned))
          (begin0
           (draw-bitmap-section src (/ dest-x sx) (/ dest-y sy) src-x src-y src-w src-h style color mask)
           (when (eq? s 'unsmoothed) (set-smoothing 'unsmoothed))
           (set-transformation t)))))

    (def/override (get-char-width)
      (if (internal-get-bitmap)
          (super get-char-width)
          (send (get-temp-bitmap-dc) get-char-width)))

    (def/override (get-char-height)
      (if (internal-get-bitmap)
          (super get-char-height)
          (send (get-temp-bitmap-dc) get-char-height)))

    (define temp-dc #f)
    (define/private (get-temp-bitmap-dc)
      (let ([dc (or (and temp-dc (weak-box-value temp-dc))
                    (let ([dc (make-object bitmap-dc% (make-object bitmap% 1 1))])
                      (set! temp-dc (make-weak-box dc))
                      dc))])
        (send dc set-font (get-font))
        dc))))

(install-bitmap-dc-class! bitmap-dc%)
