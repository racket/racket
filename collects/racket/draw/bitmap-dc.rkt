#lang scheme/base
(require scheme/class
         mred/private/syntax
         "cairo.ss"
         "color.ss"
         "bitmap.ss"
	 "dc.ss"
         "local.ss")

(provide bitmap-dc%)

(define dc-backend%
  (class default-dc-backend%
    (init [_bm #f])
    (inherit reset-cr)

    (define c #f)
    (define bm #f)
    (define b&w? #f)
    
    (when _bm
      (do-set-bitmap _bm))

    (define/private (do-set-bitmap v)
      (when c
        (cairo_destroy c)
        (set! c #f))
      (set! bm v)
      (when (and bm (send bm ok?))
        (set! c (cairo_create (send bm get-cairo-surface)))
        (set! b&w? (not (send bm is-color?)))))

    (def/public (set-bitmap [(make-or-false bitmap%) v])
      (do-set-bitmap v)
      (reset-cr))

    (def/public (get-bitmap) bm)

    (def/override (get-size)
      (values (exact->inexact (send bm get-width))
              (exact->inexact (send bm get-height))))

    (def/public (set-pixel [real? x][real? y][color% c])
      (let ([s (bytes 255 (color-red c) (color-green c) (color-blue c))])
        (set-argb-pixels x y 1 1 s)))

    (def/public (get-pixel [real? x][real? y][color% c])
      (let ([b (make-bytes 4)])
        (get-argb-pixels x y 1 1 b)
        (send c set (bytes-ref b 1) (bytes-ref b 2) (bytes-ref b 3))
        #t))

    (def/public (set-argb-pixels [exact-nonnegative-integer? x]
                                 [exact-nonnegative-integer? y]
                                 [exact-nonnegative-integer? w]
                                 [exact-nonnegative-integer? h]
                                 [bytes? bstr]
                                 [any? [set-alpha? #f]])
      (when bm
        (send bm set-argb-pixels x y w h bstr set-alpha?)))

    (def/public (get-argb-pixels [exact-nonnegative-integer? x]
                                 [exact-nonnegative-integer? y]
                                 [exact-nonnegative-integer? w]
                                 [exact-nonnegative-integer? h]
                                 [bytes? bstr]
                                 [any? [get-alpha? #f]])
      (when bm
        (send bm get-argb-pixels x y w h bstr get-alpha?)))

    (define/override (get-cr) c)

    (define/override (end-cr) (void))

    (define/override (dc-adjust-smoothing s) 
      (if b&w?
          'unsmoothed
          s))

    (define/override (install-color cr c a)
      (if b&w?
          (begin
            (cairo_set_operator cr CAIRO_OPERATOR_SOURCE)
            (if (zero? a)
                (super install-color cr c a)
                (if (and (= (color-red c) 255)
                         (= (color-green c) 255)
                         (= (color-blue c) 255))
                    (cairo_set_source_rgba cr 1.0 1.0 1.0 0.0)
                    (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0))))
          (super install-color cr c a)))

    (define/override (collapse-bitmap-b&w?) b&w?)

    (super-new)))

(define bitmap-dc%
  (dc-mixin dc-backend%))

(install-bitmap-dc-class! bitmap-dc%)
