#lang scheme/base
(require scheme/class
         mred/private/syntax
         "cairo.ss"
         "color.ss"
         "bitmap.ss"
	 "dc.ss"
         "local.ss")

(provide bitmap-dc%
         bitmap-dc-backend%)

(define bitmap-dc-backend%
  (class default-dc-backend%
    (init [_bm #f])
    (inherit reset-cr
             call-with-cr-lock)

    (define c #f)
    (define bm #f)
    (define b&w? #f)
    
    (when _bm
      (do-set-bitmap _bm #f))

    (define/private (do-set-bitmap v reset?)
      (when c
        (cairo_destroy c)
        (set! c #f))
      (set! bm v)
      (when (and bm (send bm ok?))
        (set! c (cairo_create (send bm get-cairo-surface)))
        (set! b&w? (not (send bm is-color?)))))

    (define/public (internal-set-bitmap v)
      (call-with-cr-lock
       (lambda ()
         (do-set-bitmap v #t)
         (when c (reset-cr c)))))

    (define/public (internal-get-bitmap) bm)

    (def/override (get-size)
      (let ([bm bm])
        (values (exact->inexact (send bm get-width))
                (exact->inexact (send bm get-height)))))

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

(define black (send the-color-database find-color "black"))

(define bitmap-dc%
  (class (dc-mixin bitmap-dc-backend%)
    (inherit draw-bitmap-section
             internal-set-bitmap
             internal-get-bitmap)
    
    (super-new)

    (def/public (set-bitmap [(make-or-false bitmap%) v])
      (internal-set-bitmap v))

    (def/public (get-bitmap)
      (internal-get-bitmap))
    
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
      (let ([bm (internal-get-bitmap)])
        (when bm
          (send bm set-argb-pixels x y w h bstr set-alpha?))))

    (def/public (get-argb-pixels [exact-nonnegative-integer? x]
                                 [exact-nonnegative-integer? y]
                                 [exact-nonnegative-integer? w]
                                 [exact-nonnegative-integer? h]
                                 [bytes? bstr]
                                 [any? [get-alpha? #f]])
      (let ([bm (internal-get-bitmap)])
        (when bm
          (send bm get-argb-pixels x y w h bstr get-alpha?))))

    (def/public (draw-bitmap-section-smooth [bitmap% src]
                                            [real? dest-x]
                                            [real? dest-y]
                                            [real? src-x]
                                            [real? src-y]
                                            [real? src-w]
                                            [real? src-h]
                                            [(symbol-in solid opaque xor) [style 'solid]]
                                            [(make-or-false color%) [color black]]
                                            [(make-or-false bitmap%) [mask #f]])
      (draw-bitmap-section src dest-x dest-y src-x src-y src-w src-h style color mask))))

(install-bitmap-dc-class! bitmap-dc%)
