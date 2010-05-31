#lang scheme/base
(require scheme/class
         scheme/file
         mred/private/syntax
         "cairo.ss"
	 "dc.ss"
         "font.ss"
         "local.ss"
         "ps-setup.ss")

(provide post-script-dc%)

(define dc-backend%
  (class default-dc-backend%
    (init [interactive #t]
          [parent #f]
          [use-paper-bbox #f]
          [as-eps #t])

    (define-values (margin-x margin-y)
      (let ([xb (box 0)] [yb (box 0.0)])
        (send (current-ps-setup) get-margin xb yb)
        (values (unbox xb) (unbox yb))))
    (define-values (scale-x scale-y)
      (let ([xb (box 0)] [yb (box 0.0)])
        (send (current-ps-setup) get-scaling xb yb)
        (values (unbox xb) (unbox yb))))

    (define-values (s width height)
      (let* ([pss (current-ps-setup)]
             [paper (assoc (send pss get-paper-name) paper-sizes)])
        (values
         (cairo_ps_surface_create (or (send pss get-filename)
                                      (make-temporary-file "draw~a.ps"))
                                  (cadr paper)
                                  (caddr paper))
         (cadr paper)
         (caddr paper))))
    (when as-eps
      (cairo_ps_surface_set_eps s #t))

    (define c (cairo_create s))
    (cairo_surface_destroy s)

    (init-cr-matrix)

    (define/override (get-cr) c)

    (def/override (get-size)
      (values (exact->inexact width)
              (exact->inexact height)))

    (define/override (end-cr)
      (cairo_surface_finish s)
      (cairo_destroy c)
      (set! c #f)
      (set! s #f))

    (define/override (init-cr-matrix)
      (cairo_translate c margin-x margin-y)
      (cairo_scale c scale-x scale-y))

    (define/override (get-pango font)
      (send font get-ps-pango))

    (define/override (get-font-metrics-key sx sy)
      (if (and (= sx 1.0) (= sy 1.0))
          2
          0))

    (super-new)))

(define post-script-dc%
  (dc-mixin dc-backend%))
