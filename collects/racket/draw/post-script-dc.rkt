#lang scheme/base
(require scheme/class
         scheme/file
         racket/path
         racket/math
         mred/private/syntax
         racket/gui/dynamic
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

    (define-values (s width height landscape?)
      (let ([su (if interactive
                    ((gui-dynamic-require 'get-ps-setup-from-user))
                    (current-ps-setup))])
        (cond
         [su 
          (unless (eq? su (current-ps-setup))
            (send (current-ps-setup) copy-from su))
          (let* ([pss (current-ps-setup)]
                 [to-file? (eq? (send pss get-mode) 'file)]
                 [get-file (lambda (fn)
                             ((gui-dynamic-require 'put-file)
                              "Save PostScript As"
                              #f
                              (and fn (path-only fn))
                              (and fn (file-name-from-path fn))
                              "ps"))]
                 [fn (if to-file?
                         (if interactive
                             (get-file (send pss get-file))
                             (let ([fn (send pss get-file)])
                               (or fn (get-file #f))))
                         #f)])
            (if (and to-file?
                     (not fn))
                (values #f #f #f #f)
                (let* ([paper (assoc (send pss get-paper-name) paper-sizes)]
                       [w (cadr paper)]
                       [h (caddr paper)]
                       [landscape? (eq? (send pss get-orientation) 'landscape)])
                  (values
                   (cairo_ps_surface_create (or fn
                                                (make-temporary-file "draw~a.ps"))
                                            w
                                            h)
                   w
                   h
                   landscape?))))]
         [else
          (values #f #f #f #f)])))

    (define-values (margin-x margin-y)
      (let ([xb (box 0)] [yb (box 0.0)])
        (send (current-ps-setup) get-margin xb yb)
        (values (unbox xb) (unbox yb))))
    (define-values (scale-x scale-y)
      (let ([xb (box 0)] [yb (box 0.0)])
        (send (current-ps-setup) get-scaling xb yb)
        (values (unbox xb) (unbox yb))))
    (define-values (trans-x trans-y)
      (let ([xb (box 0)] [yb (box 0.0)])
        (send (current-ps-setup) get-translation xb yb)
        (values (unbox xb) (unbox yb))))

    (when (and s as-eps)
      (cairo_ps_surface_set_eps s #t))
    (when (and s landscape?)
      (cairo_ps_surface_dsc_comment s "%%Orientation: Landscape"))

    (define c (and s (cairo_create s)))
    
    (when s (cairo_surface_destroy s))

    (define/override (get-cr) c)

    (def/override (get-size)
      (let ([w (exact->inexact (/ (- width margin-x margin-x) scale-x))]
            [h (exact->inexact (/ (- height margin-y margin-y) scale-y))])
        (if landscape?
            (values h w)
            (values w h))))

    (define/override (end-cr)
      (cairo_surface_finish s)
      (cairo_destroy c)
      (set! c #f)
      (set! s #f))

    (define/override (init-cr-matrix c)
      (cairo_translate c trans-x trans-y)
      (if landscape?
          (begin
            (cairo_translate c 0 height)
            (cairo_rotate c (/ pi -2))
            (cairo_translate c margin-y margin-x)
            (cairo_scale c scale-y scale-x))
          (begin
            (cairo_translate c margin-x margin-y)
            (cairo_scale c scale-x scale-y))))

    (define/override (get-pango font)
      (send font get-ps-pango))

    (define/override (get-font-metrics-key sx sy)
      (if (and (= sx 1.0) (= sy 1.0))
          2
          0))

    (define/override (can-combine-text? sz)
      #t)

    (super-new)))

(define post-script-dc% (dc-mixin dc-backend%))
