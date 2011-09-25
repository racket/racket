#lang racket/base
(require racket/class
         racket/file
         racket/path
         racket/math
         "syntax.rkt"
         racket/gui/dynamic
         "../unsafe/cairo.rkt"
         "dc.rkt"
         "local.rkt"
         "ps-setup.rkt"
         "page-dc.rkt"
         "write-bytes.rkt")

(provide post-script-dc%
         pdf-dc%)

(define (make-dc-backend pdf?)
  (class default-dc-backend%
    (init [interactive #t]
          [parent #f]
          [use-paper-bbox #f]
          [as-eps #t]
          [(init-w width) #f]
          [(init-h height) #f]
          [output #f])

    (let ([get-name (lambda ()
                      (init-name (if pdf? 'pdf-dc% 'post-script-dc%)))])
      (unless (or (not init-w) 
                  (and (real? init-w) (not (negative? init-w))))
        (raise-type-error (get-name) "nonnegative real or #f" init-w))
      (unless (or (not init-h) 
                  (and (real? init-h) (not (negative? init-h))))
        (raise-type-error (get-name) "nonnegative real or #f" init-h))
      (unless (or (not output)
                  (path-string? output)
                  (output-port? output))
        (raise-type-error (get-name) "path string, output port, or #f" output)))

    (define-values (s port close-port? writer width height landscape?)
      (let ([su (if interactive
                    ((gui-dynamic-require 'get-ps-setup-from-user) #f parent)
                    (current-ps-setup))])
        (cond
         [su 
          (unless (eq? su (current-ps-setup))
            (send (current-ps-setup) copy-from su))
          (let* ([pss (current-ps-setup)]
                 [to-file? (eq? (send pss get-mode) 'file)]
                 [get-file (lambda (fn)
                             ((gui-dynamic-require 'put-file)
                              (if pdf?
                                  "Save PDF As"
                                  "Save PostScript As")
                              parent
                              (and fn (path-only fn))
                              (and fn (file-name-from-path fn))
                              (if pdf? "pdf" "ps")))]
                 [fn (if to-file?
                         (or output
                             (if interactive
                                 (get-file (send pss get-file))
                                 (let ([fn (send pss get-file)])
                                   (or fn (get-file #f)))))
                         #f)])
            (if (and to-file?
                     (not fn))
                (values #f #f #f #f #f #f #f)
                (let* ([paper (assoc (send pss get-paper-name) paper-sizes)]
                       [w (ceiling
                           (if (or (not init-w) use-paper-bbox)
                               (cadr paper)
                               init-w))]
                       [h (ceiling
                           (if (or (not init-h) use-paper-bbox)
                               (caddr paper)
                               init-h))]
                       [landscape? (eq? (send pss get-orientation) 'landscape)]
                       [file (if (output-port? fn)
                                 fn
                                 (open-output-file
                                  (or fn (make-temporary-file (if pdf?
                                                                  "draw~a.pdf"
                                                                  "draw~a.ps")))
                                  #:exists 'truncate/replace))])
                  (let-values ([(w h) (if (and pdf? landscape?)
                                          (values h w)
                                          (values w h))]
                               [(writer proc) (make-port-writer file)])
                    (values
                     ((if pdf?
                          cairo_pdf_surface_create_for_stream 
                          cairo_ps_surface_create_for_stream)
                      proc
                      w
                      h)
                     file
                     (not (output-port? fn))
                     writer
                     w
                     h
                     landscape?)))))]
         [else
          (values #f #f #f #f #f #f #f)])))

    (define-values (margin-x margin-y)
      (if as-eps
          (values 0.0 0.0)
          (let ([xb (box 0)] [yb (box 0.0)])
            (send (current-ps-setup) get-margin xb yb)
            (values (unbox xb) (unbox yb)))))
    (define-values (scale-x scale-y)
      (let ([xb (box 0)] [yb (box 0.0)])
        (send (current-ps-setup) get-scaling xb yb)
        (values (unbox xb) (unbox yb))))
    (define-values (trans-x trans-y)
      (if as-eps
          (values 0.0 0.0)
          (let ([xb (box 0)] [yb (box 0.0)])
            (send (current-ps-setup) get-translation xb yb)
            (values (unbox xb) (unbox yb)))))

    (unless pdf?
      (when (and s as-eps)
        (cairo_ps_surface_set_eps s #t))
      (when (and s landscape?)
        (cairo_ps_surface_dsc_comment s "%%Orientation: Landscape")))

    (define c (and s (cairo_create s)))
    
    (when s (cairo_surface_destroy s))

    (define/override (ok?) (and c #t))

    (define/override (get-cr) c)

    (def/override (get-size)
      (let ([w (exact->inexact (/ (- width margin-x margin-x) scale-x))]
            [h (exact->inexact (/ (- height margin-y margin-y) scale-y))])
        (if (and (not pdf?) landscape?)
            (values h w)
            (values w h))))

    (define/override (get-device-scale)
      (values scale-x scale-y))

    (define/override (end-cr)
      (cairo_surface_finish s)
      (cairo_destroy c)
      (set! c #f)
      (set! s #f)
      (port-writer-wait writer)
      (set! writer #f)
      (when close-port?
        (close-output-port port))
      (set! port #f))

    (define/override (init-cr-matrix c)
      (cairo_translate c trans-x trans-y)
      (if (and landscape? (not pdf?))
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
    
    (define/override (can-mask-bitmap?)
      #f)

    (define/override (dc-adjust-cap-shape shape sx pw) shape)
    (define/override (get-hairline-width cx) (/ 1.0 (* cx 4)))

    (define is-eps? (and as-eps #t))
    (define/public (multiple-pages-ok?) (not is-eps?))

    (super-new)
    
    (when c (init-cr-matrix c))))

(define post-script-dc% (class (doc+page-check-mixin (dc-mixin (make-dc-backend #f))
                                                     'post-script-dc%)
                          (super-new)))
(define pdf-dc% (class (doc+page-check-mixin (dc-mixin (make-dc-backend #t))
                                             'pdf-dc%)
                  (super-new)))
