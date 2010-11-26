#lang racket/base
(require racket/class
         racket/file
         racket/path
         racket/math
         "syntax.rkt"
         racket/gui/dynamic
         ffi/unsafe
         ffi/unsafe/alloc
         "../unsafe/cairo.ss"
         "../unsafe/bstr.ss"
	 "dc.ss"
         "font.ss"
         "local.ss"
         "ps-setup.ss")

(provide post-script-dc%
         pdf-dc%)

(define (make-dc-backend pdf?)
  (class default-dc-backend%
    (init [interactive #t]
          [parent #f]
          [use-paper-bbox #f]
          [as-eps #t])

    (define-values (s port-box width height landscape?)
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
                         (if interactive
                             (get-file (send pss get-file))
                             (let ([fn (send pss get-file)])
                               (or fn (get-file #f))))
                         #f)])
            (if (and to-file?
                     (not fn))
                (values #f #f #f #f #f)
                (let* ([paper (assoc (send pss get-paper-name) paper-sizes)]
                       [w (cadr paper)]
                       [h (caddr paper)]
                       [landscape? (eq? (send pss get-orientation) 'landscape)]
                       [file (open-output-file
                              (or fn (make-temporary-file (if pdf?
                                                              "draw~a.pdf"
                                                              "draw~a.ps")))
                              #:exists 'truncate/replace)]
                       [port-box (make-immobile file)])
                  (let-values ([(w h) (if (and pdf? landscape?)
                                          (values h w)
                                          (values w h))])
                    (values
                     ((if pdf?
                          cairo_pdf_surface_create_for_stream 
                          cairo_ps_surface_create_for_stream)
                      write_port_bytes
                      port-box
                      w
                      h)
                     port-box ; needs to be accessible as long as `s'
                     w
                     h
                     landscape?)))))]
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

    (define/override (end-cr)
      (cairo_surface_finish s)
      (cairo_destroy c)
      (set! c #f)
      (set! s #f)
      (close-output-port (ptr-ref port-box _racket))
      (set! port-box #f))

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

    (super-new)))

(define post-script-dc% (class (dc-mixin (make-dc-backend #f))
                          (super-new)))
(define pdf-dc% (class (dc-mixin (make-dc-backend #t))
                  (super-new)))

(define (write-port-bytes port-box bytes len)
  (write-bytes (scheme_make_sized_byte_string bytes len 0) 
               (ptr-ref port-box _racket))
  CAIRO_STATUS_SUCCESS)

(define write_port_bytes (function-ptr write-port-bytes _cairo_write_func_t))

(define make-immobile ((allocator free-immobile-cell) malloc-immobile-cell))
