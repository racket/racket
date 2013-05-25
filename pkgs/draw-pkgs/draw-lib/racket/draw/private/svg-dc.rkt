#lang racket/base
(require racket/class
         "syntax.rkt"
         "../unsafe/cairo.rkt"
         "dc.rkt"
         "local.rkt"
         "page-dc.rkt"
         "write-bytes.rkt")

(provide svg-dc%)

(define dc-backend%
  (class default-dc-backend%
    (init [(init-w width)]
          [(init-h height)]
          [(init-output output)]
          [exists 'error])

    (unless (and (real? init-w) (not (negative? init-w)))
      (raise-type-error (init-name 'svg-dc%) "nonnegative real or #f" init-w))
    (unless (and (real? init-h) (not (negative? init-h)))
      (raise-type-error (init-name 'svg-dc%) "nonnegative real or #f" init-h))
    (unless (or (output-port? init-output)
                (path-string? init-output))
      (raise-type-error (init-name 'svg-dc%) "path string or output port" init-output))
    (unless (memq exists '(error append update can-update
                                 replace truncate
                                 must-truncate truncate/replace))
      (raise-type-error (init-name 'svg-dc%) 
                        "'error, 'append, 'update, 'can-update, 'replace, 'truncate, 'must-truncate, or 'truncate/replace"
                        exists))

    (define width init-w)
    (define height init-h)
    (define close-port? (path-string? init-output))

    (define port
      (if (output-port? init-output)
          init-output
          (open-output-file init-output #:exists exists)))
    (define-values (s writer)
      (let-values ([(writer proc) (make-port-writer port)])
        (values (cairo_svg_surface_create_for_stream 
                 proc
                 width
                 height)
                writer)))

    (define c (and s (cairo_create s)))    
    (when s (cairo_surface_destroy s))

    (define/override (ok?) (and c #t))

    (define/override (get-cr) c)

    (def/override (get-size)
      (values width height))

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

    (define/override (get-pango font)
      (send font get-pango))

    (define/override (get-font-metrics-key sx sy)
      (if (and (= sx 1.0) (= sy 1.0))
          3
          0))

    (define/override (can-combine-text? sz)
      #t)

    (define/public (multiple-pages-ok?) #t)

    (super-new)))

(define svg-dc% (class (doc+page-check-mixin (dc-mixin dc-backend%)
                                             'svg-dc%)
                  (super-new)))
