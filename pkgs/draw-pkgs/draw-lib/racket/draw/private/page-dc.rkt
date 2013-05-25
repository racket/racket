#lang racket/base
(require racket/class
         "syntax.rkt")

(provide doc+page-check-mixin
         multiple-pages-ok?)

(define-local-member-name multiple-pages-ok?)

(define-syntax check-page-active
  (syntax-rules ()
    [(_ check-page-status (id . args) ...) (begin (check-one-page-active check-page-status id args) ...)]))

(define-syntax check-one-page-active
  (syntax-rules ()
    [(_ check-page-status id simple ... (arg ... [opt ...]))
     (check-one-page-active 
      check-page-status id 
      simple ...
      (arg ...)
      (arg ... opt ...))]
    [(_ check-page-status id (arg ...) ...)
     (define/override id
       (case-lambda
        [(arg ...) (check-page-status 'id) (super id arg ...)]
        ...))]))

(define (doc+page-check-mixin % class-name)
  (class %
    (inherit multiple-pages-ok?)

    (define status #f)
    (define did-one-page? #f)

    (define/override (start-doc s)
      (when status
        (raise-mismatch-error (method-name class-name 'start-doc)
                              (case status
                                [(done)
                                 "document has already been ended: "]
                                [else
                                 "document has already been started: "])
                              this))
      (set! status 'doc)
      (super start-doc s))

    (define/override (end-doc)
      (unless (eq? status 'doc)
        (raise-mismatch-error (method-name class-name 'end-doc)
                              (case status
                                [(page)
                                 "current page has not been ended: "]
                                [(done)
                                 "document is already ended: "]
                                [(#f)
                                 "document is not started: "])
                              this))
      (set! status 'done)
      (super end-doc))

    (define/override (start-page)
      (unless (eq? status 'doc)
        (raise-mismatch-error (method-name class-name 'start-page)
                              (if (eq? status 'page)
                                  "current page has not been ended: "
                                  "document is not started (use the `start-doc' method): ")
                              this))
      (when did-one-page?
        (unless (multiple-pages-ok?)
          (raise-mismatch-error (method-name class-name 'start-page)
                                "cannot create multiple pages for encapsulated output: "
                                this)))
      (set! status 'page)
      (set! did-one-page? #t)
      (super start-page))

    (define/override (end-page)
      (unless (eq? status 'page)
        (raise-mismatch-error (method-name class-name 'end-page)
                              "no page is currently started: "
                              this))
      (set! status 'doc)
      (super end-page))

    (define/private (check-page-status the-method-name)
      (unless (eq? status 'page)
        (raise-mismatch-error (method-name class-name the-method-name)
                              "no page is currently started (use `start-doc' and `start-page' before drawing): "
                              this)))

    (check-page-active
     check-page-status 
     (draw-bitmap source dest-x dest-y [style [color [mask]]])
     (draw-bitmap-section source dest-x dest-y src-x src-y src-width src-height [style [color [mask]]])
     (draw-polygon pts [x [y [fill]]])
     (draw-lines pts [x [y]])
     (draw-path path [x [y [fill]]])
     (draw-ellipse x y w h)
     (draw-arc x y w h s e)
     (draw-text txt x y [combine? [offset [angle]]])
     (draw-spline x1 y1 x2 y2 x3 y3)
     (draw-rounded-rectangle x y w h [r])
     (draw-rectangle x y w h)
     (draw-point x y)
     (draw-line x1 y1 x2 y2)
     (clear)
     (erase))

    (super-new)))
