#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/atomic
         "../unsafe/cairo.rkt"
         "color.rkt"
         "syntax.rkt"
         "local.rkt"
         "bitmap.rkt"
         "gradient.rkt"
         "transform.rkt")

(provide brush%
         make-brush
         brush-list% the-brush-list
         brush-style-symbol?)

(define (brush-style-symbol? s)
  (memq s '(transparent solid opaque
                        xor hilite panel
                        bdiagonal-hatch crossdiag-hatch
                        fdiagonal-hatch cross-hatch
                        horizontal-hatch vertical-hatch)))

(define black (send the-color-database find-color "black"))

(define-local-member-name 
  s-set-key
  set-surface-handle-info
  set-immutable)

(defclass brush% object%
  (define key #f)
  (define/public (s-set-key k) (set! key k))

  (define color black)
  (properties #:check-immutable check-immutable 
              [[brush-style-symbol? style] 'solid])

  (init [(_color color) black]
        [(_style style) 'solid]
        [(_stipple stipple) #f]
        [(_gradient gradient) #f]
        [(_transformation transformation) #f])

  (set! color
        (cond
         [(string? _color) (or (send the-color-database find-color _color) black)]
         [(color . is-a? . color%)
          (color->immutable-color _color)]
         [else
          (raise-type-error (init-name 'brush%)
                            "string or color%"
                            _color)]))
  
  (set! style
        (if (brush-style-symbol? _style)
            _style
            (raise-type-error (init-name 'brush%)
                              "brush style symbol"
                              _style)))

  (define immutable? #f)
  (define lock-count 0)
  (define stipple #f)
  (define gradient #f)
  (define transformation #f)
  (define surface-handle #f)

  (when _gradient
    (unless (or (_gradient . is-a? . linear-gradient%)
                (_gradient . is-a? . radial-gradient%))
      (raise-type-error (init-name 'brush%)
                        "linear-gradient% object, radial-gradient% object, or #f"
                        _gradient))
    (set! gradient _gradient))

  (when _stipple
    (unless (_stipple . is-a? . bitmap%)
      (raise-type-error (init-name 'brush%)
                        "bitmap% or #f"
                        _stipple)))

  (when _transformation
    (unless (transformation-vector? _transformation)
      (raise-type-error (init-name 'brush%)
                        "transformation-vector"
                        _transformation))
    (when (or _gradient _stipple)
      (set! transformation (transformation-vector->immutable
                            _transformation))))

  (super-new)

  (when _stipple
    (set-stipple _stipple))

  (define/public (set-immutable) (set! immutable? #t))
  (define/public (is-immutable?) (or immutable? (positive? lock-count)))
  (define/public (adjust-lock v) (set! lock-count (+ lock-count v)))

  (define/private (check-immutable s)
    (when (or immutable? (positive? lock-count))
      (error (method-name 'brush% s) "object is ~a"
             (if immutable? "immutable" "locked"))))

  (define/public (set-color . args)
    (check-immutable 'set-color)
    (case-args
     args
     [([color% _color])
      (set! color (color->immutable-color _color))]
     [([string? _color])
      (set! color (send the-color-database find-color _color))]
     [([byte? r] [byte? g] [byte? b])
      (let ([c (make-object color%  r g b)])
        (send c set-immutable)
        (set! color c))]
     (method-name 'brush% 'set-color)))

  (define/public (get-color) color)
  (define/public (get-gradient) gradient)
  (define/public (get-transformation) transformation)

  (def/public (get-stipple) stipple)
  (define/public (set-stipple s [t #f])
    (check-immutable 'set-stipple)
    (set! stipple s)
    (set! transformation (and s t)))

  (define/public (get-surface-handle-info) surface-handle) ; local
  (def/public (get-handle) (and surface-handle
                                (vector-ref surface-handle 0)))
  (define/public (set-surface-handle-info h t)
    (set! surface-handle h)
    (set! transformation t)))

;; color style stipple gradient transformation -> brush%
;; produce an immutable brush% object
(define (make-brush #:color [color black]
                    #:style [style 'solid]
                    #:stipple [stipple #f]
                    #:gradient [gradient #f]
                    #:transformation [transformation #f]
                    #:immutable? [immutable? #t])
  (or (and (not (or stipple gradient transformation (not immutable?)))
           (send the-brush-list find-or-create-brush color style))
      (let ()
        (define brush (make-object brush% color style stipple gradient transformation))
        (when immutable?
          (send brush set-immutable))
        brush)))

;; unsafe (and so exported by `racket/draw/unsafe/brush'):
(provide (protect-out make-handle-brush))
(define (make-handle-brush handle width height [t #f]
                           #:copy? [copy? #t])
  ;; for argument checking:
  (define/top (make-handle-brush [cpointer? handle]
                                 [exact-nonnegative-integer? width]
                                 [exact-nonnegative-integer? height]
                                 [(make-or-false transformation-vector?) t])
    'ok)
  (make-handle-brush handle width height t)
  ;; arguments are ok, so proceed:
  (define s-in (cast handle _pointer _cairo_surface_t))
  (define s
    (if copy?
        (let ()
          (define s (cairo_surface_create_similar s-in CAIRO_CONTENT_COLOR_ALPHA width height))
          (define cr (cairo_create s))
          (let* ([p (cairo_pattern_create_for_surface s-in)])
            (cairo_set_source cr p)
            (cairo_pattern_destroy p)
            (cairo_rectangle cr 0 0 width height)
            (cairo_fill cr)
            (cairo_destroy cr))
          s)
        s-in))
  (define b (new brush%))
  (send b set-surface-handle-info (vector s width height
                                          ;; cache for bitmap version:
                                          #f 
                                          ;; retain original if not copied:
                                          (if copy? #f handle)) 
        t)
  b)

(provide (protect-out surface-handle-info->bitmap))
(define (surface-handle-info->bitmap hi)
  (or (vector-ref hi 3)
      (let ()
        (define width (vector-ref hi 1))
        (define height (vector-ref hi 2))
        (define bm (make-bitmap width height))
        (define s (send bm get-cairo-surface))
        (define cr (cairo_create s))
        (let* ([p (cairo_pattern_create_for_surface (vector-ref hi 0))])
          (cairo_set_source cr p)
          (cairo_pattern_destroy p)
          (cairo_rectangle cr 0 0 width height)
          (cairo_fill cr)
          (cairo_destroy cr))
        (vector-set! hi 3 bm)
        bm)))

;; ----------------------------------------

(defclass brush-list% object%
  (define brushes (make-weak-hash))
  (super-new)
  (define/public (find-or-create-brush . args)
    (let-values ([(col s)
                  (case-args
                   args
                   [([color% _color]
                     [brush-style-symbol? _style])
                    (values (color->immutable-color _color) _style)]
                   [([string? _color]
                     [brush-style-symbol? _style])
                    (values (or (send the-color-database find-color _color)
                                black)
                            _style)]
                   (method-name 'find-or-create-brush 'brush-list%))])
      (let ([key (vector (send col red) (send col green) (send col blue)
                         (send col alpha)
                         s)])
        (start-atomic)
        (begin0
          (let ([e (hash-ref brushes key #f)])
            (or (and e
                     (ephemeron-value e))
                (let* ([f (make-object brush% col s)]
                       [e (make-ephemeron key f)])
                  (send f set-immutable)
                  (send f s-set-key key)
                  (hash-set! brushes key e)
                  f)))
          (end-atomic))))))

(define the-brush-list (new brush-list%))


