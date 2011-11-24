#lang scheme/base
(require scheme/class
         ffi/unsafe/atomic
         "color.rkt"
         "syntax.rkt"
         "local.rkt"
         "bitmap.rkt"
         "gradient.rkt"
         "transform.rkt")

(provide brush%
         brush-list% the-brush-list
         brush-style-symbol?)

(define (brush-style-symbol? s)
  (memq s '(transparent solid opaque
                        xor hilite panel
                        bdiagonal-hatch crossdiag-hatch
                        fdiagonal-hatch cross-hatch
                        horizontal-hatch vertical-hatch)))

(define black (send the-color-database find-color "black"))

(define-local-member-name s-set-key)

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
                        _stipple))
    (set-stipple _stipple))

  (when _transformation
    (unless (transformation-vector? _transformation)
      (raise-type-error (init-name 'brush%)
                        "transformation-vector"
                        _transformation))
    (when (or _gradient _stipple)
      (set! transformation (transformation-vector->immutable
                            _transformation))))

  (super-new)

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
  (def/public (set-stipple [(make-or-false bitmap%) s] 
                           [(make-or-false transformation-vector?) [t #f]])
    (check-immutable 'set-stipple)
    (set! stipple s)
    (set! transformation (and s t))))

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
                  (send f s-set-key key)
                  (hash-set! brushes key e)
                  f)))
          (end-atomic))))))

(define the-brush-list (new brush-list%))


