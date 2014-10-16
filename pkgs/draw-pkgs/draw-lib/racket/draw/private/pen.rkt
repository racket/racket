#lang racket/base
(require racket/class
         ffi/unsafe/atomic
         "color.rkt"
         "syntax.rkt"
         "local.rkt"
         "bitmap.rkt")

(provide pen%
         make-pen
         pen-list% the-pen-list
         pen-width?
         pen-style-symbol?)

(define (pen-style-symbol? s)
  (memq s '(transparent solid xor hilite
                        dot long-dash short-dash dot-dash
                        xor-dot xor-long-dash xor-short-dash
                        xor-dot-dash)))

(define (pen-cap-symbol? s)
  (memq s '(round projecting butt)))

(define (pen-join-symbol? s)
  (memq s '(round bevel miter)))

(define (pen-width? v)
  (and (real? v)
       (>= v 0)
       (<= v 255)))

(define black (send the-color-database find-color "black"))

(define-local-member-name
  s-set-key
  set-immutable)

(defclass pen% object%
  (define key #f)
  (define/public (s-set-key k) (set! key k))

  (define immutable? #f)
  (define lock-count 0)
  (define stipple #f)

  (define color black)
  (properties #:check-immutable check-immutable 
              [[pen-cap-symbol? cap] 'round]
              [[pen-join-symbol? join] 'round]
              [[pen-style-symbol? style] 'solid]
              [[pen-width? width] 0])

  (init [(_color color) black]
        [(_width width) 0]
        [(_style style) 'solid]
        [(_cap cap) 'round]
        [(_join join) 'round]
        [(_stipple stipple) #f])

  (set! color
        (cond
         [(string? _color) (or (send the-color-database find-color _color) black)]
         [(color . is-a? . color%)
          (color->immutable-color _color)]
         [else
          (raise-type-error (init-name 'pen%)
                            "string or color%"
                            _color)]))
  (set! width
        (if (pen-width? _width)
            _width
            (raise-type-error (init-name 'pen%)
                              "real in [0, 255]"
                              _width)))
  
  (set! style
        (if (pen-style-symbol? _style)
            _style
            (raise-type-error (init-name 'pen%)
                              "pen style symbol"
                              _style)))
    
  (set! cap
        (if (pen-cap-symbol? _cap)
            _cap
            (raise-type-error (init-name 'pen%)
                              "pen cap symbol"
                              _cap)))
    
  (set! join
        (if (pen-join-symbol? _join)
            _join
            (raise-type-error (init-name 'pen%)
                              "pen join symbol"
                              _join)))

  (when _stipple
    (unless (_stipple . is-a? . bitmap%)
      (raise-type-error (init-name 'pen%)
                        "bitmap% or #f"
                        _stipple)))

  (super-new)

  (when _stipple (set-stipple _stipple))

  (define/public (set-immutable) (set! immutable? #t))
  (define/public (is-immutable?) (or immutable? (positive? lock-count)))
  (define/public (adjust-lock v) (set! lock-count (+ lock-count v)))

  (define/private (check-immutable s)
    (when (or immutable? (positive? lock-count))
      (error (method-name 'pen% s) "object is ~a"
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
     (method-name 'pen% 'set-color)))

  (define/public (get-color) color)

  (def/public (get-stipple) stipple)
  (define/public (set-stipple s)
    (check-immutable 'set-stipple)
    (set! stipple s)))

;; color width style cap join stipple -> pen%
;; produce an immutable pen% object
(define (make-pen #:color [color black]
                  #:width [width 0]
                  #:style [style 'solid]
                  #:cap [cap 'round]
                  #:join [join 'round]
                  #:stipple [stipple #f]
                  #:immutable? [immutable? #t])
  (or (and (not (or stipple (not immutable?)))
           (send the-pen-list find-or-create-pen color width style cap join))
      (let ()
        (define pen (make-object pen% color width style cap join stipple))
        (when immutable?
          (send pen set-immutable))
        pen)))

;; ----------------------------------------

(defclass pen-list% object%
  (define pens (make-weak-hash))
  (super-new)
  (define/public (find-or-create-pen . args)
    (let-values ([(col w s c j)
                  (case-args
                   args
                   [([color% _color]
                     [pen-width? _width]
                     [pen-style-symbol? _style]
                     [pen-cap-symbol? [_cap 'round]]
                     [pen-join-symbol? [_join 'round]])
                    (values (color->immutable-color _color) 
                            _width _style _cap _join)]
                   [([string? _color]
                     [pen-width? _width]
                     [pen-style-symbol? _style]
                     [pen-cap-symbol? [_cap 'round]]
                     [pen-join-symbol? [_join 'round]])
                    (values (or (send the-color-database find-color _color)
                                black)
                            _width _style _cap _join)]
                   (method-name 'find-or-create-pen 'pen-list%))])
      (let ([key (vector (send col red) (send col green) (send col blue)
                         (send col alpha)
                         w s c j)])
        (start-atomic)
        (begin0
         (let ([e (hash-ref pens key #f)])
           (or (and e
                    (ephemeron-value e))
               (let* ([f (make-object pen% col w s c j)]
                      [e (make-ephemeron key f)])
                 (send f set-immutable)
                 (send f s-set-key key)
                 (hash-set! pens key e)
                 f)))
         (end-atomic))))))

(define the-pen-list (new pen-list%))


