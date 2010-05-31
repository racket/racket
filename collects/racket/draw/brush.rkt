#lang scheme/base
(require scheme/class
         "color.ss"
         "syntax.ss"
         "local.ss"
         "bitmap.ss")

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

  (init-rest args)
  (super-new)

  (case-args
   args
   [() (void)]
   [([color% _color]
     [brush-style-symbol? _style])
    (set! color (color->immutable-color _color))
    (set! style _style)]
   [([string? _color]
     [brush-style-symbol? _style])
    (set! color (send the-color-database find-color _color))
    (set! style _style)]
   (init-name 'brush%))

  (define immutable? #f)
  (define lock-count 0)
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

  (define stipple #f)
  (def/public (get-stipple) stipple)
  (def/public (set-stipple [(make-or-false bitmap%) s]) 
    (check-immutable 'set-stipple)
    (let ([old-s stipple])
      (set! stipple #f)
      (when old-s (send old-s adjust-lock -1)))
    (when s (send s adjust-lock 1))
    (set! stipple s)))

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
                    (values (send the-color-database find-color _color)
                            _style)]
                   (method-name 'find-or-create-brush 'brush-list%))])
      (let ([key (vector (send col red) (send col green) (send col blue)
                         s)])
        (let ([e (hash-ref brushes key #f)])
          (or (and e
                   (ephemeron-value e))
              (let* ([f (make-object brush% col s)]
                     [e (make-ephemeron key f)])
                (send f s-set-key key)
                (hash-set! brushes key e)
                f)))))))

(define the-brush-list (new brush-list%))


