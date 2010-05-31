#lang scheme/base
(require scheme/class
         "color.ss"
         "syntax.ss"
         "local.ss"
         "bitmap.ss")

(provide pen%
         pen-list% the-pen-list
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

(define-local-member-name s-set-key)

(defclass pen% object%
  (define key #f)
  (define/public (s-set-key k) (set! key k))

  (define color black)
  (properties #:check-immutable check-immutable 
              [[pen-cap-symbol? cap] 'round]
              [[pen-join-symbol? join] 'round]
              [[pen-style-symbol? style] 'solid]
              [[pen-width? width] 0])

  (init-rest args)
  (super-new)

  (case-args
   args
   [() (void)]
   [([color% _color]
     [pen-width? _width]
     [pen-style-symbol? _style])
    (set! color (color->immutable-color _color))
    (set! width _width)
    (set! style _style)]
   [([string? _color]
     [pen-width? _width]
     [pen-style-symbol? _style])
    (set! color (send the-color-database find-color _color))
    (set! width _width)
    (set! style _style)]
   (init-name 'pen%))

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
     (method-name 'pen% 'set-color)))

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

(defclass pen-list% object%
  (define pens (make-weak-hash))
  (super-new)
  (define/public (find-or-create-pen . args)
    (let-values ([(col w s)
                  (case-args
                   args
                   [([color% _color]
                     [pen-width? _width]
                     [pen-style-symbol? _style])
                    (values (color->immutable-color _color) _width _style)]
                   [([string? _color]
                     [pen-width? _width]
                     [pen-style-symbol? _style])
                    (values (send the-color-database find-color _color)
                            _width
                            _style)]
                   (method-name 'find-or-create-pen 'pen-list%))])
      (let ([key (vector (send col red) (send col green) (send col blue)
                         w s)])
        (let ([e (hash-ref pens key #f)])
          (or (and e
                   (ephemeron-value e))
              (let* ([f (make-object pen% col w s)]
                     [e (make-ephemeron key f)])
                (send f s-set-key key)
                (hash-set! pens key e)
                f)))))))

(define the-pen-list (new pen-list%))


