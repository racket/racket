#lang racket/base
(require racket/class
         ffi/unsafe/atomic
         "../unsafe/cairo.rkt"
         "local.rkt"
         "syntax.rkt"
         "dc.rkt"
         "dc-intf.rkt"
         "bitmap.rkt"
         "bitmap-dc.rkt"
         "color.rkt"
         "point.rkt"
         "pen.rkt"
         "brush.rkt"
         "font.rkt"
         "region.rkt"
         "dc-path.rkt"
         "gradient.rkt"
         (for-syntax racket/base))

(provide record-dc%
         recorded-datum->procedure
         (protect-out record-dc-mixin
                      get-recorded-command
                      reset-recording
                      set-recording-limit))

(define-local-member-name
  get-recorded-command
  reset-recording
  set-recording-limit
  record-unconvert)

(define black (send the-color-database find-color "black"))

(define (clone-point p)
  (if (pair? p)
      p
      (cons (point-x p) (point-y p))))

(define (clone-points ps)
  (map clone-point ps))

(define (clone-color c)
  (if (string? c)
      (string->immutable-string c)
      (color->immutable-color c)))

(define (convert-color c)
  (if (string? c)
      (string->immutable-string c)
      (list (color-red c) (color-green c) (color-blue c) (color-alpha c))))

(define (unconvert-color c)
  (if (string? c)
      (string->immutable-string c)
      (make-object color% (car c) (cadr c) (caddr c) (cadddr c))))

(define (clone-pen p)
  (let ([s (send p get-stipple)])
    (if s
        (let ([p (make-object pen% 
                              (send p get-color)
                              (send p get-width)
                              (send p get-style)
                              (send p get-cap)
                              (send p get-join))])
          (send p set-stipple (clone-bitmap s))
          p)
        (send the-pen-list find-or-create-pen
              (send p get-color)
              (send p get-width)
              (send p get-style)
              (send p get-cap)
              (send p get-join)))))

(define (convert-pen p)
  (let ([s (send p get-stipple)])
    (list (convert-color (send p get-color))
          (send p get-width)
          (send p get-style)
          (send p get-cap)
          (send p get-join)
          (and s (convert-bitmap s)))))

(define (unconvert-pen l)
  (define-values (c width style cap join stipple)
    (apply values l))
  (define color (unconvert-color c))
  (if stipple
      (let ([p (make-object pen% color width style cap join)])
        (send p set-stipple (unconvert-bitmap stipple))
        p)
        (send the-pen-list find-or-create-pen color width style cap join)))

(define (clone-brush b)
  (let ([s (send b get-stipple)])
    (if s
        (let ([b (make-object brush% 
                              (send b get-color)
                              (send b get-style))]
              [t (send b get-transformation)])
          (send b set-stipple (clone-bitmap s) t)
          b)
        (let ([g (send b get-gradient)])
          (if g
              (make-object brush% 
                           (send b get-color)
                           (send b get-style)
                           #f
                           g
                           (send b get-transformation))
              (send the-brush-list find-or-create-brush
                    (send b get-color)
                    (send b get-style)))))))

(define (convert-brush b)
  (let ([s (send b get-stipple)]
        [g (send b get-gradient)])
    (list (convert-color (send b get-color))
          (send b get-style)
          (and s (convert-bitmap s))
          (and g (convert-gradient g))
          (send b get-transformation))))

(define (unconvert-brush l)
  (define-values (c style stipple gradient transformation)
    (apply values l))
  (define color (unconvert-color c))
  (if stipple
      (let ([b (make-object brush% color style)])
        (send b set-stipple (unconvert-bitmap stipple) 
              transformation)
        b)
      (if gradient
          (make-object brush% 
                       color
                       style
                       #f
                       (unconvert-gradient gradient)
                       transformation)
          (send the-brush-list find-or-create-brush color style))))

(define (convert-gradient g)
  (if (g . is-a? . linear-gradient%)
      (let-values ([(x1 y1 x2 y2) (send g get-line)])
        (list x1 y1 x2 y2 (convert-stops (send g get-stops))))
      (let-values ([(x1 y1 r1 x2 y2 r2) (send g get-circles)])
        (list x1 y1 r1 x2 y2 r2 (convert-stops (send g get-stops))))))

(define (unconvert-gradient l)
  (if (= (length l) 5)
      (make-object linear-gradient% 
                   (car l) (cadr l) (caddr l) (cadddr l)
                   (unconvert-stops (list-ref l 4)))
      (make-object radial-gradient% 
                   (car l) (cadr l) (caddr l) 
                   (list-ref l 3) (list-ref l 4) (list-ref l 5)
                   (unconvert-stops (list-ref l 6)))))

(define (convert-stops s)
  (for/list ([i (in-list s)]) (cons (car i) (convert-color (cadr i)))))
(define (unconvert-stops s)
  (for/list ([i (in-list s)]) (list (car i) (unconvert-color (cdr i)))))

(define (region-maker r)
  (if (send r internal-get-dc)
      (let ([paths (send r get-paths)])
        (lambda (dc)
          (let ([new-r (make-object region% dc)])
            (send new-r set-paths! paths)
            new-r)))
      (let ([new-r (make-object region%)])
        (send new-r union r)
        (lambda (dc) new-r))))

(define (convert-region r)
  (and r
       (cons (and (send r internal-get-dc) #t)
             (map convert-path (send r get-paths)))))

(define (unconvert-region l)
  (if l
      (let ()
        (define paths (map unconvert-path (cdr l)))
        (if (car l)
            (lambda (dc)
              (let ([new-r (make-object region% (and (car l) dc))])
                (send new-r set-paths! paths)
                new-r))
            (let ([new-r (make-object region%)])
              (send new-r set-paths! paths)
              (lambda (dc) new-r))))
      (lambda (dc) #f)))

(define (clone-path p)
  (let ([new-p (make-object dc-path%)])
    (send new-p append p)
    p))

(define (convert-path p)
  (cons (send p get-closed-points)
        (send p get-open-points)))

(define (unconvert-path l)
  (define p (new dc-path%))
  (send p set-closed+open-points (car l) (cdr l))
  p)

(define (clone-bitmap b)
  (and b
       (let* ([new-b (make-object bitmap% 
                                  (send b get-width) 
                                  (send b get-height)
                                  (not (send b is-color?))
                                  (send b has-alpha-channel?))]
              [dc (make-object bitmap-dc% new-b)])
         (send dc draw-bitmap b 0 0)
         (send dc set-bitmap #f)
         new-b)))

(define (convert-bitmap b)
  (and b
       (let ()
         (define w (send b get-width))
         (define h (send b get-height))
         (define bstr (make-bytes (* 4 w h)))
         (send b get-argb-pixels 0 0 w h bstr)
         (list w h 
               (send b is-color?)
               (send b has-alpha-channel?)
               (bytes->immutable-bytes bstr)))))

(define (unconvert-bitmap l)
  (and l
       (let ()
         (define-values (w h color? alpha? bstr)
           (apply values l))
         (define bm (make-object bitmap% w h (not color?) alpha?))
         (send bm set-argb-pixels 0 0 w h bstr)
         bm)))

(define (convert-font f)
  (list (send f get-point-size)
        (send f get-face)
        (send f get-family)
        (send f get-style)
        (send f get-weight)
        (send f get-underlined)
        (send f get-smoothing)
        (send f get-size-in-pixels)))

(define (unconvert-font l)
  (apply make-object font% l))

(define (record-dc-mixin %)
  (class %
    (super-new)

    (inherit get-origin get-scale get-rotation get-initial-matrix 
             get-pen get-brush get-font
             get-smoothing get-text-mode 
             get-alpha get-clipping-region
             translate rotate scale)

    (define record-limit +inf.0)
    (define current-size 0)

    (define/public (set-recording-limit amt)
      (set! record-limit amt))
    (define/private (continue-recording?)
      (current-size . < . record-limit))

    (define-syntax (define/record stx)
      (syntax-case stx ()
        [(_ (name arg ...))
         (let ([args (syntax->list #'(arg ...))])
           (with-syntax ([(arg-id ...)
                          (map (lambda (arg)
                                 (syntax-case arg ()
                                   [([id def-val]) #'id]
                                   [([id def-val] clone-id convert-id unconvert-id) #'id]
                                   [(id clone-id convert-id unconvert-id) #'id]
                                   [(id clone-id) #'id]
                                   [else arg]))
                               args)]
                         [(arg-formal ...)
                          (map (lambda (arg)
                                 (syntax-case arg ()
                                   [(id) #'id]
                                   [(id clone-id convert-id unconvert-id) #'id]
                                   [(id clone-id) #'id]
                                   [else arg]))
                               args)]
                         [(arg-bind ...)
                          (map (lambda (arg)
                                 (syntax-case arg ()
                                   [([id def-val])
                                    #'[id id]]
                                   [([id def-val] clone-id convert-id unconvert-id)
                                    #'[id (clone clone-id id)]]
                                   [(id clone-id convert-id unconvert-id)
                                    #'[id (clone clone-id id)]]
                                   [(id clone-id)
                                    #'[id (clone clone-id id)]]
                                   [id
                                    #'[id id]]))
                               args)]
                         [((arg-convert ...) ...)
                          (map (lambda (arg)
                                 (syntax-case arg ()
                                   [(formal clone-id convert-id unconvert-id)
                                    #'(convert convert-id)]
                                   [_ #'(values)]))
                               args)])
             #'(define/override (name arg-formal ...)
                 (super name arg-id ...)
                 (when (continue-recording?)
                   (let (arg-bind ...)
                     (record (lambda (dc) (send dc name arg-id ...))
                             (lambda () (list 'name (arg-convert ... arg-id) ...))))))))]))

    (define-syntax (generate-record-unconvert stx)
      (syntax-case stx ()
        [(_ ([clause-tags clause-rhs] ...) (defn (name arg ...)) ...)
         (with-syntax ([((arg-id ...) ...)
                        (let ([names (syntax->list #'(name ...))]
                              [argss (syntax->list #'((arg ...) ...))])
                          (map (lambda (name args)
                                 (map (lambda (arg)
                                        (syntax-case arg ()
                                          [([id def-val]) #'id]
                                          [([id def-val] clone-id convert-id unconvert-id) #'id]
                                          [(id clone-id convert-id unconvert-id) #'id]
                                          [(id clone-id) #'id]
                                          [else arg]))
                                      (syntax->list args)))
                               names
                               argss))]
                       [(((arg-bind ...) ...) ...)
                        (let ([argss (syntax->list #'((arg ...) ...))])
                          (map (lambda (args)
                                 (map (lambda (arg)
                                        (syntax-case arg ()
                                          [([id def-val] clone-id convert-id unconvert-id)
                                           #'([id (unconvert-id id)])]
                                          [(id clone-id convert-id unconvert-id)
                                           #'([id (unconvert-id id)])]
                                          [_ #'()]))
                                      (syntax->list args)))
                               argss))])
           #'(begin
               (defn (name arg ...)) ...
               (define/public (record-unconvert cmds)
                 (for/list ([cmd (in-list cmds)])
                   (define cmd-tag (car cmd))
                   (define cmd-args (cdr cmd))
                   (case cmd-tag
                     [clause-tags (apply clause-rhs cmd-args)] ...
                     [(name)
                      (apply (lambda (arg-id ...)
                               (let (arg-bind ... ...)
                                 (lambda (dc) (send dc name arg-id ...))))
                             cmd-args)]
                     ...
                     [else (error 'unconvert "bad datum: ~e" cmd-tag)])))))]))

    (define procs null)
    (define converts null)
    (define/private (record proc convert)
      (when (continue-recording?)
        (start-atomic)
        (set! current-size (add1 current-size))
        (set! procs (cons proc procs))
        (set! converts (cons convert converts))
        (end-atomic)))

    (define/public (get-recorded-command [serialize? #f])
      (and (continue-recording?)
           (if serialize?
               (for/list ([convert (in-list (reverse converts))])
                 (convert))
               (let ([procs (reverse procs)])
                 (lambda (dc)
                   (for ([proc (in-list procs)])
                     (proc dc)))))))

    (define/public (reset-recording)
      (start-atomic)
      (set! clones (make-hasheq))
      (set! converteds (make-hasheq))
      (set! procs null)
      (set! converts null)
      (set! current-size 0)
      (end-atomic)
      ;; install current configuration explicitly (so it gets recorded):
      (let-values ([(ox oy) (get-origin)]
                   [(sx sy) (get-scale)]
                   [(r) (get-rotation)]
                   [(m) (get-initial-matrix)]
                   [(p) (get-pen)]
                   [(b) (get-brush)]
                   [(s) (get-smoothing)]
                   [(f) (get-font)]
                   [(tm) (get-text-mode)]
                   [(a) (get-alpha)]
                   [(cr) (get-clipping-region)]
                   [(to-default?) (applies-to-default?)])
        (when to-default?
          (unless (and (zero? ox) (zero? oy)) (set-origin ox oy))
          (unless (and (= 1 sx) (= 1 sy)) (set-scale sx sy))
          (unless (zero? r) (set-rotation r))
          (unless (equal? m '#(1.0 0.0 0.0 1.0 0.0 0.0)) (set-initial-matrix m)))
        (unless to-default?
          (unless (equal? m '#(1.0 0.0 0.0 1.0 0.0 0.0)) (transform m))
          (unless (zero? r) (rotate r))
          (unless (and (= 1 sx) (= 1 sy)) (scale sx sy))
          (unless (and (zero? ox) (zero? oy)) (translate (- sx) (- sy))))
        (do-set-pen! p)
        (do-set-brush! b)
        (set-font f)
        (unless (and to-default? (eq? s 'unsmoothed)) (set-smoothing s))
        (unless (and to-default? (eq? tm 'transparent)) (set-text-mode tm))
        (unless (and to-default? (= a 1.0)) (set-alpha a))
        (unless (and to-default? (not cr)) (set-clipping-region cr))))

    (define/public (applies-to-default?) #t)

    (define clones (make-hasheq))
    (define/private (clone clone-x x)
      (or (let ([new-x (hash-ref clones x #f)])
            (and new-x
                 (equal? new-x x)
                 new-x))
          (let ([new-x (clone-x x)])
            (when (equal? new-x x)
              (hash-set! clones x new-x))
            new-x)))

    (define converteds (make-hasheq))
    (define/private (convert convert-x x)
      (or (hash-ref converteds x #f)
          (let ([new-x (convert-x x)])
            (hash-set! converteds x new-x)
            new-x)))

    (define/override (erase)
      (super erase)
      (reset-recording))

    (define/override (set-clipping-region r)
      (super set-clipping-region r)
      (when (continue-recording?)
        (let ([make-r (if r
                          (region-maker r)
                          (lambda (dc) #f))])
          (record (lambda (dc) (send dc set-clipping-region (make-r dc)))
                  (lambda () (list 'set-clipping-region (convert-region r)))))))

    (generate-record-unconvert
     ([(set-clipping-region) (lambda (r) 
                               (define make-r (unconvert-region r))
                               (lambda (dc)
                                 (send dc set-clipping-region (make-r dc))))])
     ;; remaining clauses are generated:

     (define/record (set-scale sx sy))
     
     (define/record (set-origin sx sy))
     
     (define/record (set-rotation r))

     (define/record (transform [mi vector->immutable-vector]))

     (define/record (set-initial-matrix [mi vector->immutable-vector]))

     (define/record (set-transformation [mi vector->immutable-vector]))

     (define/record (set-smoothing s))

     (define/record (set-alpha n))

     (define/record (set-font [f values convert-font unconvert-font]))

     (define/record (do-set-pen! [p clone-pen convert-pen unconvert-pen]))

     (define/record (do-set-brush! [b clone-brush convert-brush unconvert-brush]))

     (define/record (set-text-foreground [c clone-color convert-color unconvert-color]))

     (define/record (set-text-background [c clone-color convert-color unconvert-color]))
     
     (define/record (set-background [c clone-color convert-color unconvert-color]))
     
     (define/record (set-text-mode m))

     (define/record (set-clipping-rect x y w h))

     (define/record (clear))
     
     (define/record (draw-arc x y width height start-radians end-radians))

     (define/record (draw-ellipse x y w h))
     
     (define/record (draw-line x1 y1 x2 y2))

     (define/record (draw-point x y))
     
     (define/record (draw-lines [pts clone-points] [[x 0.0]] [[y 0.0]]))

     (define/record (draw-polygon [pts clone-points] [[x 0.0]] [[y 0.0]] [[fill-style 'odd-even]]))

     (define/record (draw-rectangle x y w h))
     
     (define/record (draw-rounded-rectangle x y w h [[radius -0.25]]))

     (define/record (draw-spline x1 y1 x2 y2 x3 y3))

     (define/record (draw-path [path clone-path convert-path unconvert-path] 
                               [[x 0.0]] [[y 0.0]] [[fill-style 'odd-even]]))
     
     (define/record (draw-text [s string->immutable-string] x y 
                               [[combine? #f]] [[offset 0]] [[angle 0.0]]))
     
     (define/record (draw-bitmap [src clone-bitmap convert-bitmap unconvert-bitmap]
                                 dx dy [[style 'solid]]
                                 [[color black] clone-color convert-color unconvert-color]
                                 [[mask #f] clone-bitmap convert-bitmap unconvert-bitmap]))
     
     (define/record (draw-bitmap-section [src clone-bitmap convert-bitmap unconvert-bitmap]
                                         dx dy sx sy sw sh [[style 'solid]] 
                                         [[color black] clone-color convert-color unconvert-color]
                                         [[mask #f] clone-bitmap convert-bitmap unconvert-bitmap])))))

;; ----------------------------------------

(define record-dc-backend%
  (class default-dc-backend%
    (init [[-width width] 640]
          [[-height height] 480])

    (define-values (width height)
      (case-args 
       (list -width -height)
       [([nonnegative-real? w]
         [nonnegative-real? h])
        (values w h)]
       (init-name 'record-dc%)))

    (define/override (ok?) #t)

    ;; We need a cair context and surface to measure text:
    (define c (cairo_create (cairo_image_surface_create CAIRO_FORMAT_ARGB32 1 1)))
    (define/override (get-cr) c)

    (def/override (get-size)
      (values (exact->inexact width)
              (exact->inexact height)))

    (super-new)))

(define record-dc%
  (class (record-dc-mixin (dc-mixin record-dc-backend%))
    (inherit reset-recording
             get-recorded-command)

    (define/public (get-recorded-procedure)
      (get-recorded-command #f))

    (define/public (get-recorded-datum)
      (get-recorded-command #t))

    (define/override (applies-to-default?) #f)
    
    (super-new)
    (reset-recording)))

(define (recorded-datum->procedure d)
  (define procs (send (new record-dc%) record-unconvert d))
  (lambda (dc)
    (unless (dc . is-a? . dc<%>)
      (raise-type-error 'recorded-datum->procedure "dc<%> object" dc))
    ;; save all the existing state:
    (define-values (ox oy) (send dc get-origin))
    (define-values (sx sy) (send dc get-scale))
    (define r (send dc get-rotation))
    (define m (send dc get-initial-matrix))
    (define p (send dc get-pen))
    (define b (send dc get-brush))
    (define s (send dc get-smoothing))
    (define f (send dc get-font))
    (define tm (send dc get-text-mode))
    (define a (send dc get-alpha))
    (define cr (send dc get-clipping-region))
    
    (for ([proc (in-list procs)])
      (proc dc))
    
    ;; Restore the state:
    (send dc set-origin ox oy)
    (send dc set-scale sx sy)
    (send dc set-rotation r)
    (send dc set-initial-matrix m)
    (send dc do-set-pen! p)
    (send dc do-set-brush! b)
    (send dc set-font f)
    (send dc set-smoothing s)
    (send dc set-text-mode tm)
    (send dc set-alpha a)
    (send dc set-clipping-region cr)))
