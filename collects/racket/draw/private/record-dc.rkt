#lang racket/base
(require racket/class
         ffi/unsafe/atomic
         "dc.rkt"
         "bitmap.rkt"
         "bitmap-dc.rkt"
         "color.rkt"
         "point.rkt"
         "pen.rkt"
         "brush.rkt"
         "region.rkt"
         "dc-path.rkt")

(provide record-dc-mixin
         get-recorded-command
         reset-recording
         set-recording-limit)

(define-local-member-name
  get-recorded-command
  reset-recording
  set-recording-limit)

(define black (send the-color-database find-color "black"))

(define (clone-point p)
  (if (pair? p)
      p
      (make-object point% (point-x p) (point-y p))))

(define (clone-color c)
  (if (string? c)
      (string->immutable-string c)
      (color->immutable-color c)))

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

(define (clone-path p)
  (let ([new-p (make-object dc-path%)])
    (send new-p append p)
    p))

(define (clone-bitmap b)
  (let* ([new-b (make-object bitmap% 
                             (send b get-width) 
                             (send b get-height)
                             (not (send b is-color?))
                             (send b has-alpha-channel?))]
         [dc (make-object bitmap-dc% new-b)])
    (send dc draw-bitmap b 0 0)
    (send dc set-bitmap #f)
    new-b))

(define (record-dc-mixin %)
  (class %
    (super-new)

    (inherit get-origin get-scale get-rotation get-initial-matrix 
             get-pen get-brush get-font
             get-smoothing get-text-mode 
             get-alpha get-clipping-region)

    (define record-limit +inf.0)
    (define current-size 0)

    (define/public (set-recording-limit amt)
      (set! record-limit amt))
    (define/private (continue-recording?)
      (current-size . < . record-limit))

    (define-syntax-rule (define/record (name arg ...))
      (define/override (name arg ...)
        (super name arg ...)
        (record (lambda (dc) (send dc name arg ...)))))

    (define procs null)
    (define/private (record proc)
      (when (continue-recording?)
        (start-atomic)
        (set! current-size (add1 current-size))
        (set! procs (cons proc procs))
        (end-atomic)))

    (define/public (get-recorded-command)
      (and (continue-recording?)
           (let ([procs (reverse procs)])
             (lambda (dc)
               (for ([proc (in-list procs)])
                 (proc dc))))))

    (define/public (reset-recording)
      (start-atomic)
      (set! procs null)
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
                   [(cr) (get-clipping-region)])
        (unless (and (zero? ox) (zero? oy)) (set-origin ox oy))
        (unless (and (= 1 sx) (= 1 sy)) (set-scale sx sy))
        (unless (zero? r) (set-rotation r))
        (unless (equal? m '#(1.0 0.0 0.0 1.0 0.0 0.0)) (set-initial-matrix m))
        (do-set-pen! p)
        (do-set-brush! b)
        (set-font f)
        (unless (eq? s 'unsmoothed) (set-smoothing s))
        (unless (eq? tm 'transparent) (set-text-mode tm))
        (unless (= a 1.0) (set-alpha a))
        (when cr (set-clipping-region cr))))

    (define clones (make-hasheq))
    (define/private (clone clone-x x)
      (or (let ([new-x (hash-ref clones x #f)])
            (and new-x
                 (equal? new-x x)
                 new-x))
          (let ([new-x (clone-x x)])
            (hash-set! clones x new-x)
            new-x)))

    (define/record (set-scale sx sy))
    
    (define/record (set-origin sx sy))
    
    (define/record (set-rotation r))

    (define/override (transform mi)
      (super transform mi)
      (when (continue-recording?)
        (let ([mi (vector->immutable-vector mi)])
          (record (lambda (dc) (send dc transform mi))))))

    (define/override (set-initial-matrix mi)
      (super set-initial-matrix mi)
      (when (continue-recording?)
        (let ([mi (vector->immutable-vector mi)])
          (record (lambda (dc) (send dc set-initial-matrix mi))))))

    (define/override (set-transformation mi)
      (super set-transformation mi)
      (when (continue-recording?)
        (let ([mi (vector->immutable-vector mi)])
          (record (lambda (dc) (send dc set-transformation mi))))))

    (define/record (set-smoothing s))

    (define/record (set-alpha n))

    (define/record (set-font f))

    (define/override (do-set-pen! p)
      (super do-set-pen! p)
      (let ([p (clone clone-pen p)])
        (record (lambda (dc) (send dc do-set-pen! p)))))

    (define/override (do-set-brush! b)
      (super do-set-brush! b)
      (when (continue-recording?)
        (let ([b (clone clone-brush b)])
          (record (lambda (dc) (send dc do-set-brush! b))))))

    (define/override (set-text-foreground c)
      (super set-text-foreground c)
      (when (continue-recording?)
        (let ([c (clone clone-color c)])
          (record (lambda (dc) (send dc set-text-foreground c))))))
    
    (define/override (set-text-background c)
      (super set-text-background c)
      (when (continue-recording?)
        (let ([c (clone clone-color c)])
          (record (lambda (dc) (send dc set-text-background c))))))
    
    (define/override (set-background c)
      (super set-background c)
      (when (continue-recording?)
        (let ([c (clone clone-color c)])
          (record (lambda (dc) (send dc set-background c))))))
    
    (define/record (set-text-mode m))

    (define/override (set-clipping-region r)
      (super set-clipping-region r)
      (when (continue-recording?)
        (let ([make-r (if r
                          (region-maker r)
                          (lambda (dc) #f))])
          (record (lambda (dc) (send dc set-clipping-region (make-r dc)))))))

    (define/record (set-clipping-rect x y w h))

    (define/record (clear))
    
    (define/override (erase)
      (super erase)
      (reset-recording))

    (define/record (draw-arc x y
                             width height
                             start-radians end-radians))

    (define/record (draw-ellipse x y w h))
    
    (define/record (draw-line x1 y1 x2 y2))

    (define/record (draw-point x y))
    
    (define/override (draw-lines pts [x 0.0] [y 0.0])
      (super draw-lines pts x y)
      (when (continue-recording?)
        (let ([pts (map (lambda (p) (clone clone-point p)) pts)])
          (record (lambda (dc) (send dc draw-lines pts x y))))))

    (define/override (draw-polygon pts [x 0.0] [y 0.0] [fill-style 'odd-even])
      (super draw-polygon pts x y fill-style)
      (when (continue-recording?)
        (let ([pts (map (lambda (p) (clone clone-point p)) pts)])
          (record (lambda (dc) (send dc draw-polygon pts x y fill-style))))))

    (define/record (draw-rectangle x y w h))
    
    (define/override (draw-rounded-rectangle x y w h [radius -0.25])
      (super draw-rounded-rectangle x y w h radius)
      (record (lambda (dc) (send dc draw-rounded-rectangle x y w h radius))))

    (define/record (draw-spline x1 y1 x2 y2 x3 y3))

    (define/override (draw-path path [x 0.0] [y 0.0] [fill-style 'odd-even])
      (super draw-path path x y fill-style)
      (when (continue-recording?)
        (let ([path (clone clone-path path)])
          (record (lambda (dc) (send dc draw-path path x y fill-style))))))
    
    (define/override (draw-text s x y [combine? #f] [offset 0] [angle 0.0])
      (super draw-text s x y combine? offset angle)
      (when (continue-recording?)
        (let ([s (string->immutable-string s)])
          (record (lambda (dc) (send dc draw-text s x y combine? offset angle))))))
    
    (define/override (draw-bitmap src dx dy [style 'solid] [color black] [mask #f])
      (super draw-bitmap src dx dy style color mask)
      (when (continue-recording?)
        (let ([src (clone clone-bitmap src)]
              [mask (and mask (clone clone-bitmap mask))])
          (record (lambda (dc) (send dc draw-bitmap src dx dy style color mask))))))
    
    (define/override (draw-bitmap-section src dx dy sx sy sw sh [style 'solid] [color black] [mask #f])
      (super draw-bitmap-section src dx dy sx sy sw sh style color mask)
      (when (continue-recording?)
        (let ([src (clone clone-bitmap src)]
              [mask (and mask (clone clone-bitmap mask))])
          (record (lambda (dc) (send dc draw-bitmap-section src dx dy sx sy sw sh style color mask))))))))
