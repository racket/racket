#lang racket/base
(require racket/class
         ffi/unsafe/atomic
         "syntax.rkt"
         "local.rkt"
         "../unsafe/cairo.rkt"
         "dc-path.rkt"
         "dc-intf.rkt"
         "point.rkt")

(provide region%

         get-paths
         set-paths!
         internal-get-dc)

(define-local-member-name
  get-paths
  set-paths!
  internal-get-dc)

(define temp-cr #f)

(define region%
  (class object%

    (init [(the-dc dc) #f])
    (define dc the-dc)
    (when dc
      (unless (dc . is-a? . dc<%>)
        (raise-type-error (init-name 'region%)
                          "dc<%> instance or #f"
                          dc)))

    ;; Intersected paths, each as (cons <path> <fill-style>),
    ;;  where <fill-style> is 'odd-even, 'winding, or 'any.
    ;; A null path list corresponds to an empty region.
    (define paths null)
    (define/public (get-paths) paths)
    (define/public (set-paths! p)
      (set! paths p)
      (set! empty-known? #f))

    (define locked 0)
    (define/public (lock-region delta) (set! locked (+ locked delta)))

    (define my-key (gensym))

    (define empty-known? #f) ; #t => `known-empty?' records `empty?' result
    (define known-empty? #f)
    (define/private (modifying who)
      (when (positive? locked)
        (error (method-name 'region% who)
               "region is locked (installed into a dc<%>): "
               this))
      (set! empty-known? #f))

    (define matrix (and dc (send dc get-clipping-matrix)))
    
    (def/public (get-dc) dc)
    (define/public (internal-get-dc) dc)

    (def/public (get-bounding-box)
      (if (null? paths)
          (values 0.0 0.0 0.0 0.0)
          (let-values ([(l t w h) (send (caar paths) get-bounding-box)])
            (let loop ([paths (cdr paths)]
                       [l l]
                       [t t]
                       [r (+ l w)]
                       [b (+ t h)])
              (if (null? paths)
                  (if matrix
                      ;; Convert absolute coordinates back to the DC's
                      ;;  logical space by inverting its transformation
                      (let ([m (send dc get-clipping-matrix)])
                        ;; Matrix is [ma mc 
                        ;;            mb md]
                        (let ([ma (vector-ref m 0)]
                              [mb (vector-ref m 2)]
                              [mc (vector-ref m 1)]
                              [md (vector-ref m 3)]
                              [dx (vector-ref m 4)]
                              [dy (vector-ref m 5)])
                          (let ([det (- (* ma md) (* mb mc))])
                            (if (zero? det)
                                ;; determinant is 0 => dc's matrix maps any area to 0
                                (values 0.0 0.0 0.0 0.0)
                                ;; tx and ty apply inverse matrix
                                (let ([tx (lambda (x y)
                                            (let ([x (- x dx)]
                                                  [y (- y dy)])
                                              (/ (- (* md x) (* mb y)) det)))]
                                      [ty (lambda (x y)
                                            (let ([x (- x dx)]
                                                  [y (- y dy)])
                                              (/ (- (* ma y) (* mc x)) det)))])
                                  ;; unwind bound-box points to pre-transformed
                                  (let ([l (tx l t)]
                                        [t (ty l t)]
                                        [r (tx r b)]
                                        [b (ty r b)])
                                    (values l t (- r l) (- b t))))))))
                      ;; no dc un-transformation needed
                      (values l t (- r l) (- b t)))
                  (let-values ([(l2 t2 w2 h2) (send (caar paths) get-bounding-box)])
                    (loop (cdr paths)
                          (min l l2)
                          (min t t2)
                          (max r (+ l2 w2))
                          (max b (+ t2 h2)))))))))

    (define/public (install-region cr scroll-dx scroll-dy align-x align-y 
                                   [init (void)] [install (lambda (cr v) (cairo_clip cr))]
                                   #:init-matrix [init-matrix (lambda (cr) (void))])
      (let ([default-fill-rule (if (ormap (lambda (pr) (eq? (cdr pr) 'odd-even)) paths)
                                   CAIRO_FILL_RULE_EVEN_ODD
                                   CAIRO_FILL_RULE_WINDING)]
            [old-matrix (and matrix
                             (let ([m (make-cairo_matrix_t 0 0 0 0 0 0)])
                               (cairo_get_matrix cr m)
                               m))])
        (when old-matrix 
          ;; each path is already transformed
          (cairo_identity_matrix cr)
          (init-matrix cr)
          (cairo_transform cr (make-cairo_matrix_t 1 0 0 1 scroll-dx scroll-dy)))
        (begin0
         (if (null? paths)
             (begin
               (cairo_new_path cr)
               (install cr init))
             (for/fold ([v init]) ([pr (in-list paths)])
               (cairo_new_path cr)
               (send (car pr) do-path cr values values)
               (cairo_set_fill_rule cr
                                    (case (cdr pr)
                                      [(odd-even) CAIRO_FILL_RULE_EVEN_ODD]
                                      [(winding) CAIRO_FILL_RULE_WINDING]
                                      [else default-fill-rule]))
               (install cr v)))
         (when old-matrix (cairo_set_matrix cr old-matrix)))))

    (def/public (is-empty?)
      (really-is-empty?))

    (define/private (with-clipping who proc)
      (unless dc
        (raise-mismatch-error (method-name 'region% who) 
                              "not allowed for a region without a drawing context: "
                              this))
      (send dc in-cairo-context 
            (lambda (cr)
              (cairo_save cr)
              (install-region cr 0 0 values values)
              (begin0
               (proc cr)
               (cairo_restore cr)))))

    (define/private (really-is-empty?)
      (or (null? paths)
          (if empty-known?
              known-empty?
              (let ([v (with-clipping
                        'is-empty?
                        (lambda (cr)
                          (let-values ([(x1 y1 x2 y2) (cairo_clip_extents cr)])
                            (or (= x1 x2) (= y1 y2)))))])
                (set! known-empty? v)
                (set! empty-known? #t)
                v))))
    
    (define/private (with-temp-cr proc)
      (let ([cr (call-as-atomic
                 (lambda ()
                   (cond
                    [temp-cr 
                     (begin0 temp-cr (set! temp-cr #f))]
                    [else
                     (let ([s (cairo_image_surface_create CAIRO_FORMAT_A8 100 100)])
                       (begin0
                        (cairo_create s)
                        (cairo_surface_destroy s)))])))])
        (begin0
         (proc cr)
         (call-as-atomic
          (lambda ()
            (cond
             [temp-cr (cairo_destroy cr)]
             [else (set! temp-cr cr)]))))))
    
    (define/public (in-region? x y)
      (with-temp-cr
       (lambda (cr)
        (let-values ([(x y)
                      (if matrix
                          ;; need to use the DC's current transformation
                          (let ([m (send dc get-clipping-matrix)])
                            (values (+ (* x (vector-ref m 0))
                                       (* y (vector-ref m 2))
                                       (vector-ref m 4))
                                    (+ (* x (vector-ref m 1))
                                       (* y (vector-ref m 3))
                                       (vector-ref m 5))))
                          ;; no transformation needed
                          (values x y))])
          (install-region cr 0 0 values values
                          #t
                          (lambda (cr v) (and v (cairo_in_fill cr x y))))))))

    (define/public (set-arc x y width height start-radians end-radians)
      (modifying 'set-arc)
      (let ([p (new dc-path%)])
        (send p move-to (+ x (/ width 2)) (+ y (/ height 2)))
        (send p arc x y width height start-radians end-radians)
        (send p close)
        (when matrix (send p transform matrix))
        (set! paths (list (cons p 'any)))))

    (define/public (set-ellipse x y width height)
      (modifying 'set-ellipse)
      (let ([p (new dc-path%)])
        (send p ellipse x y width height)
        (when matrix (send p transform matrix))
        (set! paths (list (cons p 'any)))))

    (define/public (set-path path
                             [x 0.0] [y 0.0]
                             [fill-style 'odd-even])
      (modifying 'set-path)
      (let ([p (new dc-path%)])
        (send p append path)
        (send p translate x y)
        (when matrix (send p transform matrix))
        (set! paths (list (cons p fill-style)))))

    (define/public (set-polygon pts
                                [x 0.0] [y 0.0]
                                [fill-style 'odd-even])
      (modifying 'set-polygon)
      (if (null? pts)
          (set! paths null)
          (let ([p (new dc-path%)])
            (let ([i (car pts)])
              (if (pair? i)
                  (send p move-to (+ x (car i)) (+ y (cdr i)))
                  (send p move-to (+ x (point-x i)) (+ y (point-y i)))))
            (for ([i (in-list (cdr pts))])
              (if (pair? i)
                  (send p line-to (+ x (car i)) (+ y (cdr i)))
                  (send p line-to (+ x (point-x i)) (+ y (point-y i)))))
            (send p close)
            (when matrix (send p transform matrix))
            (set! paths (list (cons p fill-style))))))

    (define/public (set-rectangle x y width height)
      (modifying 'set-rectangle)
      (let ([p (new dc-path%)])
        (send p rectangle x y width height)
        (when matrix (send p transform matrix))
        (set! paths (list (cons p 'any)))))

    (define/public (set-rounded-rectangle x y width height [radius -0.25])
      (modifying 'set-rounded-rectangle)
      (let ([p (new dc-path%)])
        (send p rounded-rectangle x y width height radius)
        (when matrix (send p transform matrix))
        (set! paths (list (cons p 'any)))))

    (define/private (check-compatible r who)
      (unless (equal? dc (send r internal-get-dc))
        (raise-mismatch-error (method-name 'region% who)
                              "different built-in dc for given region: " 
                              r)))

    (define/public (intersect r)
      (check-compatible r 'union)
      (modifying 'intersect)
      (set! paths (append paths (send r get-paths))))

    (define/public (subtract r)
      (check-compatible r 'subtract)
      (unless (null? paths)
        (let ([add-paths (send r get-paths)])
          (unless (null? add-paths)
            (let ([p paths])
              (do-union 'subtract r  (lambda (p) (rev-paths p)))
              (set! paths (append paths p)))))))

    (define/public (union r)
      (do-union 'union r values))

    (define/public (xor r)
      (do-union 'xor r (lambda (p) (rev-paths p))))

    (define/private rev-paths
      (lambda (paths)
        (map (lambda (pr)
               (let ([p (new dc-path%)])
                 (send p append (car pr))
                 (send p reverse)
                 (cons p (cdr pr))))
             paths)))

    (define/private (do-union who r adjust)
      (check-compatible r who)
      (modifying who)
      (let ([add-paths (send r get-paths)])
        (unless (null? add-paths)
          (if (null? paths)
              (set! paths add-paths)
              (let ([add-paths (adjust add-paths)])
                (let ([a (car paths)]
                      [b (car add-paths)])
                  (set! paths 
                        (cons (let ([p (new dc-path%)])
                                (send p append (car a))
                                (send p append (car b))
                                (cons p (cond
                                         [(or (eq? (cdr a) 'odd-even)
                                              (eq? (cdr b) 'odd-even))
                                          'odd-even]
                                         [(or (eq? (cdr a) 'winding)
                                              (eq? (cdr b) 'winding))
                                          'winding]
                                         [else 'any])))
                              (append (cdr paths)
                                      (cdr add-paths))))))))))

    (super-new)))
