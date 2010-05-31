#lang scheme/base
(require scheme/class
         "syntax.ss"
         "local.ss"
         "cairo.ss"
         "dc-path.ss"
         "dc-intf.ss"
         "point.ss"
         "lock.ss")

(provide region%)

(define-local-member-name
  get-paths
  internal-get-dc)

(define temp-cr #f)

(define region%
  (class object%

    (init [the-dc #f])
    (define dc the-dc)
    (unless (dc . is-a? . dc<%>)
      (raise-type-error (init-name 'region%)
                        "dc<%> instance"
                        dc))

    ;; Intersected paths, each as (cons <path> <fill-style>),
    ;;  where <fill-style> is 'odd-even, 'winding, or 'any.
    ;; A null path list corresponds to an empty region.
    (define paths null)
    (define/public (get-paths) paths)

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

    (define (ox oy) (send dc get-origin))
    (define (sx sy) (send dc get-scale))
    
    (def/public (get-dc) dc)
    (define/public (internal-get-dc) dc)

    (def/public (get-bounding-box)
      (if (null? paths)
          (values 0.0 0.0 0.0 0.0)
          (let-values ([(l t r b) (send (caar paths) get-bounding-box)])
            (let loop ([paths (cdr paths)]
                       [l l]
                       [t t]
                       [r r]
                       [b b])
              (if (null? paths)
                  (values l t r b)
                  (let-values ([(l2 t2 r2 b2) (send (caar paths) get-bounding-box)])
                    (loop (cdr paths)
                          (min l l2)
                          (min t t2)
                          (max r r2)
                          (max b b2))))))))

    (define/public (install-region cr [init (void)] [install (lambda (cr v) (cairo_clip cr))])
      (let ([default-fill-rule (if (ormap (lambda (pr) (eq? (cdr pr) 'odd-even)) paths)
                                   CAIRO_FILL_RULE_EVEN_ODD
                                   CAIRO_FILL_RULE_WINDING)])
        (for/fold ([v init]) ([pr (in-list paths)])
          (cairo_new_path cr)
          (send (car pr) do-path cr values values)
          (cairo_set_fill_rule cr
                               (case (cdr pr)
                                 [(odd-even) CAIRO_FILL_RULE_EVEN_ODD]
                                 [(winding) CAIRO_FILL_RULE_WINDING]
                                 [else default-fill-rule]))
          (install cr v))))

    (def/public (is-empty?)
      (really-is-empty?))

    (define/private (with-clipping proc)
      (send 
       dc
       in-cairo-context
       (lambda (cr)
         (cairo_save cr)
         (install-region cr)
         (begin0
          (proc cr)
          (cairo_restore cr)))))

    (define/private (really-is-empty?)
      (or (null? paths)
          (if empty-known?
              known-empty?
              (let ([v (with-clipping
                        (lambda (cr)
                          (let-values ([(x1 y1 x2 y2) (cairo_clip_extents cr)])
                            (or (= x1 x2) (= y1 y2)))))])
                (set! known-empty? v)
                (set! empty-known? #t)
                v))))

    (def/public (in-region? [real? x]
                            [real? y])
      (as-entry
       (lambda ()
         (unless temp-cr
           (set! temp-cr
                 (cairo_create
                  (cairo_image_surface_create CAIRO_FORMAT_A8 1 1))))
         (install-region temp-cr #t (lambda (cr v) (and v (cairo_in_fill temp-cr x y)))))))

    (def/public (set-arc [real? x]
                         [real? y]
                         [nonnegative-real? width]
                         [nonnegative-real? height]
                         [real? start-radians]
                         [real? end-radians])
      (modifying 'set-arc)
      (let ([p (new dc-path%)])
        (send p move-to x y)
        (send p arc x y width height start-radians end-radians)
        (send p close)
        (set! paths (list (cons p 'any)))))

    (def/public (set-ellipse [real? x]
                             [real? y]
                             [nonnegative-real? width]
                             [nonnegative-real? height])
      (modifying 'set-ellipse)
      (let ([p (new dc-path%)])
        (send p ellipse x y width height)
        (set! paths (list (cons p 'any)))))

    (def/public (set-path [dc-path% path]
                          [real? [x 0.0]]
                          [real? [y 0.0]]
                          [(symbol-in odd-even winding) [fill-style 'odd-even]])
      (modifying 'set-path)
      (let ([p (new dc-path%)])
        (send p append path)
        (set! paths (list (cons p fill-style)))))

    (def/public (set-polygon [(make-alts (make-list point%) list-of-pair-of-real?) pts]
                             [real? [x 0.0]]
                             [real? [y 0.0]]
                             [(symbol-in odd-even winding) [fill-style 'odd-even]])
      (modifying 'set-polygon)
      (if (null? pts)
          (set! paths null)
          (let ([p (new dc-path%)])
            (let ([i (car pts)])
              (if (pair? i)
                  (send p move-to (car i) (cdr i))
                  (send p move-to (point-x i) (point-y i))))
            (for ([i (in-list (cdr pts))])
              (if (pair? i)
                  (send p line-to (car i) (cdr i))
                  (send p line-to (point-x i) (point-y i))))
            (send p close)
            (set! paths (list (cons p fill-style))))))

    (def/public (set-rectangle [real? x]
                               [real? y]
                               [nonnegative-real? width]
                               [nonnegative-real? height])
      (modifying 'set-rectangle)
      (let ([p (new dc-path%)])
        (send p rectangle x y width height)
        (set! paths (list (cons p 'any)))))

    (def/public (set-rounded-rectangle [real? x]
                                       [real? y]
                                       [nonnegative-real? width]
                                       [nonnegative-real? height]
                                       [real? [radius -0.25]])
      (modifying 'set-rounded-rectangle)
      (let ([p (new dc-path%)])
        (send p rounded-rectangle x y width height radius)
        (set! paths (list (cons p 'any)))))

    (define/private (check-compatible r who)
      (unless (eq? dc (send r internal-get-dc))
        (raise-mismatch-error (method-name 'region% who)
                              "different built-in dc for given region: " 
                              r)))

    (def/public (intersect [region% r])
      (check-compatible r (lambda () (method-name 'region% 'union)))
      (modifying 'intersect)
      (set! paths (append paths (send r get-paths))))

    (def/public (subtract [region% r])
      (check-compatible r (lambda () (method-name 'region% 'subtract)))
      (unless (null? paths)
        (let ([add-paths (send r get-paths)])
          (unless (null? add-paths)
            (let ([p paths])
              (do-union 'subtract r  (lambda (p) (rev-paths p)))
              (set! paths (append paths p)))))))

    (def/public (union [region% r])
      (do-union 'union r values))

    (def/public (xor [region% r])
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
