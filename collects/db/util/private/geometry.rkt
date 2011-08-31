#lang racket/base
(require racket/list)
(provide (all-defined-out))

#|
Geometry according to mysql:
abstract Geometry
- Point = (x, y)
- abstract Curve
  - LineString = (list of points)
    - predicate Line
    - predicate LinearRing (closed and non-self-intersecting, bleh)
- abstract Surface
  - Polygon (defined by LinearRings, bleh)
      = exterior ring, list of interior rings
- GeometryCollection = (list of geometry values)
  - MultiPoint = (list of points)
  - abstract MultiCurve
    - MultiLineString = (list of line strings)
  - abstract MultiSurface
    - MultiPolygon = (list of polygons)

every geometric value has an associated Spacial Reference System (SRID), ignored by mysql

Geometry according to postgis:

same as above, but with coordinate variants: eg pointm = (x, y, m)
|#

(struct point (x y)
        #:transparent
        #:guard (lambda (x y _n)
                  (values (exact->inexact x)
                          (exact->inexact y))))

(struct line-string (points)
        #:transparent)

(define (line? x)
  (and (line-string? x)
       (let ([points (line-string-points x)])
         (and (= 2 (length points))
              (not (equal? (first points) (second points)))))))

(define (linear-ring? x)
  (and (line-string? x)
       (let ([points (line-string-points x)])
         ;; FIXME: require at least ??? points
         (equal? (first points) (last points)))))

(struct polygon (exterior interiors)
        #:transparent)

(struct multi-point (elements)
        #:transparent)

(struct multi-line-string (elements)
        #:transparent)

(struct multi-polygon (elements)
        #:transparent)

(struct geometry-collection (elements)
        #:transparent)

(define (geometry2d? x)
  (or (point? x)
      (line-string? x)
      (polygon? x)
      (multi-point? x)
      (multi-line-string? x)
      (multi-polygon? x)
      (geometry-collection? x)))

;; ----------------------------------------

;; Based on OGC 06-103r4

(define (wkb->geometry b [start 0] [end (bytes-length b)])
  (bytes->geometry 'wkb->geometry b start end #:srid? #f))

(define (bytes->geometry who b [start 0] [end (bytes-length b)]
                         #:srid? [srid? #f])
  (define (get-byte)
    (begin0 (bytes-ref b start)
      (set! start (+ start 1))))
  (define (get-uint be?)
    (begin0 (integer-bytes->integer b #f be? start (+ start 4))
      (set! start (+ start 4))))
  (define (get-multi n get-X)
    (for/list ([i (in-range n)]) (get-X)))
  (define (get-geometry)
    (let ([srid (and srid? (get-uint #f))] ;; FIXME: store srid
          [be? (zero? (get-byte))])
      (define (get-double)
        (begin0 (floating-point-bytes->real b be? start (+ start 8))
          (set! start (+ start 8))))
      (define (get-point)
        (let* ([x (get-double)]
               [y (get-double)])
          (point x y)))
      (define (get-linear-ring)
        (let ([len (get-uint be?)])
          (line-string (get-multi len get-point))))
      (let ([type (get-uint be?)])
        (case type
          ((1) (get-point))
          ((2) (let ([points (get-multi (get-uint be?) get-point)])
                 (line-string points)))
          ((3) (let ([rings (get-multi (get-uint be?) get-linear-ring)])
                 (when (null? rings)
                   (error who "polygon with zero rings"))
                 (polygon (car rings) (cdr rings))))
          ((4 5 6 7) (let ([constructor
                            (case type
                              ((4) multi-point)
                              ((5) multi-line-string)
                              ((6) multi-polygon)
                              ((7) geometry-collection))]
                           [elements (get-multi (get-uint be?) get-geometry)])
                       (constructor elements)))
          (else
           (error who "unsupported geometry type: ~s" type))))))
  (begin0 (get-geometry)
    (unless (= start end)
      (error who "~s bytes left over" (- end start)))))

;; ----

(define (geometry->wkb g
                       #:big-endian? [be? (system-big-endian?)])
  (geometry->bytes 'geometry->wkb g
                   #:big-endian? be?
                   #:srid? #f))

(define (geometry->bytes who g
                         #:big-endian? [be? (system-big-endian?)]
                         #:srid? [srid? #f])
  (define out (open-output-bytes))
  (define (put-uint n)
    (write-bytes (integer->integer-bytes n 4 #f be?) out))
  (define (put-double x)
    (write-bytes (real->floating-point-bytes x 8 be?) out))
  (define (put-point g)
    (put-double (point-x g))
    (put-double (point-y g)))
  (define (put-line-string g)
    (let ([points (line-string-points g)])
      (put-uint (length points))
      (for ([p (in-list points)])
        (put-point p))))
  (define (put-collection lst)
    (put-uint (length lst))
    (for ([g (in-list lst)])
      (put-geometry g)))
  (define (put-geometry g)
    (when srid? (put-uint 0)) ;; FIXME
    (write-byte (if be? 0 1) out)
    (cond [(point? g)
           (put-uint 1)
           (put-point g)]
          [(line-string? g)
           (put-uint 2)
           (put-line-string g)]
          [(polygon? g)
           (put-uint 3)
           (let ([rings (cons (polygon-exterior g) (polygon-interiors g))])
             (put-uint (length rings))
             (for ([ring (in-list rings)])
               (put-line-string ring)))]
          [(multi-point? g)
           (put-uint 4)
           (put-collection (multi-point-elements g))]
          [(multi-line-string? g)
           (put-uint 5)
           (put-collection (multi-line-string-elements g))]
          [(multi-polygon? g)
           (put-uint 6)
           (put-collection (multi-polygon-elements g))]
          [(geometry-collection? g)
           (put-uint 7)
           (put-collection (geometry-collection-elements g))]
          [else
           (error who "unsupported geometry type: ~e" g)]))
  (put-geometry g)
  (get-output-bytes out))

;; FIXME: define WKT functions?
;; FIXME: eventually, integrate with geos?
