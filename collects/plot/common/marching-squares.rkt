#lang racket/base

(require racket/flonum racket/fixnum racket/list racket/match
         "math.rkt")

(provide scale-normalized-line
         scale-normalized-poly
         heights->lines
         heights->high-polys
         heights->low-polys
         heights->mid-polys)

(define (scale-normalized-line line xa xb ya yb)
  (match-define (vector u1 v1 u2 v2) line)
  (vector (alpha-blend xb xa u1) (alpha-blend yb ya v1)
          (alpha-blend xb xa u2) (alpha-blend yb ya v2)))

(define (scale-normalized-poly poly xa xb ya yb)
  (for/list ([uv  (in-list poly)])
    (match-define (vector u v z) uv)
    (vector (alpha-blend xb xa u) (alpha-blend yb ya v) z)))

;(: solve-t (Float Float Float -> Float))
;; Returns the interpolated distance of z from za toward zb
;; Examples: if z = za, this returns 0.0
;;           if z = zb, this returns 1.0
;;           if z = (za + zb) / 2, this returns 0.5
;; Intuitively, regard a use (solve-t z za zb) as "the point between za and zb".
(define-syntax-rule (solve-t z za zb)
  (fl/ (fl- z za) (fl- zb za)))

#|
Z values are at these normalized coordinates:

(0,1)           (1,1)
      z4 --- z3
       |     |
       |     |
      z1 --- z2
(0,0)           (1,0)

A marching squares algorithm and all of its facet functions have this type:

    ftype m = real real real real real -> m

where 'm' is a use-specific type, such as the type of "list of lines". The
first argument is the contour value; the rest are z coordinates arranged as
above.
|#

(define (unrotate-vec v)
  (match-define (vector x y z) v)
  (vector (fl- 1.0 y) x z))

(define (mirror-x-vec v)
  (match-define (vector x y z) v)
  (vector (fl- 1.0 x) y z))

(define (mirror-y-vec v)
  (match-define (vector x y z) v)
  (vector x (fl- 1.0 y) z))

(define (flavg4 z1 z2 z3 z4)
  (fl* 0.25 (fl+ (fl+ (fl+ z1 z2) z3) z4)))

;(define-type (FType m) (Float Float Float Float Float -> m))

#|
(: make-marching-squares
   (All (m)
        (FType m) (FType m) (FType m) (FType m)
        (FType m) (FType m) (FType m) (FType m)
        (FType m) (FType m) (FType m) (FType m)
        (FType m) (FType m) (FType m) (FType m)
        -> (FType m)))
|#
;; Creates a dispatcher for a marching squares algorithm, by combining 16 facet
;; functions. See heights->lines for an example.
(define (make-marching-squares f0000 f0001 f0010 f0011
                               f0100 f0101 f0110 f0111
                               f1000 f1001 f1010 f1011
                               f1100 f1101 f1110 f1111)
  (λ (z z1 z2 z3 z4)
    (let ([p1  (z1 . fl>= . z)]
          [p2  (z2 . fl>= . z)]
          [p3  (z3 . fl>= . z)]
          [p4  (z4 . fl>= . z)])
      (if p1
          (if p2
              (if p3
                  (if p4
                      (f1111 z z1 z2 z3 z4)
                      (f1110 z z1 z2 z3 z4))
                  (if p4
                      (f1101 z z1 z2 z3 z4)
                      (f1100 z z1 z2 z3 z4)))
              (if p3
                  (if p4
                      (f1011 z z1 z2 z3 z4)
                      (f1010 z z1 z2 z3 z4))
                  (if p4
                      (f1001 z z1 z2 z3 z4)
                      (f1000 z z1 z2 z3 z4))))
          (if p2
              (if p3
                  (if p4
                      (f0111 z z1 z2 z3 z4)
                      (f0110 z z1 z2 z3 z4))
                  (if p4
                      (f0101 z z1 z2 z3 z4)
                      (f0100 z z1 z2 z3 z4)))
              (if p3
                  (if p4
                      (f0011 z z1 z2 z3 z4)
                      (f0010 z z1 z2 z3 z4))
                  (if p4
                      (f0001 z z1 z2 z3 z4)
                      (f0000 z z1 z2 z3 z4))))))))

;; =============================================================================
;; Generating contour lines

;; Marching squares return type for contour lines
;(define-type Lines (Listof (Vectorof Float)))
;; Would use (Vector Float Float Float Float) if Typed Racket could create a
;; contract wrapper for it

;; Except for opposite-corner facets, every line-returning facet function is
;; identical to the facet for its bitwise complement.

;; -----------------------------------------------------------------------------
;; all corners left out or included

;(: lines0000 (FType Lines))
(define (lines0000 z z1 z2 z3 z4) empty)
(define lines1111 lines0000)

;; -----------------------------------------------------------------------------
;; one corner included or left out

;(: lines1000 (FType Lines))
(define (lines1000 z z1 z2 z3 z4)
  (list (vector (solve-t z z1 z2) 0.0
                0.0 (solve-t z z1 z4))))

;(: lines0100 (FType Lines))
(define (lines0100 z z1 z2 z3 z4)
  (list (vector (solve-t z z1 z2) 0.0
                1.0 (solve-t z z2 z3))))

;(: lines0010 (FType Lines))
(define (lines0010 z z1 z2 z3 z4)
  (list (vector 1.0 (solve-t z z2 z3)
                (solve-t z z4 z3) 1.0)))

;(: lines0001 (FType Lines))
(define (lines0001 z z1 z2 z3 z4)
  (list (vector 0.0 (solve-t z z1 z4)
                (solve-t z z4 z3) 1.0)))

(define lines0111 lines1000)
(define lines1011 lines0100)
(define lines1101 lines0010)
(define lines1110 lines0001)

;; -----------------------------------------------------------------------------
;; adjacent corners included or left out

;(: lines1100 (FType Lines))
(define (lines1100 z z1 z2 z3 z4)
  (list (vector 0.0 (solve-t z z1 z4)
                1.0 (solve-t z z2 z3))))

;(: lines0110 (FType Lines))
(define (lines0110 z z1 z2 z3 z4)
  (list (vector (solve-t z z1 z2) 0.0
                (solve-t z z4 z3) 1.0)))

(define lines0011 lines1100)
(define lines1001 lines0110)

;; -----------------------------------------------------------------------------
;; opposite corners left out / included

;(: lines-opposite ((Float Float -> Boolean) -> (FType Lines)))
(define (lines-opposite test?)
  (λ (z z1 z2 z3 z4)
    ; disambiguate using average of corners as guess for center value
    (define z5 (flavg4 z1 z2 z3 z4))
    (if (test? z5 z)
        (list (vector (solve-t z z1 z2) 0.0
                      1.0 (solve-t z z2 z3))
              (vector 0.0 (solve-t z z1 z4)
                      (solve-t z z4 z3) 1.0))
        (list (vector (solve-t z z1 z2) 0.0
                      0.0 (solve-t z z1 z4))
              (vector 1.0 (solve-t z z2 z3)
                      (solve-t z z4 z3) 1.0)))))

(define lines1010 (lines-opposite fl>=))
(define lines0101 (lines-opposite fl<))

;(: heights->lines (FType Lines))
(define heights->lines
  (make-marching-squares
   lines0000 lines0001 lines0010 lines0011
   lines0100 lines0101 lines0110 lines0111
   lines1000 lines1001 lines1010 lines1011
   lines1100 lines1101 lines1110 lines1111))

;; =============================================================================
;; Generating polygons on the high sides of contour lines

;; Marching squares return type for polygons
;(define-type Polygons (Listof (Listof (Vectorof Float))))

(define ((rotate-high-facet f) z z1 z2 z3 z4)
  (map (λ (poly) (map unrotate-vec poly))
       (f z z2 z3 z4 z1)))

(define ((mirror-x-high-facet f) z z1 z2 z3 z4)
  (map (λ (poly) (map mirror-x-vec poly))
       (f z z2 z1 z4 z3)))

(define ((mirror-y-high-facet f) z z1 z2 z3 z4)
  (map (λ (poly) (map mirror-y-vec poly))
       (f z z4 z3 z2 z1)))

;; -----------------------------------------------------------------------------
;; all corners left out / included

;(: high-polys0000 (FType Polygons))
(define (high-polys0000 z z1 z2 z3 z4) empty)

;(: high-polys1111 (FType Polygons))
(define (high-polys1111 z z1 z2 z3 z4) (list 'full))

;; -----------------------------------------------------------------------------
;; one corner left out / included

;(: high-polys1000 (FType Polygons))
(define (high-polys1000 z z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t z z1 z2) 0.0 z)
              (vector 0.0 (solve-t z z1 z4) z))))

(define high-polys0100 (rotate-high-facet high-polys1000))
(define high-polys0010 (rotate-high-facet high-polys0100))
(define high-polys0001 (rotate-high-facet high-polys0010))

;(: high-polys0111 (FType Polygons))
(define (high-polys0111 z z1 z2 z3 z4)
  (list (list (vector (solve-t z z1 z2) 0.0 z)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector 0.0 1.0 z4)
              (vector 0.0 (solve-t z z1 z4) z))))

(define high-polys1011 (rotate-high-facet high-polys0111))
(define high-polys1101 (rotate-high-facet high-polys1011))
(define high-polys1110 (rotate-high-facet high-polys1101))

;; -----------------------------------------------------------------------------
;; adjacent corners left out / included

;(: high-polys0011 (FType Polygons))
(define (high-polys0011 z z1 z2 z3 z4)
  (list (list (vector 0.0 (solve-t z z1 z4) z)
              (vector 1.0 (solve-t z z2 z3) z)
              (vector 1.0 1.0 z3)
              (vector 0.0 1.0 z4))))

;(: high-polys0110 (FType Polygons))
(define (high-polys0110 z z1 z2 z3 z4)
  (list (list (vector (solve-t z z1 z2) 0.0 z)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector (solve-t z z4 z3) 1.0 z))))

(define high-polys1100 (mirror-y-high-facet high-polys0011))
(define high-polys1001 (mirror-x-high-facet high-polys0110))

;; -----------------------------------------------------------------------------
;; opposite corners left out / included

;(: high-polys1010 (FType Polygons))
(define (high-polys1010 z z1 z2 z3 z4)
  ; disambiguate using average of corners as guess for center value
  (define z5 (flavg4 z1 z2 z3 z4))
  (if (z5 . fl>= . z)
      (list (list (vector 0.0 0.0 z1)
                  (vector (solve-t z z1 z2) 0.0 z)
                  (vector 1.0 (solve-t z z2 z3) z)
                  (vector 1.0 1.0 z3)
                  (vector (solve-t z z4 z3) 1.0 z)
                  (vector 0.0 (solve-t z z1 z4) z)))
      (list (list (vector 0.0 0.0 z1)
                  (vector (solve-t z z1 z2) 0.0 z)
                  (vector 0.0 (solve-t z z1 z4) z))
            (list (vector 1.0 (solve-t z z2 z3) z)
                  (vector 1.0 1.0 z3)
                  (vector (solve-t z z4 z3) 1.0 z)))))

(define high-polys0101 (rotate-high-facet high-polys1010))

;(: heights->high-polys (FType Polygons))
(define heights->high-polys
  (make-marching-squares
   high-polys0000 high-polys0001 high-polys0010 high-polys0011
   high-polys0100 high-polys0101 high-polys0110 high-polys0111
   high-polys1000 high-polys1001 high-polys1010 high-polys1011
   high-polys1100 high-polys1101 high-polys1110 high-polys1111))

;(: heights->low-polys (FType Polygons))
;; Polygons on the low side of a contour are the relative complement of those
;; on the high side of the contour (relative to the whole facet).
(define heights->low-polys
  (make-marching-squares
   high-polys1111 high-polys1110 high-polys1101 high-polys1100
   high-polys1011 high-polys1010 high-polys1001 high-polys1000
   high-polys0111 high-polys0110 high-polys0101 high-polys0100
   high-polys0011 high-polys0010 high-polys0001 high-polys0000))

;; =============================================================================
;; Isoband marching squares: polygonizes contour between two isoline values

(define ((rotate-mid-facet f) za zb z1 z2 z3 z4)
  (map (λ (poly) (map unrotate-vec poly))
       (f za zb z2 z3 z4 z1)))

(define ((mirror-x-mid-facet f) za zb z1 z2 z3 z4)
  (map (λ (poly) (map mirror-x-vec poly))
       (f za zb z2 z1 z4 z3)))

(define ((mirror-y-mid-facet f) za zb z1 z2 z3 z4)
  (map (λ (poly) (map mirror-y-vec poly))
       (f za zb z4 z3 z2 z1)))

;; -----------------------------------------------------------------------------
;; all corners same

(define (mid-polys0000 za zb z1 z2 z3 z4) empty)
(define (mid-polys1111 za zb z1 z2 z3 z4) (list 'full))
(define (mid-polys2222 za zb z1 z2 z3 z4) empty)

;; -----------------------------------------------------------------------------
;; single triangle

(define (mid-polys1000 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t za z1 z2) 0.0 za)
              (vector 0.0 (solve-t za z1 z4) za))))

(define mid-polys0100 (rotate-mid-facet mid-polys1000))
(define mid-polys0010 (rotate-mid-facet mid-polys0100))
(define mid-polys0001 (rotate-mid-facet mid-polys0010))

(define (mid-polys1222 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define mid-polys2122 (rotate-mid-facet mid-polys1222))
(define mid-polys2212 (rotate-mid-facet mid-polys2122))
(define mid-polys2221 (rotate-mid-facet mid-polys2212))

;; -----------------------------------------------------------------------------
;; single trapezoid

(define (mid-polys2000 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t zb z1 z2) 0.0 zb)
              (vector (solve-t za z1 z2) 0.0 za)
              (vector 0.0 (solve-t za z1 z4) za)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define mid-polys0200 (rotate-mid-facet mid-polys2000))
(define mid-polys0020 (rotate-mid-facet mid-polys0200))
(define mid-polys0002 (rotate-mid-facet mid-polys0020))

(define (mid-polys0222 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t za z1 z2) 0.0 za)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb)
              (vector 0.0 (solve-t za z1 z4) za))))

(define mid-polys2022 (rotate-mid-facet mid-polys0222))
(define mid-polys2202 (rotate-mid-facet mid-polys2022))
(define mid-polys2220 (rotate-mid-facet mid-polys2202))

;; -----------------------------------------------------------------------------
;; single rectangle

(define (mid-polys1100 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector 1.0 0.0 z2)
              (vector 1.0 (solve-t za z2 z3) za)
              (vector 0.0 (solve-t za z1 z4) za))))

(define mid-polys0110 (rotate-mid-facet mid-polys1100))
(define mid-polys0011 (rotate-mid-facet mid-polys0110))
(define mid-polys1001 (rotate-mid-facet mid-polys0011))

(define (mid-polys1122 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector 1.0 0.0 z2)
              (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define mid-polys2112 (rotate-mid-facet mid-polys1122))
(define mid-polys2211 (rotate-mid-facet mid-polys2112))
(define mid-polys1221 (rotate-mid-facet mid-polys2211))

(define (mid-polys0022 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 (solve-t za z1 z4) za)
              (vector 1.0 (solve-t za z2 z3) za)
              (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define mid-polys2002 (rotate-mid-facet mid-polys0022))
(define mid-polys2200 (rotate-mid-facet mid-polys2002))
(define mid-polys0220 (rotate-mid-facet mid-polys2200))

;; -----------------------------------------------------------------------------
;; single pentagon

(define (mid-polys0111 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t za z1 z2) 0.0 za)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector 0.0 1.0 z4)
              (vector 0.0 (solve-t za z1 z4) za))))

(define mid-polys1011 (rotate-mid-facet mid-polys0111))
(define mid-polys1101 (rotate-mid-facet mid-polys1011))
(define mid-polys1110 (rotate-mid-facet mid-polys1101))

(define (mid-polys2111 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector 0.0 1.0 z4)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define mid-polys1211 (rotate-mid-facet mid-polys2111))
(define mid-polys1121 (rotate-mid-facet mid-polys1211))
(define mid-polys1112 (rotate-mid-facet mid-polys1121))

(define (mid-polys1002 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t za z1 z2) 0.0 za)
              (vector (solve-t za z4 z3) 1.0 za)
              (vector (solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define mid-polys2100 (rotate-mid-facet mid-polys1002))
(define mid-polys0210 (rotate-mid-facet mid-polys2100))
(define mid-polys0021 (rotate-mid-facet mid-polys0210))

(define (mid-polys1220 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector (solve-t zb z4 z3) 1.0 zb)
              (vector (solve-t za z4 z3) 1.0 za)
              (vector 0.0 (solve-t za z1 z4) za))))

(define mid-polys0122 (rotate-mid-facet mid-polys1220))
(define mid-polys2012 (rotate-mid-facet mid-polys0122))
(define mid-polys2201 (rotate-mid-facet mid-polys2012))

(define (mid-polys1200 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 1.0 (solve-t za z2 z3) za)
              (vector 0.0 (solve-t za z1 z4) za))))

(define mid-polys0120 (rotate-mid-facet mid-polys1200))
(define mid-polys0012 (rotate-mid-facet mid-polys0120))
(define mid-polys2001 (rotate-mid-facet mid-polys0012))

(define (mid-polys1022 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t za z1 z2) 0.0 za)
              (vector 1.0 (solve-t za z2 z3) za)
              (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define mid-polys2102 (rotate-mid-facet mid-polys1022))
(define mid-polys2210 (rotate-mid-facet mid-polys2102))
(define mid-polys0221 (rotate-mid-facet mid-polys2210))

;; -----------------------------------------------------------------------------
;; single hexagon

(define (mid-polys0112 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t za z1 z2) 0.0 za)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector (solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb)
              (vector 0.0 (solve-t za z1 z4) za))))

(define mid-polys2011 (rotate-mid-facet mid-polys0112))
(define mid-polys1201 (rotate-mid-facet mid-polys2011))
(define mid-polys1120 (rotate-mid-facet mid-polys1201))

(define (mid-polys2110 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector (solve-t za z4 z3) 1.0 za)
              (vector 0.0 (solve-t za z1 z4) za)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define mid-polys0211 (rotate-mid-facet mid-polys2110))
(define mid-polys1021 (rotate-mid-facet mid-polys0211))
(define mid-polys1102 (rotate-mid-facet mid-polys1021))

(define (mid-polys0121 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t za z1 z2) 0.0 za)
              (vector 1.0 0.0 z2)
              (vector 1.0 (solve-t zb z2 z3) zb)
              (vector (solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 1.0 z4)
              (vector 0.0 (solve-t za z1 z4) za))))

(define mid-polys1012 (rotate-mid-facet mid-polys0121))
(define mid-polys2101 (rotate-mid-facet mid-polys1012))
(define mid-polys1210 (rotate-mid-facet mid-polys2101))

;; -----------------------------------------------------------------------------
;; 6-sided saddle

(define (mid-polys10100 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t za z1 z2) 0.0 za)
              (vector 0.0 (solve-t za z1 z4) za))
        (list (vector 1.0 1.0 z3)
              (vector (solve-t za z4 z3) 1.0 za)
              (vector 1.0 (solve-t za z2 z3) za))))

(define (mid-polys10101 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t za z1 z2) 0.0 za)
              (vector 1.0 (solve-t za z2 z3) za)
              (vector 1.0 1.0 z3)
              (vector (solve-t za z4 z3) 1.0 za)
              (vector 0.0 (solve-t za z1 z4) za))))

(define (mid-polys1010 za zb z1 z2 z3 z4)
  (define z5 (flavg4 z1 z2 z3 z4))
  (cond [(z5 . < . za)  (mid-polys10100 za zb z1 z2 z3 z4)]
        ; (z5 . >= . zb) is impossible
        [else           (mid-polys10101 za zb z1 z2 z3 z4)]))

(define mid-polys0101 (rotate-mid-facet mid-polys1010))

(define (mid-polys1212-2 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb))
        (list (vector 1.0 1.0 z3)
              (vector (solve-t zb z4 z3) 1.0 zb)
              (vector 1.0 (solve-t zb z2 z3) zb))))

(define (mid-polys1212-1 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 1.0 1.0 z3)
              (vector (solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define (mid-polys1212 za zb z1 z2 z3 z4)
  (define z5 (flavg4 z1 z2 z3 z4))
  (cond [(z5 . >= . zb)  (mid-polys1212-2 za zb z1 z2 z3 z4)]
        ; (z5 . < . za) is impossible
        [else            (mid-polys1212-1 za zb z1 z2 z3 z4)]))

(define mid-polys2121 (rotate-mid-facet mid-polys1212))

;; -----------------------------------------------------------------------------
;; 7-sided saddle

(define (mid-polys0212-1 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t za z1 z2) 0.0 za)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 1.0 1.0 z3)
              (vector (solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb)
              (vector 0.0 (solve-t za z1 z4) za))))

(define (mid-polys0212-2 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t za z1 z2) 0.0 za)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb)
              (vector 0.0 (solve-t za z1 z4) za))
        (list (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 1.0 1.0 z3)
              (vector (solve-t zb z4 z3) 1.0 zb))))

(define (mid-polys0212 za zb z1 z2 z3 z4)
  (define z5 (flavg4 z1 z2 z3 z4))
  (cond [(z5 . < . zb)  (mid-polys0212-1 za zb z1 z2 z3 z4)]
        ; handling (z5 . < . za) separately results in a non-convex polygon
        [else           (mid-polys0212-2 za zb z1 z2 z3 z4)]))

(define mid-polys2021 (rotate-mid-facet mid-polys0212))
(define mid-polys1202 (rotate-mid-facet mid-polys2021))
(define mid-polys2120 (rotate-mid-facet mid-polys1202))

(define (mid-polys2010-1 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t zb z1 z2) 0.0 zb)
              (vector (solve-t za z1 z2) 0.0 za)
              (vector 1.0 (solve-t za z2 z3) za)
              (vector 1.0 1.0 z3)
              (vector (solve-t za z4 z3) 1.0 za)
              (vector 0.0 (solve-t za z1 z4) za)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define (mid-polys2010-0 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t zb z1 z2) 0.0 zb)
              (vector (solve-t za z1 z2) 0.0 za)
              (vector 0.0 (solve-t za z1 z4) za)
              (vector 0.0 (solve-t zb z1 z4) zb))
        (list (vector 1.0 (solve-t za z2 z3) za)
              (vector 1.0 1.0 z3)
              (vector (solve-t za z4 z3) 1.0 za))))

(define (mid-polys2010 za zb z1 z2 z3 z4)
  (define z5 (flavg4  z1 z2 z3 z4))
  (cond [(z5 . >= . za)  (mid-polys2010-1 za zb z1 z2 z3 z4)]
        ; handling (z5 . >= . zb) separately results in a non-convex polygon
        [else            (mid-polys2010-0 za zb z1 z2 z3 z4)]))

(define mid-polys0201 (rotate-mid-facet mid-polys2010))
(define mid-polys1020 (rotate-mid-facet mid-polys0201))
(define mid-polys0102 (rotate-mid-facet mid-polys1020))

;; -----------------------------------------------------------------------------
;; 8-sided saddle

(define (mid-polys0202-0 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t za z1 z2) 0.0 za)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 1.0 (solve-t za z2 z3) za))
        (list (vector 0.0 (solve-t za z1 z4) za)
              (vector (solve-t za z4 z3) 1.0 za)
              (vector (solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb))))

(define (mid-polys0202-1 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t za z1 z2) 0.0 za)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 1.0 (solve-t za z2 z3) za)
              (vector (solve-t za z4 z3) 1.0 za)
              (vector (solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb)
              (vector 0.0 (solve-t za z1 z4) za))))

(define (mid-polys0202-2 za zb z1 z2 z3 z4)
  (list (list (vector (solve-t za z1 z2) 0.0 za)
              (vector (solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (solve-t zb z1 z4) zb)
              (vector 0.0 (solve-t za z1 z4) za))
        (list (vector 1.0 (solve-t zb z2 z3) zb)
              (vector 1.0 (solve-t za z2 z3) za)
              (vector (solve-t za z4 z3) 1.0 za)
              (vector (solve-t zb z4 z3) 1.0 zb))))
              
(define (mid-polys0202 za zb z1 z2 z3 z4)
  (define z5 (flavg4 z1 z2 z3 z4))
  (cond [(z5 . < . za)  (mid-polys0202-0 za zb z1 z2 z3 z4)]
        [(z5 . < . zb)  (mid-polys0202-1 za zb z1 z2 z3 z4)]
        [else           (mid-polys0202-2 za zb z1 z2 z3 z4)]))

(define mid-polys2020 (rotate-mid-facet mid-polys0202))

#|
(printf "(define mid-polys-dispatch-table~n")
(printf "  (vector ")
(for* ([t1  (in-range 3)]
       [t2  (in-range 3)]
       [t3  (in-range 3)])
  (printf "~n  ")
  (for ([t4  (in-range 3)])
    (printf " mid-polys~a~a~a~a" t1 t2 t3 t4)))
(printf "))")
|#

(define mid-polys-dispatch-table
  (vector mid-polys0000 mid-polys0001 mid-polys0002 
          mid-polys0010 mid-polys0011 mid-polys0012 
          mid-polys0020 mid-polys0021 mid-polys0022 
          mid-polys0100 mid-polys0101 mid-polys0102 
          mid-polys0110 mid-polys0111 mid-polys0112 
          mid-polys0120 mid-polys0121 mid-polys0122 
          mid-polys0200 mid-polys0201 mid-polys0202 
          mid-polys0210 mid-polys0211 mid-polys0212 
          mid-polys0220 mid-polys0221 mid-polys0222 
          mid-polys1000 mid-polys1001 mid-polys1002 
          mid-polys1010 mid-polys1011 mid-polys1012 
          mid-polys1020 mid-polys1021 mid-polys1022 
          mid-polys1100 mid-polys1101 mid-polys1102 
          mid-polys1110 mid-polys1111 mid-polys1112 
          mid-polys1120 mid-polys1121 mid-polys1122 
          mid-polys1200 mid-polys1201 mid-polys1202 
          mid-polys1210 mid-polys1211 mid-polys1212 
          mid-polys1220 mid-polys1221 mid-polys1222 
          mid-polys2000 mid-polys2001 mid-polys2002 
          mid-polys2010 mid-polys2011 mid-polys2012 
          mid-polys2020 mid-polys2021 mid-polys2022 
          mid-polys2100 mid-polys2101 mid-polys2102 
          mid-polys2110 mid-polys2111 mid-polys2112 
          mid-polys2120 mid-polys2121 mid-polys2122 
          mid-polys2200 mid-polys2201 mid-polys2202 
          mid-polys2210 mid-polys2211 mid-polys2212 
          mid-polys2220 mid-polys2221 mid-polys2222))

(define (heights->mid-polys za zb z1 z2 z3 z4)
  (define t1 (if (z1 . fl< . za) 0 (if (z1 . fl< . zb) 1 2)))
  (define t2 (if (z2 . fl< . za) 0 (if (z2 . fl< . zb) 1 2)))
  (define t3 (if (z3 . fl< . za) 0 (if (z3 . fl< . zb) 1 2)))
  (define t4 (if (z4 . fl< . za) 0 (if (z4 . fl< . zb) 1 2)))
  (define facet-num
    (fx+ (fx+ (fx+ (fx* (fx* (fx* t1 3) 3) 3)
                   (fx* (fx* t2 3) 3))
              (fx* t3 3))
         t4))
  (define f (vector-ref mid-polys-dispatch-table facet-num))
  (f za zb z1 z2 z3 z4))
