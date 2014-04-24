#lang typed/racket/base

(require racket/list
         racket/match
         racket/promise
         math/flonum
         "split.rkt")

(provide
 ;; BSP shapes
 (struct-out points)
 (struct-out line)
 (struct-out poly)
 (struct-out lines)
 BSP-Shape
 (rename-out [shape? bsp-shape?])
 ;; BSP tree
 (struct-out bsp-node)
 (struct-out bsp-leaf)
 BSP-Tree
 ;; BSP tree operations
 build-bsp-tree
 bsp-tree-insert)

;; ===================================================================================================
;; Shape types

;; Parent type, not exported as a structure
(struct shape ([data : Any]) #:transparent)

(struct points shape ([vertices : (Listof FlVector)])
  #:transparent)

(struct poly shape ([vertices : (Listof FlVector)]
                    [lines? : (Listof Boolean)]
                    [normal : FlVector])
  #:transparent)

(struct line shape ([start : FlVector]
                    [end : FlVector])
  #:transparent)

(struct lines shape ([vertices : (Listof FlVector)])
  #:transparent)

(define-type BSP-Shape shape)

;; ===================================================================================================
;; BSP tree type

(struct: bsp-node ([plane : FlVector]
                   [neg : BSP-Tree]
                   [pos : BSP-Tree])
  #:transparent)

(struct: bsp-leaf ([shapes : (Listof BSP-Shape)])
  #:transparent)

(define-type BSP-Tree (U bsp-node bsp-leaf))

(: bsp-tree-size (-> BSP-Tree Natural))
(define (bsp-tree-size bsp)
  (match bsp
    [(bsp-node _ neg pos)  (+ (bsp-tree-size neg) (bsp-tree-size pos))]
    [(bsp-leaf shapes)  (length shapes)]))

;; ===================================================================================================

(define eps 1e-14)

(: flnonpos? (-> Flonum Boolean))
(define (flnonpos? x)
  (<= x eps))

(: flnonneg? (-> Flonum Boolean))
(define (flnonneg? x)
  (>= x (- eps)))

;; ===================================================================================================
;; Bin shapes: on the negative side of a plane, on the plane, or on the positive side

(: bin-bsp-points (-> points FlVector (Values (Listof BSP-Shape) (Listof BSP-Shape))))
(define (bin-bsp-points s plane)
  (match-define (points data vs) s)
  (define ds (map (λ ([v : FlVector]) (point3d-plane-dist v plane)) vs))
  (define-values (neg-vs pos-vs)
    (for/fold ([neg-vs : (Listof FlVector)  empty]
               [pos-vs : (Listof FlVector)  empty]
               ) ([v  (in-list vs)])
      (define d (point3d-plane-dist v plane))
      (if (d . >= . 0.0)
          (values neg-vs (cons v pos-vs))
          (values (cons v neg-vs) pos-vs))))
  (values (if (empty? neg-vs) empty (list (points data neg-vs)))
          (if (empty? pos-vs) empty (list (points data pos-vs)))))

(: bin-bsp-line (-> line FlVector Boolean (Values (Listof BSP-Shape) (Listof BSP-Shape))))
(define (bin-bsp-line s plane disjoint?)
  (match-define (line data v1 v2) s)
  (define d1 (point3d-plane-dist v1 plane))
  (define d2 (point3d-plane-dist v2 plane))
  (cond [(and (flnonneg? d1) (flnonneg? d2))  (values empty (list s))]
        [(and (flnonpos? d1) (flnonpos? d2))  (values (list s) empty)]
        [disjoint?  (values empty empty)]
        [(<= d1 0.0)
         (define v (split-line3d v1 v2 plane))
         (values (list (line data v1 v)) (list (line data v v2)))]
        [else
         (define v (split-line3d v1 v2 plane))
         (values (list (line data v v2)) (list (line data v1 v)))]))

(: bin-bsp-poly (-> poly FlVector Boolean (Values (Listof BSP-Shape) (Listof BSP-Shape))))
(define (bin-bsp-poly s plane disjoint?)
  (match-define (poly data vs ls norm) s)
  (define ds (map (λ ([v : FlVector]) (point3d-plane-dist v plane)) vs))
  (cond [(andmap flnonneg? ds)  (values empty (list s))]
        [(andmap flnonpos? ds)  (values (list s) empty)]
        [disjoint?  (values empty empty)]
        [else
         (define-values (vs1 ls1 vs2 ls2 ok?) (split-polygon3d vs ls plane))
         (cond [ok?  (values (if ((length vs2) . < . 3) empty (list (poly data vs2 ls2 norm)))
                             (if ((length vs1) . < . 3) empty (list (poly data vs1 ls1 norm))))]
               [else
                (define-values (vs1 ls1 vs2 ls2) (polygon3d-divide vs ls))
                (define-values (neg-ss1 pos-ss1) (bin-bsp-poly (poly data vs1 ls1 norm) plane #f))
                (define-values (neg-ss2 pos-ss2) (bin-bsp-poly (poly data vs2 ls2 norm) plane #f))
                (values (append neg-ss1 neg-ss2) (append pos-ss1 pos-ss2))])]))

(: bin-bsp-lines (-> lines FlVector Boolean (Values (Listof BSP-Shape) (Listof BSP-Shape))))
(define (bin-bsp-lines s plane disjoint?)
  (match-define (lines data vs) s)
  (define ds (map (λ ([v : FlVector]) (point3d-plane-dist v plane)) vs))
  (cond [(andmap flnonneg? ds)  (values empty (list s))]
        [(andmap flnonpos? ds)  (values (list s) empty)]
        [disjoint?  (values empty empty)]
        [else
         (: vertices->lines (-> (Listof (Listof FlVector)) (Listof BSP-Shape)))
         (define (vertices->lines vss)
           (append*
            (map (λ ([vs : (Listof FlVector)])
                   (define n (length vs))
                   (cond [(n . < . 2)  empty]
                         [(n . = . 2)  (list (line data (first vs) (second vs)))]
                         [else  (list (lines data vs))]))
                 vss)))
         
         (define-values (vss1 vss2) (split-lines3d vs plane))
         (values (vertices->lines vss2) (vertices->lines vss1))]))

(: bin-shapes (-> (Listof BSP-Shape) FlVector Boolean
                  (Values (U #f (Listof BSP-Shape)) (U #f (Listof BSP-Shape)))))
(define (bin-shapes ss plane disjoint?)
  (let loop ([ss ss] [neg-ss  : (Listof BSP-Shape)  empty]
                     [pos-ss  : (Listof BSP-Shape)  empty])
    (cond [(empty? ss)  (values neg-ss pos-ss)]
          [else
           (define s (first ss))
           (define-values (new-neg-ss new-pos-ss)
             (cond [(points? s)  (bin-bsp-points s plane)]
                   [(line? s)    (bin-bsp-line   s plane disjoint?)]
                   [(poly? s)    (bin-bsp-poly   s plane disjoint?)]
                   [(lines? s)   (bin-bsp-lines  s plane disjoint?)]
                   [else  (raise-argument-error 'bin-shapes "known shape" s)]))
           (cond [(and (empty? new-neg-ss) (empty? new-pos-ss))  (values #f #f)]
                 [else  (loop (rest ss)
                              (append new-neg-ss neg-ss)
                              (append new-pos-ss pos-ss))])])))

;; ===================================================================================================
;; Build BSP tree

(: bsp-polys->vertices (-> (Listof poly) (Listof FlVector)))
(define (bsp-polys->vertices ss)
  (append* (map poly-vertices ss)))

(: bsp-lines->vertices (-> (Listof line) (Listof FlVector)))
(define (bsp-lines->vertices ss)
  (append (map line-start ss) (map line-end ss)))

(: coordinate-min-max (-> (Listof FlVector) Index (Values Flonum Flonum)))
(define (coordinate-min-max vs i)
  (for/fold ([x-min : Flonum  +inf.0] [x-max : Flonum  -inf.0]) ([v  (in-list vs)])
    (define x (flvector-ref v i))
    (values (min x x-min) (max x x-max))))

(struct: interval ([min : Flonum] [max : Flonum] [weight : Natural]) #:transparent)

(: shapes->intervals (-> (Listof BSP-Shape) Index (Listof interval)))
(define (shapes->intervals ss i)
  (for/list: ([s  (in-list ss)])
    (match s
      [(points _ vs)
       (define-values (x-min x-max) (coordinate-min-max vs i))
       (interval x-min x-max 1)]
      [(line _ v1 v2)
       (define x1 (flvector-ref v1 i))
       (define x2 (flvector-ref v2 i))
       (interval (min x1 x2) (max x1 x2) 1)]
      [(poly _data vs ls _norm)
       (define-values (x-min x-max) (coordinate-min-max vs i))
       (interval x-min x-max 1)]
      [(lines _ vs)
       (define-values (x-min x-max) (coordinate-min-max vs i))
       (interval x-min x-max 1)])))

(: interval-list-union (-> (Listof interval) (Listof interval) (Listof interval)))
(define (interval-list-union I1 I2)
  (cond
    [(empty? I1)  I2]
    [(empty? I2)  I1]
    [else
     (match-define (interval a1 b1 w1) (first I1))
     (match-define (interval a2 b2 w2) (first I2))
     (cond
       [(b1 . <= . a2)
        ;; ------
        ;;        ------
        (cons (first I1) (interval-list-union (rest I1) I2))]
       [(b2 . <= . a1)
        ;;        ------
        ;; ------
        (cons (first I2) (interval-list-union I1 (rest I2)))]
       [(a1 . < . a2)
        (cond [(b2 . < . b1)
               ;; ------
               ;;   --
               (define I (interval a1 b1 (+ w1 w2)))
               (interval-list-union (cons I (rest I1)) (rest I2))]
              [else
               ;; ------           ------
               ;;    ------   or      ---
               (define I (interval a1 b2 (+ w1 w2)))
               (interval-list-union (rest I1) (cons I (rest I2)))])]
       [else
        (cond [(b2 . < . b1)
               ;;    ------        ------
               ;; ------      or   ---
               (define I (interval a2 b1 (+ w1 w2)))
               (interval-list-union (cons I (rest I1)) (rest I2))]
              [else
               ;;   --             ---        ---           ------
               ;; ------   or   ------   or   ------   or   ------
               (define I (interval a2 b2 (+ w1 w2)))
               (interval-list-union (rest I1) (cons I (rest I2)))])])]))

(: interval-split (-> (Listof interval) (Option Flonum)))
(define (interval-split all-ivls)
  (: ivls (Listof interval))
  (define ivls
    (let loop ([ivls  all-ivls])
      (cond [(empty? ivls)  empty]
            [(empty? (rest ivls))  ivls]
            [else
             (define n (length ivls))
             (define-values (ivls1 ivls2) (split-at ivls (quotient n 2)))
             (interval-list-union (loop ivls1) (loop ivls2))])))
  
  (cond [(empty? ivls)  #f]
        [(empty? (rest ivls))  #f]
        [else
         (define total-w (length all-ivls))
         (define-values (best-x best-w _)
           (for/fold ([best-x : Flonum  (interval-min (first ivls))]
                      [best-w : Integer  total-w]
                      [left-w : Integer  0]
                      ) ([ivl  (in-list ivls)])
             (define max-w (max left-w (- total-w left-w)))
             (define new-left-w (+ (interval-weight ivl) left-w))
             (cond [(max-w . < . best-w)
                    (values (interval-min ivl) max-w new-left-w)]
                   [else
                    (values best-x best-w new-left-w)])))
         best-x]))

(struct: axis ([index : Index] [size : Flonum] [min : Flonum] [max : Flonum] [mid : Flonum])
  #:transparent)

(: vertices->axes (-> (Listof FlVector) (Listof axis)))
(define (vertices->axes vs)
  (for/list ([i  (in-list '(0 1 2))])
    (define-values (x-min x-max) (coordinate-min-max vs i))
    (axis i (- x-max x-min) x-min x-max (* 0.5 (+ x-min x-max)))))

(: axial-plane (-> Index Flonum FlVector))
(define (axial-plane i x)
  (define plane (flvector 0.0 0.0 0.0 (- x)))
  (flvector-set! plane i 1.0)
  plane)

(: flvector-plane (-> FlVector FlVector FlVector FlVector))
(define (flvector-plane v1 v2 v3)
  (define x2 (flvector-ref v2 0))
  (define y2 (flvector-ref v2 1))
  (define z2 (flvector-ref v2 2))
  (define dx1 (- (flvector-ref v1 0) x2))
  (define dy1 (- (flvector-ref v1 1) y2))
  (define dz1 (- (flvector-ref v1 2) z2))
  (define dx3 (- (flvector-ref v3 0) x2))
  (define dy3 (- (flvector-ref v3 1) y2))
  (define dz3 (- (flvector-ref v3 2) z2))
  (define a (- (* dy1 dz3) (* dz1 dy3)))
  (define b (- (* dz1 dx3) (* dx1 dz3)))
  (define c (- (* dx1 dy3) (* dy1 dx3)))
  (define d (- (+ (* a x2) (* b y2) (* c z2))))
  (define n (flsqrt (+ (* a a) (* b b) (* c c))))
  (flvector (/ a n) (/ b n) (/ c n) (/ d n)))

(: line3d-planes (-> FlVector FlVector (Listof FlVector)))
(define (line3d-planes v1 v2)
  (define planes
    (list (flvector-plane v1 v2 (flvector+ v1 (flvector 1.0 0.0 0.0)))
          (flvector-plane v1 v2 (flvector+ v1 (flvector 0.0 1.0 0.0)))
          (flvector-plane v1 v2 (flvector+ v1 (flvector 0.0 0.0 1.0)))))
  (filter
   flvector?
   (for/list : (Listof (U #f FlVector)) ([plane  (in-list planes)])
     (cond [(and (flrational? (flvector-ref plane 0))
                 (flrational? (flvector-ref plane 1))
                 (flrational? (flvector-ref plane 2))
                 (flrational? (flvector-ref plane 3)))
            plane]
           [else  #f]))))

(: bsp-line-planes (-> line (Listof FlVector)))
(define (bsp-line-planes s)
  (match-define (line _ v1 v2) s)
  (line3d-planes v1 v2))

(: find-bounding-planes (-> (Listof FlVector) FlVector (Listof FlVector)))
(define (find-bounding-planes vs normal)
  (define n (length vs))
  (cond [(n . < . 3)  empty]
        [(n . = . 3)  (list (flvector-plane (first vs) (second vs) (third vs)))]
        [else
         (define a (flvector-ref normal 0))
         (define b (flvector-ref normal 1))
         (define c (flvector-ref normal 2))
         ;; Pilot plane
         (define v1 (first vs))
         (define d1 (- (+ (* a (flvector-ref v1 0))
                          (* b (flvector-ref v1 1))
                          (* c (flvector-ref v1 2)))))
         ;; Find min and max signed distances from pilot plane
         (define-values (dmin dmax)
           (for/fold ([dmin : Flonum  0.0] [dmax : Flonum  0.0]) ([v  (in-list (rest vs))])
             (define d (+ (* a (flvector-ref v 0))
                          (* b (flvector-ref v 1))
                          (* c (flvector-ref v 2))
                          d1))
             (values (min dmin d) (max dmax d))))
         ;; Return min and max plane
         (list (flvector a b c (- d1 dmin))
               (flvector a b c (- d1 dmax)))]))

(: sort-planes (-> (Listof FlVector) FlVector (Listof FlVector)))
;; Sort planes by absolute distance from a central point
(define (sort-planes planes center)
  ((inst sort FlVector Flonum) planes <
                               #:key (λ ([plane : FlVector])
                                       (abs (point3d-plane-dist center plane)))
                               #:cache-keys? #t))

(: bsp-poly-triangulate (-> poly (Listof poly)))
(define (bsp-poly-triangulate s)
  (match-define (poly data vs ls norm) s)
  (define-values (vss lss) (polygon3d-triangulate vs ls))
  (map (λ ([vs : (Listof FlVector)] [ls : (Listof Boolean)]) (poly data vs ls norm))
       vss lss))

(: bsp-poly-divide (-> poly (Listof poly)))
(define (bsp-poly-divide s)
  (match-define (poly data vs ls norm) s)
  (define-values (vs1 ls1 vs2 ls2) (polygon3d-divide vs ls))
  (cond [(empty? vs2)  (list s)]
        [else  (list (poly data vs1 ls1 norm)
                     (poly data vs2 ls2 norm))]))

(: triangulate-polygons (-> (Listof BSP-Shape) (Listof BSP-Shape)))
(define (triangulate-polygons ss)
  (append* (map (λ ([s : BSP-Shape])
                  (if (poly? s) (bsp-poly-triangulate s) (list s)))
                ss)))

(: bsp-poly-planes (-> poly (Listof FlVector)))
(define (bsp-poly-planes s)
  (define vs (poly-vertices s))
  (cond [(< (length vs) 3)  empty]
        [(= (length vs) 3)  (list (flvector-plane (first vs) (second vs) (third vs)))]
        [else
         (filter
          flvector?
          (for/list: : (Listof (U #f FlVector))
            ([v1  (in-list (append vs (take vs 2)))]
             [v2  (in-list (append (list (last vs)) vs (list (first vs))))]
             [v3  (in-list (append (take-right vs 2) vs))])
            (define plane (flvector-plane v1 v2 v3))
            (cond [(and (flrational? (flvector-ref plane 0))
                        (flrational? (flvector-ref plane 1))
                        (flrational? (flvector-ref plane 2))
                        (flrational? (flvector-ref plane 3)))
                   plane]
                  [else  #f])))]))

(: canonicalize-shapes (-> (Listof BSP-Shape) (Listof BSP-Shape)))
(define (canonicalize-shapes ss)
  (append*
   (for/list : (Listof (Listof BSP-Shape)) ([s  (in-list ss)])
     (match s
       [(points _ vs)
        (if (empty? vs) empty (list s))]
       [(line _ v1 v2) 
        (if (equal? v1 v2) empty (list s))]
       [(poly data vs ls norm)
        (let-values ([(vs ls)  (canonical-polygon3d vs ls)])
          (if ((length vs) . < . 3) empty (list (poly data vs ls norm))))]
       [(lines data vs)
        (let ([vs  (canonical-lines3d vs)])
          (define n (length vs))
          (cond [(n . < . 2)  empty]
                [(n . = . 2)  (list (line data (first vs) (second vs)))]
                [else  (list (lines data vs))]))]))))

;; ===================================================================================================

(: build-bsp-tree (-> (Listof BSP-Shape) BSP-Tree))
(define (build-bsp-tree ss)
  (let* ([ss  (canonicalize-shapes ss)])
    (build-bsp-tree* ss)))

(: bad-plane? (-> FlVector Boolean))
(define (bad-plane? plane)
  (define a (flvector-ref plane 0))
  (define b (flvector-ref plane 1))
  (define c (flvector-ref plane 2))
  (define d (flvector-ref plane 3))
  (or (and ((abs a) . < . 1e-16) ((abs b) . < . 1e-16) ((abs c) . < . 1e-16))
      (not (flrational? a)) (not (flrational? b)) (not (flrational? c)) (not (flrational? d))))

(: try-bsp-split (-> (Listof BSP-Shape) FlVector Boolean (-> (U #f BSP-Tree)) (U #f BSP-Tree)))
(define (try-bsp-split ss plane disjoint? k)
  (cond [(bad-plane? plane)  (k)]
        [else
         (define-values (neg-ss pos-ss) (bin-shapes ss plane disjoint?))
         (cond [(not (and neg-ss pos-ss))  (k)]
               [(empty? neg-ss)  (k)]
               [(empty? pos-ss)  (k)]
               [(and disjoint? ((+ (length neg-ss) (length pos-ss)) . > . (length ss)))  (k)]
               [else  (bsp-node plane (build-bsp-tree* neg-ss) (build-bsp-tree* pos-ss))])]))

(: try-bsp-split/axial-planes (-> (Listof BSP-Shape) (Listof axis) (U #f BSP-Tree)))
(define (try-bsp-split/axial-planes ss axes)
  (define sorted-axes ((inst sort axis Flonum) axes > #:key axis-size))
  (let loop ([axes sorted-axes])
    (cond [(empty? axes)  #f]
          [else
           (define i (axis-index (first axes)))
           (define split (interval-split (shapes->intervals ss i)))
           (cond [split  (define plane (axial-plane i split))
                         (try-bsp-split ss plane #t (λ () (loop (rest axes))))]
                 [else  (loop (rest axes))])])))

(: try-bsp-split/planes (-> (Listof BSP-Shape) (Listof FlVector) Boolean (U #f BSP-Tree)))
(define (try-bsp-split/planes ss planes disjoint?)
  ;; Try each plane in order
  (let loop ([planes planes])
    (cond [(empty? planes)  #f]
          [else  (try-bsp-split ss (first planes) disjoint?
                                (λ () (try-bsp-split ss (flvector- (first planes)) disjoint?
                                                     (λ () (loop (rest planes))))))])))

(: try-bsp-split/bounding-planes (-> (Listof BSP-Shape) (Listof poly) FlVector
                                          (U #f BSP-Tree)))
;; Tries splitting using polygons' bounding planes
;; Bounding planes aren't necessarily coplanar with any triangle in a polygon, so
;; splitting won't always make progress; we therefore split only disjointly
(define (try-bsp-split/bounding-planes ss ps center)
  (cond [((length ps) . > . 5)  #f]
        [else
         (define vss (map poly-vertices ps))
         (define norms (map poly-normal ps))
         (define planes (sort-planes (append* (map find-bounding-planes vss norms)) center))
         (try-bsp-split/planes ss planes #t)]))

(: try-bsp-split/triangulating (-> (Listof BSP-Shape) (U #f BSP-Tree)))
;; Tries recurring on triangulated polygons
(define (try-bsp-split/triangulating ss)
  (define new-ss (triangulate-polygons ss))
  (cond [(= (length new-ss) (length ss))  #f]
        [else  (build-bsp-tree* new-ss)]))

;; ---------------------------------------------------------------------------------------------------

(: build-bsp-tree* (-> (Listof BSP-Shape) BSP-Tree))
(define (build-bsp-tree* ss)
  (cond
    [(or (empty? ss) (empty? (rest ss)))  (bsp-leaf ss)]
    [else
     (let* ([bsp  #f]
            [bsp  (if bsp bsp (build-bsp-tree*/poly ss))]
            [bsp  (if bsp bsp (build-bsp-tree*/line ss))]
            [bsp  (if bsp bsp (build-bsp-tree*/axial ss))]
            [bsp  (if bsp bsp (bsp-leaf ss))])
       bsp)]))

;; ---------------------------------------------------------------------------------------------------
;; Phase 1: build by splitting on polygon faces

(: build-bsp-tree*/poly (-> (Listof BSP-Shape) (U #f BSP-Tree)))
(define (build-bsp-tree*/poly ss)
  (define ps (filter poly? ss))
  (cond
    [(empty? ps)  #f]
    [else
     (define axes (vertices->axes (bsp-polys->vertices ps)))
     (define center (list->flvector (map axis-mid axes)))
     
     ;; Planes defined by neighboring polygon vertices
     (define polygon-planes (delay (sort-planes (append* (map bsp-poly-planes ps)) center)))
     
     (: try-bsp-split/polygon-planes (-> Boolean (U #f BSP-Tree)))
     ;; Tries splitting using polygon-planes
     (define (try-bsp-split/polygon-planes disjoint?)
       (define planes (force polygon-planes))
       (cond [(and disjoint? ((length planes) . > . 10))  #f]
             [else  (try-bsp-split/planes ss planes disjoint?)]))
     
     (let* ([bsp  #f]
            [bsp  (if bsp bsp (try-bsp-split/axial-planes ss axes))]
            [bsp  (if bsp bsp (try-bsp-split/bounding-planes ss ps center))]
            [bsp  (if bsp bsp (try-bsp-split/polygon-planes #t))]
            [bsp  (if bsp bsp (try-bsp-split/polygon-planes #f))]
            [bsp  (if bsp bsp (try-bsp-split/triangulating ss))])
       bsp)]))

;; ---------------------------------------------------------------------------------------------------
;; Phase 2: build by splitting on planes aligned with axes and line segments

(: build-bsp-tree*/line (-> (Listof BSP-Shape) (U #f BSP-Tree)))
(define (build-bsp-tree*/line ss)
  (define ls (filter line? ss))
  (cond
    [(empty? ls)  #f]
    [else
     (define axes (vertices->axes (bsp-lines->vertices ls)))
     (define center (list->flvector (map axis-mid axes)))
     
     ;; Planes defined by line segments and basis vectors (i.e. one basis in normal is zero)
     (define line-planes (delay (sort-planes (append* (map bsp-line-planes ls)) center)))
     
     (: try-bsp-split/line-planes (-> Boolean (U #f BSP-Tree)))
     ;; Tries splitting using line-planes
     (define (try-bsp-split/line-planes disjoint?)
       (define planes (force line-planes))
       (cond [(and disjoint? ((length planes) . > . 10))  #f]
             [else  (try-bsp-split/planes ss planes disjoint?)]))
     
     (let* ([bsp  #f]
            [bsp  (if bsp bsp (try-bsp-split/axial-planes ss axes))]
            [bsp  (if bsp bsp (try-bsp-split/line-planes #t))]
            [bsp  (if bsp bsp (try-bsp-split/line-planes #f))])
       bsp)]))

;; ---------------------------------------------------------------------------------------------------
;; Phase 3: build by splitting on axis-aligned planes using lines and points vertices

(: build-bsp-tree*/axial (-> (Listof BSP-Shape) (U #f BSP-Tree)))
(define (build-bsp-tree*/axial ss)
  (define ls (filter lines? ss))
  (define ps (filter points? ss))
  (cond
    [(and (empty? ls) (empty? ps))  #f]
    [else
     (define axes (vertices->axes (append (append* (map lines-vertices ls))
                                          (append* (map points-vertices ps)))))
     (define center (list->flvector (map axis-mid axes)))
     
     (: try-nondisjoint-split (-> (U #f BSP-Tree)))
     (define (try-nondisjoint-split)
       (match-define (axis i size _mn _mx mid) (argmax axis-size axes))
       (cond [(size . < . 0.01)  #f]
             [else
              (define plane (axial-plane i mid))
              (try-bsp-split ss plane #f (λ () #f))]))
     
     (let* ([bsp  #f]
            [bsp  (if bsp bsp (try-bsp-split/axial-planes ss axes))]
            [bsp  (if bsp bsp (try-nondisjoint-split))])
       bsp)]))

;; ===================================================================================================
;; BSP tree insert

(: bsp-tree-insert (-> BSP-Tree (Listof BSP-Shape) BSP-Tree))
(define (bsp-tree-insert bsp ss)
  (bsp-tree-insert* bsp (canonicalize-shapes ss)))

(: bsp-tree-insert* (-> BSP-Tree (Listof BSP-Shape) BSP-Tree))
(define (bsp-tree-insert* bsp ss)
  (cond [(empty? ss)  bsp]
        [else
         (match bsp
           [(bsp-leaf other-ss)
            (build-bsp-tree* (append other-ss ss))]
           [(bsp-node plane neg pos)
            (define-values (neg-ss pos-ss) (bin-shapes ss plane #f))
            (if (and neg-ss pos-ss)
                (bsp-node plane (bsp-tree-insert* neg neg-ss) (bsp-tree-insert* pos pos-ss))
                (error 'bsp-tree-insert* "shouldn't happen"))])]))
