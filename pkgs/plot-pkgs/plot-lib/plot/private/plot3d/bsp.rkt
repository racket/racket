#lang typed/racket/base

(require racket/list
         racket/match
         racket/promise
         math/flonum
         "split.rkt")

(provide
 ;; BSP shapes
 (struct-out point)
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

(struct point shape ([vertex : FlVector])
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
                   [shapes : (Listof BSP-Shape)]
                   [left : BSP-Tree]
                   [right : BSP-Tree])
  #:transparent)

(struct: bsp-leaf ([shapes : (Listof BSP-Shape)])
  #:transparent)

(define-type BSP-Tree (U bsp-node bsp-leaf))

(: bsp-tree-size (-> BSP-Tree Natural))
(define (bsp-tree-size bsp)
  (match bsp
    [(bsp-node _ shapes left right)  (+ (length shapes) (bsp-tree-size left) (bsp-tree-size right))]
    [(bsp-leaf shapes)  (length shapes)]))

;; ===================================================================================================

(define eps 1e-14)

(: flzero? (-> Flonum Boolean))
(define (flzero? x)
  (<= (flabs x) eps))

(: flnonpos? (-> Flonum Boolean))
(define (flnonpos? x)
  (<= x eps))

(: flnonneg? (-> Flonum Boolean))
(define (flnonneg? x)
  (>= x (- eps)))

;; ===================================================================================================
;; Bin shapes: on the negative side of a plane, on the plane, or on the positive side

(: bin-bsp-point (-> point FlVector (Values (Listof BSP-Shape)
                                            (Listof BSP-Shape)
                                            (Listof BSP-Shape))))
(define (bin-bsp-point s plane)
  (define d (point3d-plane-dist (point-vertex s) plane))
  (cond [(flzero?   d)  (values empty (list s) empty)]
        [(flnonpos? d)  (values (list s) empty empty)]
        [else           (values empty empty (list s))]))

(: bin-bsp-line (-> line FlVector (Values (Listof BSP-Shape)
                                          (Listof BSP-Shape)
                                          (Listof BSP-Shape))))
(define (bin-bsp-line s plane)
  (match-define (line data v1 v2) s)
  (define d1 (point3d-plane-dist v1 plane))
  (define d2 (point3d-plane-dist v2 plane))
  (cond [(and (flzero?   d1) (flzero?   d2))  (values empty (list s) empty)]
        [(and (flnonpos? d1) (flnonpos? d2))  (values (list s) empty empty)]
        [(and (flnonneg? d1) (flnonneg? d2))  (values empty empty (list s))]
        [(<= d1 0.0)
         (define v (split-line3d v1 v2 plane))
         (values (list (line data v1 v)) empty (list (line data v v2)))]
        [else
         (define v (split-line3d v1 v2 plane))
         (values (list (line data v v2)) empty (list (line data v1 v)))]))

(: bin-bsp-poly (-> poly FlVector (Values (Listof BSP-Shape)
                                          (Listof BSP-Shape)
                                          (Listof BSP-Shape))))
(define (bin-bsp-poly s plane)
  (match-define (poly data vs ls norm) s)
  (define ds (map (λ ([v : FlVector]) (point3d-plane-dist v plane)) vs))
  (cond [(andmap flzero?   ds)  (values empty (list s) empty)]
        [(andmap flnonpos? ds)  (values (list s) empty empty)]
        [(andmap flnonneg? ds)  (values empty empty (list s))]
        [else
         (define-values (vs1 ls1 vs2 ls2 ok?) (split-polygon3d vs ls plane))
         (cond [ok?  (values (if ((length vs2) . < . 3) empty (list (poly data vs2 ls2 norm)))
                             empty
                             (if ((length vs1) . < . 3) empty (list (poly data vs1 ls1 norm))))]
               [else
                (define-values (vs1 ls1 vs2 ls2) (polygon3d-divide vs ls))
                (define-values (los1 mids1 his1) (bin-bsp-poly (poly data vs1 ls1 norm) plane))
                (define-values (los2 mids2 his2) (bin-bsp-poly (poly data vs2 ls2 norm) plane))
                (values (append los1 los2) (append mids1 mids2) (append his1 his2))])]))

(: bin-bsp-lines (-> lines FlVector (Values (Listof BSP-Shape)
                                            (Listof BSP-Shape)
                                            (Listof BSP-Shape))))
(define (bin-bsp-lines s plane)
  (match-define (lines data vs) s)
  (define ds (map (λ ([v : FlVector]) (point3d-plane-dist v plane)) vs))
  (cond [(andmap flzero?   ds)  (values empty (list s) empty)]
        [(andmap flnonpos? ds)  (values (list s) empty empty)]
        [(andmap flnonneg? ds)  (values empty empty (list s))]
        [else
         (: vertices->lines (-> (Listof (Listof FlVector)) (Listof lines)))
         (define (vertices->lines vss)
           (append*
            (map (λ ([vs : (Listof FlVector)])
                   (if ((length vs) . < . 2) empty (list (lines data vs))))
                 vss)))
         
         (define-values (vss1 vss2) (split-lines3d vs plane))
         (values (vertices->lines vss2) empty (vertices->lines vss1))]))

(: bin-shapes (-> (Listof BSP-Shape) FlVector (Values (Listof BSP-Shape)
                                                      (Listof BSP-Shape)
                                                      (Listof BSP-Shape))))
(define (bin-shapes ss plane)
  (for/fold: ([los  : (Listof BSP-Shape)  empty]
              [mids : (Listof BSP-Shape)  empty]
              [his  : (Listof BSP-Shape)  empty]
              ) ([s  (in-list ss)])
    (define-values (new-los new-mids new-his)
      (cond [(point? s)  (bin-bsp-point s plane)]
            [(line? s)   (bin-bsp-line  s plane)]
            [(poly? s)   (bin-bsp-poly  s plane)]
            [(lines? s)  (bin-bsp-lines s plane)]
            [else  (raise-argument-error 'bin-shapes "known shape" s)]))
    (values (append new-los los) (append new-mids mids) (append new-his his))))

;; ===================================================================================================
;; Build BSP tree

(: bsp-polys->vertices (-> (Listof poly) (Listof FlVector)))
(define (bsp-polys->vertices ss)
  (append* (map poly-vertices ss)))

(: shapes->intervals (-> (Listof BSP-Shape) Index (Listof (Pair Flonum Flonum))))
(define (shapes->intervals ss i)
  (for/list: ([s  (in-list ss)])
    (match s
      [(point _ v)
       (cons (flvector-ref v i) (flvector-ref v i))]
      [(line _ v1 v2)
       (define x1 (flvector-ref v1 i))
       (define x2 (flvector-ref v2 i))
       (cons (min x1 x2) (max x1 x2))]
      [(poly _data vs ls _norm)
       (define xs (map (λ ([v : FlVector]) (flvector-ref v i)) vs))
       (cons (apply min xs) (apply max xs))]
      [(lines _ vs)
       (define xs (map (λ ([v : FlVector]) (flvector-ref v i)) vs))
       (cons (apply min xs) (apply max xs))])))

(struct: interval ([min : Flonum] [max : Flonum] [weight : Natural]) #:transparent)

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

(: interval-split (-> (Listof (Pair Flonum Flonum)) (Option Flonum)))
(define (interval-split ps)
  (: ivls (Listof interval))
  (define ivls
    (let loop ([ivls  (map (λ ([p : (Pair Flonum Flonum)]) (interval (car p) (cdr p) 1)) ps)])
      (cond [(empty? ivls)  empty]
            [(empty? (rest ivls))  ivls]
            [else
             (define n (length ivls))
             (define-values (ivls1 ivls2) (split-at ivls (quotient n 2)))
             (interval-list-union (loop ivls1) (loop ivls2))])))
  
  (cond [(empty? ivls)  #f]
        [(empty? (rest ivls))  #f]
        [else
         (define total-w (length ps))
         (define-values (best-x best-w _)
           (for/fold ([best-x : Flonum  (interval-min (first ivls))]
                      [best-w : Integer  (length ps)]
                      [left-w : Integer  0]
                      ) ([ivl  (in-list ivls)])
             (define max-w (max left-w (- total-w left-w)))
             (define new-left-w (+ (interval-weight ivl) left-w))
             (cond [(max-w . < . best-w)
                    (values (interval-min ivl) max-w new-left-w)]
                   [else
                    (values best-x best-w new-left-w)])))
         best-x]))

(struct: axis ([index : Index] [size : Flonum] [min : Flonum] [max : Flonum])
  #:transparent)

(: vertices->axes (-> (Listof FlVector) (Listof axis)))
(define (vertices->axes vs)
  (for/list ([i  (in-list '(0 1 2))])
    (define xs (map (λ ([v : FlVector]) (flvector-ref v i)) vs))
    (define x-min (apply min xs))
    (define x-max (apply max xs))
    (axis i (- x-max x-min) x-min x-max)))

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

(: remove-null-shapes (-> (Listof BSP-Shape) (Listof BSP-Shape)))
(define (remove-null-shapes ss)
  (append*
   (for/list : (Listof (Listof BSP-Shape)) ([s  (in-list ss)])
     (match s
       [(? point?)  (list s)]
       [(line _ v1 v2) 
        (if (equal? v1 v2) empty (list s))]
       [(poly data vs ls norm)
        (let-values ([(vs ls)  (canonical-polygon3d vs ls)])
          (if ((length vs) . < . 3) empty (list (poly data vs ls norm))))]
       [(lines data vs)
        (let ([vs  (canonical-lines3d vs)])
          (if ((length vs) . < . 2) empty (list (lines data vs))))]))))

(: build-bsp-tree (-> (Listof BSP-Shape) BSP-Tree))
(define (build-bsp-tree ss)
  (let* ([ss  (remove-null-shapes ss)]
         ;[ss  (triangulate-polygons ss)]
         )
    (build-bsp-tree* ss)))

(: bsp-split (-> (Listof BSP-Shape) FlVector Boolean (-> (U #f BSP-Tree)) (U #f BSP-Tree)))
(define (bsp-split ss plane disjoint? k)
  (define-values (los mids his) (bin-shapes ss plane))
  (cond [(and (empty? los)  (empty? mids))  (k)]
        [(and (empty? mids) (empty? his))   (k)]
        [(and (empty? los)  (empty? his))   (bsp-leaf mids)]
        [(and disjoint? ((+ (length los) (length mids) (length his)) . > . (length ss)))  (k)]
        [else  (bsp-node plane mids (build-bsp-tree* los) (build-bsp-tree* his))]))

(: build-bsp-tree* (-> (Listof BSP-Shape) BSP-Tree))
(define (build-bsp-tree* ss)
  (define ps (filter poly? ss))
  (define vs (remove-duplicates (bsp-polys->vertices ps)))
  (cond [(empty? vs)  (bsp-leaf ss)]
        [(empty? (rest ss))  (bsp-leaf ss)]
        [else
         ;; Get axes sorted decreasing in size
         (define axes* (vertices->axes vs))
         (define axes ((inst sort axis Flonum) axes* > #:key axis-size))
         
         ;; Center point of bounding box containing the shapes
         (define center
           (list->flvector
            (map (λ ([a : axis]) (* 0.5 (+ (axis-min a) (axis-max a)))) axes*)))
         
         (define polygon-planes (delay (append* (map bsp-poly-planes ps))))
         
         (: try-disjoint-axial-planes (-> (U #f BSP-Tree)))
         (define (try-disjoint-axial-planes)
           (let loop ([axes axes])
             (cond [(empty? axes)  #f]
                   [else
                    (define i (axis-index (first axes)))
                    (define split (interval-split (shapes->intervals ss i)))
                    (cond [split  (define plane (axial-plane i split))
                                  (bsp-split ss plane #t (λ () (loop (rest axes))))]
                          [else  (loop (rest axes))])])))
         
         (: try-planes (-> Boolean (Listof FlVector) (U #f BSP-Tree)))
         (define (try-planes disjoint? planes)
           ;; Sort planes by absolute distance from center
           (define sorted-planes
             ((inst sort FlVector Flonum) planes <
                                          #:key (λ ([plane : FlVector])
                                                  (abs (point3d-plane-dist center plane)))
                                          #:cache-keys? #t))
           ;; Try each in order
           (let loop ([planes  sorted-planes])
             (cond [(empty? planes)  #f]
                   [else  (bsp-split ss (first planes) disjoint? (λ () (loop (rest planes))))])))
         
         (: try-bounding-planes (-> (U #f BSP-Tree)))
         (define (try-bounding-planes)
           ;; Bounding planes aren't necessarily coplanar with any triangle in a polygon, so
           ;; we need to do a disjoint split or splitting won't necessarily make progress
           (cond [((length ps) . > . 5)  #f]
                 [else
                  (define vss (map poly-vertices ps))
                  (define norms (map poly-normal ps))
                  (define planes
                    (append* (map (λ ([vs : (Listof FlVector)] [norm : FlVector])
                                    (find-bounding-planes vs norm))
                                  vss norms)))
                  (try-planes #t planes)]))
         
         (: try-polygon-planes (-> Boolean (U #f BSP-Tree)))
         (define (try-polygon-planes disjoint?)
           (define planes (force polygon-planes))
           (cond [(and disjoint? ((length planes) . > . 10))  #f]
                 [else  (try-planes disjoint? planes)]))
         
         (: try-triangulating (-> (U #f BSP-Tree)))
         (define (try-triangulating)
           (define new-ss (triangulate-polygons ss))
           (cond [(= (length new-ss) (length ss))  #f]
                 [else  (build-bsp-tree* new-ss)]))
         
         (let* ([bsp  #f]
                [bsp  (if bsp bsp (try-disjoint-axial-planes))]
                [bsp  (if bsp bsp (try-bounding-planes))]
                [bsp  (if bsp bsp (try-polygon-planes #t))]
                [bsp  (if bsp bsp (try-polygon-planes #f))]
                [bsp  (if bsp bsp (try-triangulating))]
                [bsp  (if bsp bsp (bsp-leaf ss))])
           bsp)]))

(: bsp-tree-insert (-> BSP-Tree (Listof BSP-Shape) BSP-Tree))
(define (bsp-tree-insert bsp ss)
  (bsp-tree-insert* bsp (remove-null-shapes ss)))

(: bsp-tree-insert* (-> BSP-Tree (Listof BSP-Shape) BSP-Tree))
(define (bsp-tree-insert* bsp ss)
  (cond [(empty? ss)  bsp]
        [else
         (match bsp
           [(bsp-leaf other-ss)
            (build-bsp-tree* (append other-ss ss))]
           [(bsp-node plane other-ss left right)
            (define-values (los mids his) (bin-shapes ss plane))
            (bsp-node plane
                      (append other-ss mids)
                      (bsp-tree-insert* left los)
                      (bsp-tree-insert* right his))])]))
