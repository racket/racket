#lang typed/racket/base

(require racket/list
         racket/flonum)

(provide point3d-plane-dist
         split-line3d
         canonical-polygon3d
         split-polygon3d
         canonical-lines3d
         split-lines3d
         polygon3d-divide
         polygon3d-triangulate)

;; ===================================================================================================

(: plane-line-intersect (-> Flonum Flonum Flonum Flonum
                            Flonum Flonum Flonum Flonum Flonum Flonum
                            (Values Flonum Flonum Flonum)))
(define (plane-line-intersect a b c d x1 y1 z1 x2 y2 z2)
  (define dot1 (+ (* a x1) (* b y1) (* c z1)))
  (define dot2 (+ (* a x2) (* b y2) (* c z2)))
  (define denom (- dot1 dot2))
  (if (zero? denom)
      (values x2 y2 z2)
      (let ([t  (/ (+ dot1 d) denom)])
        (values (+ x1 (* t (- x2 x1)))
                (+ y1 (* t (- y2 y1)))
                (+ z1 (* t (- z2 z1)))))))

;; ===================================================================================================
;; Points

(: point3d-plane-dist (-> FlVector FlVector Flonum))
(define (point3d-plane-dist v plane)
  (+ (* (flvector-ref plane 0) (flvector-ref v 0))
     (* (flvector-ref plane 1) (flvector-ref v 1))
     (* (flvector-ref plane 2) (flvector-ref v 2))
     (flvector-ref plane 3)))

;; ===================================================================================================
;; Splitting lines

(: split-line3d (-> FlVector FlVector FlVector FlVector))
(define (split-line3d v1 v2 plane)
  (define-values (x y z)
    (plane-line-intersect
     (flvector-ref plane 0) (flvector-ref plane 1) (flvector-ref plane 2) (flvector-ref plane 3)
     (flvector-ref v1 0) (flvector-ref v1 1) (flvector-ref v1 2)
     (flvector-ref v2 0) (flvector-ref v2 1) (flvector-ref v2 2)))
  (flvector x y z))

;; ===================================================================================================
;; Splitting polygons

(: canonical-polygon3d (-> (Listof FlVector) (Listof Boolean)
                           (Values (Listof FlVector) (Listof Boolean))))
(define (canonical-polygon3d vs ls)
  (cond
    [(empty? vs)  (values vs empty)]
    [else
     (define-values (new-vs new-ls)
       (for/fold ([vs : (Listof FlVector)  empty]
                  [ls : (Listof Boolean)   empty]
                  ) ([v1  (in-list (cons (last vs) vs))]
                     [v2  (in-list vs)]
                     [l   (in-list ls)])
         (if (equal? v1 v2)
             (values vs ls)
             (values (cons v2 vs) (cons l ls)))))
     (values (reverse new-vs) (reverse new-ls))]))

(: split-polygon3d (-> (Listof FlVector) (Listof Boolean) FlVector
                       (Values (Listof FlVector) (Listof Boolean)
                               (Listof FlVector) (Listof Boolean)
                               Boolean)))
(define (split-polygon3d vs ls plane)
  (define a (flvector-ref plane 0))
  (define b (flvector-ref plane 1))
  (define c (flvector-ref plane 2))
  (define d (flvector-ref plane 3))
  
  (: in-bounds? (-> Flonum Flonum Flonum Boolean))
  (define (in-bounds? x1 y1 z1)
    ((+ (* a x1) (* b y1) (* c z1) d) . >= . 0.0))
  
  (define v1 (last vs))
  (define x1 (flvector-ref v1 0))
  (define y1 (flvector-ref v1 1))
  (define z1 (flvector-ref v1 2))
  (define v1? (in-bounds? x1 y1 z1))
  
  (define-values (vs1 ls1 vs2 ls2 crossings _x1 _y1 _z1 _v1?)
    (for/fold: ([vs1 : (Listof FlVector)  empty]
                [ls1 : (Listof Boolean)   empty]
                [vs2 : (Listof FlVector)  empty]
                [ls2 : (Listof Boolean)   empty]
                [crossings : Natural  0]
                [x1 : Flonum  x1]
                [y1 : Flonum  y1]
                [z1 : Flonum  z1]
                [v1? : Boolean  v1?]
                ) ([v2  (in-list vs)]
                   [l   (in-list ls)])
      (define x2 (flvector-ref v2 0))
      (define y2 (flvector-ref v2 1))
      (define z2 (flvector-ref v2 2))
      (define v2? (in-bounds? x2 y2 z2))
      (cond
        [(and v1? v2?)       (values (cons v2 vs1) (cons l ls1) vs2 ls2 crossings x2 y2 z2 v2?)]
        [(not (or v1? v2?))  (values vs1 ls1 (cons v2 vs2) (cons l ls2) crossings x2 y2 z2 v2?)]
        [v1?
         (let-values ([(x y z)  (plane-line-intersect a b c d x1 y1 z1 x2 y2 z2)])
           (define v (flvector x y z))
           (values (cons v vs1) (cons l ls1) (list* v2 v vs2) (list* l #f ls2)
                   (+ crossings 1) x2 y2 z2 v2?))]
        [else
         (let-values ([(x y z)  (plane-line-intersect a b c d x1 y1 z1 x2 y2 z2)])
           (define v (flvector x y z))
           (values (list* v2 v vs1) (list* l #f ls1) (cons v vs2) (cons l ls2)
                   (+ crossings 1) x2 y2 z2 v2?))])))
  
  (let-values ([(vs1 ls1)  (canonical-polygon3d (reverse vs1) (reverse ls1))]
               [(vs2 ls2)  (canonical-polygon3d (reverse vs2) (reverse ls2))])
    (values vs1 ls1 vs2 ls2
            (crossings . <= . 2))))

;; ===================================================================================================
;; Splitting connected lines

(: canonical-lines3d (All (A) ((Listof A) -> (Listof A))))
(define (canonical-lines3d xs)
  (if (empty? xs)
      xs
      (cons (first xs)
            (for/list: : (Listof A) ([x1  (in-list xs)]
                                     [x2  (in-list (rest xs))]
                                     #:unless (equal? x1 x2))
              x2))))

(: split-lines3d (-> (Listof FlVector) FlVector (Values (Listof (Listof FlVector))
                                                        (Listof (Listof FlVector)))))
(define (split-lines3d vs plane)
  (define a (flvector-ref plane 0))
  (define b (flvector-ref plane 1))
  (define c (flvector-ref plane 2))
  (define d (flvector-ref plane 3))
  
  (: in-bounds? (-> Flonum Flonum Flonum Boolean))
  (define (in-bounds? x1 y1 z1)
    ((+ (* a x1) (* b y1) (* c z1) d) . >= . 0.0))
  
  (define v1 (first vs))
  (define x1 (flvector-ref v1 0))
  (define y1 (flvector-ref v1 1))
  (define z1 (flvector-ref v1 2))
  (define v1? (in-bounds? x1 y1 z1))
  
  (define-values (vss1 vss2 _x1 _y1 _z1 _v1?)
    (for/fold: ([vss1 : (Listof (Listof FlVector))  (if v1? (list (list v1)) (list empty))]
                [vss2 : (Listof (Listof FlVector))  (if v1? (list empty) (list (list v1)))]
                [x1 : Flonum  x1]
                [y1 : Flonum  y1]
                [z1 : Flonum  z1]
                [v1? : Boolean  v1?]
                ) ([v2  (in-list (rest vs))])
      (define x2 (flvector-ref v2 0))
      (define y2 (flvector-ref v2 1))
      (define z2 (flvector-ref v2 2))
      (define v2? (in-bounds? x2 y2 z2))
      (cond
        [(and v1? v2?)       (values (cons (cons v2 (first vss1)) (rest vss1)) vss2 x2 y2 z2 v2?)]
        [(not (or v1? v2?))  (values vss1 (cons (cons v2 (first vss2)) (rest vss2)) x2 y2 z2 v2?)]
        [v1?
         (define-values (x y z) (plane-line-intersect a b c d x1 y1 z1 x2 y2 z2))
         (define v (flvector x y z))
         (values (cons (cons v (first vss1)) (rest vss1))
                 (cons (list v2 v) vss2)
                 x2 y2 z2 v2?)]
        [else
         (define-values (x y z) (plane-line-intersect a b c d x1 y1 z1 x2 y2 z2))
         (define v (flvector x y z))
         (values (cons (list v2 v) vss1)
                 (cons (cons v (first vss2)) (rest vss2))
                 x2 y2 z2 v2?)])))
  
  (values (map (inst canonical-lines3d FlVector) (filter (compose not empty?) vss1))
          (map (inst canonical-lines3d FlVector) (filter (compose not empty?) vss2))))

;; ===================================================================================================
;; Dividing and triangulating polygons

(: polygon3d-divide (-> (Listof FlVector) (Listof Boolean)
                        (Values (Listof FlVector) (Listof Boolean)
                                (Listof FlVector) (Listof Boolean))))
(define (polygon3d-divide vs-list ls-list)
  ;(printf "vs-list = ~v~n" vs-list)
  (define n (length vs-list))
  (cond
    [(<= n 3)  (values vs-list ls-list empty empty)]
    [else
     (define vs (list->vector vs-list))
     (define ls (list->vector ls-list))
     (define n/2 (quotient n 2))
     
     (define opposite (λ ([i1 : Fixnum]) (modulo (+ i1 n/2) n)))
     
     (define opposite-dist
       (λ ([i1 : Fixnum])
         (define v1 (vector-ref vs i1))
         (define v2 (vector-ref vs (opposite i1)))
         (define x (- (flvector-ref v2 0) (flvector-ref v1 0)))
         (define y (- (flvector-ref v2 1) (flvector-ref v1 1)))
         (define z (- (flvector-ref v2 2) (flvector-ref v1 2)))
         (+ (* x x) (* y y) (* z z))))
     
     (define-values (i1 _)
       (for/fold ([best-i : Fixnum  0]
                  [best-dist : Flonum  (opposite-dist 0)]
                  ) ([i1 : Positive-Fixnum  (in-range 1 n)])
         (define dist (opposite-dist i1))
         (if (dist . < . best-dist)
             (values i1 dist)
             (values best-i best-dist))))
     
     (define i2 (opposite i1))
     
     (: extract (-> Fixnum Fixnum (Values (Listof FlVector) (Listof Boolean))))
     (define (extract i1 i2)
       (let loop ([i i2] [new-vs : (Listof FlVector)  empty]
                         [new-ls : (Listof Boolean)   empty])
         (cond [(= i i1)  (values (reverse (cons (vector-ref vs i) new-vs))
                                  (reverse (cons (vector-ref ls i) new-ls)))]
               [else
                (loop (modulo (+ i 1) n)
                      (cons (vector-ref vs i) new-vs)
                      (cons (if (= i i2) #f (vector-ref ls i)) new-ls))])))
     
     (define-values (vs1 ls1) (extract i1 i2))
     (define-values (vs2 ls2) (extract i2 i1))
     (values vs1 ls1 vs2 ls2)]))

(: polygon3d-triangulate (-> (Listof FlVector) (Listof Boolean)
                             (Values (Listof (Listof FlVector)) (Listof (Listof Boolean)))))
(define (polygon3d-triangulate vs ls)
  (if (empty? vs)
      (values empty empty)
      (let loop ([vs vs] [ls ls])
        (define-values (vs1 ls1 vs2 ls2) (polygon3d-divide vs ls))
        (cond [(empty? vs2)  (values (list vs1) (list ls1))]
              [else
               (define-values (vss1 lss1) (polygon3d-triangulate vs1 ls1))
               (define-values (vss2 lss2) (polygon3d-triangulate vs2 ls2))
               (values (append vss1 vss2) (append lss1 lss2))]))))
