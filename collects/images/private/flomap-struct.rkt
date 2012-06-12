#lang typed/racket/base

(require racket/match
         (only-in racket/unsafe/ops
                  unsafe-flvector-ref unsafe-flvector-set!
                  unsafe-fx+)
         racket/performance-hint
         "flonum.rkt")

(provide flomap flomap? flomap-values flomap-components flomap-width flomap-height
         ;; Accessors
         flomap-size coords->index
         unsafe-flomap-ref flomap-ref flomap-bilinear-ref
         unsafe-flomap-ref* flomap-ref* flomap-bilinear-ref*
         ;; Basic constructors
         make-flomap build-flomap inline-build-flomap
         make-flomap* build-flomap* inline-build-flomap*
         flomap-ref-component flomap-take-components flomap-drop-components flomap-append-components)

(struct: flomap ([values : FlVector] [components : Integer] [width : Integer] [height : Integer])
  #:transparent
  #:guard
  (λ (vs c w h name)
    (with-asserts ([c  nonnegative-fixnum?] [w  nonnegative-fixnum?] [h  nonnegative-fixnum?])
      (unless (= (flvector-length vs) (* c w h))
        (error 'flomap "expected flvector of length ~e; given one of length ~e"
               (* c w h) (flvector-length vs)))
      (values vs c w h))))

(begin-encourage-inline

  (: flomap-size (flomap -> (values Nonnegative-Fixnum Nonnegative-Fixnum)))
  (define (flomap-size fm)
    (match-define (flomap _vs _c w h) fm)
    (with-asserts ([w  nonnegative-fixnum?] [h  nonnegative-fixnum?])
      (values w h)))
  
  (: coords->index (Integer Integer Integer Integer Integer -> Fixnum))
  (define (coords->index c w k x y)
    (fx+ k (fx* c (fx+ x (fx* y w)))))
  
  (: unsafe-flomap-ref (FlVector Integer Integer Integer Integer Integer Integer -> Float))
  (define (unsafe-flomap-ref vs c w h k x y)
    (cond [(and (x . fx>= . 0) (x . fx< . w)
                (y . fx>= . 0) (y . fx< . h))
           (unsafe-flvector-ref vs (coords->index c w k x y))]
          [else  0.0]))
  
  (: unsafe-flomap-ref* (FlVector Integer Integer Integer Integer Integer -> FlVector))
  (define (unsafe-flomap-ref* vs c w h x y)
    (cond [(and (x . fx>= . 0) (x . fx< . w)
                (y . fx>= . 0) (y . fx< . h))
           (define i (coords->index c w 0 x y))
           (define point-vs (make-flvector c))
           (let: loop : Void ([k : Nonnegative-Fixnum  0])
             (when (k . < . c)
               (unsafe-flvector-set! point-vs k (unsafe-flvector-ref vs (unsafe-fx+ i k)))
               (loop (unsafe-fx+ k 1))))
           point-vs]
          [else  (make-flvector c 0.0)]))
  
  (: flomap-ref (flomap Integer Integer Integer -> Float))
  (define (flomap-ref fm k x y)
    (match-define (flomap vs c w h) fm)
    (unless (and (k . >= . 0) (k . < . c))
      (raise-type-error 'flomap-ref (format "nonnegative fixnum < ~e" c) k))
    (unsafe-flomap-ref vs c w h k x y))
  
  (: flomap-ref* (flomap Integer Integer -> FlVector))
  (define (flomap-ref* fm x y)
    (match-define (flomap vs c w h) fm)
    (unsafe-flomap-ref* vs c w h x y))
  
  ) ; begin-encourage-inline

(: flomap-bilinear-ref (flomap Integer Real Real -> Float))
(define (flomap-bilinear-ref fm k x y)
  (match-define (flomap vs c w h) fm)
  (cond [(and (k . >= . 0) (k . < . c))
         (let ([x  (- (real->double-flonum x) 0.5)]
               [y  (- (real->double-flonum y) 0.5)])
           (cond [(and (x . > . -1.0) (x . < . (->fl w))
                       (y . > . -1.0) (y . < . (->fl h)))
                  (define floor-x (floor x))
                  (define floor-y (floor y))
                  (define x0 (fl->fx floor-x))
                  (define y0 (fl->fx floor-y))
                  (define x1 (unsafe-fx+ x0 1))
                  (define y1 (unsafe-fx+ y0 1))
                  (define v00 (unsafe-flomap-ref vs c w h k x0 y0))
                  (define v10 (unsafe-flomap-ref vs c w h k x1 y0))
                  (define v01 (unsafe-flomap-ref vs c w h k x0 y1))
                  (define v11 (unsafe-flomap-ref vs c w h k x1 y1))
                  (define xα (- x floor-x))
                  (fl-convex-combination (fl-convex-combination v00 v10 xα)
                                         (fl-convex-combination v01 v11 xα)
                                         (- y floor-y))]
                 [else  0.0]))]
        [else
         (raise-type-error 'flomap-bilinear-ref (format "nonnegative fixnum < ~e" c) 1 fm k x y)]))

(: flomap-bilinear-ref* (flomap Real Real -> FlVector))
(define (flomap-bilinear-ref* fm x y)
  (match-define (flomap vs c w h) fm)
  (let ([x  (- (real->double-flonum x) 0.5)]
        [y  (- (real->double-flonum y) 0.5)])
    (cond [(and (x . > . -1.0) (x . < . (->fl w))
                (y . > . -1.0) (y . < . (->fl h)))
           (define floor-x (floor x))
           (define floor-y (floor y))
           (define x0 (fl->fx floor-x))
           (define y0 (fl->fx floor-y))
           (define x1 (unsafe-fx+ x0 1))
           (define y1 (unsafe-fx+ y0 1))
           (define vs00 (unsafe-flomap-ref* vs c w h x0 y0))
           (define vs10 (unsafe-flomap-ref* vs c w h x1 y0))
           (define vs01 (unsafe-flomap-ref* vs c w h x0 y1))
           (define vs11 (unsafe-flomap-ref* vs c w h x1 y1))
           (define xα (- x floor-x))
           (define yα (- y floor-y))
           (define point-vs (make-flvector c))
           (let: loop : FlVector ([k : Nonnegative-Fixnum  0])
             (cond [(k . < . c)
                    (define v00 (unsafe-flvector-ref vs00 k))
                    (define v10 (unsafe-flvector-ref vs10 k))
                    (define v01 (unsafe-flvector-ref vs01 k))
                    (define v11 (unsafe-flvector-ref vs11 k))
                    (define v (fl-convex-combination (fl-convex-combination v00 v10 xα)
                                                     (fl-convex-combination v01 v11 xα)
                                                     yα))
                    (unsafe-flvector-set! point-vs k v)
                    (loop (unsafe-fx+ k 1))]
                   [else  point-vs]))]
          [else  (make-flvector c 0.0)])))

;; ===================================================================================================
;; Construction and conversion

(: make-flomap (case-> (Integer Integer Integer -> flomap)
                       (Integer Integer Integer Real -> flomap)))
(define make-flomap
  (case-lambda
    [(c w h)    (flomap (make-flvector (* c w h)) c w h)]
    [(c w h v)  (flomap (make-flvector (* c w h) (real->double-flonum v)) c w h)]))

#;
(: inline-build-flomap (Integer Integer Integer
                                (Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum
                                                    Nonnegative-Fixnum -> Float)
                                -> flomap))
(define-syntax-rule (inline-build-flomap components width height f)
  (let: ([c : Integer  components]
         [w : Integer  width]
         [h : Integer  height])
    (with-asserts ([c  nonnegative-fixnum?] [w  nonnegative-fixnum?] [h  nonnegative-fixnum?])
      (define fm (make-flomap c w h))
      (define vs (flomap-values fm))
      (let: y-loop : flomap ([y : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  0])
        (cond
          [(y . fx< . h)
           (let: x-loop : flomap ([x : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
             (cond
               [(x . fx< . w)
                (let: k-loop : flomap ([k : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
                  (cond
                    [(k . fx< . c)  (unsafe-flvector-set! vs i (f k x y i))
                                    (k-loop (unsafe-fx+ k 1) (unsafe-fx+ i 1))]
                    [else  (x-loop (unsafe-fx+ x 1) i)]))]
               [else  (y-loop (unsafe-fx+ y 1) i)]))]
          [else  fm])))))

(: build-flomap (Integer Integer Integer
                         (Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum -> Real)
                         -> flomap))
(define (build-flomap c w h f)
  (inline-build-flomap c w h (λ (k x y i) (real->double-flonum (f k x y)))))

#;
(: inline-build-flomap* (Integer Integer Integer
                                 (Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum
                                                     -> FlVector)
                                 -> flomap))
(define-syntax-rule (inline-build-flomap* components width height f)
  (let: ([c : Integer  components]
         [w : Integer  width]
         [h : Integer  height])
    (with-asserts ([c  nonnegative-fixnum?] [w  nonnegative-fixnum?] [h  nonnegative-fixnum?])
      (define fm (make-flomap c w h))
      (define vs (flomap-values fm))
      (let: y-loop : flomap ([y : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  0])
        (cond
          [(y . fx< . h)
           (let: x-loop : flomap ([x : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
             (cond
               [(x . fx< . w)
                (define: point-vs : FlVector  (f x y i))
                (cond
                  [(fx= (flvector-length point-vs) c)
                   (let: k-loop : flomap ([k : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
                     (cond
                       [(k . fx< . c)  (unsafe-flvector-set! vs i (unsafe-flvector-ref point-vs k))
                                       (k-loop (unsafe-fx+ k 1) (unsafe-fx+ i 1))]
                       [else  (x-loop (unsafe-fx+ x 1) i)]))]
                  [else  (raise-type-error 'inline-build-flomap* (format "length-~e FlVector" c)
                                           point-vs)])]
               [else  (y-loop (unsafe-fx+ y 1) i)]))]
          [else  fm])))))

(: make-flomap* (Integer Integer (U (Vectorof Real) FlVector) -> flomap))
(define (make-flomap* w h vs)
  (let ([vs  (->flvector vs)])
    (define c (flvector-length vs))
    (inline-build-flomap* c w h (λ (_x _y _i) vs))))

(: build-flomap* (Integer Integer Integer
                          (Nonnegative-Fixnum Nonnegative-Fixnum -> (U (Vectorof Real) FlVector))
                          -> flomap))
(define (build-flomap* c w h f)
  (inline-build-flomap*
   c w h
   (λ (x y i)
     (define point-vs0 (f x y))
     (define point-vs1 (->flvector point-vs0))
     (cond [(fx= (flvector-length point-vs1) c)  point-vs1]
           [else  (raise-type-error 'build-flomap* (format "length-~e Vector or FlVector" c)
                                    point-vs0)]))))

(: flomap-ref-component (flomap Integer -> flomap))
(define (flomap-ref-component fm k)
  (match-define (flomap vs c w h) fm)
  (unless (and (k . >= . 0) (k . < . c))
    (raise-type-error 'flomap-ref-components (format "nonnegative fixnum < ~e" c) k))
  (inline-build-flomap 1 w h (λ (_k x y _i) (unsafe-flvector-ref vs (coords->index c w k x y)))))

(: flomap-take-components (flomap Integer -> flomap))
(define (flomap-take-components fm c)
  (match-define (flomap vs old-c w h) fm)
  (unless (and (c . >= . 0) (c . <= . old-c))
    (raise-type-error 'flomap-take-components (format "nonnegative fixnum <= ~e" old-c) c))
  (inline-build-flomap c w h (λ (k x y _i) (unsafe-flvector-ref vs (coords->index old-c w k x y)))))

(: flomap-drop-components (flomap Integer -> flomap))
(define (flomap-drop-components fm c)
  (match-define (flomap vs old-c w h) fm)
  (unless (and (c . >= . 0) (c . <= . old-c))
    (raise-type-error 'flomap-drop-components (format "nonnegative fixnum <= ~e" old-c) c))
  (define new-c (fx- old-c c))
  (with-asserts
   ([new-c  nonnegative-fixnum?])
   (inline-build-flomap new-c w h (λ (k x y _i)
                                    (unsafe-flvector-ref vs (coords->index old-c w (fx+ k c) x y))))))

(: flomap-append-components2 (flomap flomap -> flomap))
(define (flomap-append-components2 fm1 fm2)
  (match-define (flomap vs1 d1 w1 h1) fm1)
  (match-define (flomap vs2 d2 w2 h2) fm2)
  (unless (and (= w1 w2) (= h1 h2))
    (error 'flomap-append-components "expected same-size flomaps; given sizes ~e×~e and ~e×~e"
           w1 h1 w2 h2))
  (inline-build-flomap (fx+ d1 d2) w1 h1
                       (λ (k x y _i)
                         (define k2 (fx- k d1))
                         (cond [(k2 . fx< . 0)  (unsafe-flvector-ref vs1 (coords->index d1 w1 k x y))]
                               [else  (unsafe-flvector-ref vs2 (coords->index d2 w2 k2 x y))]))))

(: flomap-append-components (flomap flomap * -> flomap))
(define (flomap-append-components fm . fms)
  (for/fold ([fm1 fm]) ([fm2  (in-list fms)])
    (flomap-append-components2 fm1 fm2)))
