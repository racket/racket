#lang typed/racket/base

(require racket/flonum
         (except-in racket/fixnum fl->fx fx->fl)
         racket/match
         (except-in racket/unsafe/ops unsafe-flvector-ref unsafe-flvector-set!)
         "flonum.rkt")

(provide flomap flomap? flomap-values flomap-components flomap-width flomap-height
         ;; Accessors
         flomap-size flomap-ref flomap-bilinear-ref coords->index
         ;; Basic constructors
         make-flomap make-flomap/components build-flomap inline-build-flomap
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

(: flomap-size (flomap -> (values Nonnegative-Fixnum Nonnegative-Fixnum)))
(define (flomap-size fm)
  (match-define (flomap _vs _c w h) fm)
  (with-asserts ([w  nonnegative-fixnum?] [h  nonnegative-fixnum?])
    (values w h)))

#;;(: coords->index (Integer Integer Integer Integer Integer -> Fixnum))
(define (coords->index c w k x y)
  (fx+ k (fx* c (fx+ x (fx* y w)))))

(define-syntax-rule (coords->index c w k x y)
  (fx+ k (fx* c (fx+ x (fx* y w)))))

(: unsafe-flomap-ref (FlVector Integer Integer Integer Integer Integer Integer -> Flonum))
(define (unsafe-flomap-ref vs c w h k x y)
  (cond [(and (x . fx>= . 0) (x . fx< . w)
              (y . fx>= . 0) (y . fx< . h))
         (unsafe-flvector-ref vs (coords->index c w k x y))]
        [else  0.0]))

(: flomap-ref (flomap Integer Integer Integer -> Flonum))
(define (flomap-ref fm k x y)
  (match-define (flomap vs c w h) fm)
  (unless (and (k . >= . 0) (k . < . c))
    (raise-type-error 'flomap-ref (format "nonnegative fixnum < ~e" c) k))
  (unsafe-flomap-ref vs c w h k x y))

(: flomap-bilinear-ref (flomap Integer Real Real -> Flonum))
(define (flomap-bilinear-ref fm k x y)
  (match-define (flomap vs c w h) fm)
  (unless (and (k . >= . 0) (k . < . c))
    (raise-type-error 'flomap-bilinear-ref (format "nonnegative fixnum < ~e" c) k))
  (let ([x  (- (exact->inexact x) 0.5)]
        [y  (- (exact->inexact y) 0.5)])
    (define floor-x (floor x))
    (define floor-y (floor y))
    (define x0 (fl->fx floor-x))
    (define y0 (fl->fx floor-y))
    (define x1 (fx+ x0 1))
    (define y1 (fx+ y0 1))
    (define v00 (unsafe-flomap-ref vs c w h k x0 y0))
    (define v10 (unsafe-flomap-ref vs c w h k x1 y0))
    (define v01 (unsafe-flomap-ref vs c w h k x0 y1))
    (define v11 (unsafe-flomap-ref vs c w h k x1 y1))
    (define xα (- x floor-x))
    (fl-convex-combination (fl-convex-combination v00 v10 xα)
                           (fl-convex-combination v01 v11 xα)
                           (- y floor-y))))

;; ===================================================================================================
;; Construction and conversion

(: make-flomap (case-> (Integer Integer Integer -> flomap)
                       (Integer Integer Integer Real -> flomap)))
(define make-flomap
  (case-lambda
    [(c w h)    (flomap (make-flvector (* c w h)) c w h)]
    [(c w h v)  (flomap (make-flvector (* c w h) (exact->inexact v)) c w h)]))

(define-syntax-rule (inline-build-flomap components width height f)
  (let: ([c : Integer  components]
         [w : Integer  width]
         [h : Integer  height])
    (with-asserts ([c  nonnegative-fixnum?] [w  nonnegative-fixnum?] [h  nonnegative-fixnum?])
      (define vs (make-flvector (* c w h)))
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
          [else  (flomap vs c w h)])))))

(: build-flomap (Integer Integer Integer
                         (Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum
                                             Nonnegative-Fixnum -> Real)
                         -> flomap))
(define (build-flomap components width height fun)
  (inline-build-flomap components width height (λ (k x y i) (exact->inexact (fun k x y i)))))

(: make-flomap/components (Integer Integer (Listof Real) -> flomap))
(define (make-flomap/components w h vs)
  (let ([vs  (apply flvector (map exact->inexact vs))])
    (define c (flvector-length vs))
    (inline-build-flomap c w h (λ (k _x _y _i) (unsafe-flvector-ref vs k)))))

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
    (error 'flomap-append-components
           "expected flomaps with equal dimension; given dimensions ~e×~e and ~e×~e"
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
