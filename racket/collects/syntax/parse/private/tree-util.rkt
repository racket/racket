#lang racket/base
(provide (all-defined-out))

;; T = (list* T ... T) | (vector T ...) | (make-prefab-struct Key T ...) | Any

;; child-map : T (T -> T) -> T
(define (child-map x f)
  (cond [(pair? x)
         (let pairloop ([x x])
           (if (pair? x)
               (let ([h (f (car x))] [t (pairloop (cdr x))])
                 (if (and (eq? h (car x)) (eq? t (cdr x))) x (cons h t)))
               (if (null? x) null (f x))))]
        [(vector? x)
         (define len (vector-length x))
         (define v (make-vector len))
         (for ([i (in-range len)])
           (vector-set! v i (f (vector-ref x i))))
         (if (for/and ([xe (in-vector x)] [ve (in-vector v)]) (eq? xe ve)) x v)]
        [(prefab-struct-key x)
         => (lambda (key)
              (define xv (struct->vector x))
              (if (for/fold ([same? #t]) ([i (in-range 1 (vector-length xv))])
                    (define xe (vector-ref xv i))
                    (define xe* (f xe))
                    (vector-set! xv i xe*)
                    (and same? (eq? xe* xe)))
                  x
                  (apply make-prefab-struct key (cdr (vector->list xv)))))]
        ;; FIXME: box, hash, ...
        [else x]))

;; tree-transform : T (T -> T) Boolean -> T
(define (tree-transform x post-f [root? #t])
  (define (loop x) (post-f (loop* x)))
  (define (loop* x) (child-map x loop))
  (if root? (loop x) (loop* x)))

;; tree-transform-preorder : T (T (T -> T) Boolean -> T
(define (tree-transform-preorder x pre-f [root? #t])
  (define (loop x) (pre-f x (lambda ([y x]) (loop* y))))
  (define (loop* x) (child-map x loop))
  (if root? (loop x) (loop* x)))

;; ------------------------------------------------------------

;; child-reduce : T (T -> X) (X ... -> X) -> X
(define (child-reduce x f reduce)
  (cond [(pair? x)
         (define xs
           (let pairloop ([x x])
             (if (pair? x)
                 (cons (f (car x)) (pairloop (cdr x)))
                 (if (null? x) null (list (f x))))))
         (apply reduce xs)]
        [(vector? x)
         (apply reduce (for/list ([e (in-vector x)]) (f e)))]
        [(prefab-struct-key x)
         (apply reduce (for/list ([e (in-vector (struct->vector x) 1)]) (f e)))]
        [else (reduce)]))

;; child-reduce-left : T (T -> X) (X ... -> X) -> X
;; Avoids apply and aux lists, but bad for eg append.
(define (child-reduce-left x f reduce)
  (cond [(pair? x) ;; LEFT
         (let pairloop ([acc (reduce)] [x x])
           (if (pair? x)
               (pairloop (reduce acc (f (car x))) (cdr x))
               (if (null? x) acc (reduce acc (f x)))))]
        [(vector? x)
         (for/fold ([acc (reduce)]) ([e (in-vector x)])
           (reduce acc (f e)))]
        [(prefab-struct-key x)
         (for/fold ([acc (reduce)]) ([e (in-vector (struct->vector x) 1)])
           (reduce acc (f e)))]
        [else (reduce)]))

(define (tree-reduce x pre-f reduce [root? #t])
  (define (loop x) (pre-f x (lambda () (loop* x))))
  (define (loop* x) (child-reduce x loop reduce))
  (if root? (loop x) (loop* x)))

(define (tree-reduce-left x pre-f reduce [root? #t])
  (define (loop x) (pre-f x (lambda () (loop* x))))
  (define (loop* x) (child-reduce-left x loop reduce))
  (if root? (loop x) (loop* x)))

;; ------------------------------------------------------------

;; child-foldl : T (T X -> X) X -> X
(define (child-foldl x f acc)
  (cond [(pair? x) ;; LEFT
         (let pairloop ([x x] [acc acc])
           (if (pair? x)
               (pairloop (cdr x) (f (car x) acc))
               (if (null? x) acc (f x acc))))]
        [(vector? x)
         (for/fold ([acc acc]) ([e (in-vector x)])
           (f e acc))]
        [(prefab-struct-key x)
         (for/fold ([acc acc]) ([e (in-vector (struct->vector x) 1)])
           (f e acc))]
        [else acc]))

(define (tree-foldl x pre-f acc [root? #t])
  (define (loop x acc) (pre-f x acc (lambda ([acc acc]) (loop* x acc))))
  (define (loop* x acc) (child-foldl x loop acc))
  (if root? (loop x acc) (loop* x acc)))

;; ------------------------------------------------------------

;; child-ormap : T (T -> X/#f) -> X/#f
(define (child-ormap x f)
  (cond [(pair? x)
         (let pairloop ([x x])
           (if (pair? x)
               (or (f (car x)) (pairloop (cdr x)))
               (if (null? x) #f (f x))))]
        [(vector? x)
         (for/or ([e (in-vector x)]) (f e))]
        [(prefab-struct-key x)
         (for/or ([e (in-vector (struct->vector x) 1)]) (f e))]
        [else #f]))

;; tree-ormap : T (T -> X/#f) -> X/#f
(define (tree-ormap x pre-f [root? #t])
  (define (loop x) (pre-f x (lambda () (loop* x))))
  (define (loop* x) (child-ormap x loop))
  (if root? (loop x) (loop* x)))
