#lang racket/base
(require "../common/prefab.rkt"
         "../common/inline.rkt"
         racket/fixnum)

(provide datum-map
         datum-has-elements?)

;; `(datum-map v f)` walks over `v`, traversing objects that
;; `datum->syntax` traverses to convert content to syntax objects.
;; 
;; `(f tail? d)` is called on each datum `d`, where `tail?`
;; indicates that the value is a pair/null in a `cdr` --- so that it
;; doesn't need to be wrapped for `datum->syntax`, for example
;;
;; `gf` is like `f`, but `gf` is used when the argument might be
;; syntax; if `gf` is provided, `f` can assume that its argument
;; is not syntax
;;
;; If a `seen` argument is provided, then it should be an `eq?`-based
;; hash table, and cycle checking is enabled; when a cycle is
;; discovered, the procedure attached to 'cycle-fail in the initial
;; table is called

;; The inline version uses `f` only in an application position to
;; help avoid allocating a closure. It also covers only the most common
;; cases, defering to the general (not inlined) function for other cases.
(define-inline (datum-map s f [gf f] [seen #f])
  (let loop ([tail? #f] [s s] [prev-depth 0])
    (define depth (fx+ 1 prev-depth)) ; avoid cycle-checking overhead for shallow cases
    (cond
     [(and seen (depth . fx> . 32))
      (datum-map-slow tail? s  (lambda (tail? s) (gf tail? s)) seen)]
     [(null? s) (f tail? s)]
     [(pair? s)
      (f tail? (cons (loop #f (car s) depth)
                     (loop #t (cdr s) depth)))]
     [(symbol? s) (f #f s)]
     [(boolean? s) (f #f s)]
     [(number? s) (f #f s)]
     [(or (vector? s) (box? s) (prefab-struct-key s) (hash? s))
      (datum-map-slow tail? s (lambda (tail? s) (gf tail? s)) seen)]
     [else (gf #f s)])))

(define (datum-map-slow tail? s f seen)
  (let loop ([tail? tail?] [s s] [prev-seen seen])
    (define seen
      (cond
       [(and prev-seen (datum-has-elements? s))
        (cond
         [(hash-ref prev-seen s #f)
          ((hash-ref prev-seen 'cycle-fail) s)]
         [else (hash-set prev-seen s #t)])]
       [else prev-seen]))
    (cond
     [(null? s) (f tail? s)]
     [(pair? s)
      (f tail? (cons (loop #f (car s) seen)
                     (loop #t (cdr s) seen)))]
     [(or (symbol? s) (boolean? s) (number? s))
      (f #f s)]
     [(vector? s)
      (f #f (vector->immutable-vector
             (for/vector #:length (vector-length s) ([e (in-vector s)])
                         (loop #f e seen))))]
     [(box? s)
      (f #f (box-immutable (loop #f (unbox s) seen)))]
     [(immutable-prefab-struct-key s)
      => (lambda (key)
           (f #f
              (apply make-prefab-struct
                     key
                     (for/list ([e (in-vector (struct->vector s) 1)])
                       (loop #f e seen)))))]
     [(and (hash? s) (immutable? s))
      (cond
       [(hash-eq? s)
        (f #f
           (for/hasheq ([(k v) (in-hash s)])
             (values k (loop #f v seen))))]
       [(hash-eqv? s)
        (f #f
           (for/hasheqv ([(k v) (in-hash s)])
             (values k (loop #f v seen))))]
       [else
        (f #f
           (for/hash ([(k v) (in-hash s)])
             (values k (loop #f v seen))))])]
     [else (f #f s)])))

(define (datum-has-elements? d)
  (or (pair? d)
      (vector? d)
      (box? d)
      (immutable-prefab-struct-key d)
      (and (hash? d) (immutable? d) (positive? (hash-count d)))))
