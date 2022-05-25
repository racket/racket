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
;; doesn't need to be wrapped for `datum->syntax`, for example;
;; the `tail?` argument is actually #f or a fixnum for a lower bound
;; on `cdr`s that have been taken
;;
;; `gf` is like `f`, but `gf` is used when the argument might be
;; syntax; if `gf` is provided, `f` can assume that its argument
;; is not syntax
;;
;; If a `seen` argument is provided, then it should be an `eq?`-based
;; hash table, and cycle checking is enabled; when a cycle is
;; discovered, the procedure attached to 'cycle-fail in the initial
;; table is called
;;
;; If a `known-pairs` argument is provided, then it should be an
;; `eq?`-based hash table to map pairs that can be returned as-is
;; in a `tail?` position

;; The inline version uses `f` only in an application position to
;; help avoid allocating a closure. It also covers only the most common
;; cases, defering to the general (not inlined) function for other cases.
(define-inline (datum-map s f [gf f] [seen #f] [known-pairs #f])
  (let loop ([tail? #f] [s s] [prev-depth 0])
    (define depth (fx+ 1 prev-depth)) ; avoid cycle-checking overhead for shallow cases
    (cond
     [(and seen (depth . fx> . 32))
      (datum-map-slow tail? s  (lambda (tail? s) (gf tail? s)) seen known-pairs)]
     [(null? s) (f tail? s)]
     [(pair? s)
      (f tail? (cons (loop #f (car s) depth)
                     (loop 1 (cdr s) depth)))]
     [(symbol? s) (f #f s)]
     [(boolean? s) (f #f s)]
     [(number? s) (f #f s)]
     [(or (vector? s) (box? s) (prefab-struct-key s) (hash? s))
      (datum-map-slow tail? s (lambda (tail? s) (gf tail? s)) seen known-pairs)]
     [else (gf #f s)])))

(define (datum-map-slow tail? s f seen known-pairs)
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
      (cond
        [(and known-pairs
              tail?
              (hash-ref known-pairs s #f))
         s]
        [else
         (f tail? (cons (loop #f (car s) seen)
                        (loop (if tail? (fx+ 1 tail?) 1)  (cdr s) seen)))])]
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
      (f #f
         (hash-map/copy s
                        (lambda (k v)
                          (values k (loop #f v seen)))
                        #:kind 'immutable))]
     [else (f #f s)])))

(define (datum-has-elements? d)
  (or (pair? d)
      (vector? d)
      (box? d)
      (immutable-prefab-struct-key d)
      (and (hash? d) (immutable? d) (positive? (hash-count d)))))
