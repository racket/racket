#lang racket/base
(require racket/linklet)

;; Re-implement just enough deserialization to deal with 'decl
;; linklets, so we can get `required`, etc.

(provide deserialize-instance
         (struct-out module-use))

(struct module-use (module phase))
(struct provided (binding protected? syntax?))

(define (deserialize-module-path-indexes gen-vec order-vec)
  (define gen (make-vector (vector-length gen-vec) #f))
  (for ([d (in-vector gen-vec)]
        [i (in-naturals)])
    (vector-set!
     gen
     i
     (cond
      [(eq? d 'top) (error 'deserialize-module-path-indexes "expected top")]
      [(box? d) (module-path-index-join #f #f)]
      [else
       (module-path-index-join (vector-ref d 0)
                               (and ((vector-length d) . > . 1)
                                    (vector-ref gen (vector-ref d 1))))])))
  (for/vector #:length (vector-length order-vec) ([p (in-vector order-vec)])
    (vector-ref gen p)))

(define (deserialize mpis inspector bulk-binding-registry
                     num-mutables mutable-vec 
                     num-shared shared-vec
                     mutable-fill-vec
                     result-vec)
  (unless (zero? num-mutables) (error 'deserialize "mutables not supported"))

  (define shared-vs (make-vector num-shared #f))
  (define shared-rest
    (for/fold ([r (vector->list shared-vec)]) ([i (in-range num-shared)])
      (define-values (v rest) (decode r mpis shared-vs))
      (vector-set! shared-vs i v)
      rest))
  (unless (null? shared-rest)
    (error 'deserialize "unexpected leftover serialized form for shared: ~s" shared-rest))

  (define-values (v v-rest) (decode (vector->list result-vec) mpis shared-vs))
  (unless (null? v-rest)
    (error 'deserialize "unexpected leftover serialized form: ~s" v-rest))
  
  v)

(define (decode r mpis shared-vs)
  (let loop ([r r])
    (define (discard r n)
      (for/fold ([r (cdr r)]) ([i (in-range n)])
        (define-values (v v-rest) (loop r))
        v-rest))
    (cond
      [(null? r) (error 'deserialize "unexpected end of serialized form")]
      [else
       (define i (car r))
       (case i
         [(#:ref)
          (values (vector-ref shared-vs (cadr r)) (cddr r))]
         [(#:inspector)
          (values 'inspector (cdr r))]
         [(#:cons)
          (define-values (a a-rest) (loop (cdr r)))
          (define-values (d d-rest) (loop a-rest))
          (values (cons a d) d-rest)]
         [(#:list)
          (define-values (rev rest)
            (for/fold ([accum '()] [r (cddr r)]) ([i (in-range (cadr r))])
              (define-values (a a-rest) (loop r))
              (values (cons a accum) a-rest)))
          (values (reverse rev) rest)]
         [(#:mpi)
          (values (vector-ref mpis (cadr r)) (cddr r))]
         [(#:hash #:hasheq #:hasheqv)
          (define ht (case i
                       [(#:hash) (hash)]
                       [(#:hasheq) (hasheq)]
                       [(#:hasheqv) (hasheqv)]))
          (for/fold ([ht ht] [r (cddr r)]) ([i (in-range (cadr r))])
            (define-values (k k-rest) (loop r))
            (define-values (v v-rest) (loop k-rest))
            (values (hash-set ht k v) v-rest))]
         [(#:provided)
          (define-values (bdg bdg-rest) (loop (cdr r)))
          (define-values (prot? prot?-rest) (loop bdg-rest))
          (define-values (stx? stx?-rest) (loop prot?-rest))
          (values (provided bdg prot? stx?) stx?-rest)]
         [(#:module-binding)
          (values 'binding (discard r 10))]
         [(#:simple-module-binding)
          (values 'binding (discard r 4))]
         [else
          (cond
            [(or (symbol? i)
                 (number? i)
                 (string? i)
                 (null? i)
                 (hash? i)
                 (boolean? i))
             (values i (cdr r))]
            [else
             (error 'deserialize "unsupported instruction: ~s" i)])])])))

(define (syntax-module-path-index-shift . args)
  (error 'syntax-module-path-index-shift "not supported"))

(define (syntax-shift-phase-level . args)
  (error 'syntax-shift-phase-level "not supported"))

(define deserialize-instance
  (make-instance 'deserialize #f 'constant
                 'deserialize-module-path-indexes deserialize-module-path-indexes
                 'syntax-module-path-index-shift syntax-module-path-index-shift
                 'syntax-shift-phase-level syntax-shift-phase-level
                 'module-use module-use
                 'deserialize deserialize))
