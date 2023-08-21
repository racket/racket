#lang racket/base
(require racket/linklet
         compiler/zo-parse
         compiler/zo-marshal
         compiler/faslable-correlated
         racket/phase+space)

;; Re-implement just enough deserialization to deal with 'decl
;; linklets, so we can get `required`, etc.

(provide deserialize-instance
         (struct-out module-use)

         deserialize-requires-and-provides

         (struct-out faslable-correlated-linklet)
         strip-correlated)

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
         [(#:hash #:hashalw #:hasheq #:hasheqv #:hasheqv/phase+space)
          (define ht (case i
                       [(#:hash) (hash)]
                       [(#:hashalw) (hashalw)]
                       [(#:hasheq) (hasheq)]
                       [(#:hasheqv  #:hasheqv/phase+space) (hasheqv)]))
          (for/fold ([ht ht] [r (cddr r)]) ([j (in-range (cadr r))])
            (define-values (k k-rest)
              (if (and (eq? i '#:hasheqv/phase+space)
                       (pair? (car r)))
                  (values (phase+space (caar r) (cdar r))
                          (cdr r))
                  (loop r)))
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
                 (boolean? i)
                 (and (pair? i)
                      (phase? (car i))
                      (symbol? (cdr i))))
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

(define (make-eager-instance)
  (make-instance 'instance #f 'constant
                 '.namespace (current-namespace)
                 '.dest-phase 0
                 '.self (module-path-index-join #f #f)
                 '.bulk-binding-registry #f
                 '.inspector (current-inspector)
                 'swap-top-level-scopes (lambda (s original-scopes-s new-ns) s)))

;; ----------------------------------------

;; Returns (values mpi-vector requires recur-requires provides phase-to-link-modules)
(define (deserialize-requires-and-provides l)
  (define ht (linkl-bundle-table l))
  (let ([data-l (hash-ref ht 'data #f)]  ; for module
        [decl-l (hash-ref ht 'decl #f)]  ; for module
        [link-l (hash-ref ht 'link #f)]) ; for top level
    (define (zo->linklet l)
      (cond
        [(faslable-correlated-linklet? l)
         (compile-linklet (strip-correlated (faslable-correlated-linklet-expr l))
                          (faslable-correlated-linklet-name l))]
        [(linklet? l) l]
        [else
         (let ([o (open-output-bytes)])
           (zo-marshal-to (linkl-bundle (hasheq 'data l)) o)
           (parameterize ([read-accept-compiled #t])
             (define b (read (open-input-bytes (get-output-bytes o))))
             (hash-ref (linklet-bundle->hash b) 'data)))]))
    (cond
      [(and data-l
            decl-l)
       (define data-i (instantiate-linklet (zo->linklet data-l)
                                           (list deserialize-instance)))
       (define decl-i (instantiate-linklet (zo->linklet decl-l)
                                           (list deserialize-instance
                                                 data-i)))
       (values (instance-variable-value data-i '.mpi-vector)
               (instance-variable-value decl-i 'requires)
               (instance-variable-value decl-i 'recur-requires)
               (instance-variable-value decl-i 'provides)
               (instance-variable-value decl-i 'phase-to-link-modules))]
      [link-l
       (define link-i (instantiate-linklet (zo->linklet link-l)
                                           (list deserialize-instance
                                                 (make-eager-instance))))
       (values (instance-variable-value link-i '.mpi-vector)
               '()
               '()
               '#hasheqv()
               (instance-variable-value link-i 'phase-to-link-modules))]
      [else (values '#() '() '() '#hasheqv() '#hasheqv())])))
