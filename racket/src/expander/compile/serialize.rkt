#lang racket/base
(require (for-syntax racket/base)
         racket/fasl
         "serialize-property.rkt"
         "serialize-state.rkt"
         "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../syntax/binding-table.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/module-binding.rkt"
         "../syntax/local-binding.rkt"
         "../syntax/bulk-binding.rkt"
         "../namespace/provided.rkt"
         "../common/module-path.rkt"
         "../common/module-path-intern.rkt"
         "module-use.rkt"
         "../host/linklet.rkt"
         "built-in-symbol.rkt"
         "reserved-symbol.rkt")

;; Serialization is mostly for syntax object and module path indexes.
;;
;; Serialization is implemented by a combination of direct handling
;; for some primitive datatypes, `prop:serialize` handlers attached
;; to some structure types, and deserialization functions provided
;; by the same modules as the serialization handlers.
;;
;; Module path indexes are serialized to code that runs to reconstruct
;; the module path indexes. Syntax objects and other data is
;; serialized to somewhat expression-shaped data and interpreted for
;; deserialization, where that interpretation can refer to an array of
;; already-deserialized module path indexes.
;;
;; To support sharing and cycles, serialized data is represented by:
;;
;;  - a vector of "shell" descriptions to allocate mutatable objects,
;;    such as mutable vectors and hash tables;
;;
;;  - a vector of initializations for shared, immutable values (which
;;    can refer to mutable values)
;;
;;  - a vector of "fill" descriptions to complete the construction of
;;    mutable values (whcih can refer to mutable and shared values);
;;    and
;;
;;  - a final value construction (which can refer to shared and
;;    mutable values).
;;
;; In general, a deserialized object is represented as a pair of a
;; symbol tag and data, including a `quote` tag to represent arbitrary
;; quoted data (that's non-cyclic and with no internal sharing). A few
;; special cases enable a more compact representation:
;;
;;  - numbers, booleans, symbols, and path srclocs are represented
;;    as themselves (i.e., self-quoting, in a sense);
;;
;;  - #&<number> is a reference to a mutable or shared value at
;;    position <number> in a deserialization array;
;;
;;  - #(<elem> ...) is a srcloc whose source is not a path
;;
;;  - #:inspector and #:bulk-binding-registry refer to
;;    instantiation-time values supplied as imported to the
;;    deserializing linklet
;;
;; In addition to all the complexities of detecting sharing and cycles
;; and breaking cycles on mutable boundaries, the serialization
;; process also prunes unreachable scopes and interns some values that
;; formerly were not shared.

(provide make-module-path-index-table
         add-module-path-index!
         add-module-path-index!/pos
         generate-module-path-index-deserialize
         mpis-as-vector

         generate-module-data-linklet
         generate-module-declaration-linklet

         generate-deserialize

         deserialize-instance
         deserialize-imports
         
         serialize-module-uses
         serialize-phase-to-link-module-uses)

;; ----------------------------------------
;; Module path index serialization

(struct module-path-index-table (positions intern))

(define (make-module-path-index-table)
  (module-path-index-table (make-hasheq) ; module-path-index -> pos
                           (make-module-path-index-intern-table)))

(define (add-module-path-index! mpis mpi)
  (define pos
    (add-module-path-index!/pos mpis mpi))
  (and pos
       `(unsafe-vector*-ref ,mpi-vector-id ,pos)))

(define (add-module-path-index!/pos mpis mpi)
  (cond
   [(not mpi) #f]
   [mpi
    (let ([mpi (intern-module-path-index! (module-path-index-table-intern mpis) mpi)]
          [positions (module-path-index-table-positions mpis)])
      (or (hash-ref positions mpi #f)
          (let ([pos (hash-count positions)])
            (hash-set! positions mpi pos)
            pos)))]))

(define (generate-module-path-index-deserialize mpis)
  (define (unique-list v)
    (if (pair? v)
        (for/list ([i (in-list v)]) i) ; avoid non-deterministic sharing
        v))
  (define positions (module-path-index-table-positions mpis))
  (define gen-order (make-hasheqv))
  (define rev-positions
    (for/hasheqv ([(k v) (in-hash positions)])
      (values v k)))
  ;; Create mpis used earlier first:
  (for ([i (in-range (hash-count rev-positions))])
    (define mpi (hash-ref rev-positions i))
    (let loop ([mpi mpi])
      (unless (hash-ref gen-order mpi #f)
        (define-values (name base) (module-path-index-split mpi))
        (when base
          (loop base))
        (hash-set! gen-order mpi (hash-count gen-order)))))
  (define rev-gen-order
    (for/hasheqv ([(k v) (in-hash gen-order)])
      (values v k)))
  (define gens
    (for/vector #:length (hash-count gen-order) ([i (in-range (hash-count gen-order))])
      (define mpi (hash-ref rev-gen-order i))
      (define-values (path base) (module-path-index-split mpi))
      (cond
       [(top-level-module-path-index? mpi)
        'top]
       [(not path)
        (box (or (unique-list
                  (resolved-module-path-name
                   (module-path-index-resolved mpi)))
                 'self))]
       [(not base)
        (vector path)]
       [base
        (vector path (hash-ref gen-order base))])))
  `(deserialize-module-path-indexes
    ;; Vector of deserialization instructions, where earlier
    ;; must be constructed first:
    ',gens
    ;; Vector of reordering to match reference order:
    ',(for/vector ([i (in-range (hash-count rev-positions))])
        (hash-ref gen-order (hash-ref rev-positions i)))))

(define (deserialize-module-path-indexes gen-vec order-vec)
  (define gen (make-vector (vector-length gen-vec) #f))
  (for ([d (in-vector gen-vec)]
        [i (in-naturals)])
    (vector-set!
     gen
     i
     (cond
      [(eq? d 'top) (deserialize-module-path-index)]
      [(box? d) (deserialize-module-path-index (unbox d))]
      [else
       (deserialize-module-path-index (vector*-ref d 0)
                                      (and ((vector*-length d) . > . 1)
                                           (vector*-ref gen (vector*-ref d 1))))])))
  (for/vector #:length (vector-length order-vec) ([p (in-vector order-vec)])
              (vector*-ref gen p)))

(define (mpis-as-vector mpis)
  (define positions (module-path-index-table-positions mpis))
  (define vec (make-vector (hash-count positions) #f))
  (for ([(mpi pos) (in-hash positions)])
    (vector-set! vec pos mpi))
  vec)

;; Convert `let*` into chunks of `let` as much as possible
(define (make-let* bindings body)
  (let loop ([vars #hasheq()] [group null] [bindings bindings])
    (cond
     [(null? bindings) `(let-values ,(reverse group) ,body)]
     [(has-symbol? (cadar bindings) vars)
      `(let-values ,(reverse group) ,(loop #hasheq() null bindings))]
     [else
      (loop (hash-set vars (caaar bindings) #t)
            (cons (car bindings) group)
            (cdr bindings))])))

(define (has-symbol? d vars)
  (or (and (symbol? d) (hash-ref vars d #f))
      (and (pair? d)
           (or (has-symbol? (car d) vars)
               (has-symbol? (cdr d) vars)))))

;; ----------------------------------------

(define (generate-module-data-linklet mpis)
  `(linklet
    ;; imports
    (,deserialize-imports)
    ;; exports
    (,mpi-vector-id)
    ;; body
    (define-values (,inspector-id) (current-code-inspector))
    (define-values (,mpi-vector-id)
      ,(generate-module-path-index-deserialize mpis))))

(define (generate-module-declaration-linklet mpis self requires provides
                                             phase-to-link-module-uses-expr)
  `(linklet
    ;; imports
    (,deserialize-imports
     [,mpi-vector-id])
    ;; exports
    (self-mpi
     requires
     provides
     phase-to-link-modules)
    ;; body
    (define-values (self-mpi) ,(add-module-path-index! mpis self))
    (define-values (requires) ,(generate-deserialize requires mpis #:syntax-support? #f))
    (define-values (provides) ,(generate-deserialize provides mpis #:syntax-support? #f))
    (define-values (phase-to-link-modules) ,phase-to-link-module-uses-expr)))

;; ----------------------------------------
;; Module-use serialization --- as an expression, like module path
;; indexes, and unlike everything else

(define (serialize-module-uses mus mpis)
  (for/list ([mu (in-list mus)])
    `(module-use
      ,(add-module-path-index! mpis (module-use-module mu))
      ,(module-use-phase mu))))

(define (interned-literal? v)
  (or (null? v)
      (boolean? v)
      (and (fixnum? v)
           (v . < . (sub1 (expt 2 30)))
           (v . > . (- (expt 2 30))))
      (symbol? v)
      (char? v)
      (keyword? v)))

(define (serialize-phase-to-link-module-uses phase-to-link-module-uses mpis)
  (define phases-in-order (sort (hash-keys phase-to-link-module-uses) <))
  `(hasheqv ,@(apply
               append
               (for/list ([phase (in-list phases-in-order)])
                 (list phase `(list ,@(serialize-module-uses (hash-ref phase-to-link-module-uses phase)
                                                             mpis)))))))

;; ----------------------------------------
;; Serialization for everything else

(define (generate-deserialize v mpis #:syntax-support? [syntax-support? #t])
  (define reachable-scopes (find-reachable-scopes v))
  
  (define state (make-serialize-state reachable-scopes))
  
  (define mutables (make-hasheq)) ; v -> pos
  (define objs (make-hasheq))     ; v -> step
  (define shares (make-hasheq))   ; v -> #t
  (define obj-step 0)
  
  ;; Build table of sharing and mutable values
  (define frontier null)
  (define add-frontier!
    (case-lambda
      [(v) (set! frontier (cons v frontier))]
      [(kind v) (add-frontier! v)]))
  (let frontier-loop ([v v])
    (let loop ([v v])
      (cond
       [(or (interned-literal? v)
            (module-path-index? v))
        ;; no need to find sharing
        (void)]
       [(hash-ref objs v #f)
        (unless (hash-ref mutables v #f)
          (hash-set! shares v #t))]
       [else
        (cond
         [(serialize-fill!? v)
          ;; Assume no sharing in non-mutable part
          (hash-set! mutables v (hash-count mutables))
          ((serialize-fill!-ref v) v add-frontier! state)]
         [(serialize? v)
          ((serialize-ref v) v 
           (case-lambda
             [(sub-v) (loop sub-v)]
             [(kind sub-v) (loop sub-v)])
           state)]
         [(pair? v)
          (loop (car v))
          (loop (cdr v))]
         [(vector? v)
          (if (or (immutable? v)
                  (zero? (vector-length v)))
              (for ([e (in-vector v)])
                (loop e))
              (begin
                (hash-set! mutables v (hash-count mutables))
                (for ([e (in-vector v)])
                  (add-frontier! e))))]
         [(box? v)
          (if (immutable? v)
              (loop (unbox v))
              (begin
                (hash-set! mutables v (hash-count mutables))
                (add-frontier! (unbox v))))]
         [(hash? v)
          (if (immutable? v)
              (for ([k (in-list (sorted-hash-keys v))])
                (loop k)
                (loop (hash-ref v k)))
              (begin
                (hash-set! mutables v (hash-count mutables))
                (for ([k (in-list (sorted-hash-keys v))])
                  (add-frontier! k)
                  (add-frontier! (hash-ref v k)))))]
         [(prefab-struct-key v)
          (for ([e (in-vector (struct->vector v) 1)])
            (loop e))]
         [(srcloc? v)
          (unless (path? (srcloc-source v))
            (for ([e (in-vector (struct->vector v) 1)])
              (loop e)))]
         [else
          (void)])
        ;; `v` may already be in `objs`, but to get the order right
        ;; for unmarshaling, we need to map it to ka new step number
        (hash-set! objs v obj-step)
        (set! obj-step (add1 obj-step))]))
    (unless (null? frontier)
      (define l frontier)
      (set! frontier null)
      (for ([v (in-list l)])
        (frontier-loop v))))

  ;; Maybe object steps to positions in a vector after mutables
  (define num-mutables (hash-count mutables))
  (define share-step-positions
    (let ([share-steps (for/list ([obj (in-hash-keys shares)])
                         (hash-ref objs obj))])
      (for/hasheqv ([step (in-list (sort share-steps <))]
                    [pos (in-naturals num-mutables)])
        (values step pos))))
  
  ;; Accumulate the serialized stream:
  (define stream null)
  (define stream-size 0)

  (define (next-push-position) stream-size)
  
  (define (quoted? pos)
    (define v (list-ref stream (- stream-size (add1 pos))))
    (or (not (keyword? v))
        (eq? '#:quote v)))
  
  (define (ser-reset! pos)
    (set! stream (list-tail stream (- stream-size pos)))
    (set! stream-size pos))
  
  (define (reap-stream!)
    (begin0
     (list->vector (reverse stream))
     (set! stream null)
     (set! stream-size 0)))

  ;; Handle a reference to an object that may be shared
  ;; or mutable
  (define ser-push!
    (case-lambda
      [(v)
       (cond
        [(hash-ref shares v #f)
         (define n (hash-ref share-step-positions (hash-ref objs v)))
         (ser-push! 'tag '#:ref)
         (ser-push! 'exact n)]
        [(hash-ref mutables v #f)
         => (lambda (n)
              (ser-push! 'tag '#:ref)
              (ser-push! 'exact n))]
        [else (ser-push-encoded! v)])]
      [(kind v)
       (case kind
         [(exact)
          (set! stream (cons v stream))
          (set! stream-size (add1 stream-size))]
         [(tag)
          (ser-push! 'exact v)]
         [(reference)
          (cond
           [(hash-ref shares v #f)
            (define n (hash-ref share-step-positions (hash-ref objs v)))
            (ser-push! 'exact n)]
           [(hash-ref mutables v #f)
            => (lambda (n)
                 (ser-push! 'exact n))]
           [else
            (ser-push! v)])]
         [else (ser-push! v)])]))
  
  ;; Handle an immutable, not-shared (or on RHS of binding) value
  (define (ser-push-encoded! v)
    (cond
     [(keyword? v)
      (ser-push! 'tag '#:quote)
      (ser-push! 'exact v)]
     [(module-path-index? v)
      (ser-push! 'tag '#:mpi)
      (ser-push! 'exact (add-module-path-index!/pos mpis v))]
     [(serialize? v)
      ((serialize-ref v) v ser-push! state)]
     [(and (list? v)
           (pair? v)
           (pair? (cdr v)))
      (define start-pos (next-push-position))
      (ser-push! 'tag '#:list)
      (ser-push! 'exact (length v))
      (define all-quoted?
        (for/fold ([all-quoted? #t]) ([i (in-list v)])
          (define i-pos (next-push-position))
          (ser-push! i)
          (and all-quoted?
               (quoted? i-pos))))
      (when all-quoted?
        (ser-reset! start-pos)
        (ser-push-optional-quote!)
        (ser-push! 'exact v))]
     [(pair? v)
      (define start-pos (next-push-position))
      (ser-push! 'tag '#:cons)
      (define a-pos (next-push-position))
      (ser-push! (car v))
      (define d-pos (next-push-position))
      (ser-push! (cdr v))
      (when (and (quoted? a-pos) (quoted? d-pos))
        (ser-reset! start-pos)
        (ser-push-optional-quote!)
        (ser-push! 'exact v))]
     [(box? v)
      (define start-pos (next-push-position))
      (ser-push! 'tag '#:box)
      (define v-pos (next-push-position))
      (ser-push! (unbox v))
      (when (quoted? v-pos)
        (ser-reset! start-pos)
        (ser-push-optional-quote!)
        (ser-push! 'exact v))]
     [(vector? v)
      (define start-pos (next-push-position))
      (ser-push! 'tag '#:vector)
      (ser-push! 'exact (vector-length v))
      (define all-quoted?
        (for/fold ([all-quoted? #t]) ([i (in-vector v)])
          (define i-pos (next-push-position))
          (ser-push! i)
          (and all-quoted?
               (quoted? i-pos))))
      (when all-quoted?
        (ser-reset! start-pos)
        (ser-push-optional-quote!)
        (ser-push! 'exact v))]
     [(hash? v)
      (define start-pos (next-push-position))
      (define as-set? (for/and ([val (in-hash-values v)])
                        (eq? val #t)))
      (ser-push! 'tag (if as-set?
                          (cond
                           [(hash-eq? v) '#:seteq]
                           [(hash-eqv? v) '#:seteqv]
                           [else '#:set])
                          (cond
                           [(hash-eq? v) '#:hasheq]
                           [(hash-eqv? v) '#:hasheqv]
                           [else '#:hash])))
      (ser-push! 'exact (hash-count v))
      (define ks (sorted-hash-keys v))
      (define all-quoted?
        (for/fold ([all-quoted? #t]) ([k (in-list ks)])
          (define k-pos (next-push-position))
          (ser-push! k)
          (define v-pos (next-push-position))
          (unless as-set?
            (ser-push! (hash-ref v k)))
          (and all-quoted?
               (quoted? k-pos)
               (or as-set? (quoted? v-pos)))))
      (when all-quoted?
        (ser-reset! start-pos)
        (ser-push-optional-quote!)
        (ser-push! 'exact v))]
     [(prefab-struct-key v)
      => (lambda (k)
           (define vec (struct->vector v))
           (define start-pos (next-push-position))
           (ser-push! 'tag '#:prefab)
           (ser-push! 'exact k)
           (ser-push! 'exact (sub1 (vector-length vec)))
           (define all-quoted?
             (for/fold ([all-quoted? #t]) ([i (in-vector vec 1)])
               (define i-pos (next-push-position))
               (ser-push! i)
               (and all-quoted?
                    (quoted? i-pos))))
           (when all-quoted?
             (ser-reset! start-pos)
             (ser-push-optional-quote!)
             (ser-push! 'exact v)))]
     [(srcloc? v)
      (cond
        [(path? (srcloc-source v))
         ;; Let core printer handle it --- and truncate the path if it
         ;; can't be made relative on serialize
         (ser-push-optional-quote!)
         (ser-push! 'exact v)]
        [else
         (ser-push! 'tag '#:srcloc)
         (ser-push! (srcloc-source v))
         (ser-push! (srcloc-line v))
         (ser-push! (srcloc-column v))
         (ser-push! (srcloc-position v))
         (ser-push! (srcloc-span v))])]
     [else
      (ser-push-optional-quote!)
      (ser-push! 'exact v)]))

  ;; A no-op, but can be made to push '#:quote as a debugging aid
  (define (ser-push-optional-quote!)
    ;; (ser-push! 'tag '#:quote)
    (void))
  
  ;; Generate the shell of a mutable value --- uses a different
  ;; encoding then the one for most other purposes
  (define (ser-shell! v)
    (cond
     [(serialize-fill!? v) ((serialize-ref v) v ser-push! state)]
     [(box? v) (ser-push! 'tag '#:box)]
     [(vector? v)
      (ser-push! 'tag '#:vector)
      (ser-push! 'exact (vector-length v))]
     [(hash? v) (ser-push! 'tag (cond
                                 [(hash-eq? v) '#:hasheq]
                                 [(hash-eqv? v) '#:hasheqv]
                                 [else '#:hash]))]
     [else
      (error 'ser-shell "unknown mutable: ~e" v)]))
  
  ;; Fill in the content of a mutable shell --- also a different
  ;; encoding
  (define (ser-shell-fill! v)
    (cond
     [(serialize-fill!? v) ((serialize-fill!-ref v) v ser-push! state)]
     [(box? v)
      (ser-push! 'tag '#:set-box!)
      (ser-push! (unbox v))]
     [(vector? v)
      (ser-push! 'tag '#:set-vector!)
      (ser-push! 'exact (vector-length v))
      (for ([v (in-vector v)])
        (ser-push! v))]
     [(hash? v)
      (ser-push! 'tag '#:set-hash!)
      (ser-push! 'exact (hash-count v))
      (define ks (sorted-hash-keys v))
      (for ([k (in-list ks)])
        (ser-push! k)
        (ser-push! (hash-ref v k)))]
     [else
      (error 'ser-shell-fill "unknown mutable: ~e" v)]))
  
  ;; Prepare mutable shells, first:
  (define rev-mutables (for/hasheqv ([(k v) (in-hash mutables)])
                         (values v k)))
  (define mutable-shell-bindings
    (begin
      (for ([i (in-range (hash-count mutables))])
        (ser-shell! (hash-ref rev-mutables i)))
      (reap-stream!)))
  
  ;; Prepare shared values:
  (define rev-shares (for/hasheqv ([obj (in-hash-keys shares)])
                       (values (hash-ref share-step-positions (hash-ref objs obj))
                               obj)))
  (define shared-bindings
    (begin
      (for ([i (in-range num-mutables (+ num-mutables (hash-count shares)))])
        (ser-push-encoded! (hash-ref rev-shares i)))
      (reap-stream!)))
  
  ;; Fill in mutable values
  (define mutable-fills
    (begin
      (for ([i (in-range (hash-count mutables))])
        (ser-shell-fill! (hash-ref rev-mutables i)))
      (reap-stream!)))

  ;; Final result:
  (define result
    (begin
      (ser-push! v)
      (reap-stream!)))
  
  ;; Put it all together:
  (define (finish mutable-shell-bindings-expr shared-bindings-expr  mutable-fills-expr result-expr)
    `(deserialize
      ,mpi-vector-id
      ,(if syntax-support? inspector-id #f)
      ,(if syntax-support? bulk-binding-registry-id #f)
      ',(hash-count mutables)
      ,mutable-shell-bindings-expr
      ',(hash-count shares)
      ,shared-bindings-expr
      ,mutable-fills-expr
      ,result-expr))

  ;; Putting the quoted-data construction into one vector makes
  ;; it easy to specialize in some back ends to a more compact
  ;; format.
  `(let-values ([(data) ',(vector mutable-shell-bindings
                                  shared-bindings
                                  mutable-fills
                                  result)])
     ,(finish '(unsafe-vector*-ref data 0)
              '(unsafe-vector*-ref data 1)
              '(unsafe-vector*-ref data 2)
              '(unsafe-vector*-ref data 3))))

(define (sorted-hash-keys ht)
  (define ks (hash-keys ht))
  (cond
   [(null? ks) ks]
   [(null? (cdr ks)) ks]
   [(andmap symbol? ks)
    (sort ks symbol<?)]
   [(andmap scope? ks)
    (sort ks scope<?)]
   [(andmap shifted-multi-scope? ks)
    (sort ks shifted-multi-scope<?)]
   [(andmap real? ks)
    (sort ks <)]
   [else
    ;; (log-error "unable to sort hash keys ~s" ks)
    ks]))

;; ----------------------------------------
;; Deserialization

;; The `deserialize` function interprets the encoding generated by
;; serialization (with hardwired calls to various deserialization
;; functions provided by other modules)

(define (deserialize mpis inspector bulk-binding-registry
                     num-mutables mutable-vec 
                     num-shared shared-vec
                     mutable-fill-vec
                     result-vec)
  (define shared (make-vector (+ num-mutables num-shared) 'uninit))
  ;; Make mutable shells
  (for/fold ([pos 0]) ([i (in-range num-mutables)])
    (define-values (d next-pos)
      (decode-shell mutable-vec pos mpis inspector bulk-binding-registry shared))
    (vector-set! shared i d)
    next-pos)
  ;; Construct shared values; note that later constructions can refer
  ;; to earlier ones
  (for/fold ([pos 0]) ([i (in-range num-mutables (+ num-mutables num-shared))])
    (define-values (d next-pos)
      (decode shared-vec pos mpis inspector bulk-binding-registry shared))
    (vector-set! shared i d)
    next-pos)
  ;; Fill in mutable shells
  (for/fold ([pos 0]) ([i (in-range num-mutables)]
                       [v (in-vector shared)])
    (decode-fill! v mutable-fill-vec pos mpis inspector bulk-binding-registry shared))
  ;; Construct the final result
  (define-values (result done-pos)
    (decode result-vec 0 mpis inspector bulk-binding-registry shared))
  result)

;; Decode the construction of a mutable variable
(define (decode-shell vec pos mpis inspector bulk-binding-registry shared)
  (case (vector*-ref vec pos)
    [(#:box) (values (box #f) (add1 pos))]
    [(#:vector) (values (make-vector (vector*-ref vec (add1 pos))) (+ pos 2))]
    [(#:hash) (values (make-hasheq) (add1 pos))]
    [(#:hasheq) (values (make-hasheq) (add1 pos))]
    [(#:hasheqv) (values (make-hasheqv) (add1 pos))]
    [else (decode vec pos mpis inspector bulk-binding-registry shared)]))

;; The decoder that is used for most purposes
(define (decode vec pos mpis inspector bulk-binding-registry shared)
  (define-syntax decodes
    (syntax-rules ()
      [(_ (id ...) rhs) (decodes #:pos (add1 pos) (id ...) rhs)]
      [(_ #:pos pos () rhs) (values rhs pos)]
      [(_ #:pos pos ([#:ref id0] id ...) rhs)
       (let-values ([(id0 next-pos) (let ([i (vector*-ref vec pos)])
                                      (if (exact-integer? i)
                                          (values (vector*-ref shared i) (add1 pos))
                                          (decode vec pos mpis inspector bulk-binding-registry shared)))])
         (decodes #:pos next-pos (id ...) rhs))]
      [(_ #:pos pos (id0 id ...) rhs)
       (let-values ([(id0 next-pos) (decode vec pos mpis inspector bulk-binding-registry shared)])
         (decodes #:pos next-pos (id ...) rhs))]))
  (define-syntax-rule (decode* (deser id ...))
    (decodes (id ...) (deser id ...)))
  (case (vector*-ref vec pos)
    [(#:ref)
     (values (vector*-ref shared (vector*-ref vec (add1 pos)))
             (+ pos 2))]
    [(#:inspector) (values inspector (add1 pos))]
    [(#:bulk-binding-registry) (values bulk-binding-registry (add1 pos))]
    [(#:syntax)
     (decodes
      (content [#:ref context] [#:ref srcloc])
      (deserialize-syntax content
                          context
                          srcloc
                          #f
                          #f
                          inspector))]
    [(#:datum->syntax)
     (decodes
      (content [#:ref context] [#:ref srcloc])
      (deserialize-datum->syntax content
                                 context
                                 srcloc
                                 inspector))]
    [(#:syntax+props)
     (decodes
      (content [#:ref context] [#:ref srcloc] props tamper)
      (deserialize-syntax content
                          context
                          srcloc
                          props
                          tamper
                          inspector))]
    [(#:srcloc)
     (decode* (srcloc source line column position span))]
    [(#:quote)
     (values (vector*-ref vec (add1 pos)) (+ pos 2))]
    [(#:mpi)
     (values (vector*-ref mpis (vector*-ref vec (add1 pos)))
             (+ pos 2))]
    [(#:box)
     (decode* (box-immutable v))]
    [(#:cons)
     (decode* (cons a d))]
    [(#:list #:vector)
     (define len (vector*-ref vec (add1 pos)))
     (define r (make-vector len))
     (define next-pos
       (for/fold ([pos (+ pos 2)]) ([i (in-range len)])
         (define-values (v next-pos) (decodes #:pos pos (v) v))
         (vector-set! r i v)
         next-pos))
     (values (if (eq? (vector*-ref vec pos) '#:list)
                 (vector->list r)
                 (vector->immutable-vector r))
             next-pos)]
    [(#:hash #:hasheq #:hasheqv)
     (define ht (case (vector*-ref vec pos)
                  [(#:hash) (hash)]
                  [(#:hasheq) (hasheq)]
                  [(#:hasheqv) (hasheqv)]))
     (define len (vector*-ref vec (add1 pos)))
     (for/fold ([ht ht] [pos (+ pos 2)]) ([i (in-range len)])
       (decodes #:pos pos (k v) (hash-set ht k v)))]
    [(#:set #:seteq #:seteqv)
     (define s (case (vector*-ref vec pos)
                 [(#:set) (set)]
                 [(#:seteq) (seteq)]
                 [(#:seteqv) (seteqv)]))
     (define len (vector*-ref vec (add1 pos)))
     (for/fold ([s s] [pos (+ pos 2)]) ([i (in-range len)])
       (decodes #:pos pos (k) (set-add s k)))]
    [(#:prefab)
     (define-values (key next-pos) (decodes #:pos (add1 pos) (k) k))
     (define len (vector*-ref vec next-pos))
     (define-values (r done-pos)
       (for/fold ([r null] [pos (add1 next-pos)]) ([i (in-range len)])
         (decodes #:pos pos (v) (cons v r))))
     (values (apply make-prefab-struct key (reverse r))
             done-pos)]
    [(#:scope)
     (decode* (deserialize-scope))]
    [(#:scope+kind)
     (decode* (deserialize-scope kind))]
    [(#:interned-scope)
     (decode* (make-interned-scope id))]
    [(#:multi-scope)
     (decode* (deserialize-multi-scope name scopes))]
    [(#:shifted-multi-scope)
     (decode* (deserialize-shifted-multi-scope phase multi-scope))]
    [(#:table-with-bulk-bindings)
     (decode* (deserialize-table-with-bulk-bindings syms bulk-bindings))]
    [(#:bulk-binding-at)
     (decode* (deserialize-bulk-binding-at scopes bulk))]
    [(#:representative-scope)
     (decode* (deserialize-representative-scope kind phase))]
    [(#:module-binding)
     (decode* (deserialize-full-module-binding
               module sym phase
               nominal-module
               nominal-phase
               nominal-sym
               nominal-require-phase
               free=id
               extra-inspector
               extra-nominal-bindings))]
    [(#:simple-module-binding)
     (decode* (deserialize-simple-module-binding module sym phase nominal-module))]
    [(#:local-binding)
     (decode* (deserialize-full-local-binding key free=id))]
    [(#:bulk-binding)
     (decode* (deserialize-bulk-binding prefix excepts mpi provide-phase-level phase-shift bulk-binding-registry))]
    [(#:provided)
     (decode* (deserialize-provided binding protected? syntax?))]
    [else
     (values (vector*-ref vec pos) (add1 pos))]))

;; Decode the filling of mutable values, which has its own encoding
;; variant
(define (decode-fill! v vec pos mpis inspector bulk-binding-registry shared)
  (case (vector*-ref vec pos)
    [(#f) (add1 pos)]
    [(#:set-box!)
     (define-values (c next-pos)
       (decode vec (add1 pos) mpis inspector bulk-binding-registry shared))
     (set-box! v c)
     next-pos]
    [(#:set-vector!)
     (define len (vector*-ref vec (add1 pos)))
     (for/fold ([pos (+ pos 2)]) ([i (in-range len)])
       (define-values (c next-pos)
         (decode vec pos mpis inspector bulk-binding-registry shared))
       (vector-set! v i c)
       next-pos)]
    [(#:set-hash!)
     (define len (vector*-ref vec (add1 pos)))
     (for/fold ([pos (+ pos 2)]) ([i (in-range len)])
       (define-values (key next-pos)
         (decode vec pos mpis inspector bulk-binding-registry shared))
       (define-values (val done-pos)
         (decode vec next-pos mpis inspector bulk-binding-registry shared))
       (hash-set! v key val)
       done-pos)]
    [(#:scope-fill!)
     (define-values (c next-pos)
       (decode vec (add1 pos) mpis inspector bulk-binding-registry shared))
     (deserialize-scope-fill! v c)
     next-pos]
    [(#:representative-scope-fill!)
     (define-values (a next-pos)
       (decode vec (add1 pos) mpis inspector bulk-binding-registry shared))
     (define-values (d done-pos)
       (decode vec next-pos mpis inspector bulk-binding-registry shared))
     (deserialize-representative-scope-fill! v a d)
     done-pos]
    [else
     (error 'deserialize "bad fill encoding: ~v" (vector*-ref vec pos))]))
 
;; ----------------------------------------
;; For pruning unreachable scopes in serialization

(define (find-reachable-scopes v)
  (define seen (make-hasheq))
  (define reachable-scopes (seteq))
  (define (get-reachable-scopes) reachable-scopes)
  (define scope-triggers (make-hasheq))

  (let loop ([v v])
    (cond
     [(interned-literal? v) (void)]
     [(hash-ref seen v #f) (void)]
     [else
      (hash-set! seen v #t)
      (cond
       [(scope-with-bindings? v)
        (set! reachable-scopes (set-add reachable-scopes v))
        
        ((reach-scopes-ref v) v loop)

        (for ([proc (in-list (hash-ref scope-triggers v null))])
          (proc loop))
        (hash-remove! scope-triggers v)
 
        ;; A binding may have a `free-id=?` equivalence;
        ;; that equivalence is reachable if all the scopes in the
        ;; binding set are reachable; for a so-far unreachable scope,
        ;; record a trigger in case the scope bcomes reachable later
        ((scope-with-bindings-ref v)
         v
         get-reachable-scopes
         loop
         (lambda (sc-unreachable b)
           (hash-update! scope-triggers
                         sc-unreachable
                         (lambda (l) (cons b l))
                         null)))]
       [(reach-scopes? v)
        ((reach-scopes-ref v) v loop)]
       [(pair? v)
        (loop (car v))
        (loop (cdr v))]
       [(vector? v)
        (for ([e (in-vector v)])
          (loop e))]
       [(box? v)
        (loop (unbox v))]
       [(hash? v)
        (for ([(k v) (in-hash v)])
          (loop k)
          (loop v))]
       [(prefab-struct-key v)
        (for ([e (in-vector (struct->vector v) 1)])
          (loop e))]
       [(srcloc? v)
        (loop (srcloc-source v))]
       [else
        (void)])]))
  
  reachable-scopes)

;; ----------------------------------------
;; Set up the instance to import into deserializing linklets

(define deserialize-imports
  '(deserialize-module-path-indexes
    syntax-module-path-index-shift
    syntax-shift-phase-level
    module-use
    deserialize))

;; To avoid a higher-order use of a keyword-accepting function:
(define syntax-module-path-index-shift/no-keywords
  (let ([syntax-module-path-index-shift
         (lambda (s from-mpi to-mpi [inspector #f])
           (syntax-module-path-index-shift s from-mpi to-mpi inspector))])
    syntax-module-path-index-shift))
    
(define deserialize-instance
  (make-instance 'deserialize #f 'constant
                 'deserialize-module-path-indexes deserialize-module-path-indexes
                 'syntax-module-path-index-shift syntax-module-path-index-shift/no-keywords
                 'syntax-shift-phase-level syntax-shift-phase-level
                 'module-use module-use
                 'deserialize deserialize))
