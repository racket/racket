#lang racket/base
(require (for-syntax racket/base racket/fixnum)
         racket/fixnum
         racket/unsafe/ops
         "../compile/serialize-property.rkt")

;; A scope set is represented as a list of scopes sorted by decreasing
;; scope id, plus a count and an order-independent hash code that is
;; maintained incrementally: each scope has a pseudo-random key, and
;; the set's code is the `fxxor` of its scopes' keys (as in Zobrist
;; hashing).
;;
;; New scopes have larger ids, and most operations add, remove, or
;; flip a scope that is among the most recent few in a set. With
;; the most recent scope first, those operations allocate only a few
;; pairs, and the result shares its tail with the original set.
;; Shared tails also let equality and subset tests stop early. The
;; hash code makes `equal?`-based hashing on scope sets constant time,
;; and it lets `scope-set=?` reject almost every pair of distinct sets
;; without looking at elements. The most recent scope, which is where
;; bindings are attached, is the first element.
;;
;; The representation relies on these properties of scopes:
;;
;;  * Distinct scopes have distinct ids, so that the sorted list for
;;    a set is unique. (The id counter in "../syntax/scope.rkt" is
;;    shared by all places and threads.)
;;
;;  * A scope's id and key never change.
;;
;; Scope sets are immutable, so they are safe to share among threads.
;; An operation that changes an old scope in a large set copies the
;; pairs for all of the newer scopes, so it takes time proportional
;; to the number of newer scopes.

(provide (struct-out scope-base)
         make-scope-key
         interned-scope-id-base

         scope-set?
         empty-scope-set
         scope-set-empty?
         scope-set-count
         scope-set-max
         scope-set-member?
         scope-set-add
         scope-set-remove
         scope-set-flip
         scope-set=?
         scope-set-subset?
         scope-set-subset-of-union?
         scope-set-union
         scope-set-subtract
         scope-set->list
         list->scope-set
         in-scope-set

         list->scope-ops
         scope-ops-update
         scope-ops-compose
         scope-set-apply-ops)

;; Every scope has an `id` (exact rational) and a fixnum `key`
(struct scope-base (id key)
  #:authentic)

;; Multipliers are fixnums on every platform, including 32-bit ones
(define-syntax (define-fixnum-constants stx)
  (syntax-case stx ()
    [(_ [id n] ...)
     (begin
       (for ([n (in-list (syntax->datum #'(n ...)))])
         (unless (fixnum-for-every-system? n)
           (raise-syntax-error #f "not a fixnum on every system" stx n)))
       #'(begin (define id n) ...))]))

(define-fixnum-constants
  [m1 #x1bd1e995]
  [m2 #x0c1b3c6d]
  [m3 #x097a2d39])

(define (mix n)
  (let* ([h (fx*/wraparound n m1)]
         [h (fxxor h (fxrshift h 17))]
         [h (fx*/wraparound h m2)]
         [h (fxxor h (fxrshift h 13))]
         [h (fx*/wraparound h m3)])
    (fxxor h (fxrshift h 16))))

;; Ids can be bignums on a 32-bit platform after enough scopes, so
;; only the low bits of a non-fixnum integer contribute to its key
(define (fixnum-bits n)
  (if (fixnum? n)
      n
      (bitwise-and n #x1fffffff)))

;; The key is a function of the id only, so hash codes for scope sets
;; are as deterministic as scope ids
(define (make-scope-key id)
  (cond
    [(fixnum? id) (mix id)]
    [(exact-integer? id) (mix (fixnum-bits id))]
    [else
     (mix (fx+/wraparound (fx*/wraparound (mix (fixnum-bits (numerator id))) 31)
                          (fixnum-bits (denominator id))))]))

;; Interned scopes get ids above this base, so they sort as the
;; newest scopes and sit at the front of every set. Code that adds and
;; removes an interned scope, such as a Rhombus space, then touches
;; only the front of a set instead of copying all of it. A fixnum on
;; 64-bit platforms; on a 32-bit platform these ids are bignums.
(define interned-scope-id-base (expt 2 59))

;; Scope ids are fixnums, except for the representatives of
;; multi-scopes (and, on a 32-bit platform, very new scopes and
;; interned scopes)
(define-syntax-rule (sc> a b)
  (> (scope-base-id a) (scope-base-id b)))

(struct scope-set (elems  ; list of scopes, decreasing by id
                   count  ; length of `elems`
                   code)  ; fxxor of scope keys
  #:authentic
  #:sealed
  #:property prop:equal+hash
  (list (lambda (a b eql?) (scope-set=? a b))
        (lambda (a hc) (scope-set-code a))
        (lambda (a hc) (scope-set-code a)))
  #:property prop:serialize
  (lambda (s ser-push! state)
    ;; Scopes in increasing order
    (ser-push! 'tag '#:scope-set)
    (ser-push! 'exact (scope-set-count s))
    (for ([sc (in-list (reverse (scope-set-elems s)))])
      (ser-push! sc)))
  #:property prop:reach-scopes
  (lambda (s extra-shifts reach)
    (for ([sc (in-list (scope-set-elems s))])
      (reach sc extra-shifts))))

(define empty-scope-set (scope-set null 0 0))

;; There is only one empty scope set
(define (make-scope-set l count code)
  (if (null? l)
      empty-scope-set
      (scope-set l count code)))

(define (scope-set-empty? s) (null? (scope-set-elems s)))

;; The most recent scope in a non-empty set that is not interned, or
;; the most recent interned scope if all are interned; this is the
;; scope that holds bindings for the set
(define (scope-set-max s)
  (define l (scope-set-elems s))
  (let loop ([p l])
    (cond
      [(null? p) (car l)]
      [(< (scope-base-id (car p)) interned-scope-id-base) (car p)]
      [else (loop (cdr p))])))

(define (scope-set-member? s sc)
  (let loop ([l (scope-set-elems s)])
    (cond
      [(null? l) #f]
      [(eq? (car l) sc) #t]
      [(sc> sc (car l)) #f]
      [else (loop (cdr l))])))

;; Copy the elements of `l` that precede its tail `p` onto `tail`
(define (prefix-onto l p tail)
  (let loop ([l l])
    (if (eq? l p)
        tail
        (cons (car l) (loop (cdr l))))))

;; Adding, removing, and flipping all find the place in the list
;; where `sc` is or would be: `p` is the tail of the elements `l` of
;; `s` that starts there. Each takes time and space proportional to
;; the number of scopes in `s` that are newer than `sc`.

(define-syntax-rule (define-scope-set-update (name s sc) l p
                      #:present present-expr
                      #:absent absent-expr)
  (define (name s sc)
    (define l (scope-set-elems s))
    (let loop ([p l])
      (cond
        [(or (null? p) (sc> sc (car p))) absent-expr]
        [(eq? (car p) sc) present-expr]
        [else (loop (cdr p))]))))

(define (scope-set-with s sc l p)
  (scope-set (prefix-onto l p (cons sc p))
             (fx+ (scope-set-count s) 1)
             (fxxor (scope-set-code s) (scope-base-key sc))))

(define (scope-set-without s sc l p)
  (make-scope-set (prefix-onto l p (cdr p))
                  (fx- (scope-set-count s) 1)
                  (fxxor (scope-set-code s) (scope-base-key sc))))

(define-scope-set-update (scope-set-add s sc) l p
  #:present s
  #:absent (scope-set-with s sc l p))

(define-scope-set-update (scope-set-remove s sc) l p
  #:present (scope-set-without s sc l p)
  #:absent s)

(define-scope-set-update (scope-set-flip s sc) l p
  #:present (scope-set-without s sc l p)
  #:absent (scope-set-with s sc l p))

(define (scope-set=? a b)
  (or (eq? a b)
      (and (fx= (scope-set-code a) (scope-set-code b))
           (fx= (scope-set-count a) (scope-set-count b))
           ;; The lists have the same length, and they may share a tail
           (let loop ([la (scope-set-elems a)] [lb (scope-set-elems b)])
             (or (eq? la lb)
                 (and (eq? (car la) (car lb))
                      (loop (cdr la) (cdr lb))))))))

;; A merge that stops as soon as the lists share a tail or `b` has too
;; few scopes left; `na` and `nb` are the lengths of `la` and `lb`
(define (scope-set-subset? a b)
  (or (eq? a b)
      (let loop ([la (scope-set-elems a)] [na (scope-set-count a)]
                 [lb (scope-set-elems b)] [nb (scope-set-count b)])
        (cond
          [(eq? la lb) #t]
          [(null? la) #t]
          [(fx< nb na) #f]
          [else
           (define x (car la))
           (define y (car lb))
           (cond
             [(eq? x y) (loop (cdr la) (fx- na 1) (cdr lb) (fx- nb 1))]
             [(sc> x y) #f]
             [else (loop la na (cdr lb) (fx- nb 1))])]))))

;; Is `a` a subset of the union of `b` and the scopes in the list
;; `extras`? The `extras` list is expected to be short. This is the
;; test that binding resolution needs, and it avoids constructing
;; the union.
(define (scope-set-subset-of-union? a b extras)
  (cond
    [(null? extras) (scope-set-subset? a b)]
    [else
     (let loop ([la (scope-set-elems a)] [lb (scope-set-elems b)])
       (cond
         [(eq? la lb) #t]
         [(null? la) #t]
         [else
          (define x (car la))
          (cond
            [(memq x extras) (loop (cdr la) lb)]
            [else
             (let b-loop ([lb lb])
               (cond
                 [(null? lb) #f]
                 [(eq? x (car lb)) (loop (cdr la) (cdr lb))]
                 [(sc> x (car lb)) #f]
                 [else (b-loop (cdr lb))]))])]))]))

(define (scope-set-union a b)
  (cond
    [(eq? a b) a]
    [(scope-set-empty? b) a]
    [(scope-set-empty? a) b]
    [else (scope-set-apply-ops a (sorted-scopes->ops (scope-set-elems b) 'add))]))

(define (scope-set-subtract a b)
  (if (scope-set-empty? b)
      a
      (scope-set-apply-ops a (sorted-scopes->ops (scope-set-elems b) 'remove))))

;; Most recent first
(define (scope-set->list s)
  (scope-set-elems s))

;; Elements of `l` can be in any order and can include duplicates
(define (list->scope-set l)
  (scope-set-apply-ops empty-scope-set (list->scope-ops l 'add)))

;; Iterates most recent first
(define-sequence-syntax in-scope-set
  (lambda () #'scope-set->list)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ s-expr)]
       #'[(id) (in-list (scope-set-elems s-expr))]]
      [_ #f])))

;; ----------------------------------------
;; Scope operations: a list of `(cons scope op)` with `op` as 'add,
;; 'remove, or 'flip, where each scope appears at most once, sorted by
;; decreasing scope id. Lazy propagation records pending operations
;; this way, so that composing two sets of pending operations is a
;; merge and applying them to a scope set is a merge.

;; Result of performing `new` after `cur` (which can be #f), where
;; #f means no operation
(define (compose-op cur new)
  (case new
    [(add) 'add]
    [(remove) 'remove]
    [else (case cur
            [(add) 'remove]
            [(remove) 'add]
            [(flip) #f]
            [else 'flip])]))

(define (sorted-scopes->ops scs op)
  (for/list ([sc (in-list scs)])
    (cons sc op)))

;; Operations to perform `op`, which must be 'add or 'remove, on each
;; scope in `scs`, which can be in any order and can include duplicates
(define (list->scope-ops scs op)
  (define (sorted? scs)
    (or (null? scs)
        (let loop ([sc (car scs)] [scs (cdr scs)])
          (or (null? scs)
              (and (sc> sc (car scs))
                   (loop (car scs) (cdr scs)))))))
  (define (remove-adjacent-duplicates scs)
    (cond
      [(or (null? scs) (null? (cdr scs))) scs]
      [(eq? (car scs) (cadr scs)) (remove-adjacent-duplicates (cdr scs))]
      [else (cons (car scs) (remove-adjacent-duplicates (cdr scs)))]))
  (sorted-scopes->ops (if (sorted? scs)
                          scs
                          (remove-adjacent-duplicates
                           (sort scs (lambda (a b) (sc> a b)))))
                      op))

;; Add `op` for `sc` after the operations in `ops`
(define (scope-ops-update ops sc op)
  (let loop ([l ops])
    (cond
      [(or (null? l) (sc> sc (caar l)))
       (prefix-onto ops l (cons (cons sc op) l))]
      [(eq? (caar l) sc)
       (define new-op (compose-op (cdar l) op))
       (cond
         [(eq? new-op (cdar l)) ops]
         [new-op (prefix-onto ops l (cons (cons sc new-op) (cdr l)))]
         [else (prefix-onto ops l (cdr l))])]
      [else (loop (cdr l))])))

;; Operations equivalent to `base` followed by `new`
(define (scope-ops-compose base new)
  (let loop ([b base] [n new])
    (cond
      [(null? n) b]
      [(null? b) n]
      [else
       (define bsc (caar b))
       (define nsc (caar n))
       (cond
         [(eq? bsc nsc)
          (define op (compose-op (cdar b) (cdar n)))
          (if op
              (cons (if (eq? op (cdar b)) (car b) (cons bsc op))
                    (loop (cdr b) (cdr n)))
              (loop (cdr b) (cdr n)))]
         [(sc> bsc nsc) (cons (car b) (loop (cdr b) n))]
         [else (cons (car n) (loop b (cdr n)))])])))

;; Apply scope operations to a scope set. The result shares the tail
;; of `s` after the oldest scope mentioned by `ops`.
(define (scope-set-apply-ops s ops)
  (cond
    [(null? ops) s]
    [(null? (cdr ops))
     (define sc (caar ops))
     (case (cdar ops)
       [(add) (scope-set-add s sc)]
       [(remove) (scope-set-remove s sc)]
       [else (scope-set-flip s sc)])]
    [else
     (define count (scope-set-count s))
     (define code (scope-set-code s))
     (define (changed! sc delta)
       (set! count (fx+ count delta))
       (set! code (fxxor code (scope-base-key sc))))
     (define new-l
       (let loop ([l (scope-set-elems s)] [ops ops])
         (cond
           [(null? ops) l]
           [else
            (define sc (caar ops))
            (define op (cdar ops))
            (cond
              [(and (pair? l) (sc> (car l) sc))
               ;; a scope in the set that is newer than the next op's scope
               (define rest (loop (cdr l) ops))
               (if (eq? rest (cdr l))
                   l
                   (cons (car l) rest))]
              [(and (pair? l) (eq? (car l) sc))
               ;; op for a scope in the set
               (cond
                 [(eq? op 'add) (loop l (cdr ops))]
                 [else
                  (changed! sc -1)
                  (loop (cdr l) (cdr ops))])]
              [else
               ;; op for a scope not in the set
               (cond
                 [(eq? op 'remove) (loop l (cdr ops))]
                 [else
                  (changed! sc 1)
                  (cons sc (loop l (cdr ops)))])])])))
     (if (eq? new-l (scope-set-elems s))
         s
         (make-scope-set new-l count code))]))

;; ----------------------------------------

(module+ test
  ;; Randomized comparison with `seteq`s, with scope sets built from
  ;; each other so that they share tails
  (define N 40)
  (define scopes
    (for/vector ([i (in-range N)])
      ;; include some non-integer ids, as for representative scopes
      ;; and some bignum ids, as on a 32-bit platform
      (define id (cond
                   [(zero? (modulo i 7)) (+ i 1/2)]
                   [(zero? (modulo i 11)) (+ (expt 2 62) i)]
                   [else i]))
      (scope-base id (make-scope-key id))))
  (define (random-scope) (vector-ref scopes (random N)))
  (define (random-op) (vector-ref #(add remove flip) (random 3)))

  (define (ref-op ref sc op)
    (case op
      [(add) (hash-set ref sc #t)]
      [(remove) (hash-remove ref sc)]
      [else (if (hash-ref ref sc #f)
                (hash-remove ref sc)
                (hash-set ref sc #t))]))
  (define (ref-list ref)
    (sort (hash-keys ref) > #:key scope-base-id))

  (define (check who ok?)
    (unless ok? (error 'scope-set-test "failed: ~a" who)))
  (define (check-same who s ref)
    (check who (equal? (scope-set->list s) (ref-list ref)))
    (check who (= (scope-set-count s) (hash-count ref)))
    (check who (eq? (scope-set-empty? s) (zero? (hash-count ref))))
    (check who (eq? (eq? s empty-scope-set) (zero? (hash-count ref))))
    (check who (= (scope-set-code s)
                  (for/fold ([code 0]) ([sc (in-hash-keys ref)])
                    (fxxor code (scope-base-key sc)))))
    (unless (scope-set-empty? s)
      (check who (eq? (scope-set-max s)
                      (let ([l (ref-list ref)])
                        (or (for/first ([sc (in-list l)]
                                        #:when (< (scope-base-id sc) interned-scope-id-base))
                              sc)
                            (car l)))))))

  ;; pool of (cons scope-set reference)
  (define POOL 64)
  (define pool (make-vector POOL (cons empty-scope-set (hasheq))))
  (define (pick) (vector-ref pool (random POOL)))
  (define (remember! s ref) (vector-set! pool (random POOL) (cons s ref)))

  (define (random-ops)
    (for/fold ([ops null]) ([i (in-range (random 6))])
      (scope-ops-update ops (random-scope) (random-op))))
  (define (ref-apply-ops ref ops)
    (for/fold ([ref ref]) ([o (in-list ops)])
      (ref-op ref (car o) (cdr o))))

  (for ([i (in-range 200000)])
    (define p (pick))
    (define s (car p))
    (define ref (cdr p))
    (case (random 8)
      [(0 1 2)
       (define sc (random-scope))
       (define op (random-op))
       (define new-s (case op
                       [(add) (scope-set-add s sc)]
                       [(remove) (scope-set-remove s sc)]
                       [else (scope-set-flip s sc)]))
       (define new-ref (ref-op ref sc op))
       (check-same op new-s new-ref)
       (when (equal? ref new-ref) (check 'unchanged (eq? s new-s)))
       (remember! new-s new-ref)]
      [(3)
       (define ops (random-ops))
       (define ops2 (random-ops))
       (define new-ref (ref-apply-ops (ref-apply-ops ref ops) ops2))
       (define new-s (scope-set-apply-ops s (scope-ops-compose ops ops2)))
       (check-same 'apply-ops new-s new-ref)
       (check 'apply-twice (scope-set=? new-s (scope-set-apply-ops (scope-set-apply-ops s ops) ops2)))
       (remember! new-s new-ref)]
      [(4)
       (define l (for/list ([i (in-range (random 8))]) (random-scope)))
       (define op (if (zero? (random 2)) 'add 'remove))
       (define new-ref (for/fold ([ref ref]) ([sc (in-list l)]) (ref-op ref sc op)))
       (check-same 'list-ops (scope-set-apply-ops s (list->scope-ops l op)) new-ref)
       (check-same 'list (list->scope-set l) (for/hasheq ([sc (in-list l)]) (values sc #t)))]
      [(5)
       (define sc (random-scope))
       (check 'member (eq? (scope-set-member? s sc) (hash-ref ref sc #f)))]
      [else
       (define p2 (pick))
       (define s2 (car p2))
       (define ref2 (cdr p2))
       (define same? (equal? ref ref2))
       (check '=? (eq? (scope-set=? s s2) same?))
       (check 'equal? (eq? (equal? s s2) same?))
       (when same? (check 'hash (= (equal-hash-code s) (equal-hash-code s2))))
       (check 'subset? (eq? (scope-set-subset? s s2) (hash-keys-subset? ref ref2)))
       (define extras (for/list ([i (in-range (random 3))]) (random-scope)))
       (check 'subset-of-union?
              (eq? (scope-set-subset-of-union? s s2 extras)
                   (hash-keys-subset? ref (for/fold ([ref ref2]) ([sc (in-list extras)])
                                            (hash-set ref sc #t)))))
       (check-same 'union (scope-set-union s s2)
                   (for/fold ([ref ref]) ([sc (in-hash-keys ref2)]) (hash-set ref sc #t)))
       (check-same 'subtract (scope-set-subtract s s2)
                   (for/fold ([ref ref]) ([sc (in-hash-keys ref2)]) (hash-remove ref sc)))
       (check 'iterate (equal? (for/list ([sc (in-scope-set s)]) sc) (scope-set->list s)))])))
