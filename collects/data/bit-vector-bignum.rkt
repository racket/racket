#lang racket/base

;;; This implementation uses bignums to represent the bit-vector

(require (for-syntax racket/base
                     unstable/wrapc
                     syntax/for-body)
         racket/match
         racket/dict         
         racket/contract/base
         racket/vector
         racket/unsafe/ops)

(define ((bad-index-error who index))
  (raise-mismatch-error who "index out of range: " index))

(define (make-bit-vector size [fill #f])
  (cond
    [fill (bit-vector (- (arithmetic-shift 1 size) 1) size)]
    [else (bit-vector 0 size)]))

(define (bit-vector* . init-bits)
  (define bv (make-bit-vector (length init-bits)))
  (for ([i (in-naturals)]
        [b (in-list init-bits)])
    (bit-vector-set! bv i b))
  bv)

(define (bit-vector-iterate-first bv)
  (if (zero? (bit-vector-count bv)) #f 0))

(define (bit-vector-iterate-next bv pos)
  (if (>= (+ pos 1) (bit-vector-count bv))
      #f
      (+ pos 1)))

(define (bit-vector-iterate-key bv key)
  key)

(define (bit-vector-iterate-value bv key)
  (bit-vector-ref bv key))

(define (in-bit-vector/fun bv)
  (unless (bit-vector? bv)
    (raise-type-error 'in-bit-vector "bit-vector" bv))
  (in-dict-values bv))

(define-sequence-syntax in-bit-vector
  (lambda () #'in-bit-vector/fun)
  (lambda (stx)
    (syntax-case stx ()
      [[(var) (in-bv bv-expr)]
       (with-syntax ([bv-expr-c (wrap-expr/c #'bit-vector? #'bv-expr #:macro #'in-bv)])
         (syntax/loc stx
           [(var)
            (:do-in ([(bv) bv-expr-c])
                    (void) ;; outer-check; handled by contract
                    ([n 0] [size (bit-vector-count bv)]) ;; loop bindings
                    (< n size) ;; pos-guard
                    ([(var) (bit-vector-ref bv n)]) ;; inner bindings
                    #t ;; pre-guard
                    #t ;; post-guard
                    ((add1 n) (bit-vector-count bv)))]))]
      [[(var ...) (in-bv bv-expr)]
       (with-syntax ([bv-expr-c (wrap-expr/c #'bit-vector? #'bv-expr #:macro #'in-bv)])
         (syntax/loc stx
           [(var ...) (in-bit-vector bit-expr-c)]))]
      [_ #f])))

(define (bit-vector-copy bv)
  (bit-vector (bit-vector-bits bv)
              (bit-vector-count bv)))

(define (bit-vector-ref bv n 
                        [default (bad-index-error 'bit-vector-ref n)])
  (cond
    [(>= n (bit-vector-count bv))
     (if (procedure? default)
         (default)
         default)]
    [else
     (bitwise-bit-set? (bit-vector-bits bv) n)]))

(define (bit-vector-set! bv n b)
  (define bits (bit-vector-bits bv))
  (define mask (arithmetic-shift 1 n))
  (cond
    [b 
     (set-bit-vector-bits! bv (bitwise-ior bits mask))]
    [(bitwise-bit-set? bits n)
     (set-bit-vector-bits! bv (bitwise-xor bits mask))]
    [else (void)]))

(define (bit-vector-count* bv)
  (bit-vector-count bv))

; A bit vector is represented as a bignum
(struct bit-vector (bits count)
  #:property prop:dict/contract
  (list (vector-immutable bit-vector-ref
                          bit-vector-set!
                          #f ;; set
                          #f ;; remove!
                          #f ;; remove
                          bit-vector-count*
                          bit-vector-iterate-first
                          bit-vector-iterate-next
                          bit-vector-iterate-key
                          bit-vector-iterate-value)
        (vector-immutable exact-nonnegative-integer?
                          boolean?
                          exact-nonnegative-integer?
                          #f #f #f))
  #:methods gen:equal+hash
  [(define (equal-proc x y recursive-equal?)
     (and (= (bit-vector-bits x) (bit-vector-bits y))
          (= (bit-vector-count x) (bit-vector-count y))))
   (define (hash-code x hc)
     (bitwise-xor
      (hc (bit-vector-bits x)) (hc (bit-vector-count x))))
   (define hash-proc  hash-code)
   (define hash2-proc hash-code)]
  #:property prop:sequence in-bit-vector
  #:mutable)

(define (grow-bit-vector bv)
  (set-bit-vector-count! bv (+ (bit-vector-count bv) 1))
  bv)

(define (shrink-bit-vector bv i)
  (set-bit-vector-count! bv i)
  bv)

(define-for-syntax (for_/vector stx orig-stx for_/vector-stx 
                                for_/fold/derived-stx wrap-all?)
  (syntax-case stx ()
    [(_ (for-clause ...) body ...)
     (with-syntax ([orig-stx orig-stx]
                   [for_/fold/derived for_/fold/derived-stx]
                   [((middle-body ...) (last-body ...)) 
                    (split-for-body stx #'(body ...))])
       (syntax/loc stx
         (let-values ([(bv i)
                       (for_/fold/derived
                        orig-stx
                        ([bv (make-bit-vector 0)]
                         [i 0])
                        (for-clause ...) 
                        middle-body ...
                        (let ([new-bv (if (eq? i (bit-vector-count bv))
                                          (grow-bit-vector bv)
                                          bv)])
                          (bit-vector-set! new-bv i (let () last-body ...))
                          (values new-bv (unsafe-fx+ i 1))))])
           (shrink-bit-vector bv i))))]
    [(_ #:length length-expr #:fill fill-expr (for-clause ...) body ...)
     (with-syntax ([orig-stx orig-stx]
                   [(limited-for-clause ...)
                    ;; If `wrap-all?', wrap all binding clauses. Otherwise, wrap
                    ;; only the first and the first after each keyword clause:
                    (let loop ([fcs (syntax->list #'(for-clause ...))] [wrap? #t])
                      (cond
                        [(null? fcs) null]
                        [(keyword? (syntax-e (car fcs)))
                         (if (null? (cdr fcs))
                             fcs
                             (list* (car fcs) (cadr fcs) (loop (cddr fcs) #t)))]
                        [(not wrap?)
                         (cons (car fcs) (loop (cdr fcs) #f))]
                        [else
                         (define fc (car fcs))
                         (define wrapped-fc
                           (syntax-case fc ()
                             [[ids rhs]
                              (or (identifier? #'ids)
                                  (let ([l (syntax->list #'ids)])
                                    (and l (andmap identifier? l))))
                              (syntax/loc fc [ids (stop-after
                                                   rhs
                                                   (lambda x
                                                     (unsafe-fx= i len)))])]
                             [_ fc]))
                         (cons wrapped-fc
                               (loop (cdr fcs) wrap-all?))]))]
                   [((middle-body ...) (last-body ...)) (split-for-body stx #'(body ...))]
                   [for_/vector for_/vector-stx]
                   [for_/fold/derived for_/fold/derived-stx])
       (syntax/loc stx
         (let ([len length-expr])
           (unless (exact-nonnegative-integer? len)
             (raise-argument-error 'for_/vector "exact-nonnegative-integer?" len))
           (let ([v (make-bit-vector len fill-expr)])
             (unless (zero? len)
               (for_/fold/derived
                orig-stx 
                ([i 0])
                (limited-for-clause ...)
                middle-body ...
                (bit-vector-set! v i (let () last-body ...))
                (add1 i)))
             v))))]
    [(_ #:length length-expr (for-clause ...) body ...)
     (for_/vector #'(fv #:length length-expr #:fill #f (for-clause ...) body ...) 
                  orig-stx for_/vector-stx for_/fold/derived-stx wrap-all?)]))

(define-syntax (for/bit-vector stx)
  (for_/vector stx stx #'for/bit-vector #'for/fold/derived #f))

(define-syntax (for*/bit-vector stx)
  (for_/vector stx stx #'for*/bit-vector #'for*/fold/derived #t))

(provide/contract
 [bit-vector?
  (-> any/c any)]
 [rename bit-vector* bit-vector
         (->* () () #:rest (listof boolean?) bit-vector?)]
 [make-bit-vector
  (->* (exact-nonnegative-integer?) (boolean?)   bit-vector?)]
 [bit-vector-ref
  (->* (bit-vector? exact-nonnegative-integer?) (any/c) any)]
 [bit-vector-set!
  (-> bit-vector? exact-nonnegative-integer? boolean? any)] 
 [bit-vector-count
  (-> bit-vector? any)]
 [bit-vector-copy
  (-> bit-vector? bit-vector?)])

(provide in-bit-vector
         for/bit-vector 
         for*/bit-vector)
