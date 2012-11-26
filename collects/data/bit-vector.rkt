#lang racket/base
(require (for-syntax racket/base
                     unstable/wrapc
                     syntax/for-body)
         racket/match
         racket/dict         
         racket/contract/base
         racket/vector
         racket/unsafe/ops)

(define bits-in-a-word
  (if (fixnum? (expt 2 61)) 
      ; 32 or 64-bit fixnums?
      62 30))

(define largest-fixnum
  (- (expt 2 bits-in-a-word) 1))

(define ((bad-index-error who index))
  (raise-mismatch-error who "index out of range: " index))

(define (make-bit-vector size [fill #f])
  (define word-size (add1 (quotient size bits-in-a-word)))
  (define words (make-vector word-size (if fill largest-fixnum 0)))
  (bit-vector words size word-size))

(define (bit-vector* . init-bits)
  (define bv (make-bit-vector (length init-bits)))
  (for ([i (in-naturals)]
        [b (in-list init-bits)])
    (bit-vector-set! bv i b))
  bv)

(define (bit-vector-ref bv n 
                        [default (bad-index-error 'bit-vector-ref n)])
  (cond
    [(>= n (bit-vector-size bv))
     (if (procedure? default)
         (default)
         default)]
    [else
     (define-values (wi bi) (quotient/remainder n bits-in-a-word))
     (match bv
       [(struct bit-vector (words size word-size))
        (define word (vector-ref words wi))
        (define bit  (bitwise-bit-set? word bi))
        bit])]))

(define (bit-vector-iterate-first bv)
  (if (zero? (bit-vector-size bv)) #f 0))

(define (bit-vector-iterate-next bv pos)
  (if (>= (+ pos 1) (bit-vector-size bv))
      #f
      (+ pos 1)))

(define (bit-vector-iterate-key bv key)
  key)

(define (bit-vector-iterate-value bv key)
  (bit-vector-ref bv key))

(define (bit-vector-set! bv n b)
  (define-values (wi bi) (quotient/remainder n bits-in-a-word))
  (match bv
    [(struct bit-vector (words size word-size))
     (define word (vector-ref words wi))
     (define bit  (bitwise-bit-set? word bi))
     (unless (eq? bit b)
       (vector-set! words wi (bitwise-xor word (expt 2 bi))))]))

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
                    ([n 0] [size (bit-vector-size bv)]) ;; loop bindings
                    (< n size) ;; pos-guard
                    ([(var) (bit-vector-ref bv n)]) ;; inner bindings
                    #t ;; pre-guard
                    #t ;; post-guard
                    ((add1 n) (bit-vector-size bv)))]))]
      [[(var ...) (in-bv bv-expr)]
       (with-syntax ([bv-expr-c (wrap-expr/c #'bit-vector? #'bv-expr #:macro #'in-bv)])
         (syntax/loc stx
           [(var ...) (in-bit-vector bit-expr-c)]))]
      [_ #f])))

(define (bit-vector-count bv)
  (bit-vector-size bv))

(define (bit-vector-copy bv)
  (bit-vector (vector-copy (bit-vector-words bv))
              (bit-vector-size bv)
              (bit-vector-word-size bv)))

; A bit vector is represented as a vector of words.
; Each word contains 30 or 62 bits depending on the size of a fixnum.
(struct bit-vector (words size word-size)
  ; words     is the vector of words
  ; size      is the number of bits in bitvector
  ; word-size is the number of words in words
  #:property prop:dict/contract
  (list (vector-immutable bit-vector-ref
                          bit-vector-set!
                          #f ;; set
                          #f ;; remove!
                          #f ;; remove
                          bit-vector-count
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
     (let ([vx (bit-vector-words x)]
           [vy (bit-vector-words y)]
           [nx (bit-vector-size x)]
           [ny (bit-vector-size y)]
           [wsx (bit-vector-word-size x)]
           [wsy (bit-vector-word-size y)])
       (and (= nx ny) (= wsx wsy) 
            (for/and ([index (in-range (- (vector-length vx) 1))])
              (eqv? (vector-ref vx index)
                    (vector-ref vy index)))
            ; TODO: check last word
            )))
   (define (hash-code x hc)
     (let ([v (bit-vector-words x)]
           [n (bit-vector-size x)]
           [ws (bit-vector-word-size x)])
       (bitwise-xor
        (hc ws) (hc n)
        (for/fold ([h 1]) ([i (in-range (vector-length v))])
          (bitwise-xor h (hc (vector-ref v i)))))))
   (define hash-proc  hash-code)
   (define hash2-proc hash-code)]
  #:property prop:sequence in-bit-vector)

(define (grow-bit-vector bv)
  (define s (bit-vector-size bv))
  (define w (bit-vector-words bv))
  (define v (make-vector (* 2 (vector-length w)) 0))
  (define new (bit-vector v (* 2 s) (bit-vector-word-size bv)))
  (for ([i (in-range (vector-length w))])
    (vector-set! v i (vector-ref w i)))
  new)

(define (shrink-bit-vector bv i)
  (define nws (add1 (quotient i bits-in-a-word)))
  (bit-vector (vector-copy (bit-vector-words bv) 0 nws)
              i nws))

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
                        ([bv (make-bit-vector (* 16 bits-in-a-word))]
                         [i 0])
                        (for-clause ...) 
                        middle-body ...
                        (let ([new-bv (if (eq? i (bit-vector-size bv))
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
  (-> bit-vector? exact-nonnegative-integer? any/c any)] 
 [bit-vector-count
  (-> bit-vector? any)]
 [bit-vector-copy
  (-> bit-vector? bit-vector?)])

(provide in-bit-vector for/bit-vector for*/bit-vector)
