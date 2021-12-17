#lang racket/base

(require "stream.rkt"
         "private/sequence.rkt"
         "fixnum.rkt"
         "flonum.rkt"
         racket/contract/combinator
         racket/contract/base
         (for-syntax racket/base)
         (only-in racket/contract/private/prop
                  contract-struct-stronger?
                  contract-struct-equivalent?
                  trust-me)
         syntax/stx)

(provide empty-sequence
         sequence->list
         sequence-length
         sequence-ref
         sequence-tail
         sequence-append
         sequence-map
         sequence-andmap
         sequence-ormap
         sequence-for-each
         sequence-fold
         sequence-filter
         sequence-add-between
         sequence-count
         (rename-out [-sequence/c sequence/c])
         in-syntax
         (contract-out [in-slice (exact-positive-integer? sequence? . -> . any)]))

(define empty-sequence
  (make-do-sequence
   (λ ()
      (values
       void
       void
       #f
       (λ (pos) #f)
       #f
       #f))))

(define (sequence->list s)
  (for/list ([v s]) v))

(define (sequence-length s)
  (unless (sequence? s) (raise-argument-error 'sequence-length "sequence?" s))
  (cond [(exact-nonnegative-integer? s) s]
        [(list? s) (length s)]
        [(vector? s) (vector-length s)]
        [(flvector? s) (flvector-length s)]
        [(fxvector? s) (fxvector-length s)]
        [(string? s) (string-length s)]
        [(bytes? s) (bytes-length s)]
        [(hash? s) (hash-count s)]
        [else
         (for/fold ([c 0]) ([i (in-values*-sequence s)])
           (add1 c))]))

(define (sequence-ref s i)
  (unless (sequence? s) (raise-argument-error 'sequence-ref "sequence?" s))
  (unless (exact-nonnegative-integer? i)
    (raise-argument-error 'sequence-ref "exact-nonnegative-integer?" i))
  (let ([v (for/fold ([c #f]) ([v (in-values*-sequence s)]
                               [j (in-range (add1 i))]
                               #:unless (j . < . i))
             (or v '(#f)))])
    (cond
     [(not v)
      (raise-arguments-error
       'sequence-ref
       "sequence ended before index"
       "index" i
       "sequence" s)]
     [(list? v) (apply values v)]
     [else v])))

(define (sequence-tail seq i)
  (unless (sequence? seq) (raise-argument-error 'sequence-tail "sequence?" seq))
  (unless (exact-nonnegative-integer? i)
    (raise-argument-error 'sequence-tail "exact-nonnegative-integer?" i))
  (cond
   [(zero? i) seq]
   [(stream? seq) (stream-tail seq i)]
   [(exact-nonnegative-integer? seq) (stream-tail (in-range seq) i)]
   [else
    (make-do-sequence
     (lambda ()
       (let loop ([next (lambda () (sequence-generate* seq))] [n i])
         (cond
          [(zero? n)
           (let-values ([(vals next) (next)])
             (values (lambda (v+n) (apply values (car v+n)))
                     (lambda (v+n)
                       (let-values ([(vals next) ((cdr v+n))])
                         (cons vals next)))
                     (cons vals next)
                     car
                     #f
                     #f))]
          [else
           (let-values ([(vals next) (next)])
             (if vals
                 (loop next (sub1 n))
                 (raise-arguments-error
                  'sequence-ref
                  "sequence ended before index"
                  "index" i
                  "sequence" seq)))]))))]))

(define (sequence-append . l)
  (if (null? l)
      empty-stream
      (if (andmap stream? l)
          (apply stream-append l)
          (apply in-sequences l))))

(define (sequence-map f s)
  (unless (procedure? f) (raise-argument-error 'sequence-map "procedure?" f))
  (unless (sequence? s) (raise-argument-error 'sequence-map "sequence?" s))
  (if (stream? s)
      (stream-map f s)
      (make-do-sequence
       (lambda ()
         (let-values ([(vals next) (sequence-generate* s)])
           (values (lambda (v+n) (apply f (car v+n)))
                   (lambda (v+n)
                     (let-values ([(vals next) ((cdr v+n))])
                       (cons vals next)))
                   (cons vals next)
                   car
                   #f
                   #f))))))

(define (sequence-filter f s)
  (unless (procedure? f) (raise-argument-error 'sequence-filter "procedure?" f))
  (unless (sequence? s) (raise-argument-error 'sequence-filter "sequence?" s))
  (if (stream? s)
      (stream-filter f s)
      (make-do-sequence
       (lambda ()
         (let loop ([next (lambda () (sequence-generate* s))])
           (let-values ([(vals next) (next)])
             (if (or (not vals)
                     (apply f vals))
                 (values (lambda (v+n) (apply values (car v+n)))
                         (lambda (v+n)
                           (let loop ([next (cdr v+n)])
                             (let-values ([(vals next) (next)])
                               (if (or (not vals)
                                       (apply f vals))
                                   (cons vals next)
                                   (loop next)))))
                         (cons vals next)
                         car
                         #f
                         #f)
                 (loop next))))))))

(define (sequence-add-between s e)
  (unless (sequence? s) (raise-argument-error 'sequence-add-between "sequence?" s))
  (if (stream? s)
      (stream-add-between s e)
      (make-do-sequence
       (lambda ()
         (let-values ([(vals next) (sequence-generate* s)])
           (values (lambda (v+n) (let ([vals (car v+n)])
                                   (if (eq? vals #t)
                                       e
                                       (apply values vals))))
                   (lambda (v+n)
                     (if (eq? (car v+n) #t)
                         (cdr v+n)
                         (let-values ([(vals next) ((cdr v+n))])
                           (if vals
                               (cons #t (cons vals next))
                               (cons #f next)))))
                   (cons vals next)
                   car
                   #f
                   #f))))))


(define (sequence/c-name this)
  (define min-count (sequence/c-min-count this))
  (define ctcs (sequence/c-ctcs this))
  (apply build-compound-type-name 'sequence/c
         (append
          (if min-count
              (list '#:min-count min-count)
              '())
          ctcs)))

(define (sequence/c-first-order this)
  (define n-cs (length (sequence/c-ctcs this)))
   (λ (val)
     (cond
       [(list? val) (= n-cs 1)]
       [(vector? val) (= n-cs 1)]
       [(hash? val) (= n-cs 2)]
       [else (sequence? val)])))

(define (sequence/c-late-neg-projection this)
  (define ctcs (sequence/c-ctcs this))
  (define n-cs (length ctcs))
  (define min-count (sequence/c-min-count this))
  (define elem/mk-projs
    (for/list ([ctc (in-list ctcs)])
      (contract-late-neg-projection ctc)))
  (define list-late-neg-proj
    (and (= n-cs 1)
         (get/build-late-neg-projection (listof (car ctcs)))))
  (λ (orig-blame)
    (define blame (blame-add-context orig-blame "an element of"))
    (define ps (for/list ([mk-proj (in-list elem/mk-projs)])
                 (mk-proj blame)))
    (define lst/blame (and list-late-neg-proj (list-late-neg-proj orig-blame)))
    (cond
      [(= n-cs 1)
       (define p (car ps))
       (λ (seq neg-party)
         (cond
           [(list? seq)
            (when min-count
              (unless (>= (length seq) min-count)
                (raise-blame-error orig-blame #:missing-party neg-party seq
                                   '(expected: "a sequence with at least ~a elements" given: "~e")
                                   min-count seq)))
            (lst/blame seq neg-party)]
           [else
            (unless (sequence? seq)
              (raise-blame-error
               orig-blame #:missing-party neg-party seq
               '(expected: "a sequence" given: "~e")
               seq))
            (define blame+neg-party (cons orig-blame neg-party))
            (define result-seq
              (make-do-sequence
               (lambda ()
                 (let*-values ([(more? next) (sequence-generate seq)])
                   (values
                    (lambda (idx)
                      (call-with-values
                       next
                       (case-lambda
                         [(elem)
                          (with-contract-continuation-mark
                              blame+neg-party
                            (p elem neg-party))]
                         [elems
                          (raise-wrong-elem-count blame neg-party seq n-cs elems)])))
                    add1
                    0
                    (lambda (idx)
                      (define ans (more?))
                      (when (and min-count (idx . < . min-count))
                        (unless ans
                          (raise-too-few-elements orig-blame neg-party seq min-count)))
                      ans)
                    (lambda elems #t)
                    (lambda (idx . elems) #t))))))
            (cond
              [(stream? seq) (sequence->stream result-seq)]
              [else result-seq])]))]
      [else
       (λ (seq neg-party)
         (unless (sequence? seq)
           (raise-blame-error
            orig-blame #:missing-party neg-party seq
            '(expected: "a sequence" given: "~e")
            seq))
         (define blame+neg-party (cons orig-blame neg-party))
         (define result-seq
           (make-do-sequence
            (lambda ()
              (let*-values ([(more? next) (sequence-generate seq)])
                (values
                 (lambda (idx)
                   (call-with-values
                    next
                    (lambda elems
                      (with-contract-continuation-mark
                          blame+neg-party
                        (define n-elems (length elems))
                        (unless (= n-elems n-cs)
                          (raise-wrong-elem-count blame neg-party seq n-cs elems))
                        (apply
                         values
                         (for/list ([elem (in-list elems)]
                                    [p (in-list ps)])
                           (p elem neg-party)))))))
                 add1
                 0
                 (lambda (idx)
                   (define ans (more?))
                   (when (and min-count (idx . < . min-count))
                     (unless ans
                       (raise-too-few-elements orig-blame neg-party seq min-count)))
                   ans)
                 (lambda elems #t)
                 (lambda (idx . elems) #t))))))
         (cond
           [(list? seq) (sequence->list result-seq)]
           [(stream? seq) (sequence->stream result-seq)]
           [else result-seq]))])))

(define (raise-too-few-elements orig-blame neg-party seq min-count)
  (raise-blame-error
   orig-blame #:missing-party neg-party
   seq
   '(expected: "a sequence that contains at least ~a values" given: "~e")
   min-count
   seq))

(define (raise-wrong-elem-count blame neg-party seq n-cs elems)
  (define n-elems (length elems))
  (raise-blame-error
   blame #:missing-party neg-party seq
   '(expected: "a sequence of ~a values" given: "~a values\n values: ~e")
   n-cs n-elems elems))

(define (sequence/c-stronger this that)
  (and (sequence/c? that)
       (>= (or (sequence/c-min-count this) 0)
           (or (sequence/c-min-count that) 0))
       (let ([this-ctcs (sequence/c-ctcs this)]
             [that-ctcs (sequence/c-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (for/and ([this-ctc (in-list this-ctcs)]
                        [that-ctc (in-list that-ctcs)])
                (contract-struct-stronger? this-ctc that-ctc))))))

(define (sequence/c-equivalent this that)
  (and (sequence/c? that)
       (= (or (sequence/c-min-count this) 0)
          (or (sequence/c-min-count that) 0))
       (let ([this-ctcs (sequence/c-ctcs this)]
             [that-ctcs (sequence/c-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (for/and ([this-ctc (in-list this-ctcs)]
                        [that-ctc (in-list that-ctcs)])
                (contract-struct-equivalent? this-ctc that-ctc))))))

(struct sequence/c (ctcs min-count)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:name sequence/c-name
   #:first-order sequence/c-first-order
   #:late-neg-projection sequence/c-late-neg-projection
   #:stronger sequence/c-stronger
   #:equivalent sequence/c-equivalent))

(define -sequence/c
  (let ([make-sequence/c sequence/c])
    (define (sequence/c #:min-count [min-count #f] . elem/cs)
      (unless (or (exact-nonnegative-integer? min-count)
                  (not min-count))
        (raise-argument-error 'sequence/c
                              (format "~s" '(or/c exact-nonnegative-integer? #f))
                              min-count))
      (define ctcs (for/list ([elem/c (in-list elem/cs)])
                     (coerce-contract 'sequence/c elem/c)))
      (make-sequence/c ctcs min-count))
    sequence/c))


;; additional sequence constructors

(define-sequence-syntax in-syntax
  (λ () #'in-syntax/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(id) (_ arg)]
       #'[(id) (in-list (in-syntax/proc arg))]])))

(define (in-syntax/proc stx)
  (or (stx->list stx)
      (raise-type-error 'in-syntax "stx-list" stx)))

(define (in-slice k seq)
  (make-do-sequence
   (λ ()
     (define-values (more? get) (sequence-generate seq))
     (values
      (λ (_)
        (for/list ([i (in-range k)] #:when (more?))
          (get)))
      values
      #f
      #f
      (λ (val) (pair? val))
      #f))))
