#lang racket/base

(require "stream.rkt"
         "private/sequence.rkt"
         racket/contract/combinator
         racket/contract/base)

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
         sequence/c)

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
  (for/fold ([c 0]) ([i (in-values*-sequence s)])
    (add1 c)))

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

(define (sequence/c #:min-count [min-count #f] . elem/cs)
  (unless (or (exact-nonnegative-integer? min-count)
              (not min-count))
    (raise-argument-error 'sequence/c
                          (format "~s" '(or/c exact-nonnegative-integer? #f))
                          min-count))
  (define ctcs (for/list ([elem/c (in-list elem/cs)])
                 (coerce-contract 'sequence/c elem/c)))
  (define elem/mk-projs 
    (for/list ([ctc (in-list ctcs)])
      (contract-projection ctc)))
  (define n-cs (length elem/cs))
  (make-contract
   #:name (apply build-compound-type-name 'sequence/c 
                 (append
                  (if min-count
                      (list '#:min-count min-count)
                      '())
                  ctcs))
   #:first-order 
   (λ (val)
     (and (sequence? val)
          (if (vector? val) (= n-cs 1) #t)
          (if (list? val)   (= n-cs 1) #t)
          (if (hash? val)   (= n-cs 2) #t)))
   #:projection
   (λ (orig-blame)
     (define blame (blame-add-context orig-blame "an element of"))
     (define ps (for/list ([mk-proj (in-list elem/mk-projs)])
                  (mk-proj blame)))
     (λ (seq)
       (unless (sequence? seq)
         (raise-blame-error
          orig-blame seq
          '(expected: "a sequence" given: "~e")
          seq))
       (define result-seq
       (make-do-sequence
        (lambda ()
          (let*-values ([(more? next) (sequence-generate seq)])
            (values
             (lambda (idx)
               (call-with-values 
                next
                (lambda elems
                  (define n-elems (length elems))
                  (unless (= n-elems n-cs)
                    (raise-blame-error
                     blame seq
                     '(expected: "a sequence of ~a values" given: "~a values\n values: ~e")
                     n-cs n-elems elems))
                  (apply
                   values
                   (for/list ([elem (in-list elems)]
                              [p (in-list ps)])
                     (p elem))))))
             add1
             0
             (lambda (idx) 
               (define ans (more?))
               (when (and min-count (idx . < . min-count))
                 (unless ans
                   (raise-blame-error 
                    orig-blame
                    seq
                    '(expected: "a sequence that contains at least ~a values" given: "~e")
                    min-count
                    seq)))
               ans)
             (lambda elems #t)
             (lambda (idx . elems) #t))))))
       (cond
         [(list? seq) (sequence->list result-seq)]
         [(stream? seq) (sequence->stream result-seq)]
         [else result-seq])))))
