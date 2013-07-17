#lang racket/base

(require racket/generic)

(define-generics numeric
  #:defined-predicate numeric-supports?
  (decrement numeric)
  (is-zero? numeric)
  (is-even? numeric)
  (is-odd? numeric)
  #:defaults
  ([number?
    (define decrement sub1)
    (define is-zero? zero?)
    (define is-odd? odd?)])
  #:fallbacks
  [(define (is-even? x) (is-even?-fallback x))
   (define (is-odd? x) (is-odd?-fallback x))])

(define (is-even?-fallback x)
  (cond
    [(numeric-supports? x 'is-odd?) (not (is-odd? x))]
    [(is-zero? x) #true]
    [else (is-odd? (decrement x))]))

(define (is-odd?-fallback x)
  (cond
    [(numeric-supports? x 'is-even?) (not (is-even? x))]
    [(is-zero? x) #false]
    [else (is-even? (decrement x))]))

(struct peano-zero []
  #:transparent
  #:methods gen:numeric
  [(define (is-zero? x) #true)])
(struct peano-add1 [to]
  #:transparent
  #:methods gen:numeric
  [(define (is-zero? x) #false)
   (define (decrement x) (peano-add1-to x))])
(define (make-peano n)
  (for/fold ([p (peano-zero)]) ([i (in-range n)])
    (peano-add1 p)))

(struct binary [bits]
  #:transparent
  #:methods gen:numeric
  [(define (is-zero? x) (null? (binary-bits x)))
   (define (decrement x)
     (binary
       (let loop ([bits (binary-bits x)])
         (cond
           [(eq? (car bits) #false)
            (cons #true (loop (cdr bits)))]
           [(pair? (cdr bits))
            (cons #false (cdr bits))]
           [else '()]))))
   (define (odd? x)
     (and (pair? x) (car x)))])
(define (make-binary n)
  (binary
    (let loop ([n n])
      (cond
        [(zero? n) null]
        [else (cons (odd? n) (loop (quotient n 2)))]))))

(struct parity [even?]
  #:transparent
  #:methods gen:numeric
  [(define (is-even? x) (parity-even? x))])
(define (make-parity n)
  (parity (even? n)))

(struct wrapped [number]
  #:transparent
  #:methods gen:numeric
  [(define (is-zero? n) (zero? (wrapped-number n)))
   (define (decrement n) (wrapped (sub1 (wrapped-number n))))
   (define (is-even? n) (even? (wrapped-number n)))
   (define (is-odd? n) (odd? (wrapped-number n)))])
(define (make-wrapped n)
  (wrapped n))

(struct generic-wrapped [numeric]
  #:transparent
  #:methods gen:numeric
  [(define/generic numeric-decrement decrement)
   (define/generic numeric-zero? is-zero?)
   (define/generic numeric-even? is-even?)
   (define/generic numeric-odd? is-odd?)
   (define (decrement n)
     (generic-wrapped
       (numeric-decrement
         (generic-wrapped-numeric n))))
   (define (is-zero? n)
     (numeric-zero?
       (generic-wrapped-numeric n)))
   (define (is-even? n)
     (numeric-even?
       (generic-wrapped-numeric n)))
   (define (is-odd? n)
     (numeric-odd?
       (generic-wrapped-numeric n)))])
(define (make-generic-wrapped n)
  (generic-wrapped (make-peano n)))

(module+ test
  (require rackunit rackunit/text-ui racket/port)

  (define max-n 10)

  (define (tests name make)
    (test-suite name
      (test-suite "is-even?"
        (for {[i (in-range max-n)]}
          (test-case (number->string i)
            (check-equal? (is-even? (make i)) (even? i)))))
      (test-suite "is-odd?"
        (for {[i (in-range max-n)]}
          (test-case (number->string i)
            (check-equal? (is-odd? (make i)) (odd? i)))))))

  (parameterize {[current-output-port (open-output-nowhere)]}
    (run-tests
      (test-suite "fallbacks"
        (tests "built-in" values)
        (tests "peano" make-peano)
        (tests "binary" make-binary)
        (tests "parity" make-parity)
        (tests "wrapped" make-wrapped)
        (tests "generic-wrapped" make-generic-wrapped)))
    (void)))
