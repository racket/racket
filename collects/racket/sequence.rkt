#lang racket/base

(require "stream.rkt"
         "private/sequence.rkt")

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
         sequence-count)

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
