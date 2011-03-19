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
  (unless (sequence? s) (raise-type-error 'sequence-length "sequence" s))
  (for/fold ([c 0]) ([i (in-values*-sequence s)])
    (add1 c)))

(define (sequence-ref s i)
  (unless (sequence? s) (raise-type-error 'sequence-ref "sequence" s))
  (unless (exact-nonnegative-integer? i)
    (raise-type-error 'sequence-ref "nonnegative exact integer" i))
  (let ([v (for/fold ([c #f]) ([v (in-values-sequence s)]
                               [i (in-range (add1 i))])
             v)])
    (if (list? v)
        (apply values v)
        (raise-mismatch-error 
         'sequence-ref
         (format "sequence ended before element ~e: "
                 (add1 i))
         s))))

(define (sequence-tail seq i)
  (unless (sequence? seq) (raise-type-error 'sequence-tail "sequence" seq))
  (unless (exact-nonnegative-integer? i)
    (raise-type-error 'sequence-tail "nonnegative exact integer" i))
  (cond
   [(zero? i) seq]
   [else (let loop ([s (sequence->stream seq)] [n i])
           (cond
            [(zero? n) (in-stream s)]
            [(stream-empty? s) 
             (raise-mismatch-error 
              'sequence-ref
              (format "sequence ended before ~e element~a: "
                      i
                      (if (= i 1) "" "s"))
              seq)]
            [else (loop (stream-rest s)
                        (sub1 n))]))]))

(define (sequence-append . l)
  (if (null? l)
      empty-stream
      (if (andmap stream? l)
          (apply stream-append l)
          (apply in-sequences l))))

(define (sequence-map f s)
  (unless (procedure? f)
    (raise-type-error 'sequence-map "expects a procedure as the first argument, given ~e" f))
  (if (stream? s)
      (stream-map f s)
      (in-stream (stream-map f (sequence->stream s)))))

(define (sequence-filter f s)
  (unless (procedure? f) (raise-type-error 'sequence-filter "procedure" f))
  (unless (sequence? s) (raise-type-error 'sequence-filter "sequence" s))
  (if (stream? s)
      (stream-filter f s)
      (in-stream (stream-filter f (sequence->stream s)))))

(define (sequence-add-between s e)
  (unless (sequence? s) (raise-type-error 'sequence-ad-between "sequence" s))
  (if (stream? s)
      (stream-add-between s e)
      (in-stream (stream-add-between (sequence->stream s) e))))
