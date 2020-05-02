#lang racket/base
(require (for-syntax racket/base))

(provide H K J Q M L N I
         times
         unknown
         with-hash-variants
         make-large-equal-key/share1
         make-large-equal-key/share2)

;; Iterations for slow nested things
(define H 10)

;; Iterations for nested things
(define K (* H 10))

;; Iterations for fast nested things
(define J (* K 5))

;; Iterations for slow things:
(define Q 100000)

;; The depth used for non-tail recursion, typically:
(define M (* Q 10))

;; Intermediate count:
(define L (* M 10))

;; Number of iteraitons used for a loop, typically
(define N (* L 10))

;; Number of times to run each benchmark:
(define I 3)

(define-syntax times
  (syntax-rules ()
    [(_ e)
     (let loop ([v #f] [i I])
       (if (zero? i)
           v
           (loop (time e) (sub1 i))))]))

(define (unknown x) x)
(set! unknown unknown)

(define-syntax (with-hash-variants stx)
  (syntax-case stx ()
    [(_ body ...)
     (let ()
       (define (body-with prefix
                          #:empty empty-stx
                          #:key key-stx
                          #:other-key other-key-stx
                          #:make-key make-key-stx
                          #:make-val make-val-stx
                          #:for/hash for/hash-stx)
         (let loop ([body (syntax->list #'(body ...))])
           (cond
             [(null? body) null]
             [else
              (syntax-case (car body) (quote)
                [(quote sym)
                 (identifier? #'sym)
                 (cons #`(quote #,(string->symbol (format "~a:~a" (syntax-e #'sym) prefix)))
                       (loop (cdr body)))]
                [#:only
                 (if (eq? prefix (syntax-e (cadr body)))
                     (loop (cddr body))
                     (loop (cddddr body)))]
                [_
                 (cons
                  (let loop ([e (car body)])
                    (cond
                      [(eq? e 'EMPTY) empty-stx]
                      [(eq? e 'KEY) key-stx]
                      [(eq? e 'OTHER-KEY) other-key-stx]
                      [(eq? e 'MAKE-KEY) make-key-stx]
                      [(eq? e 'MAKE-VAL) make-val-stx]
                      [(eq? e 'FOR/HASH) for/hash-stx]
                      [(syntax? e) (datum->syntax e (loop (syntax-e e)))]
                      [(pair? e) (cons (loop (car e)) (loop (cdr e)))]
                      [else e]))
                  (loop (cdr body)))])])))
       #`(begin
           #,@(body-with 'eq#t
                         #:empty #'#hasheq()
                         #:key #''a
                         #:other-key #''not-there
                         #:make-key #'values
                         #:make-val #'(lambda (v) #t)
                         #:for/hash #'for/hasheq)
           #,@(body-with 'eq
                         #:empty #'#hasheq()
                         #:key #''a
                         #:other-key #''not-there
                         #:make-key #'values
                         #:make-val #'values
                         #:for/hash #'for/hasheq)
           #,@(body-with 'eqv
                         #:empty #'#hasheqv()
                         #:key #'12345
                         #:other-key #'1/3
                         #:make-key #'values
                         #:make-val #'values
                         #:for/hash #'for/hasheqv)
           #,@(body-with 'equal
                         #:empty #'#hash()
                         #:key #''(a)
                         #:other-key #''(not-there)
                         #:make-key #'box
                         #:make-val #'values
                         #:for/hash #'for/hash)))]))

(define (make-data)
  (list 1 2 3
        (string-copy "Hello, world!")
        'apple
        (box (seconds->date 0))
        1/2))

(define share1-a (make-data))
(define share1-b (make-data))
(define share2-a (make-data))
(define share2-b (make-data))

(define (make-large-equal-key/share1 c)
  (vector share1-a c share1-b))

(define (make-large-equal-key/share2 c)
  (vector share2-a c share2-b))
