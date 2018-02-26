#lang racket/base
(require "match.rkt")

(provide make-state
         extract-state!
         mutated?
         referenced?
         state-first-pass?
         state-tops-pass?
         adjust-state!
         state-implicit-reference!
         state-implicitly-referenced?)

;; The state table maps
;;
;;  * symbols (for variables names) to
;;     - 'mutated
;;     - an integer for the use count
;;
;;  * `lam` records for union-find of functions
;;    that tail-call each other
;;
;;  * `(if ...)` expressions to a table of used
;;    references for each branch
;;
;;  * '#:runstack to information recorded and used
;;     by "runstack.rkt"
;;
;;  * '#:implicit to a table of implicity referenced
;;    variables; an implicit reference happens when
;;    a variable is passed in-place in a tail call
;;
(define (make-state) (make-hasheq))

(define (mutated? v) (eq? v 'mutated))
(define (referenced? v) v)

(define (state-first-pass? state)
  (not (hash-ref state '#:done? #f)))

(define (state-tops-pass? state)
  (hash-ref state '#:tops? #f))

(define (adjust-state! state id delta)
  (define new-n (+ (hash-ref state id 0) delta))
  (if (zero? new-n)
      (hash-remove! state id)
      (hash-set! state id new-n)))

(define (state-implicit-reference! state id)
  (define ir (or (hash-ref state '#:implicit #f)
                 (let ([ht (make-hasheq)])
                   (hash-set! state '#:implicit ht)
                   ht)))
  (hash-set! ir id #t))
  
(define (state-implicitly-referenced? state id)
  (define ir (hash-ref state '#:implicit #f))
  (and ir (hash-ref ir id #f)))

;; ----------------------------------------

(define (extract-state! state e)
  (define (extract-state! e)
    (match e
      [`(define ,_ ,rhs)
       (extract-state! rhs)]
      [`(define-values ,_ ,rhs)
       (extract-state! rhs)]
      [`(begin ,es ...)
       (for ([e (in-list es)])
         (extract-state! e))]
      [`(begin0 ,es ...)
       (extract-state! `(begin . ,es))]
      [`(lambda ,ids . ,body)
       (extract-state! `(begin . ,body))]
      [`(case-lambda [,idss . ,bodys] ...)
       (for ([body (in-list bodys)])
         (extract-state! `(begin . ,body)))]
      [`(quote ,_) state]
      [`(if ,tst ,thn ,els)
       (extract-state! tst)
       (extract-state! thn)
       (extract-state! els)]
      [`(with-continuation-mark ,key ,val ,body)
       (extract-state! key)
       (extract-state! val)
       (extract-state! body)]
      [`(let . ,_)
       (extract-let-state! e)]
      [`(letrec . ,_)
       (extract-let-state! e)]
      [`(letrec* ([,ids . ,_] ...) . ,_)
       (for ([id (in-list ids)])
         (hash-set! state id 'mutated))
       (extract-let-state! e)]
      [`(set! ,id ,rhs)
       (hash-set! state id 'mutated)
       (extract-state! rhs)]
      [`(,rator ,rands ...)
       (extract-state! `(begin ,rator . ,rands))]
      [`,_
       (when (symbol? e)
         (unless (mutated? (hash-ref state e #f))
           (hash-set! state e (add1 (hash-ref state e 0)))))]))

  (define (extract-let-state! e)
    (match e
      [`(,_ ([,_ ,rhss] ...) . ,body)
       (for ([rhs (in-list rhss)])
         (extract-state! rhs))
       (extract-state! `(begin . ,body))]))

  (extract-state! e))
