#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics
         racket/list
         racket/match)

(provide (all-defined-out))

(define the-error "confuses the lhs value for the rhs value in cons type rule")

(define-rewrite bug1
  ((:lookup Γ v_0 τ_0) 
   (:lookup Γ v_1 τ_1)
   . rest)
  ==> 
  ((:lookup Γ v_0 τ_0) 
   (:lookup Γ v_0 τ_1)
   . rest)
  #:context (define-judgment-form)
  #:variables (rest)
  #:exactly-once)

(define-rewrite return-stopped-rw
  (let ([closure (apply-reduction-relation* 
                         red
                         `(,p ,r_0 ,ι_0)
                         #:stop-when
                         (let ([count 0])
                           (λ (_)
                             (begin0
                               (count . > . 1000)
                               (set! count (add1 count))))))])
           (or (empty? closure)
               (and (= 1 (length closure))
                    (match (car closure)
                      [`(,p ,r ,ι)
                       (equal? ι 'halt)]))))
  ==>
  (let* ([stopped #f]
         [closure (apply-reduction-relation* 
                   red
                   `(,p ,r_0 ,ι_0)
                   #:stop-when
                   (let ([count 0])
                     (λ (_)
                       (begin0
                         (count . > . 100)
                         (when (count . > . 100)
                           (set! stopped #t))
                         (set! count (add1 count))))))])
    (or stopped
        (empty? closure)
        (and (= 1 (length closure))
             (match (car closure)
               [`(,p ,r ,ι)
                (equal? ι 'halt)]))))
  #:exactly-once)

(include/rewrite (lib "redex/examples/list-machine/list-machine.rkt") list-machine)

(include/rewrite (lib "redex/examples/list-machine/list-machine-typing.rkt") list-machine-typing bug1)

(include/rewrite "generators.rkt" generators bug-mod-rw return-stopped-rw)

(define small-counter-example
  (term (l0 : (begin (cons v0 Z v0) halt) end)))

(test small-counter-example)
