#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics
         racket/list
         racket/match)

(provide (all-defined-out))

(define the-error "var-set may skip a var with matching id (in reduction)")

(define-rewrite bug2
  ((where #t (different v v_2))
   (var-set r v_2 a_2 r_2) . rest)
  ==> 
  ((var-set r v_2 a_2 r_2) . rest)
  #:context (define-judgment-form)
  #:variables (rest)
  #:exactly-once)


;; if reduction terminates in less than 20 steps, check it ends with halt
;; (if there is a reduction cycle, the empty list is returned)
;; (when the reduction is stopped artificially, the current term is returned)
;; 20 is a small number, but some terms can get exponentially large
(define-rewrite stop-earlier-rw
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
                         (count . > . 20)
                         (when (count . > . 20)
                           (set! stopped #t))
                         (set! count (add1 count))))))])
    (or stopped
        (empty? closure)
        (and (= 1 (length closure))
             (match (car closure)
               [`(,p ,r ,ι)
                (equal? ι 'halt)]))))
  #:exactly-once)

(include/rewrite (lib "redex/examples/list-machine/list-machine.rkt") list-machine bug2)

(include/rewrite (lib "redex/examples/list-machine/list-machine-typing.rkt") list-machine-typing)

(include/rewrite "generators.rkt" generators bug-mod-rw stop-earlier-rw)

(define small-counter-example
  (term (l0 : (begin (cons v0 v0 v0) halt) end)))

(test small-counter-example)
