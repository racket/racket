#lang racket/base

(require redex/examples/list-machine/list-machine
         redex/examples/list-machine/list-machine-typing
         (prefix-in typed: redex/benchmark/models/list-machine/ls-typed-gen)
         (only-in redex/private/generate-term pick-an-index)
         redex/reduction-semantics
         racket/bool
         racket/list
         racket/match)

(provide (all-defined-out))

(module+ adhoc-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'grammar)
  (define (generate)
    (generate-term list-machine-typing ((l0 : ι p) Π) 7)))

(module+ enum-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'enum)
  (define (generate [p-value 0.5])
    (generate-term list-machine-typing ((l0 : ι p) Π) #:i-th (pick-an-index p-value))))

(module+ ordered-mod
  (provide generate get-generator type)
  (define (get-generator)
    (let ([index 0])
      (λ () (begin0
              (generate index)
              (set! index (add1 index))))))
  (define type 'ordered)
  (define (generate [index 0])
    (generate-term list-machine-typing ((l0 : ι p) Π) #:i-th index)))

(module+ typed-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'search)
  (define (generate)
    (match (generate-term typed:list-machine-typing
                          #:satisfying
                          (typed:check-program (l0 : ι p) Π)
                          7)
      [`(typed:check-program ,p ,Π)
       `(,p ,Π)]
      [#f #f])))

(module+ check-mod
  (provide check type-check check-progress)
  
  (define (check-progress p)
    (define r_0 (term (empty v0 ↦ nil)))
    (define ι_0 (car (judgment-holds (program-lookup ,p l0 ι) ι)))
    (or (equal? ι_0 'halt)
        (and
         (= 1
            (length (apply-reduction-relation 
                     red
                     `(,p ,r_0 ,ι_0))))
         (let ([closure (apply-reduction-relation* 
                         red
                         `(,p ,r_0 ,ι_0)
                         #:stop-when
                         (let ([count 0])
                           (λ (_)
                             (begin0
                               (count . > . 1000)
                               (set! count (add1 count))))))])
           ;; if reduction terminates in less than 1000 steps, check it ends with halt
           ;; (if the #:stop-when condition is true, we get back an empty list, 
           ;; and the same thing for a reduction cycle)
           (or (empty? closure)
               (and (= 1 (length closure))
                    (match (car closure)
                      [`(,p ,r ,ι)
                       (equal? ι 'halt)])))))))

  (define (type-check p Π)
    (judgment-holds (check-program ,p ,Π)))
  
  (define (check pΠ)
    (or (not pΠ)
        (match pΠ
          [`(,p ,Π)
           (implies (type-check p Π) (check-progress p))]))))

