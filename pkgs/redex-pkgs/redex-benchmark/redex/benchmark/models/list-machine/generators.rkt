#lang racket/base

(require redex/examples/list-machine/list-machine
         redex/examples/list-machine/list-machine-typing
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
    (generate-term list-machine-typing (l0 : ι p) 7)))

(module+ enum-mod
  (provide generate get-generator type)
  (define (get-generator) generate)
  (define type 'enum)
  (define (generate [p-value 0.5])
    (generate-term list-machine-typing (l0 : ι p) #:i-th (pick-an-index p-value))))

(module+ ordered-mod
  (provide generate get-generator type)
  (define (get-generator)
    (let ([index 0])
      (λ () (begin0
              (generate index)
              (set! index (add1 index))))))
  (define type 'ordered)
  (define (generate [index 0])
    (generate-term list-machine-typing (l0 : ι p) #:i-th index)))

(module+ check-mod
  (provide check)
  
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
  
  ;; TODO : change this to generate the program and type as a pair, (as the typed 
  ;; generator does), so we are testing the different strategies fairly?
  (define (type-check p)
    ;; need to provide a program typing, so generate 10 randomly and
    ;; see if any succeed...
    ;; (succeeds more often than one might assume)
    (let loop ([i 0])
      (cond
        [(i . > . 10) #f]
        [else
         (define guess-Π (generate-term list-machine-typing (l0 : (v0 : nil empty) Π) 7))
         (or (and (judgment-holds (check-program ,p ,guess-Π))
                  guess-Π)
             (loop (add1 i)))])))
  
  (define (check p)
    (or (not p)
        (implies (type-check p) (check-progress p)))))

