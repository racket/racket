#lang racket/base

(require racket/sequence) ; for re-export

(provide in-syntax in-pairs in-sequence-forever sequence-lift in-slice)

;; ELI: Besides the awful name, this is the same as
;;      (in-sequences seq (in-cycle (in-value val)))
(define (in-sequence-forever seq val)
  (make-do-sequence
   (λ ()
     (let-values ([(more? gen) (sequence-generate seq)])
       (values (λ (e) (if (more?) (gen) val))
               (λ (_) #t)
               #t
               (λ (_) #t)
               (λ _ #t)
               (λ _ #t))))))

;; ELI: How is this different from `sequence-map'?
(define (sequence-lift f seq)
  (make-do-sequence
   (λ ()
     (let-values ([(more? gen) (sequence-generate seq)])
       (values (λ (e) (call-with-values gen f))
               (λ (_) #t)
               #t
               (λ (_) (more?))
               (λ _ #t)
               (λ _ #t))))))

