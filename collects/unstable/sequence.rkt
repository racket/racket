#lang racket/base

(require (for-syntax racket/base) racket/contract/base)

;; Added by samth:

(provide in-syntax in-pairs in-sequence-forever sequence-lift)

(define-sequence-syntax in-syntax
  (λ () #'(λ (e) (in-list (syntax->list e))))
  (λ (stx)
    (syntax-case stx ()
      [[ids (_ arg)]
       #'[ids (in-list (syntax->list arg))]])))

(define (in-pairs seq)
  (make-do-sequence
   (λ ()
     (let-values ([(more? gen) (sequence-generate seq)])
       (values (λ (e) (let ([e (gen)]) (values (car e) (cdr e))))
               (λ (_) #t)
               #t
               (λ (_) (more?))
               (λ _ #t)
               (λ _ #t))))))

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

;; Added by stamourv (from David Vanderson (david.vanderson at gmail.com)):

(provide/contract
 [in-slice (exact-positive-integer? any/c . -> . any)])

(define (in-slice k seq)
  (unless (sequence? seq) (raise-type-error 'in-slice "sequence" seq))
  (make-do-sequence
   (λ ()
     (define-values (more? get) (sequence-generate seq))
     (values
      (λ (_)
        (for/list ([i k] #:when (more?))
          (get)))
      values
      #f
      #f
      (λ (val) (0 . < . (length val)))
      #f))))
