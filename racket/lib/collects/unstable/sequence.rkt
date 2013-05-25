#lang racket/base

(require (for-syntax racket/base) racket/contract/base syntax/stx)

;; Added by samth:

(provide in-syntax in-pairs in-sequence-forever sequence-lift)

;; ELI: I don't see a point in this over using `syntax->list' directly.
;; (Eg, the latter is something that can be used when the programmer
;; knows that it's a list, in contrast to this code which will just
;; throw an error.)
(define-sequence-syntax in-syntax
  (λ () #'in-syntax/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(id) (_ arg)]
       #'[(id) (in-list (in-syntax/proc arg))]])))

(define (in-syntax/proc stx)
  (or (stx->list stx)
      (raise-type-error 'in-syntax "stx-list" stx)))

;; ELI: This is very specific, and indeed there are no uses of it
;; anywhere in the tree other than in TR where it came from.
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

;; Added by stamourv (from David Vanderson (david.vanderson at gmail.com)):

(provide/contract
 [in-slice (exact-positive-integer? any/c . -> . any)])

(define (in-slice k seq)
  ;; ELI: what's the point of using `any/c' above and then checking it here?
  (unless (sequence? seq) (raise-type-error 'in-slice "sequence" seq))
  (make-do-sequence
   (λ ()
     (define-values (more? get) (sequence-generate seq))
     (values
      (λ (_)
        ;; ELI: Add an `in-range'
        (for/list ([i k] #:when (more?))
          (get)))
      values
      #f
      #f
      ;; ELI: Use `pair?'
      (λ (val) (0 . < . (length val)))
      #f))))
