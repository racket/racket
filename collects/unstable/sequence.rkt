#lang racket/base

(require (for-syntax racket/base) racket/contract/base)

;; Added by samth:

(provide in-syntax in-pairs in-sequence-forever sequence-lift)

(define-sequence-syntax in-syntax
  (lambda () #'(lambda (e) (in-list (syntax->list e))))
  (lambda (stx)
    (syntax-case stx ()
      [[ids (_ arg)]
       #'[ids (in-list (syntax->list arg))]])))

(define (in-pairs seq)
  (make-do-sequence
   (lambda ()
     (let-values ([(more? gen) (sequence-generate seq)])
       (values (lambda (e) (let ([e (gen)]) (values (car e) (cdr e))))
               (lambda (_) #t)
               #t
               (lambda (_) (more?))
               (lambda _ #t)
               (lambda _ #t))))))

(define (in-sequence-forever seq val)
  (make-do-sequence
   (lambda ()
     (let-values ([(more? gen) (sequence-generate seq)])
       (values (lambda (e) (let ([e (if (more?) (gen) val)]) e))
               (lambda (_) #t)
               #t
               (lambda (_) #t)
               (lambda _ #t)
               (lambda _ #t))))))

(define (sequence-lift f seq)
  (make-do-sequence
   (lambda ()
     (let-values ([(more? gen) (sequence-generate seq)])
       (values (lambda (e) (f (gen)))
               (lambda (_) #t)
               #t
               (lambda (_) (more?))
               (lambda _ #t)
               (lambda _ #t))))))


;; Added by stamourv (from David Vanderson (david.vanderson at gmail.com)):

(provide/contract
 [in-slice (exact-positive-integer? any/c . -> . any)])

(define (in-slice k seq)
  (unless (sequence? seq) (raise-type-error 'in-slice "sequence" seq))
  (make-do-sequence
   (lambda ()
     (define-values (more? get) (sequence-generate seq))
     (values
      (lambda (_)
        (for/list ((i k)
                   #:when (more?))
          (get)))
      values
      #f
      #f
      (lambda (val) (0 . < . (length val)))
      #f))))
