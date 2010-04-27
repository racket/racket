#lang scheme/base

(require (for-syntax scheme/base))

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
