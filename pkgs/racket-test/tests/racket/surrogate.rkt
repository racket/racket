#lang racket
(require rackunit racket/surrogate)

(let ()
  (define-values (host-mixin host<%> surrogate% surrogate<%>)
    (surrogate (override m (x))))

  (define o
    (new (host-mixin
          (class object%
            (define/public (m x) (+ x 1))
            (super-new)))))

  (check-equal? (send o m 2) 3)

  (send o set-surrogate
        (new (class surrogate%
               (define/override (m that that-m x)
                 x)
               (super-new))))

  (check-equal? (send o m 2) 2)

  (send o set-surrogate
        (new (class surrogate%
               (define/override (m that that-m x)
                 (that-m (* x x)))
               (super-new))))

  (check-equal? (send o m 4) 17)

  (send o set-surrogate #f)

  (check-equal? (send o m 2) 3))

(let ()
  (define-values (host-mixin host<%> surrogate% surrogate<%>)
    (surrogate (augment 0 m (x))))

  (define o
    (new (host-mixin
          (class object%
            (define/pubment (m x) (inner (+ x 1) m x))
            (super-new)))))

  (check-equal? (send o m 2) 0)

  (send o set-surrogate
        (new (class surrogate%
               (define/override (m that that-m x)
                 x)
               (super-new))))

  (check-equal? (send o m 2) 2)

  (send o set-surrogate
        (new (class surrogate%
               (define/override (m that that-m x)
                 (that-m (* x x)))
               (super-new))))

  (check-equal? (send o m 4) 0)

  (send o set-surrogate #f)

  (check-equal? (send o m 2) 0))


(let ()
  (define-values (host-mixin host<%> surrogate% surrogate<%>)
    (surrogate #:use-wrapper-proc
               (override m ())
               (override n x)))

  (define p (make-parameter #f))

  (define o
    (new (host-mixin
          (class object%
            (define/public (m) (p))
            (define/public (n . x) x)
            (super-new)))))

  (check-equal? (send o m) #f)

  (send o set-surrogate
        (new (class surrogate%
               (define/override (m that that-m)
                 (list (p) (that-m)))
               (super-new))))

  (check-equal? (send o m) (list #f #f))

  (send o set-surrogate-wrapper-proc
        (λ (fallback thunk)
          (parameterize ([p #t])
            (thunk))))

  (check-equal? (send o m) (list #t #t))

  (send o set-surrogate-wrapper-proc (λ (fallback thunk) (thunk)))
  (send o set-surrogate
        (new (class surrogate%
               (define/override (m that that-m) 15)
               (super-new))))

  (check-equal? (send o m) 15)
  
  (send o set-surrogate-wrapper-proc (λ (fallback thunk) (fallback)))

  (check-equal? (send o m) #f)

  (send o set-surrogate-wrapper-proc (λ (fallback thunk) (thunk)))
  
  (check-equal? (send o n 1 2 3) '(1 2 3))
  
  (send o set-surrogate
        (new (class surrogate%
               (define/override (n that that-m . args) (cons 'surrogate args))
               (super-new))))

  (check-equal? (send o n 1 2 3) '(surrogate 1 2 3))

  (send o set-surrogate-wrapper-proc (λ (fallback thunk) (cons 'wrapper (fallback))))

  (check-equal? (send o n 1 2 3) '(wrapper 1 2 3))
  
  )

