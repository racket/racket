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
