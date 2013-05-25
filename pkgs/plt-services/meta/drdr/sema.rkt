#lang racket

(define (semaphore-wait* sema how-many)
  (unless (zero? how-many)
    (semaphore-wait sema)
    (semaphore-wait* sema (sub1 how-many))))

(provide/contract
 [semaphore-wait* (semaphore? exact-nonnegative-integer? . -> . void)])
