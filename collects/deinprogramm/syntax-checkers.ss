#lang scheme/base

(provide check-for-id!
         check-for-id-list!)

(define (check-for-id! arg error-msg)
  (when (not (identifier? arg))
      (raise-syntax-error #f error-msg arg)))

(define (check-for-id-list! args error-msg)
  (for-each (lambda (arg)
              (check-for-id! arg error-msg))
            args)
  (cond ((check-duplicate-identifier args)
         => (lambda (dup)
              (raise-syntax-error 
               #f 
               "Bezeichner doppelt gebunden"
               args dup)))
        (else #t)))
