#lang racket/base
(require racket/generic
         rackunit)

(define-generics fish
  [eat fish n]
  [swim fish])

(struct standard (size)
        #:transparent
        #:methods
        gen:fish
        [(define (eat self n) (standard (+ n (standard-size self))))
         (define (swim self) (standard (max 0 (- (standard-size self) 1))))])

(define-values (prop:color color? color-ref) (make-impersonator-property 'color))

(define phil (standard 1))
(check-equal? (standard 6) (eat (standard 1) 5))

(define (chaperone-eat p)
  (chaperone-procedure p
                       (lambda (self amt)
                         (values (lambda (r)
                                   (chaperone-struct r struct:standard
                                                     prop:color 'blue))
                                 self
                                 amt))))
  
(define not-phil
  (impersonate-generics gen:fish 
                        phil
                        [eat chaperone-eat]))
(check-true (impersonator-of? not-phil phil))
(check-false (chaperone-of? not-phil phil))
(check-false (color? not-phil))
(check-true (color? (eat not-phil 2)))

(define like-phil
  (chaperone-generics gen:fish 
                      phil
                      [eat chaperone-eat]))
(check-true (chaperone-of? like-phil phil))
(check-false (color? like-phil))
(check-true (color? (eat like-phil 2)))

(define just-like-phil
  (chaperone-generics gen:fish 
                      phil
                      #:properties (list prop:color 'red)))
(check-true (color? just-like-phil))
(check-false (color? (eat just-like-phil 2)))

(define still-not-phil
  (impersonate-generics gen:fish 
                        phil
                        [eat chaperone-eat]
                        #:properties (list prop:color 'red)))
(check-true (color? still-not-phil))
(check-true (color? (eat still-not-phil 2)))


