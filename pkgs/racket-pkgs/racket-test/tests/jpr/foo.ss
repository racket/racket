#lang scheme
(read-accept-reader #t) 

(define (chercher-definition fonc f)    
  (define (good-def? expr)
    (and (pair? expr)
         (equal? (car expr) 'define)
         (or (equal? (cadr expr) fonc)                                ; (define fonc ...)
             (and (pair? (cadr expr)) (equal? (caadr expr) fonc)))))  ; (define (fonc ...) ...)
  (call-with-input-file f 
    (lambda (p-in)
      (car (filter good-def? (list-ref (read p-in) 3))))))   ; (module foo scheme (#%module-begin ...))

(define (foo x y)   ; comment
  (+ x y))

(printf "The definition of the function foo in this file foo.ss is :\n")
(chercher-definition 'foo "foo.ss")
