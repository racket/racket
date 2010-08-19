#lang racket/base
(require (only-in plai define-type)
         racket/contract)

(define-type Foo
  [bar (v any/c)
       (v2 any/c)])

(define (to-string print v)
  (let ([s (open-output-string)])
    (print v s)
    (get-output-string s)))

(define (check a b) 
  (unless (equal? a b) (error 'check "failed: ~s vs. ~s" a b)))

(check (to-string print (bar "a" 'b)) "(bar \"a\" 'b)")
(check (to-string write (bar "a" 'b)) "#(struct:bar \"a\" b)")
(check (to-string display (bar "a" 'b)) "#(struct:bar a b)")

(check (to-string print (list (bar "a" (list 'b)))) "(list (bar \"a\" '(b)))")
(check (to-string write (list (bar "a" (list 'b)))) "(#(struct:bar \"a\" (b)))")
(check (to-string display (list (bar "a" (list 'b)))) "(#(struct:bar a (b)))")
