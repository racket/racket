#lang typed-scheme

(define: x : (Number . Boolean) (cons 3 #f))

(define: y : Number (car x))

(define: z : Boolean (cdr x))

