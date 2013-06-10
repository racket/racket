#lang racket
(provide != ! & \|
         ==> ==
         sign entier
         
         a60:sin
         a60:cos
         a60:arctan
         a60:sqrt
         a60:abs
         a60:ln
         a60:exp
         
         prints printn
         printsln printnln)

(define (!= a b)
  (not (= a b)))

(define (! a)
  (unless (boolean? a)
    (raise-type-error '! "boolean" a))
  (not a))

(define (& a b)
  (unless (boolean? a)
    (raise-type-error '& "boolean" 0 a b))
  (unless (boolean? b)
    (raise-type-error '& "boolean" 1 a b))
  (and a b))

(define (\| a b)
  (unless (boolean? a)
    (raise-type-error '\| "boolean" 0 a b))
  (unless (boolean? b)
    (raise-type-error '\| "boolean" 1 a b))
  (or a b))

(define (==> a b)
  (unless (boolean? a)
    (raise-type-error '=> "boolean" 0 a b))
  (unless (boolean? b)
    (raise-type-error '=> "boolean" 1 a b))
  (or (not a) b))

(define (== a b)
  (unless (boolean? a)
    (raise-type-error '== "boolean" 0 a b))
  (unless (boolean? b)
    (raise-type-error '== "boolean" 1 a b))
  (eq? a b))

(define (get-number who v)
  (let ([v (v)])
    (unless (number? v)
      (raise-type-error who "number" v))
    v))

(define (get-string who v)
  (let ([v (v)])
    (unless (string? v)
      (raise-type-error who "string" v))
    v))

(define (sign k v)
  (k (let ([v (get-number 'sign v)])
       (cond
         [(< v 0) -1]
         [(> v 0) 1]
         [else 0]))))

(define (entier k v)
  (k (inexact->exact (floor (get-number 'entier v)))))

(define (a60:abs k v)
  (k (abs (get-number 'abs v))))

(define (a60:sqrt k v)
  (k (sqrt (get-number 'sqrt v))))

(define (a60:sin k v)
  (k (sin (get-number 'sin v))))

(define (a60:cos k v)
  (k (cos (get-number 'cos v))))

(define (a60:exp k v)
  (k (exp (get-number 'exp v))))

(define (a60:arctan k v)
  (k (atan (get-number 'arctan v))))

(define (a60:ln k v)
  (k (log (get-number 'ln v))))

(define (printsln k v)
  (k (printf "~a\n" (get-string 'printsln v))))

(define (printnln k v)
  (k (printf "~a\n" (get-number 'printnln v))))

(define (prints k v)
  (k (printf "~a" (get-string 'prints v))))

(define (printn k v)
  (k (printf "~a" (get-number 'printn v))))
