#lang racket/base
(require rackunit
         redex/reduction-semantics
         (for-syntax racket/base))

(module test racket/base)

(define-syntax (try-it stx)
  (syntax-case stx ()
    [(_ N l p)
     (with-syntax ([line (syntax-line stx)])
       #'(test-begin
          (for ([i (in-range N)])
            (check-not-exn
             (λ ()
                (define term
                  (generate-term l p #:i-th i))
                (unless (redex-match l p term)
                  (error 'bad-term (format "line ~a: i=~a" line i))))))))]))

;; base types
(define-language Base
  (a any)
  (num number)
  (s string)
  (nat natural)
  (i integer)
  (r real)
  (b boolean))

(try-it 100 Base a)
(try-it 100 Base num)
(try-it 100 Base s)
(try-it 100 Base nat)
(try-it 100 Base i)
(try-it 100 Base r)
(try-it 2 Base b)

;; Repeat test
(define-language Rep
  (r (variable variable ...)))

(try-it 100 Rep r)

;; Recursion test
(define-language Λc
  (e (e e)
     (λ (x) e)
     x)
  (x (variable-except λ)))

(try-it 250 Λc e)
(try-it 24 Λc x)

;; De Bruijn for performance comparison
(define-language DBλc
  (e (e e)
     (λ e)
     x)
  (x natural))

(try-it 500 DBλc e)


;; Name test
(define-language Named
  (n (number_1 number_1)))

;; Very slow, to be fixed
(try-it 100 Named n)

(define-language not-SKI
  (y x
     s
     k
     i)
  (x (variable-except s k i)))

(try-it 22 not-SKI x)
(try-it 25 not-SKI y)

(define-language λv
  (e (e e ...)
     (if0 e e e)
     x
     v)
  (v (λ (x ...) e)
     number
     +)
  (E (v ... E e ...)
     (if0 E e e)
     hole)
  (x (variable-except λ + if0)))

(try-it 100 λv e)
(try-it 100 λv v)
(try-it 100 λv E)
(try-it 25 λv x)

;; No longer supported
(define-language M
  (m (x_!_1 x_!_1))
  (p (number_!_1 number_!_1))
  (n (p_!_1 p_!_1))
  (x number)

  ;; Example of poorly behaved mismatch
  (ambig (y ... y ...)))

(try-it 100 M m)
(try-it 100 M n)
(try-it 100 M p)
;; Ambiguity kills us here
;; (try-it 2 M (ambig_!_1 ambig_!_1))

;; test variable filtering
(define-language Vars
  (mention a b c x y z 2 #f #\c (vec 1 2))
  (varpre (variable-prefix moo))
  (varexc (variable-except x λ))
  (varnom variable-not-otherwise-mentioned))

(try-it 100 Vars varpre)
(try-it 100 Vars varexc)
(try-it 100 Vars varnom)

;; Named repeats
(define-language NRep
  (v  (natural ..._1 natural ..._1))
  (v2 (v ..._1 v ..._2 v ..._1 v ..._2))
  (v3 (natural_1 ..._1 natural_1 ..._1))
  (v4 (((natural_1 #t) ..._1) ..._2 ((#f natural_1) ..._1) ..._2))
  ;; The motherlode
  (v5 ((string_7 (((natural_1 variable_2) ..._1 any_3) ..._2)) ..._3 (((any_3 (variable_2 natural_1) ..._1) ..._2) string_7) ..._3)))

(try-it 100 NRep v)
(try-it 100 NRep v2)
(try-it 100 NRep v3)
(try-it 100 NRep v4)
(try-it 100 NRep v5)

;; Test production sort
(define-language rec
  (e (e e)
     v)
  (v (λ (x) e)
     x)
  (x variable-not-otherwise-mentioned))

(try-it 100 rec e)
(try-it 100 rec v)

;; Hole/in-hole test
(define-language Holes
  (h hole)
  (ctx (cons hole boolean)
       (cons boolean hole))
  (hide (pair ctx (hide-hole ctx)))
  (i (in-hole ctx number))
  (i2 (in-hole hide real)))

(try-it 4 Holes ctx)
(try-it 100 Holes i)
(try-it 100 Holes i2)
(try-it 1 Holes hole)
(try-it 100 Holes (in-hole hole number))
(try-it 100 Holes (in-hole (cons hole boolean) (cons number string)))
(try-it 100 Holes (in-hole (cons hole number_1) number_1))

;; Cross test
(define-language CrossLang
  (e (e e)
     (λ (x) e)
     x)
  (x variable-not-otherwise-mentioned))

(try-it 100 CrossLang e)
(try-it 100 CrossLang x)
(try-it 100 CrossLang (cross e))
(try-it 1 CrossLang (cross x))
(try-it 100 CrossLang (in-hole (cross x) e))
(try-it 100 CrossLang (in-hole (cross e) x))

