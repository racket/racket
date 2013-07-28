#lang scheme/base
(require "test-utils.rkt" (for-syntax scheme/base))
(require (rep type-rep)
         (r:infer infer)
         (types numeric-tower union utils abbrev)
         rackunit)



(provide fv-tests)

(define-syntax-rule (fv-t ty elems ...)
  (let ([ty* ty])
    (test-check (format "~a" ty*)
                equal?
                (fv ty*)
                (list (quote elems) ...))))

(define (fv-tests)
  (test-suite "Tests for fv"
              (fv-t -Number)
              [fv-t (-v a) a]
              [fv-t (-poly (a) a)]
              [fv-t (-poly (a b c d e) a)]
              [fv-t (-poly (b) (-v a)) a]
              [fv-t (-poly (b c d e) (-v a)) a]
              [fv-t (-mu a (-lst a))]
              [fv-t (-mu a (-lst (-pair a (-v b)))) b]

              [fv-t (->* null (-v a) -Number) a] ;; check that a is CONTRAVARIANT
              ))

(define-syntax-rule (i2-t t1 t2 (a b) ...)
  (test-check (format "~a ~a" t1 t2)
              equal?
              (infer t1 t2 (fv t1) (fv t1))
              (list (list a b) ...)))

(define-syntax-rule (i2-l t1 t2 fv (a b) ...)
  (test-check (format "~a ~a" t1 t2)
              equal?
              (infer/list t1 t2 fv fv)
              (list (list a b) ...)))

(define (f t1 t2) (infer t1 t2 (fv t1) (fv t1)))

(define-syntax-rule (i2-f t1 t2)
  (test-false (format "~a ~a" t1 t2)
              (f t1 t2)))
#|
(define (i2-tests)
  (test-suite "Tests for infer"
              [i2-t (-v a) N ('a N)]
              [i2-t (-pair (-v a) (-v a)) (-pair N (Un N B)) ('a (Un N B))]
              [i2-t (-lst (-v a)) (-pair N (-pair N (-val null))) ('a N)]
              [i2-t (-lst (-v a)) (-pair N (-pair B (-val null))) ('a (Un N B))]
              [i2-t Univ (Un N B)]

              [i2-t ((-v a) . -> . (-v b)) (-> N N) ('b N) ('a N)]
              [i2-l (list (-v a) (-v a) (-v b)) (list (Un (-val 1) (-val 2)) N N) '(a b) ('b N) ('a N)]
              [i2-l (list (-> (-v a) Univ) (-lst (-v a)))  (list (-> N (Un N B)) (-lst N)) '(a) ('a N)]
              [i2-l (list (-> (-v a) (-v b)) (-lst (-v a)))  (list (-> N N) (-lst (Un (-val 1) (-val 2)))) '(a b) ('b N) ('a N)]
              [i2-l (list  (-lst (-v a)))  (list  (-lst (Un B N))) '(a) ('a (Un N B))]
              ;; error tests
              [i2-f (-lst (-v a)) Univ]))



(define-syntax-rule (co-t a b c)
  (test-case (format "~a ~a" a b)
             (check equal? ((combine 'co) a b) c)
             (check equal? ((combine 'co) b a) c)))
(define-syntax-rule (co-f a b)
  (test-case (format "fail ~a ~a" a b)
             (check-exn exn:infer? (lambda () ((combine 'co) a b)))
             (check-exn exn:infer? (lambda () ((combine 'co) b a)))))

(define-syntax-rule (un-t a b c)
  (test-case (format "~a ~a" a b)
             (check equal? (s (g ((table:un 'co) a b))) (s c))
             (check equal? (s (g ((table:un 'co) b a))) (s c))))
(define-syntax-rule (un-f a b)
  (test-case (format "fail ~a ~a" a b)
             (check-exn exn:infer? (lambda () ((table:un 'co) a b)))
             (check-exn exn:infer? (lambda () ((table:un 'co) b a)))))

;; examples for testing combine

(define c-ex1 `(contra ,(Un N B)))
(define c-ex2 `(contra ,B))
(define c-ex3 `(#f ,N))
(define c-ex4 `(co ,N))
(define c-ex5 `(co ,B))
(define c-ex6 `fail)

;; examples for testing table:un

(define m-ex1
  (table:sexp->eq `((a ,c-ex3) (b ,c-ex6) (c ,c-ex5) (d ,c-ex1))))
(define m-ex2
  (table:sexp->eq `((a ,c-ex4) (b ,c-ex4) (c ,c-ex2) (d ,c-ex2))))
(define m-ex3
  (table:sexp->eq `((a ,c-ex4) (b ,c-ex4) (c ,c-ex2) (d ,c-ex5))))


(define (combine-tests)
  (test-suite "Combine/Table Union Tests"
              (co-t c-ex1 c-ex2 c-ex2)
              (co-t c-ex2 c-ex2 c-ex2)
              (co-f c-ex3 c-ex2)
              (co-f c-ex4 c-ex5)
              (co-t c-ex5 c-ex2 `(both ,B))
              (co-t c-ex5 c-ex6 c-ex5)
              [co-t c-ex3 c-ex4 c-ex4]
              [un-t m-ex1 m-ex2 `((b (co ,N)) (a (co ,N)) (c (both ,B)) (d (contra ,B)))]
              [un-f m-ex1 m-ex3]))


(define (g e) (table:to-sexp e))

(define (s e)
  (sort e (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))))

|#
(define-go fv-tests)
