#lang typed-scheme

(: id (All (a) (a -> a)))
(define (id x) x)

(: pure (All (a) (a -> (All (Env) (Env -> a)))))
(define (pure c)
 (plambda: (Env) ([env : Env]) c))

(pure 4)
; REPL reports type (Any -> Integer)
; Shouldn't it be (All (Env) (Env -> Integer))?

(pure id)
; REPL reports type (Any -> (All (a) (a -> a)))
; Shouldn't it be (All (Env) (Env -> (All (a) (a -> a))))?

(: ap (All (Env a b) ((Env -> (a -> b)) (Env -> a) -> (Env -> b))))
(define (ap f x)
 (λ (env)
   ((f env) (x env))))

(ap (pure id) (pure 4))
