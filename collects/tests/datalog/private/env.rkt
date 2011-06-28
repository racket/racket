#lang racket
(require rackunit
         datalog/ast
         datalog/private/env)

(provide env-tests)

(define t1 (make-constant #f 't1))
(define t2 (make-constant #f 't2))

(define env-tests
  (test-suite
   "env"
   
   (test-equal? "default" (lookup (empty-env) 'v) #f)
   (test-equal? "default" (lookup (empty-env) 'v t1) t1)
   (test-equal? "extend" (lookup (extend (empty-env) 'v1 t1) 'v1) t1)
   (test-equal? "extend"
                (lookup (extend (extend (empty-env) 'v1 t1)
                                'v1 t1)
                        'v1)
                t1)))
