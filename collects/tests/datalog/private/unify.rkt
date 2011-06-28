#lang racket
(require rackunit
         datalog/ast
         datalog/private/env
         datalog/private/unify)
(require/expose datalog/private/unify (chase))

(provide unify-tests)

(define unify-tests
  (test-suite
   "unify"
   
   (test-suite
    "chase"
    (test-equal? "con" (chase (empty-env) (make-constant #f 'k1))
                 (make-constant #f 'k1))
    (test-equal? "var" (chase (empty-env) (make-variable #f 'v1))
                 (make-variable #f 'v1))
    (test-equal? "var->con"
                 (chase (extend (empty-env) 'v1 (make-constant #f 'k1)) (make-variable #f 'v1))
                 (make-constant #f 'k1))
    (test-equal? "var->var->con"
                 (chase (extend (extend (empty-env) 'v2 (make-constant #f 'k1))
                                'v1 (make-variable #f 'v2))
                        (make-variable #f 'v1))
                 (make-constant #f 'k1)))
   
   (test-suite
    "unify-term"
    (test-equal? "con/con" (unify-term (empty-env) (make-constant #f 'k1) (make-constant #f 'k1))
                 (empty-env))
    (test-false "con/con" (unify-term (empty-env) (make-constant #f 'k1) (make-constant #f 'k2)))
    (test-equal? "var/con" (unify-term (empty-env) (make-variable #f 'v1) (make-constant #f 'k2))
                 (extend (empty-env) 'v1 (make-constant #f 'k2)))
    (test-equal? "con/var" (unify-term (empty-env) (make-constant #f 'k2) (make-variable #f 'v1))
                 (extend (empty-env) 'v1 (make-constant #f 'k2)))
    (test-equal? "var/var" (unify-term (empty-env) (make-variable #f 'v1) (make-variable #f 'v2))
                 (extend (empty-env) 'v1 (make-variable #f 'v2))))
   
   (test-suite
    "unify-terms"
    (test-equal? "con/con" (unify-terms (empty-env) (list (make-constant #f 'k1)) (list (make-constant #f 'k1)))
                 (empty-env))
    (test-false "con/con" (unify-terms (empty-env) (list (make-constant #f 'k1)) (list (make-constant #f 'k2))))
    (test-false "/con" (unify-terms (empty-env) (list) (list (make-constant #f 'k2))))
    (test-false "con/" (unify-terms (empty-env) (list (make-constant #f 'k2)) (list))))
   
   (test-suite
    "unify"
    (test-false "lit/lit" (unify (make-literal #f 'lit1 empty) (make-literal #f 'lit2 empty)))
    (test-equal? "con/con" (unify (make-literal #f 'lit1 (list (make-constant #f 'k1)))
                                  (make-literal #f 'lit1 (list (make-constant #f 'k1))))
                 (empty-env)))))
