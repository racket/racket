#lang racket
(require rackunit
         datalog/private/subst
         datalog/ast
         datalog/private/env)
(require/expose datalog/private/subst (subst-literal shuffle))

(provide subst-tests)

(define (gensym-var? v)
  (define s (variable-sym v))
  (not (eq? s (string->symbol (symbol->string s)))))

(define subst-tests
  (test-suite
   "subst"
   
   (test-suite
    "subst-term"
    (test-equal? "con" 
                 (subst-term (empty-env) (make-constant #f 'v1))
                 (make-constant #f 'v1))
    (test-equal? "var def" 
                 (subst-term (empty-env) (make-variable #f 'v1))
                 (make-variable #f 'v1))
    (test-equal? "var" 
                 (subst-term (extend (empty-env) 'v1 (make-constant #f 'v1)) (make-variable #f 'v1))
                 (make-constant #f 'v1)))
   
   (test-suite
    "subst-literal"
    (test-equal? "con"
                 (subst-literal (empty-env) (make-literal #f 'lit (list (make-constant #f 'v1))))
                 (make-literal #f 'lit (list (make-constant #f 'v1))))
    (test-equal? "var def"
                 (subst-literal (extend (empty-env) 'v1 (make-constant #f 'v1)) (make-literal #f 'lit (list (make-variable #f 'v1))))
                 (make-literal #f 'lit (list (make-constant #f 'v1))))
    (test-equal? "var def"
                 (subst-literal (extend (empty-env) 'v1 (make-constant #f 'v1)) (make-literal #f 'lit (list (make-variable #f 'v1))))
                 (make-literal #f 'lit (list (make-constant #f 'v1)))))
   
   (test-suite
    "subst-clause"
    (test-equal? "con"
                 (subst-clause (empty-env) (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1))) empty))
                 (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1))) empty))
    (test-equal? "var def"
                 (subst-clause (extend (empty-env) 'v1 (make-constant #f 'v1)) 
                               (make-clause #f (make-literal #f 'lit (list (make-variable #f 'v1))) empty))
                 (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1))) empty))
    (test-equal? "var def"
                 (subst-clause (extend (empty-env) 'v1 (make-constant #f 'v1)) 
                               (make-clause #f (make-literal #f 'lit (list (make-variable #f 'v1))) empty))
                 (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1))) empty))
    
    (test-equal? "con"
                 (subst-clause (empty-env) (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1))) 
                                                        (list (make-literal #f 'lit (list (make-constant #f 'v1))))))
                 (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1)))
                              (list (make-literal #f 'lit (list (make-constant #f 'v1))))))
    (test-equal? "var def"
                 (subst-clause (extend (empty-env) 'v1 (make-constant #f 'v1)) 
                               (make-clause #f (make-literal #f 'lit (list (make-variable #f 'v1))) 
                                            (list (make-literal #f 'lit (list (make-variable #f 'v1))))))
                 (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1)))
                              (list (make-literal #f 'lit (list (make-constant #f 'v1))))))
    (test-equal? "var def"
                 (subst-clause (extend (empty-env) 'v1 (make-constant #f 'v1)) 
                               (make-clause #f (make-literal #f 'lit (list (make-variable #f 'v1)))
                                            (list (make-literal #f 'lit (list (make-variable #f 'v1))))))
                 (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1)))
                              (list (make-literal #f 'lit (list (make-constant #f 'v1)))))))
   
   (test-suite
    "shuffle"
    (test-equal? "con" 
                 (shuffle (empty-env) (make-literal #f 'lit (list (make-constant #f 'v1))))
                 (empty-env))
    (test-equal? "var" 
                 (shuffle (extend (empty-env) 'v1 (make-constant #f 'k1)) (make-literal #f 'lit (list (make-variable #f 'v1))))
                 (extend (empty-env) 'v1 (make-constant #f 'k1)))
    (test-not-false "var"
                    (gensym-var? (lookup (shuffle (empty-env)
                                                  (make-literal #f 'lit (list (make-variable #f 'v1))))
                                         'v1))))
   
   (test-suite
    "rename-question"
    (test-equal? "l" (rename-question (make-literal #f 'lit (list (make-constant #f 'v1))))
                 (make-literal #f 'lit (list (make-constant #f 'v1))))
    (test-not-false "l"
                    (gensym-var?
                     (first
                      (literal-terms
                       (rename-question (make-literal #f 'lit (list (make-variable #f 'v1)))))))))
   
   (test-suite
    "rename-clause"
    (test-equal? "c" (rename-clause (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1))) empty))
                 (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1))) empty))
    (test-not-false "c"
                    (gensym-var? 
                     (first
                      (literal-terms
                       (clause-head 
                        (rename-clause (make-clause #f (make-literal #f 'lit (list (make-variable #f 'v1))) empty)))))))
    (test-not-false "c"
                    (gensym-var? 
                     (first
                      (literal-terms
                       (first
                        (clause-body
                         (rename-clause (make-clause #f (make-literal #f 'lit (list (make-constant #f 'v1)))
                                                     (list (make-literal #f 'lit (list (make-variable #f 'v1)))))))))))))))
