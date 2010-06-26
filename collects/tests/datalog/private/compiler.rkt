#lang racket
(require rackunit
         (for-template datalog/eval)
         datalog/parse
         datalog/private/compiler)

(provide compiler-tests)

(define s1
  (parse-statement
   (open-input-string
    "parent(john,douglas).")))
  
(define compiler-tests
  (test-suite
   "compiler"
   
   (test-equal? "stmt"
                (syntax->datum (compile-stmt s1))
                `(eval-statement ,s1))
   (test-equal? "module"
                (syntax->datum (compile-module (list s1)))
                `(begin (eval-statement ,s1)))))
   