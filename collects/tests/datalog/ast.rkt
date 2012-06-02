#lang racket
(require rackunit
         datalog/ast
         racket/runtime-path)

(provide ast-tests)

(define-runtime-path ast.rkt "ast.rkt")
(define sym1-srcloc (list ast.rkt 1 1 10 3))
(define sym2-srcloc (list #f #f #f #f #f))
(define lit1-srcloc (list ast.rkt #f #f 3000 3))
(define cl1-srcloc sym2-srcloc)

(define ast-tests
  (test-suite
   "ast"
   
   (test-suite
    "datum-equal?"
    (test-not-false "str/str" (datum-equal? "str" "str"))
    (test-false "str/str" (datum-equal? "str1" "str2"))
    (test-not-false "sym/sym" (datum-equal? 'sym1 'sym1))
    (test-false "sym/sym" (datum-equal? 'sym1 'sym2))
    (test-false "str/sym" (datum-equal? "str" 'sym))
    (test-false "sym/str" (datum-equal? 'sym "str")))
   
   (test-suite
    "variable-equal?"
    (test-not-false "var/var" (variable-equal? (make-variable #f 'sym1) (make-variable #f 'sym1)))
    (test-not-false "var/var" (variable-equal? (make-variable #f 'sym1) (make-variable sym1-srcloc 'sym1)))
    (test-false "var/var" (variable-equal? (make-variable #f 'sym1) (make-variable #f 'sym2)))
    (test-false "var/var" (variable-equal? (make-variable #f 'sym1) (make-variable sym2-srcloc 'sym2))))
   
   (test-suite
    "constant-equal?"
    (test-not-false "sym/sym" (constant-equal? (make-constant #f 'sym1) (make-constant #f 'sym1)))
    (test-not-false "sym/sym" (constant-equal? (make-constant #f 'sym1) (make-constant sym1-srcloc 'sym1)))
    (test-false "sym/sym" (constant-equal? (make-constant #f 'sym1) (make-constant sym1-srcloc 'sym2)))
    (test-not-false "str/str" (constant-equal? (make-constant #f "sym1") (make-constant #f "sym1")))
    (test-not-false "str/str" (constant-equal? (make-constant #f "sym1") (make-constant sym1-srcloc "sym1")))
    (test-false "str/str" (constant-equal? (make-constant #f "sym1") (make-constant sym1-srcloc "sym2")))
    (test-false "sym/str" (constant-equal? (make-constant #f 'sym1) (make-constant sym1-srcloc "sym2")))
    (test-false "str/sym" (constant-equal? (make-constant sym1-srcloc "sym2") (make-constant #f 'sym1))))
   
   (test-suite
    "term-equal?"
    (test-not-false "var/var" (term-equal? (make-variable #f 'sym1) (make-variable #f 'sym1)))
    (test-not-false "var/var" (term-equal? (make-variable #f 'sym1) (make-variable sym1-srcloc 'sym1)))
    (test-false "var/var" (term-equal? (make-variable #f 'sym1) (make-variable #f 'sym2)))
    (test-false "var/var" (term-equal? (make-variable #f 'sym1) (make-variable sym2-srcloc 'sym2)))
    (test-not-false "sym/sym" (term-equal? (make-constant #f 'sym1) (make-constant #f 'sym1)))
    (test-not-false "sym/sym" (term-equal? (make-constant #f 'sym1) (make-constant sym1-srcloc 'sym1)))
    (test-false "sym/sym" (term-equal? (make-constant #f 'sym1) (make-constant sym1-srcloc 'sym2)))
    (test-not-false "str/str" (term-equal? (make-constant #f "sym1") (make-constant #f "sym1")))
    (test-not-false "str/str" (term-equal? (make-constant #f "sym1") (make-constant sym1-srcloc "sym1")))
    (test-false "str/str" (term-equal? (make-constant #f "sym1") (make-constant sym1-srcloc "sym2")))
    (test-false "sym/str" (term-equal? (make-constant #f 'sym1) (make-constant sym1-srcloc "sym2")))
    (test-false "str/sym" (term-equal? (make-constant sym1-srcloc "sym2") (make-constant #f 'sym1)))    
    (test-false "con/var" (term-equal? (make-constant sym1-srcloc "sym2") (make-variable #f 'sym1)))
    (test-false "var/con" (term-equal? (make-variable #f 'sym1) (make-constant sym1-srcloc "sym2"))))
   
   (test-suite
    "literal-equal?"
    (test-not-false "lit" (literal-equal? (make-literal #f 'lit1 empty) (make-literal lit1-srcloc 'lit1 empty)))
    (test-not-false "lit" (literal-equal? (make-literal #f 'lit1 (list (make-variable #f 'sym1)))
                                          (make-literal lit1-srcloc 'lit1 (list (make-variable #f 'sym1)))))
    (test-not-false "lit" (literal-equal? (make-literal #f 'lit1 (list (make-variable #f 'sym1)))
                                          (make-literal lit1-srcloc 'lit1 (list (make-variable sym1-srcloc 'sym1)))))
    (test-false "lit" (literal-equal? (make-literal #f 'lit1 empty) (make-literal lit1-srcloc 'lit2 empty)))
    (test-false "lit" (literal-equal? (make-literal #f 'lit1 (list (make-variable #f 'sym1))) (make-literal lit1-srcloc 'lit2 empty)))
    (test-false "lit" (literal-equal? (make-literal #f 'lit1 (list (make-variable #f 'sym1)))
                                      (make-literal lit1-srcloc 'lit2 (list (make-variable sym1-srcloc 'sym2))))))
   
   (test-suite
    "clause-equal?"
    (test-not-false "lit" (clause-equal? (make-clause #f (make-literal #f 'lit1 empty) empty)
                                         (make-clause #f (make-literal #f 'lit1 empty) empty)))
    (test-not-false "lit" (clause-equal? (make-clause #f (make-literal #f 'lit1 empty) (list (make-literal #f 'lit1 empty)))
                                         (make-clause #f (make-literal #f 'lit1 empty) (list (make-literal #f 'lit1 empty)))))
    (test-not-false "lit" (clause-equal? (make-clause #f (make-literal #f 'lit1 empty) empty)
                                         (make-clause cl1-srcloc (make-literal #f 'lit1 empty) empty)))
    (test-false "lit" (clause-equal? (make-clause #f (make-literal #f 'lit1 empty) empty)
                                     (make-clause #f (make-literal #f 'lit2 empty) empty)))
    (test-false "lit" (clause-equal? (make-clause #f (make-literal #f 'lit1 empty) (list (make-literal #f 'lit1 empty)))
                                     (make-clause #f (make-literal #f 'lit1 empty) empty)))
    (test-false "lit" (clause-equal? (make-clause #f (make-literal #f 'lit1 empty) (list (make-literal #f 'lit1 empty)))
                                     (make-clause #f (make-literal #f 'lit1 empty) (list (make-literal #f 'lit2 empty))))))))
