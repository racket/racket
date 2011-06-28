#lang racket
(require rackunit
         datalog/ast
         datalog/parse
         "util.rkt")

(provide parse-tests)

(define (test-literal-parse str res)
  (test-literal str (parse-literal (open-input-string str)) res))
(define (test-clause-parse str res)
  (test-clause str (parse-clause (open-input-string str)) res))

(define parse-tests
  (test-suite
   "parse"
   
   (test-suite
    "literal"
    (test-literal-parse "parent(john, douglas)"
                        (make-literal #f 'parent (list (make-constant #f 'john) (make-constant #f 'douglas))))
    (test-literal-parse "1 = 2"
                        (make-literal #f '= (list (make-constant #f '|1|) (make-constant #f '|2|))))
    (test-literal-parse "1 != 2"
                        (make-literal #f '!= (list (make-constant #f '|1|) (make-constant #f '|2|))))
    (test-literal-parse "zero-arity-literal"
                        (make-literal #f 'zero-arity-literal empty))
    (test-literal-parse "zero-arity-literal()"
                        (make-literal #f 'zero-arity-literal empty))
    (test-literal-parse "\"=\"(3,3)"
                        (make-literal #f "=" (list (make-constant #f '|3|) (make-constant #f '|3|))))
    (test-literal-parse "\"\"(-0-0-0,&&&,***,\"\00\")"
                        (make-literal #f "" (list (make-constant #f '-0-0-0)
                                                  (make-constant #f '&&&)
                                                  (make-constant #f '***)
                                                  (make-constant #f "\00")))))
   
   (test-suite
    "clause"
    (test-clause-parse "parent(john, douglas)"
                       (make-clause #f (make-literal #f 'parent (list (make-constant #f 'john) (make-constant #f 'douglas))) empty))
    (test-clause-parse "ancestor(A, B) :- parent(A, B)"
                       (make-clause #f (make-literal #f 'ancestor (list (make-variable #f 'A) (make-variable #f 'B)))
                                    (list (make-literal #f 'parent (list (make-variable #f 'A) (make-variable #f 'B))))))
    (test-clause-parse "ancestor(A, B) :- parent(A, C), ancestor(C, B)"
                       (make-clause #f (make-literal #f 'ancestor (list (make-variable #f 'A) (make-variable #f 'B)))
                                    (list (make-literal #f 'parent (list (make-variable #f 'A) (make-variable #f 'C)))
                                          (make-literal #f 'ancestor (list (make-variable #f 'C) (make-variable #f 'B)))))))
   
   (test-suite
    "statement"
    (test-not-false "assertion" (assertion? (parse-statement (open-input-string "parent(john, douglas)."))))
    (test-not-false "retraction" (retraction? (parse-statement (open-input-string "parent(john, douglas)~"))))
    (test-not-false "query" (query? (parse-statement (open-input-string "parent(john, douglas)?")))))))
