
(htdp-err/rt-test (add1) "add1: expects 1 argument, but found none")
(htdp-err/rt-test (add1 'a 'b 'c) "add1: expects only 1 argument, but found 3")
(htdp-err/rt-test (define x x) "x is used here before its definition")
(htdp-err/rt-test (add1 'a) "add1: expects a number, given 'a")
(htdp-err/rt-test (+ 'a 1) "[+]: expects a number as 1st argument, given 'a")
(htdp-err/rt-test (+ 1 'a) "[+]: expects a number as 2nd argument, given 'a")

(htdp-syntax-test #'() "function call: expected a function after the open parenthesis, but nothing's there")

(htdp-syntax-test #'#%app)
(htdp-syntax-test #'quote)
(htdp-syntax-test #'(quote 1 2))

(htdp-syntax-test #'define "define: expected an open parenthesis before define, but found none")
(htdp-syntax-test #'(define) "define: expected a variable name, or a function name and its variables (in parentheses)")
(htdp-syntax-test #'(define x) "define: expected an expression after the variable name")
(htdp-syntax-test #'(define x 10 12) "define: expected only one expression after the variable name")
(htdp-syntax-test #'(define (10 y) 12) "define: expected the name of the function, but found a number")
(htdp-syntax-test #'(define (10) 12) "define: expected the name of the function, but found a number")
(htdp-syntax-test #'(define ("x" y) 12) "define: expected the name of the function, but found a string")
(htdp-syntax-test #'(define (y 10) 12) "define: expected a variable, but found a number")
(htdp-syntax-test #'(define (y "x") 12) "define: expected a variable, but found a string")
(htdp-syntax-test #'(define (y z 10) 12) "define: expected a variable, but found a number")
(htdp-syntax-test #'(define (x y) 10 12) "define: expected only one expression for the function body, but found 1 extra part")
(htdp-syntax-test #'(define (x y y) 10) "define: found a variable that is used more than once: y")
(htdp-syntax-test #'(define () 10) "define: expected a name for the function, but nothing's there")
(htdp-syntax-test #'(define 1 10) "define: expected a variable name, or a function name and its variables (in parentheses), but found a number")
(htdp-syntax-test #'(define x lambda) "lambda: expected an open parenthesis before lambda, but found none")
(htdp-syntax-test #'(define x (lambda)) "lambda: expected at least one variable (in parentheses) after lambda, but nothing's there")
(htdp-syntax-test #'(define x (lambda (x))) "lambda: expected an expression for the function body, but nothing's there")
(htdp-syntax-test #'(define x (lambda y)) "lambda: expected at least one variable (in parentheses) after lambda, but found something else")
(htdp-syntax-test #'(define x (lambda y 10) "lambda: expected at least one variable (in parentheses) after lambda, but found something else"))
(htdp-syntax-test #'(define x (lambda (10) 10)) "lambda: expected a variable, but found a number")
(htdp-syntax-test #'(define x (lambda (x 10) 10)) "lambda: expected a variable, but found a number")
(htdp-syntax-test #'(define x (lambda (y) 10 11)) "lambda: expected only one expression for the function body, but found 1 extra part")
(htdp-syntax-test #'(define x (lambda (y y) 10)) "lambda: found a variable that is used more than once: y")
(htdp-syntax-test #'(+ (define x 5)) "define: found a definition that is not at the top level")

;; Keywords:
(htdp-syntax-test #'(define (define y) 12) "define: expected the name of the function, but found a keyword")
(htdp-syntax-test #'(define (lambda y) 12) "define: expected the name of the function, but found a keyword")
(htdp-syntax-test #'(define (cond y) 12) "define: expected the name of the function, but found a keyword")
(htdp-syntax-test #'(define (if y) 12) "define: expected the name of the function, but found a keyword")
(htdp-syntax-test #'(define (y define) 12) "define: expected a variable, but found a keyword")
(htdp-syntax-test #'(define (y lambda) 12) "define: expected a variable, but found a keyword")
(htdp-syntax-test #'(define (y cond) 12) "define: expected a variable, but found a keyword")
(htdp-syntax-test #'(define (y if) 12) "define: expected a variable, but found a keyword")
(htdp-syntax-test #'(define (y and) 12) "define: expected a variable, but found a keyword")
(htdp-syntax-test #'(define (y or) 12) "define: expected a variable, but found a keyword")
(htdp-syntax-test #'(define (y empty) 12) "define: expected a variable, but found a keyword")

(htdp-syntax-test #'define-struct "define-struct: expected an open parenthesis before define-struct, but found none")
(htdp-syntax-test #'(define-struct) "define-struct: expected the structure name after define-struct, but nothing's there")
(htdp-syntax-test #'(define-struct a) "define-struct: expected at least one field name (in parentheses) after the structure name, but nothing's there")
(htdp-syntax-test #'(define-struct a (b) 10) "define-struct: expected nothing after the field names, but found 1 extra part")
(htdp-syntax-test #'(define-struct a (b) 10 11 12) "define-struct: expected nothing after the field names, but found 3 extra parts")
(htdp-syntax-test #'(define-struct 10 (b)) "define-struct: expected the structure name after define-struct, but found a number")
(htdp-syntax-test #'(define-struct a b) "define-struct: expected at least one field name (in parentheses) after the structure name, but found something else")
(htdp-syntax-test #'(define-struct a (10)) "define-struct: expected a field name, but found a number")
(htdp-syntax-test #'(define-struct a (b 10)) "define-struct: expected a field name, but found a number")
(htdp-syntax-test #'(define-struct (a) (b)) "define-struct: expected the structure name after define-struct, but found a part")
(htdp-syntax-test #'(define-struct a (b b)) "define-struct: found a field name that is used more than once: b")
(htdp-syntax-test #'(define-struct lambda (b)) "define-struct: expected the structure name after define-struct, but found a keyword")
(htdp-syntax-test #'(+ 1 (define-struct a (b))) "define-struct: found a definition that is not at the top level")

(htdp-top (define x 5))
(htdp-top (define (f y) (+ x y)))
(htdp-test 5 'lookup x)
(htdp-test 9 'app-f (f 4))
(htdp-top (define f2 (lambda (y) (+ x y))))
(htdp-test 15 'app-f (f 10))
(htdp-top-pop 1)
(htdp-top-pop 1)
(htdp-top-pop 1)

(htdp-top (define-struct a0 ()))
(htdp-top (define-struct a1 (b)))
(htdp-top (define-struct a3 (b c d)))
(htdp-test #t 'a0? (a0? (make-a0)))
(htdp-test #t 'a1? (a1? (make-a1 1)))
(htdp-test #t 'a3? (a3? (make-a3 1 2 3)))
(htdp-test #f 'a1? (a1? (make-a3 1 2 3)))
(htdp-test #f 'a3? (a3? (make-a1 1)))
(htdp-err/rt-test (a1-b 10) "a1-b: expects an a1, given 10")
(htdp-syntax-test #'(a0 1 2 3) "a0: expected a function after the open parenthesis, but found a structure name")

(htdp-syntax-test #'cond "cond: expected an open parenthesis before cond, but found none")
(htdp-syntax-test #'(cond) "cond: expected a clause after cond, but nothing's there")
(htdp-syntax-test #'(cond 1) "cond: expected a clause with a question and an answer, but found a number")
(htdp-syntax-test #'(cond [#t 6] 2) "cond: expected a clause with a question and an answer, but found a number")
(htdp-syntax-test #'(cond [else 6] [#f 10]) "cond: found an else clause that isn't the last clause in its cond expression")
(htdp-syntax-test #'(cond [else 6] [else 10]) "cond: found an else clause that isn't the last clause in its cond expression")
(htdp-syntax-test #'(cond []) "cond: expected a clause with a question and an answer, but found an empty part")
(htdp-syntax-test #'(cond [1]) "cond: expected a clause with a question and an answer, but found a clause with only one part")
(htdp-syntax-test #'(cond [1 2 3]) "cond: expected a clause with a question and an answer, but found a clause with 3 parts")
(htdp-syntax-test #'(cond [1 2][]) "cond: expected a clause with a question and an answer, but found an empty part")
(htdp-syntax-test #'(cond [1 2][3 4 5]) "cond: expected a clause with a question and an answer, but found a clause with 3 parts")

(htdp-test 17 'cond (cond [else 17]))
(htdp-test 18 'cond (cond [#t 18]))
(htdp-test 19 'cond (cond [(zero? 10) 0] [#t 19]))
(htdp-test 19 'cond (cond [(zero? 10) 0] [else 19]))


(htdp-err/rt-test (cond [#f 10]) "cond: all question results were false") ;; Should it be a different exception?
(define rx:not-true-or-false "not true or false")
(htdp-err/rt-test (cond [1 10]) rx:not-true-or-false)

(htdp-syntax-test #'if "if: expected an open parenthesis before if, but found none")
(htdp-syntax-test #'(if) "if: expected a question and two answers, but nothing's there")
(htdp-syntax-test #'(if #t) "if: expected a question and two answers, but found only 1 part")
(htdp-syntax-test #'(if #t 1) "if: expected a question and two answers, but found only 2 parts")
(htdp-syntax-test #'(if #t 1 2 3) "if: expected a question and two answers, but found 4 parts")

(htdp-err/rt-test (if 1 2 3) rx:not-true-or-false)

(htdp-syntax-test #'and "and: expected an open parenthesis before and, but found none")
(htdp-syntax-test #'(and) "and: expects at least 2 arguments, but found none")
(htdp-syntax-test #'(and #t) "and: expects at least 2 arguments, but found only 1")

(htdp-err/rt-test (and 1 #t) rx:not-true-or-false)
(htdp-err/rt-test (and #t 1) rx:not-true-or-false)
(htdp-test #f 'ok-and (and #t #f 1))

(htdp-syntax-test #'or "or: expected an open parenthesis before or, but found none")
(htdp-syntax-test #'(or) "or: expects at least 2 arguments, but found none")
(htdp-syntax-test #'(or #t) "or: expects at least 2 arguments, but found only 1")

(htdp-err/rt-test (or 1 #f) rx:not-true-or-false)
(htdp-err/rt-test (or #f 1) rx:not-true-or-false)
(htdp-test #t 'ok-or (or #f #t 1))

(htdp-test #t 'empty? (empty? empty))
(htdp-test #t 'cons? (cons? (cons 1 empty)))


(htdp-test #t 'boolean? (boolean? true))
(htdp-test #t 'boolean? (boolean? false))
(htdp-test #t 'eq? (eq? #t true))
(htdp-test #t 'eq? (eq? #f false))

(htdp-test -9 '- (- 9))

(htdp-top (define-struct an-example-structure (first-field second-field)))
(htdp-error-test #'(define an-example-structure 5))
(htdp-error-test #'(define (an-example-structure x) 5))
(htdp-error-test #'(define-struct an-example-structure (y)))
(htdp-error-test #'(define-struct an-example (structure y)))
(htdp-top-pop 1)

(htdp-top (define an-example-value 12))
(htdp-error-test #'(define an-example-value 5))
(htdp-error-test #'(define (an-example-value x) 5))
(htdp-error-test #'(define-struct an-example-value (y)))
(htdp-error-test #'(define-struct an-example (value y)))
(htdp-top-pop 1)

(htdp-top (define (an-example-function x) x))
(htdp-error-test #'(define an-example-function 5))
(htdp-error-test #'(define (an-example-function x) 5))
(htdp-error-test #'(define-struct an-example-function (y)))
(htdp-error-test #'(define-struct an-example (function y)))
(htdp-top-pop 1)




(htdp-test #t 'equal? (equal? 1 1))
(htdp-test #t 'equal? (equal? (list 1) (list 1)))
(htdp-test #t 'equal? (equal? (list #i1.0 2) (list #i1.0 2)))
(htdp-test #f 'equal? (equal? (list #i1.0 2) (list #i1.0 #i2.0)))
(htdp-test #t 'equal? (equal? (list "apple") (list "apple")))
(htdp-test #f 'equal? (equal? (list "apple") (list 'apple)))
(htdp-test #f 'equal? (equal? (list "apple") (list "banana")))
(htdp-test #t 'equal? (equal? (make-posn 1 2) (make-posn 1 2)))
(htdp-test #f 'equal? (equal? (make-posn 1 #i2.0) (make-posn 1 2)))
(htdp-test #t 'equal? (equal? (make-a1 2) (make-a1 2)))
(htdp-test #f 'equal? (equal? (make-a1 #i2.0) (make-a1 2)))

(htdp-test #t 'equal~? (equal~? 1 1 #i0.1))
(htdp-test #t 'equal~? (equal~? (list 1) (list 1) #i0.1))
(htdp-test #t 'equal~? (equal~? (list #i1.0 2) (list #i1.0 2) #i0.1))
(htdp-test #t 'equal~? (equal~? (list #i1.0 2) (list #i1.0 #i2.0) #i0.1))
(htdp-test #f 'equal~? (equal~? (list #i1.0 2) (list #i1.0 2.2) #i0.1))
(htdp-test #t 'equal~? (equal~? (list "apple") (list "apple") #i0.2))
(htdp-test #f 'equal~? (equal~? (list "apple") (list 'apple) #i0.2))
(htdp-test #f 'equal~? (equal~? (list "apple") (list "banana") #i0.2))
(htdp-test #t 'equal~? (equal~? (make-posn 1 2) (make-posn 1 2) #i0.2))
(htdp-test #t 'equal~? (equal~? (make-posn 1 #i2.0) (make-posn 1 2) #i0.2))
(htdp-test #t 'equal~? (equal~? (make-a1 2) (make-a1 2) #i0.2))
(htdp-test #t 'equal~? (equal~? (make-a1 #i2.0) (make-a1 2) #i0.2))
(htdp-test #f 'equal~? (equal~? (make-a1 #i2.3) (make-a1 2) #i0.2))

(htdp-test #t 'string-contains (string-contains? "x" "abxy"))
(htdp-test #f 'string-contains (string-contains? "x" "abc"))
(htdp-test #f 'string-contains (string-contains? "(" "abc"))
(htdp-test #t 'string-contains (string-contains? "(" "ab(c"))

(htdp-top-pop 1)
(htdp-top-pop 1)
(htdp-top-pop 1)

;; Teachpacks with higher-order primitives
;;  Some further tests are in beg-bega.rkt
(module my-teachpack mzscheme
  (require lang/prim)
  (provide go)
  (define-higher-order-primitive go real-go (_ proc))
  (define (real-go a b) a))
(htdp-teachpack my-teachpack)

(htdp-top (define (my-f x) x))
(htdp-top (define-struct foo (a b)))

(htdp-test 5 'tp (go 5 add1))
(htdp-test 5 'tp (go 5 my-f))
(htdp-test 5 'tp (go 5 foo?))
(htdp-test 5 'tp (go 5 make-foo))
(htdp-test 5 'tp (go 5 foo-a))
(htdp-test 5 'tp (go 5 go))

(htdp-top-pop 1)
(htdp-top-pop 1)
(htdp-teachpack-pop)

;; Check require
(htdp-top (require (lib "unit.rkt" "mzlib")))
(htdp-test #f unit? 12)
(htdp-top-pop 1)
(htdp-top (require mzlib/unit))
(htdp-test #f unit? 12)
(htdp-top-pop 1)

;; Error messages
(htdp-top (define my-x 5))
(htdp-top (define (my-f x) (+ x 5)))

(htdp-syntax-test #'(cond [true my-x 5]) #rx"found a clause with 3 parts")
(htdp-syntax-test #'(define foo17 my-x 5) #rx"define: expected only one expression after the variable name foo17, but found 1 extra part")
(htdp-syntax-test #'(my-y 17) #rx"my-y: this function is not defined")
(htdp-syntax-test #'(cond [true my-y 17]) #rx"my-y: this variable is not defined")
(htdp-syntax-test #'(define my-f 12) #rx"cannot be re-defined")
(htdp-syntax-test #'(define (my-x h) 12) #rx"cannot be re-defined")
(htdp-top-pop 1)
(htdp-top-pop 1)
(htdp-syntax-test #'define #rx"define: expected an open parenthesis before define, but found none")


(htdp-syntax-test #'(require) #rx"found nothing")
(htdp-syntax-test #'(require a!) #rx"bad syntax for a module path")
(htdp-syntax-test #'(require "a" "b") #rx"a single module name")
(htdp-syntax-test #'(require "") #rx"empty")
(htdp-syntax-test #'(require "/a") #rx"start with a slash")
(htdp-syntax-test #'(require "a/") #rx"end with a slash")
(htdp-syntax-test #'(require "a%&#^%") #rx"string can contain only")
(htdp-syntax-test #'(require (lib)) #rx"expected at least one string")
(htdp-syntax-test #'(require (lib "a" "b/")) #rx"end with a slash")
(htdp-syntax-test #'(require (lib "a" 2)) #rx"string for a lib path")
(htdp-syntax-test #'(require (planet "a" 2)) #rx"not a valid planet path")
(htdp-syntax-test #'(require (planet "test-connectionλ.ss" ("planet" "test-connection.plt" 1 0)))
                  #rx"string can contain only")

(define rx:dots-error "found a template")

;; CCE: These test the error handling for ...
;; They should be duplicated for .. through ......
;; but (for-each (lambda foo bar) baz) won't work here.
(htdp-err/rt-test ... rx:dots-error)
(htdp-err/rt-test (+ 1 ... 2) rx:dots-error)
(htdp-err/rt-test (... 1 2) rx:dots-error)
(htdp-err/rt-test (if false 1 ...) rx:dots-error)
(htdp-test 1 'ok-dots (if true 1 ...))
(htdp-error-test #'(set! ... true))

(htdp-top (check-expect 1))
(htdp-syntax-test #'1 "check-expect: expects 2 arguments, but found only 1")
(htdp-top-pop 1)

(htdp-syntax-test #'(cons (check-expect 1 1) empty))
(htdp-syntax-test #'(define (f x) (check-expect 1 x)))
(htdp-syntax-test #'(define (f x) (check-expect 1 x) x))

(htdp-top (check-expect 1 1))
(htdp-test 2 'two 2)
(htdp-top-pop 1)

(htdp-top (check-expect 1 2))
(htdp-test 2 'two 2) ;; test failure recorded in teaching lang...
(htdp-top-pop 1)

(htdp-top (check-expect 1 (/ 1 0)))
(htdp-test 2 'two 2)
(htdp-top-pop 1)

(htdp-top (check-error (/ 1 0) "division by zero"))
(htdp-test 2 'two 2)
(htdp-top-pop 1)

(htdp-top (check-error (/ 1 0)))
(htdp-test 2 'two 2)
(htdp-top-pop 1)

(htdp-top (check-error 1))
(htdp-test 2 'two 2)
(htdp-top-pop 1)

(htdp-top (check-error (/ 1 0) "wrong error"))
(htdp-test 2 'two 2)
(htdp-top-pop 1)

(htdp-top (check-error 0 "not error"))
(htdp-test 2 'two 2)
(htdp-top-pop 1)

(htdp-top (check-within 1 2 3))
(htdp-test 2 'two 2)
(htdp-top-pop 1)


;; -----------------------------------------------------------------------------
;; mf's tests for string functions replacing chars 

(htdp-test "h" 'string-ith (string-ith "hell" 0))

(htdp-err/rt-test (string-ith "hell" 4)
                  (exn-type-and-msg exn:fail:contract?
                                    "string-ith: expected an exact integer in [0, 4) (i.e., less than the length of the given string) for the second argument, but received 4"))

(htdp-err/rt-test (string-ith 10 4)
                  (exn-type-and-msg exn:fail:contract?
                                    "string-ith: expected a string for the first argument, but received 10"))


(htdp-err/rt-test (string-ith "10" 'a)
                  (exn-type-and-msg exn:fail:contract?
                                    "string-ith: expected a natural number for the second argument, but received 'a"))

(htdp-test "aaa" 'replicate (replicate 3 "a"))

(htdp-test "ababab" 'replicate (replicate 3 "ab"))

(htdp-err/rt-test (replicate 3 10)
                  (exn-type-and-msg exn:fail:contract? "replicate: expected a string, but received 10"))

(htdp-test "\n" 'int->string (int->string 10))

(htdp-err/rt-test (int->string 56555)
                  (exn-type-and-msg exn:fail:contract? "int->string: expected an exact integer in [0,55295] or [57344 1114111], but received 56555"))

(htdp-err/rt-test (int->string "A")
                  (exn-type-and-msg exn:fail:contract? "int->string: expected an exact integer in [0,55295] or [57344 1114111], but received \"A\""))

(htdp-test 65 'string->int (string->int "A"))

(htdp-err/rt-test (string->int 10)
                  (exn-type-and-msg exn:fail:contract? "string->int: expected a 1-letter string, but received a string: 10"))

(htdp-err/rt-test (string->int "AB")
                  (exn-type-and-msg exn:fail:contract? "string->int: expected a 1-letter string, but received \"AB\""))

(htdp-test (list "h" "e" "l" "l" "o") 'explode (explode "hello"))

(htdp-err/rt-test (explode 10)
                  (exn-type-and-msg exn:fail:contract? "explode: expected a string, but received 10"))

(htdp-test "hello" 'implode (implode (list "h" "e" "l" "l" "o")))

(htdp-err/rt-test (implode 10)
                  (exn-type-and-msg exn:fail:contract? "implode: expected a list of 1-letter strings, but received: 10"))

(htdp-err/rt-test (implode (list "he" "l"))
                  (exn-type-and-msg exn:fail:contract? "implode: expected a list of 1-letter strings, but received '(\"he\" \"l\")"))


(htdp-test true 'string-numeric? (string-numeric? "0"))
(htdp-test true 'string-numeric? (string-numeric? "10"))
(htdp-test false 'string-numeric? (string-numeric? "a"))
(htdp-test false 'string-numeric? (string-numeric? "ab"))

(htdp-err/rt-test (string-numeric? 10)
                  (exn-type-and-msg exn:fail:contract? "string-numeric?: expected a string, but received 10"))

(htdp-test false 'string-alphabetic? (string-alphabetic? "a0"))
(htdp-test true 'string-alphabetic? (string-alphabetic? "a"))
(htdp-test true 'string-alphabetic? (string-alphabetic? "ba"))
(htdp-test true 'string-alphabetic? (string-alphabetic? "ab"))

(htdp-test true 'string-whitespace? (string-whitespace? "  "))
(htdp-test true 'string-whitespace? (string-whitespace? "  \t"))
(htdp-test false 'string-whitespace? (string-whitespace? "ABC"))

(htdp-test false 'string-upper-case? (string-upper-case? "  "))
(htdp-test false 'string-upper-case? (string-upper-case? "AB\t"))
(htdp-test true 'string-upper-case? (string-upper-case? "ABC"))

(htdp-test false 'string-lower-case? (string-lower-case? "  "))
(htdp-test false 'string-lower-case? (string-lower-case? "ab\t"))
(htdp-test true 'string-lower-case? (string-lower-case? "abc"))

(htdp-err/rt-test (error "a" "a") #rx"^aa$")
(htdp-err/rt-test (error 'a "a") #rx"^a: a$")
(htdp-err/rt-test (error "This is" " an err" "or" " message with a number: " 5)
                  #rx"^This is an error message with a number: 5$")
(htdp-err/rt-test (error "several numbers " 1 2 3 4 5 6 7)
                  #rx"^several numbers 1234567$")
(htdp-err/rt-test (error "several numbers " 1 " 2 " 3 " 4")
                  #rx"^several numbers 1 2 3 4$")
