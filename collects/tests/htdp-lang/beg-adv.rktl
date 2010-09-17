
(htdp-syntax-test #'())
(htdp-syntax-test #'#%app)

(htdp-syntax-test #'quote)
(htdp-syntax-test #'(quote 1 2))

(htdp-syntax-test #'define)
(htdp-syntax-test #'(define))
(htdp-syntax-test #'(define x))
(htdp-syntax-test #'(define x 10 12))
(htdp-syntax-test #'(define (10 y) 12))
(htdp-syntax-test #'(define (10) 12))
(htdp-syntax-test #'(define ("x" y) 12))
(htdp-syntax-test #'(define (y 10) 12))
(htdp-syntax-test #'(define (y "x") 12))
(htdp-syntax-test #'(define (y z 10) 12))
(htdp-syntax-test #'(define (x y) 10 12))
(htdp-syntax-test #'(define (x y y) 10))
(htdp-syntax-test #'(define () 10))
(htdp-syntax-test #'(define 1 10))
(htdp-syntax-test #'(define x lambda))
(htdp-syntax-test #'(define x (lambda)))
(htdp-syntax-test #'(define x (lambda (x))))
(htdp-syntax-test #'(define x (lambda y)))
(htdp-syntax-test #'(define x (lambda y 10)))
(htdp-syntax-test #'(define x (lambda (10) 10)))
(htdp-syntax-test #'(define x (lambda (x 10) 10)))
(htdp-syntax-test #'(define x (lambda (y) 10 11)))
(htdp-syntax-test #'(define x (lambda (y) 10 11)))
(htdp-syntax-test #'(define x (lambda (y y) 10)))
(htdp-syntax-test #'(+ (define x 5)))

;; Keywords:
(htdp-syntax-test #'(define (define y) 12))
(htdp-syntax-test #'(define (lambda y) 12))
(htdp-syntax-test #'(define (cond y) 12))
(htdp-syntax-test #'(define (if y) 12))
(htdp-syntax-test #'(define (y define) 12))
(htdp-syntax-test #'(define (y lambda) 12))
(htdp-syntax-test #'(define (y cond) 12))
(htdp-syntax-test #'(define (y if) 12))
(htdp-syntax-test #'(define (y and) 12))
(htdp-syntax-test #'(define (y or) 12))
(htdp-syntax-test #'(define (y true) 12))
(htdp-syntax-test #'(define (y false) 12))
(htdp-syntax-test #'(define (y empty) 12))

(htdp-syntax-test #'define-struct)
(htdp-syntax-test #'(define-struct))
(htdp-syntax-test #'(define-struct a))
(htdp-syntax-test #'(define-struct a (b) 10))
(htdp-syntax-test #'(define-struct a (b) 10 11 12))
(htdp-syntax-test #'(define-struct 10 (b)))
(htdp-syntax-test #'(define-struct a b))
(htdp-syntax-test #'(define-struct a (10)))
(htdp-syntax-test #'(define-struct a (b 10)))
(htdp-syntax-test #'(define-struct (a) (b)))
(htdp-syntax-test #'(define-struct a (b b)))
(htdp-syntax-test #'(define-struct lambda (b)))
(htdp-syntax-test #'(+ 1 (define-struct a (b))))

(htdp-top (define x 5))
(htdp-top (define (f y) (+ x y)))
(htdp-test 5 'lookup x)
(htdp-test 9 'app-f (f 4))
(htdp-top (define f2 (lambda (y) (+ x y))))
(htdp-test 15 'app-f (f 10))
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
(htdp-err/rt-test (a1-b 10) #rx"a1-b")
(htdp-syntax-test #'(a0 1 2 3))

(htdp-syntax-test #'cond)
(htdp-syntax-test #'(cond))
(htdp-syntax-test #'(cond 1))
(htdp-syntax-test #'(cond [#t 6] 2))
(htdp-syntax-test #'(cond [else 6] [#f 10]))
(htdp-syntax-test #'(cond [else 6] [else 10]))
(htdp-syntax-test #'(cond []))
(htdp-syntax-test #'(cond [1]))
(htdp-syntax-test #'(cond [1 2 3]))
(htdp-syntax-test #'(cond [1 2][]))
(htdp-syntax-test #'(cond [1 2][3 4 5]))

(htdp-test 17 'cond (cond [else 17]))
(htdp-test 18 'cond (cond [#t 18]))
(htdp-test 19 'cond (cond [(zero? 10) 0] [#t 19]))
(htdp-test 19 'cond (cond [(zero? 10) 0] [else 19]))

(htdp-err/rt-test (cond [#f 10]) exn:fail?) ;; Should it be a different exception?
(define rx:not-true-or-false "not true or false")
(htdp-err/rt-test (cond [1 10]) rx:not-true-or-false)

(htdp-syntax-test #'if)
(htdp-syntax-test #'(if))
(htdp-syntax-test #'(if #t))
(htdp-syntax-test #'(if #t 1))
(htdp-syntax-test #'(if #t 1 2 3))

(htdp-err/rt-test (if 1 2 3) rx:not-true-or-false)

(htdp-syntax-test #'and)
(htdp-syntax-test #'(and))
(htdp-syntax-test #'(and #t))

(htdp-err/rt-test (and 1 #t) rx:not-true-or-false)
(htdp-err/rt-test (and #t 1) rx:not-true-or-false)
(htdp-test #f 'ok-and (and #t #f 1))

(htdp-syntax-test #'or)
(htdp-syntax-test #'(or))
(htdp-syntax-test #'(or #t))

(htdp-err/rt-test (or 1 #f) rx:not-true-or-false)
(htdp-err/rt-test (or #f 1) rx:not-true-or-false)
(htdp-test #t 'ok-or (or #f #t 1))

(htdp-test #t 'empty? (empty? empty))
(htdp-test #t 'cons? (cons? (cons 1 empty)))

(htdp-err/rt-test (cons 1 2))
(htdp-err/rt-test (append (list 1) 2))

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

(htdp-top-pop 1)
(htdp-top-pop 1)
(htdp-top-pop 1)

;; Teachpacks with higher-order primitives
;;  Some further tests are in beg-bega.ss
(module my-teachpack mzscheme
  (require (lib "prim.ss" "lang"))
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
(htdp-top (require (lib "unit.ss" "mzlib")))
(htdp-test #f unit? 12)
(htdp-top-pop 1)
(htdp-top (require mzlib/unit))
(htdp-test #f unit? 12)
(htdp-top-pop 1)

;; Error messages
(htdp-top (define my-x 5))
(htdp-top (define (my-f x) (+ x 5)))
(htdp-syntax-test #'(cond [true my-x 5]) #rx"found a clause with 3 parts")
(htdp-syntax-test #'(define foo17 my-x 5) #rx"found one extra part")
(htdp-syntax-test #'(my-y 17) #rx"not defined, not a parameter, and not a primitive name")
(htdp-syntax-test #'(cond [true my-y 17]) #rx"not defined, not a parameter, and not a primitive name")
(htdp-syntax-test #'(define my-f 12) #rx"cannot be re-defined")
(htdp-syntax-test #'(define (my-x h) 12) #rx"cannot be re-defined")
(htdp-top-pop 1)
(htdp-top-pop 1)
(htdp-syntax-test #'define #rx"does not follow")

(htdp-syntax-test #'(require) #rx"found nothing")
(htdp-syntax-test #'(require a!) #rx"bad syntax for a module path")
(htdp-syntax-test #'(require "a" "b") #rx"a single module name")
(htdp-syntax-test #'(require "") #rx"empty")
(htdp-syntax-test #'(require "/a") #rx"start with a slash")
(htdp-syntax-test #'(require "a/") #rx"end with a slash")
(htdp-syntax-test #'(require "a%&#^%") #rx"string can contain only")
(htdp-syntax-test #'(require (lib)) #rx"expected at least two strings")
(htdp-syntax-test #'(require (lib "a")) #rx"expected at least two strings")
(htdp-syntax-test #'(require (lib "a" "b/")) #rx"end with a slash")
(htdp-syntax-test #'(require (lib "a" 2)) #rx"string for a lib path")
(htdp-syntax-test #'(require (planet "a" 2)) #rx"not a valid planet path")

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

(htdp-err/rt-test (string-ith "hell" 4) exn:fail:contract?
  #;
  (string-append 
    "string-ith:"
    " <exact integer in [0, length of the given string (4)]>"
    " for second argument expected, given "
    "4"))

(htdp-err/rt-test (string-ith 10 4) exn:fail:contract?
  #;
  (string-append "string-ith: <string> for first argument expected, given "
    "10"))

(htdp-err/rt-test (string-ith "10" 'a) exn:fail:contract?
  #;
  (string-append "string-ith: <natural number> for second argument expected, given "
    "a"))

(htdp-test "aaa" 'replicate (replicate 3 "a"))

(htdp-test "ababab" 'replicate (replicate 3 "ab"))

(htdp-err/rt-test (replicate 3 10) exn:fail:contract?
  #;
  "replicate: <string> expected, given 10")

(htdp-test "\n" 'int->string (int->string 10))

(htdp-err/rt-test (int->string 56555) exn:fail:contract?
  #;
  (string-append 
    "int->string: <exact integer in [0,55295] or [57344 1114111]> expected, given "
    "56555"))

(htdp-err/rt-test (int->string "A") exn:fail:contract?
  #;
  (string-append 
    "int->string: <exact integer in [0,55295] or [57344 1114111]> expected, given "
    (format "~s" "A")))

(htdp-test 65 'string->int (string->int "A"))

(htdp-err/rt-test (string->int 10) exn:fail:contract?
  #;
  (string-append "string->int: " 1-LETTER " expected, not a string: 10"))

(htdp-err/rt-test (string->int "AB") exn:fail:contract?
  #;
  (string-append
    "string->int: " 1-LETTER " expected, given " (format "~s" "AB")))

(htdp-test (list "h" "e" "l" "l" "o") 'explode (explode "hello"))

(htdp-err/rt-test (explode 10) exn:fail:contract?
  #;
  (string-append "explode: <string> expected, given " "10"))

(htdp-test "hello" 'implode (implode (list "h" "e" "l" "l" "o")))

(htdp-err/rt-test (implode 10) exn:fail:contract?
  #;
  (string-append "implode: " 1-LETTER* " expected, not a <list>: 10"))

(htdp-err/rt-test (implode (list "he" "l")) exn:fail:contract?
  #;
  (string-append "implode: " 1-LETTER* " expected, given " 
    (format "~s" (list "he" "l"))))

(htdp-test true 'string-numeric? (string-numeric? "0"))
(htdp-test true 'string-numeric? (string-numeric? "10"))
(htdp-test false 'string-numeric? (string-numeric? "a"))
(htdp-test false 'string-numeric? (string-numeric? "ab"))

(htdp-err/rt-test (string-numeric? 10) exn:fail:contract?
  #;
  (string-append "string-numeric?: <string> expected, given 10"))


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

