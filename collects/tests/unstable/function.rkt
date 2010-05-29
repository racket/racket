#lang racket
(require rackunit rackunit/text-ui unstable/function
         "helpers.rkt")

(define list/kw (make-keyword-procedure list))

(run-tests
 (test-suite "function.ss"

   (test-suite "Simple Functions"

     (test-suite "identity"
       (test-case "unique symbol"
         (let* ([sym (gensym)])
           (check-eq? (identity sym) sym))))

     (test-suite "const"
       (test-case "unique symbol"
         (let* ([sym (gensym)])
           (check-eq? ((const sym) 'x #:y 'z) sym))))

     (test-suite "thunk"
       (test-case "unique symbol"
         (let* ([count 0]
                [f (thunk (set! count (+ count 1)) count)])
           (check = count 0)
           (check = (f) 1)
           (check = count 1)))))

   (test-suite "Higher Order Predicates"

     (test-suite "negate"
       (test-case "integer?"
         (check-false ((negate integer?) 5)))
       (test-case "not integer?"
         (check-true ((negate integer?) 1/5)))
       (test-case "non-boolean"
         (check-false ((negate symbol->string) 'sym)))
       (test-case "binary"
         (check-false ((negate +) 1 2 3))))

     (test-suite "conjoin"
       (test-case "no functions"
         (check-true ((conjoin) 'x #:y 'z)))
       (test-case "true"
         (check-true ((conjoin integer? exact?) 1)))
       (test-case "false"
         (check-false ((conjoin integer? exact?) 1.0)))
       (test-case "false"
         (check-false ((conjoin integer? exact?) 0.5))))

     (test-suite "disjoin"
       (test-case "no functions"
         (check-false ((disjoin) 'x #:y 'z)))
       (test-case "true"
         (check-true ((disjoin integer? exact?) 1)))
       (test-case "true"
         (check-true ((disjoin integer? exact?) 1/2)))
       (test-case "false"
         (check-false ((disjoin integer? exact?) 0.5)))))

   (test-suite "Currying and (Partial) Application"

     (test-suite "call"
       (test-case "string-append"
         (check-equal? (call string-append "a" "b" "c") "abc")))

     (test-suite "papply"
       (test-case "list"
         (check-equal? ((papply list 1 2) 3 4) (list 1 2 3 4)))
       (test-case "sort"
         (check-equal?
          ((papply sort '((1 a) (4 d) (2 b) (3 c)) #:cache-keys? #f)
           < #:key car)
          '((1 a) (2 b) (3 c) (4 d)))))

     (test-suite "papplyr"
       (test-case "list"
         (check-equal? ((papplyr list 1 2) 3 4) (list 3 4 1 2)))
       (test-case "sort"
         (check-equal?
          ((papplyr sort < #:key car)
           '((1 a) (4 d) (2 b) (3 c)) #:cache-keys? #f)
          '((1 a) (2 b) (3 c) (4 d)))))

     (test-suite "curryn"
       (test-case "1"
         (check-equal? (curryn 0 list/kw 1) '(() () 1)))
       (test-case "1 / 2"
         (check-equal? ((curryn 1 list/kw 1) 2) '(() () 1 2)))
       (test-case "1 / 2 / 3"
         (check-equal? (((curryn 2 list/kw 1) 2) 3) '(() () 1 2 3)))
       (test-case "1 a"
         (check-equal? (curryn 0 list/kw 1 #:a "a")
                       '((#:a) ("a") 1)))
       (test-case "1 a / 2 b"
         (check-equal? ((curryn 1 list/kw 1 #:a "a") 2 #:b "b")
                       '((#:a #:b) ("a" "b") 1 2)))
       (test-case "1 a / 2 b / 3 c"
         (check-equal? (((curryn 2 list/kw 1 #:a "a") 2 #:b "b") 3 #:c "c")
                       '((#:a #:b #:c) ("a" "b" "c") 1 2 3))))

     (test-suite "currynr"
       (test-case "1"
         (check-equal? (currynr 0 list/kw 1) '(() () 1)))
       (test-case "1 / 2"
         (check-equal? ((currynr 1 list/kw 1) 2) '(() () 2 1)))
       (test-case "1 / 2 / 3"
         (check-equal? (((currynr 2 list/kw 1) 2) 3) '(() () 3 2 1)))
       (test-case "1 a"
         (check-equal? (currynr 0 list/kw 1 #:a "a")
                       '((#:a) ("a") 1)))
       (test-case "1 a / 2 b"
         (check-equal? ((currynr 1 list/kw 1 #:a "a") 2 #:b "b")
                       '((#:a #:b) ("a" "b") 2 1)))
       (test-case "1 a / 2 b / 3 c"
         (check-equal? (((currynr 2 list/kw 1 #:a "a") 2 #:b "b") 3 #:c "c")
                       '((#:a #:b #:c) ("a" "b" "c") 3 2 1)))))

   (test-suite "Eta Expansion"
     (test-suite "eta"
       (test-ok (define f (eta g))
                (define g add1)
                (check-equal? (f 1) 2)))
     (test-suite "eta*"
       (test-ok (define f (eta* g x))
                (define g add1)
                (check-equal? (f 1) 2))
       (test-bad (define f (eta* g x))
                 (define g list)
                 (f 1 2))))

   (test-suite "Parameter Arguments"

     (test-suite "lambda/parameter"
       (test-case "provided"
         (let* ([p (make-parameter 0)])
           (check = ((lambda/parameter ([x #:param p]) x) 1) 1)))
       (test-case "not provided"
         (let* ([p (make-parameter 0)])
           (check = ((lambda/parameter ([x #:param p]) x)) 0)))
       (test-case "argument order / provided"
         (let* ([p (make-parameter 3)])
           (check-equal? ((lambda/parameter (x [y 2] [z #:param p])
                                            (list x y z))
                          4 5 6)
                         (list 4 5 6))))
       (test-case "argument order / not provided"
         (let* ([p (make-parameter 3)])
           (check-equal? ((lambda/parameter (x [y 2] [z #:param p])
                                            (list x y z))
                          1)
                         (list 1 2 3))))))))
