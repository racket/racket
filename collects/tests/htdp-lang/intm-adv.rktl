
(htdp-err/rt-test (/) "/: expects at least 1 argument, but found none")
(htdp-err/rt-test (pi) #px"function call: expected a function after the open parenthesis, but received 3[.]14\\d+$")
(htdp-err/rt-test (pi 1 2) #px"function call: expected a function after the open parenthesis, but received 3[.]14\\d+$")

(htdp-top (define (f x) x))
(htdp-err/rt-test (f 1 2) "f: expects only 1 argument, but found 2")
(htdp-top-pop 1)

;; These are true for beginner, but the operators are syntax, so
;; arity-test doesn't work.

(htdp-syntax-test #'local "local: expected an open parenthesis before local, but found none")
(htdp-syntax-test #'(local) "local: expected at least one definition (in square brackets) after local, but nothing's there")
(htdp-syntax-test #'(local ()) "local: expected an expression after the local definitions, but nothing's there")
(htdp-syntax-test #'(local 1) "local: expected at least one definition (in square brackets) after local, but found a number")
(htdp-syntax-test #'(local 1 1) "local: expected at least one definition (in square brackets) after local, but found a number")
(htdp-syntax-test #'(local () 1 2) "local: expected only one expression after the local definitions, but found 1 extra part")
(htdp-syntax-test #'(local [1] 1 2) "local: expected a definition, but found a number")
(htdp-syntax-test #'(local [(+ 1 2)] 1) "local: expected a definition, but found a part")
(htdp-syntax-test #'(local [(define x)] 1) "define: expected an expression after the variable name x, but nothing's there")
(htdp-syntax-test #'(local [(define x 1) (define x 2)] 1) "local: x was defined locally more than once")
(htdp-syntax-test #'(local [(define (x a) 12) (+ 1 2)] 1) "local: expected a definition, but found a part")

(htdp-err/rt-test (local [(define x y) (define y 5)] 10)
                  (exn-type-and-msg exn:fail:contract:variable?
                                    "local variable used before its definition: y"))

(htdp-test 1 'local (local () 1))
(htdp-test 5 'local (local [(define y 5) (define x y)] x))
(htdp-test #t 'local (local [(define (even n) (if (zero? n) true (odd (sub1 n))))
			(define (odd n) (if (zero? n) false (even (sub1 n))))]
		       (even 100)))
(htdp-test 19 (local [(define (f x) (+ x 10))] f) 9)
(htdp-test 16 'local (local [(define (f x) (+ x 10))] (f 6)))

(htdp-syntax-test #'letrec "letrec: expected an open parenthesis before letrec, but found none")
(htdp-syntax-test #'(letrec) "letrec: expected at least one binding (in parentheses) after letrec, but nothing's there")
(htdp-syntax-test #'(letrec ()) "letrec: expected an expression after the bindings, but nothing's there")
(htdp-syntax-test #'(letrec 1 2) "letrec: expected at least one binding (in parentheses) after letrec, but found a number")
(htdp-syntax-test #'(letrec 1 2 3) "letrec: expected at least one binding (in parentheses) after letrec, but found a number")
(htdp-syntax-test #'(letrec (10) 1) "letrec: expected a binding with a variable and an expression, but found a number")
(htdp-syntax-test #'(letrec ([x]) 1) "letrec: expected an expression after the name x, but nothing's there")
(htdp-syntax-test #'(letrec ([x 2 3]) 1) "letrec: expected only one expression after the name x, but found 1 extra part")
(htdp-syntax-test #'(letrec ([x 5] 10) 1) "letrec: expected a binding with a variable and an expression, but found a number")
(htdp-syntax-test #'(letrec ([1 5]) 1) "letrec: expected a variable for the binding, but found a number")
(htdp-syntax-test #'(letrec ([1 5 6]) 1) "letrec: expected a variable for the binding, but found a number")
(htdp-syntax-test #'(letrec ([x 5]) 1 2) "letrec: expected only one expression after the bindings, but found 1 extra part")
(htdp-syntax-test #'(letrec ([x 5][x 6]) 1) "letrec: x was defined locally more than once")

(htdp-err/rt-test (letrec ([x y] [y 5]) 10)
                  (exn-type-and-msg exn:fail:contract:variable?
                                    "local variable used before its definition: y"))
                  
(htdp-test 1 'letrec (letrec () 1))
(htdp-test 5 'letrec (letrec ([y 5][x y]) x))
(htdp-test #t 'letrec (letrec ([even (lambda (n) (if (zero? n) true (odd (sub1 n))))]
			  [odd (lambda (n) (if (zero? n) false (even (sub1 n))))])
		   (even 100)))
(htdp-test 19 (letrec ([f (lambda (x) (+ x 10))]) f) 9)
(htdp-test 16 'letrec (letrec ([f (lambda (x) (+ x 10))]) (f 6)))

(htdp-syntax-test #'let "let: expected an open parenthesis before let, but found none")
(htdp-syntax-test #'(let) "let: expected at least one binding (in parentheses) after let, but nothing's there")
(htdp-syntax-test #'(let ()) "let: expected an expression after the bindings, but nothing's there")
(htdp-syntax-test #'(let 1 2) "let: expected at least one binding (in parentheses) after let, but found a number")
(htdp-syntax-test #'(let 1 2 3) "let: expected at least one binding (in parentheses) after let, but found a number")
(htdp-syntax-test #'(let (10) 1) "let: expected a binding with a variable and an expression, but found a number")
(htdp-syntax-test #'(let ([x]) 1) "let: expected an expression after the name x, but nothing's there")
(htdp-syntax-test #'(let ([x 2 3]) 1) "let: expected only one expression after the name x, but found 1 extra part" )
(htdp-syntax-test #'(let ([x 5] 10) 1) "let: expected a binding with a variable and an expression, but found a number")
(htdp-syntax-test #'(let ([1 5]) 1) "let: expected a variable for the binding, but found a number")
(htdp-syntax-test #'(let ([1 5 6]) 1) "let: expected a variable for the binding, but found a number")
(htdp-syntax-test #'(let ([x 5]) 1 2) "let: expected only one expression after the bindings, but found 1 extra part")
(htdp-syntax-test #'(let ([x 5][x 6]) 1) "let: x was defined locally more than once")

(htdp-test 1 'let (let () 1))
(htdp-test 5 'let (let ([y 5]) (let ([x y]) x)))
(htdp-test 6 'let (let ([y 6]) (let ([y 10][x y]) x)))
(htdp-test 19 (let ([f (lambda (x) (+ x 10))]) f) 9)
(htdp-test 16 'let (let ([f (lambda (x) (+ x 10))]) (f 6)))

(htdp-syntax-test #'let* "let*: expected an open parenthesis before let*, but found none")
(htdp-syntax-test #'(let*) "let*: expected at least one binding (in parentheses) after let*, but nothing's there")
(htdp-syntax-test #'(let* ()) "let*: expected an expression after the bindings, but nothing's there")
(htdp-syntax-test #'(let* 1 2) "let*: expected at least one binding (in parentheses) after let*, but found a number")
(htdp-syntax-test #'(let* 1 2 3) "let*: expected at least one binding (in parentheses) after let*, but found a number")
(htdp-syntax-test #'(let* (10) 1) "let*: expected a binding with a variable and an expression, but found a number")
(htdp-syntax-test #'(let* ([x]) 1) "let*: expected an expression after the name x, but nothing's there")
(htdp-syntax-test #'(let* ([x 2 3]) 1) "let*: expected only one expression after the name x, but found 1 extra part")
(htdp-syntax-test #'(let* ([x 5] 10) 1) "let*: expected a binding with a variable and an expression, but found a number")
(htdp-syntax-test #'(let* ([1 5]) 1) "let*: expected a variable for the binding, but found a number")
(htdp-syntax-test #'(let* ([1 5 6]) 1) "let*: expected a variable for the binding, but found a number")
(htdp-syntax-test #'(let* ([x 5]) 1 2) "let*: expected only one expression after the bindings, but found 1 extra part")

(htdp-test 1 'let* (let* () 1))
(htdp-test 6 'let* (let* ([x 5][x 6]) x))
(htdp-test 9 'let* (let* ([x 8][x (add1 x)]) x))
(htdp-test 5 'let* (let* ([y 5]) (let* ([x y]) x)))
(htdp-test 10 'let* (let* ([y 6]) (let* ([y 10][x y]) x)))
(htdp-test 19 (let* ([f (lambda (x) (+ x 10))]) f) 9)
(htdp-test 16 'let* (let* ([f (lambda (x) (+ x 10))]) (f 6)))

(htdp-test 7779 'time (time 7779))
(htdp-syntax-test #'time "time: expected an open parenthesis before time, but found none")
(htdp-syntax-test #'(time) "time: expected an expression after time, but nothing's there")
(htdp-syntax-test #'(time 1 2) "time: expected only one expression after time, but found 1 extra part")
(htdp-syntax-test #'(time (define x 5)) "define: found a definition that is not at the top level")

(htdp-err/rt-test (foldr car 2 '(1 2 3))
                  "foldr : first argument must be a function that expects two arguments, given")

(htdp-err/rt-test (foldl car 2 '(1 2 3))
  "foldl : first argument must be a function that expects two arguments, given #<procedure:car>")

(htdp-err/rt-test (build-string 2 add1)
  "build-string : the second argument must be a function that produces a character, given #<procedure:add1>, which produced 1 when given 0")

(htdp-test 0 '+ (+))
(htdp-test 1 '+ (+ 1))
(htdp-test 1 '* (*))
(htdp-test 1 '* (* 1))
(htdp-err/rt-test (-) (exn-type-and-msg exn:application:arity? #rx"-: expects at least 1 argument, but found none"))
(htdp-err/rt-test (/) (exn-type-and-msg exn:application:arity? #rx"/: expects at least 1 argument, but found none"))
;(htdp-test 1 (/ 1) exn:application:arity?)

;; Check that `local' works with macros that expand to `begin':
(module my-multi-defn racket/base
  (provide multi)
  (define-syntax-rule (multi a b)
    (begin
      (define a 1)
      (define b 2))))
(htdp-teachpack my-multi-defn)

(htdp-test '(2 1) 'local (local [(multi x y)]
                           (list y x)))
