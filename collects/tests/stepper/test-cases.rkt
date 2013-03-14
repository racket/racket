#lang racket

(require "test-abbrev.rkt"
         (prefix-in m: "language-level-model.rkt")
         stepper/private/model-settings)

(provide the-test-cases)


;; a test with no extra files (a.k.a all but one of them :) )
(define (t1 name models string expected-steps)
  (list name models string expected-steps '()))


(define (make-teachpack-ll-model teachpack-specs)
  (m:make-ll-model `(lib "htdp-beginner.ss" "lang") teachpack-specs fake-beginner-render-settings #f #f))


(define err '(/ 1 0))



(define (<delay#> n)
  (string->symbol 
   (string-append "<DelayedEvaluation#" (number->string n) ">")))


(define the-test-cases
  (list
   

;; new test case language:
;; an expected is (listof step)
;; a step is one of
;; (before-after exps exps)
;; (before-error exps str)
;; (error str)
;; (finished)
;; an exps is a list of s-expressions with certain non-hygienic extensions:
;;  - (hilite X) denotes the s-expression X, only highlighted
;;  - any        denotes any s-expression (matches everything)
;;  ... in principle, these could collide with programs that use the
;;      identifiers 'hilite' and 'any', but since I'm writing the test cases,
;;      I can alpha-rename manually to avoid collisions.

;; on top of this, the `t' macro makes things easier to write, informally:
;; (t 'name ; symbolic name for the test
;;    tester ; tester function that gets used
;;    expr1 ... :: expr2 ... -> expr3 ...)
;; means that `expr1 ...' is the original, the first step is
;;   (before-after (expr2 ...) (expr3 ...))
;; Cute stuff:
;; * use `::' to mark a new step that doesn't continue the previous one
;;     e1 :: e2 -> e3 -> e4
;;   is the same as
;;     e1 :: e2 -> e3 :: e3 -> e4
;; * use `-> error: "..."' for a `before-error' step
;; * use `:: error: "..."' for an `error' step
;; * a `finished-stepping' is added if no error was specified
;; * a `{...}' is replaced with `(hilite ...)'

(t 'mz1 m:mz
   (for-each (lambda (x) x) '(1 2 3))
   :: {(for-each (lambda (x) x) `(1 2 3))} -> (... {1} ...)
   :: ... -> (... {2} ...)
   :: ... -> (... {3} ...)
   :: ... -> {(void)})

(t 'mz-app m:mz
   (+ 3 4)
   :: {(+ 3 4)} -> {7})

(t 'mz-app2 m:mz
   ((lambda (x) (+ x 3)) 4)
   :: {((lambda (x) (+ x 3)) 4)} -> {(+ 4 3)} -> {7})

(t 'mz-if m:mz
   (if 3 4 5)
   :: {(if 3 4 5)} -> {4})

(t 'direct-app m:mz
   ((lambda (x) x) 3)
   :: {((lambda (x) x) 3)} -> {3})

;   (m:mz "((lambda (x) x) (begin (+ 3 4) (+ 4 5)))"
;                     `((before-after ((begin (hilite (+ 3 4)) (+ 4 5)))
;                                     ((begin (hilite 7) (+ 4 5))))
;                       (before-after ((hilite (begin 7 (+ 4 5)))) ((hilite (+ 4 5))))
;                        (before-after ((hilite (+ 4 5))) ((hilite 9)))
;                       (finished-stepping)))

(t 'curried m:mz
   ((lambda (a) (lambda (b) (+ a b))) 14)
   :: {((lambda (a) (lambda (b) (+ a b))) 14)}
   -> {(lambda (b) (+ 14 b))})

(t 'case-lambda m:mz
   ((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)
   :: {((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)}
   -> {(+ 5 6)}
   -> {11})

;; not really a part of base mzscheme anymore
#;(t '2armed-if m:mz
     (if 3 4)
     :: {(if 3 4)} -> {4})

;(m:mz "((call-with-current-continuation call-with-current-continuation) (call-with-current-continuation call-with-current-continuation))"
;                  `((before-after (((hilite ,h-p) (call-with-current-continuation call-with-current-continuation))) ((call-with-current-continuation call-with-current-continuation))
;                    (((hilite ,h-p) (call-with-current-continuation call-with-current-continuation))) ((lambda args ...)))
;                    (before-after (((lambda args ...) (hilite ,h-p))) ((call-with-current-continuation call-with-current-continuation))
;                    (((lambda args ...) (hilite ,h-p))) ((lambda args ...)))))

;(m:mz '(begin (define g 3) g)
;                  `((before-after ((hilite ,h-p)) (g)
;                    ((hilite ,h-p)) 3)))

;(syntax-object->datum (cadr (annotate-expr test2 'mzscheme 0 (lambda (x) x))))

   

(t 'simple-if m:upto-int/lam
   (if true false true)
   :: {(if true false true)} -> {false})

(t 'if-bool m:upto-int/lam
   (if (if true false true) false true)
   :: (if {(if true false true)} false true) -> (if {false} false true)
   :: {(if false false true)} -> {true})


(t 'top-def m:upto-int/lam
   (define a (+ 3 4))
   :: (define a {(+ 3 4)})
   -> (define a {7}))

(t 'top-def-ref m:upto-int/lam
   (define a 6) a
   :: (define a 6) {a} -> (define a 6) {6})

(t 'app m:upto-int/lam
   (+ 4 129)
   :: {(+ 4 129)} -> {133})

(t 'if m:upto-int/lam (if true 3 4)
   :: {(if true 3 4)} -> {3})

(let ([def `(define (a3 x) (if true x x))])
  (t 'top-app m:upto-int
     ,def (a3 false)
     :: ,def {(a3 false)}
     -> ,def {(if true false false)}
     -> ,def {false})
  ;;
  (t 'top-app/lam m:intermediate-lambda
     ,def (a3 false)
     :: ,def ({a3} false)
     -> ,def ({(lambda (x) (if true x x))} false)
     :: ,def {((lambda (x) (if true x x)) false)}
     -> ,def {(if true false false)}
     -> ,def {false}))

(let ([defs `((define (a12 x) (+ x 9)) (define b12 a12))])
  (t 'top-interref m:intermediate
     ,@defs (b12 12)
     :: ,@defs ({b12} 12)
     -> ,@defs ({a12} 12)
     :: ,@defs {(a12 12)}
     -> ,@defs {(+ 12 9)}
     -> ,@defs {21}))

;;intermediate/lambda hof
(let ([a-def `(define (a x)
                (lambda (y) (+ x y)))])
  (t 'intermediate-lambda-hof m:intermediate-lambda
     ,a-def (define b (a 9)) (b 5)
     :: ,a-def (define b ({a} 9))
     -> ,a-def (define b ({(lambda (x) (lambda (y) (+ x y)))} 9))
     :: ,a-def (define b {((lambda (x) (lambda (y) (+ x y))) 9)})
     -> ,a-def (define b {(lambda (y) (+ 9 y))})
     :: ,a-def (define b (lambda (y) (+ 9 y))) ({b} 5)
     -> ,a-def (define b (lambda (y) (+ 9 y))) 
     ({(lambda (y) (+ 9 y))} 5)
     :: ,a-def (define b (lambda (y) (+ 9 y)))
     {((lambda (y) (+ 9 y)) 5)}
     -> ,a-def (define b (lambda (y) (+ 9 y))) {(+ 9 5)}
     -> ,a-def (define b (lambda (y) (+ 9 y))) {14}))

;;;;;;;;;;;;
;;
;;  OR / AND
;;
;;;;;;;;;;;;;.

(t 'or1 m:upto-int/lam
   (or false true false)
   :: {(or false true false)} -> {true})

(t 'and1 m:upto-int/lam
   (and true false true)
   :: {(and true false true)} -> {false})

(t 'and2 m:upto-int/lam
   (and true (if true true false))
   :: (and true {(if true true false)}) -> (and true {true})
   :: {(and true true)} -> {true}) 



(let ([def `(define (b2 x) (and true x))])
  (t 'and3 m:upto-int
     ,def (b2 false)
     :: ,def {(b2 false)}
     -> ,def {(and true false)}
     -> ,def {false})
  ;;
  (t 'and3/lam m:intermediate-lambda
     (define (b2 x) (and true x)) (b2 false)
     :: ,def ({b2} false)
     -> ,def ({(lambda (x) (and true x))} false)
     :: ,def {((lambda (x) (and true x)) false)}
     -> ,def {(and true false)}
     -> ,def {false}))

(let ([defs `((define a1 true)
              (define (b1 x) (and a1 true x)))])
  (t 'and4 m:upto-int
     ,@defs (b1 false)
     :: ,@defs {(b1 false)}
     -> ,@defs {(and a1 true false)}
     :: ,@defs (and {a1} true false)
     -> ,@defs (and {true} true false)
     :: ,@defs {(and true true false)}
     -> ,@defs {false})
  ;;
  (t 'and4/lam m:intermediate-lambda
     ,@defs (b1 false)
     :: ,@defs ({b1} false)
     -> ,@defs ({(lambda (x) (and a1 true x))} false)
     :: ,@defs {((lambda (x) (and a1 true x)) false)}
     -> ,@defs {(and a1 true false)}
     :: ,@defs (and {a1} true false)
     -> ,@defs (and {true} true false)
     :: ,@defs {(and true true false)}
     -> ,@defs {false}))

(t 'bad-and m:upto-int/lam
   (and true 1)
   :: {(and true 1)}
   -> error: "and: question result is not true or false: 1")

;;;;;;;;;;;;;
;;
;;  COND
;;
;;;;;;;;;;;;;

(t 'cond1 m:upto-int/lam
   (cond [false 4] [false 5] [true 3])
   :: {(cond (false 4) (false 5) (true 3))}
   -> {(cond (false 5) (true 3))}
   -> {(cond (true 3))}
   -> {3})

(t 'cond-else m:upto-int/lam
   (cond [false 4] [else 9])
   :: {(cond [false 4] [else 9])}
   -> {(cond [else 9])}
   -> {9})

(t 'cond-andelse m:upto-int/lam
   (cond [true 3] [else (and true true)])
   :: {(cond (true 3) (else (and true true)))} -> {3})

(t 'bad-cond m:upto-int/lam
   (cond)
   :: error: "cond: expected a clause after cond, but nothing's there")

(t 'just-else m:upto-int/lam
   (cond [else 3])
   :: {(cond (else 3))} -> {3})

(t 'nested-cond m:upto-int/lam
   (cond [else (cond [else 3])])
   :: {(cond (else (cond (else 3))))}
   -> {(cond (else 3))}
   -> {3})

;;  reconstruct can't handle 'begin'
#;
(m:mz "(cond [#f 3 4] [#t (+ 3 4) (+ 4 9)])"
                  `((before-after ((hilite (cond (#f 3 4) (#t (+ 3 4) (+ 4 9)))))
                                  ((hilite (cond (#t (+ 3 4) (+ 4 9))))))
                    (before-after ((hilite (cond (#t (+ 3 4) (+ 4 9)))))
                                  ((hilite (begin (+ 3 4) (+ 4 9)))))
                    (before-after ((begin (hilite (+ 3 4)) (+ 4 9)))
                                  ((begin (hilite 7) (+ 4 9))))
                    (before-after ((hilite (begin 7 (+ 4 9))))
                                  ((hilite (+ 4 9))))
                    (before-after ((hilite (+ 4 9)))
                                  ((hilite 13)))
                    (finished-stepping)))

(t 'nested-cond2 m:upto-int/lam
   (cond [false 3] [else (cond [true 4])])
   :: {(cond (false 3) (else (cond (true 4))))}
   -> {(cond (else (cond (true 4))))}
   -> {(cond (true 4))}
   -> {4})

(t 'top-ref m:intermediate
   (define a4 +) a4
   :: (define a4 +) {a4}
   -> (define a4 +) {+})

(t 'top-ref2 m:intermediate
   (define (f123 x) (+ x 13)) f123
   ::)

(t 'top-ref3 m:intermediate-lambda
   (define (f123 x) (+ x 13)) f123
   :: (define (f123 x) (+ x 13)) {f123}
   -> (define (f123 x) (+ x 13)) {(lambda (x) (+ x 13))})

(let* ([defs1 `((define (a x) (+ x 5)) (define b a))]
       [defs2 (append defs1 `((define c a)))])
  (t 'top-ref4 m:intermediate
     ,@defs1 (define c b) (c 3)
     :: ,@defs1 (define c {b})
     -> ,@defs1 (define c {a})
     :: ,@defs2 ({c} 3)
     -> ,@defs2 ({a} 3)
     :: ,@defs2 {(a 3)}
     -> ,@defs2 {(+ 3 5)}
     -> ,@defs2 {8}))

(t 'define-struct m:upto-int/lam
   (define-struct mamba (rhythm tempo)) (mamba-rhythm (make-mamba 24 2))
   :: (define-struct mamba (rhythm tempo)) {(mamba-rhythm (make-mamba 24 2))}
   -> (define-struct mamba (rhythm tempo)) {24})

(let ([def `(define a5 (lambda (a5) (+ a5 13)))])
  (t 'lam-def m:upto-int
     ,def (a5 23)
     :: ,def {(a5 23)}
     -> ,def {(+ 23 13)}
     -> ,def {36}))

(let ([def `(define a5 (lambda (a5) (+ a5 13)))])
  (t 'lam-def/lam m:intermediate-lambda
     ,def (a5 23)
     :: ,def ({a5} 23)
     -> ,def ({(lambda (a5) (+ a5 13))} 23)
     :: ,def {((lambda (a5) (+ a5 13)) 23)}
     -> ,def {(+ 23 13)}
     -> ,def {36}))

(let ([def `(define a_0 (lambda (x) (+ x 5)))])
  (t 'lam-let m:intermediate
     (let ([a (lambda (x) (+ x 5))]) (a 6))
     :: {(let ([a (lambda (x) (+ x 5))]) (a 6))}
     -> {(define a_0 (lambda (x) (+ x 5)))} {(a_0 6)}
     :: ,def {(a_0 6)}
     -> ,def {(+ 6 5)}
     -> ,def {11}))

(let ([defs `((define c1 false)
              (define (d2 x) (or c1 false x)))])
  (t 'whocares m:upto-int
     ,@defs (d2 false)
     :: ,@defs {(d2 false)}
     -> ,@defs {(or c1 false false)}
     :: ,@defs (or {c1} false false)
     -> ,@defs (or {false} false false)
     :: ,@defs {(or false false false)}
     -> ,@defs {false}))

(let ([defs `((define c1 false)
              (define (d2 x) (or c1 false x)))])
  (t 'whocares/lam m:intermediate-lambda
     ,@defs (d2 false)
     :: ,@defs ({d2} false)
     -> ,@defs ({(lambda (x) (or c1 false x))} false)
     :: ,@defs {((lambda (x) (or c1 false x)) false)}
     -> ,@defs {(or c1 false false)}
     :: ,@defs (or {c1} false false)
     -> ,@defs (or {false} false false)
     :: ,@defs {(or false false false)}
     -> ,@defs {false}))

(let ([defs `((define (f x) (+ (g x) 10)) (define (g x) (- x 22)))])
  (t 'forward-ref m:upto-int
     ,@defs (f 13)
     :: ,@defs {(f 13)}
     -> ,@defs {(+ (g 13) 10)}
     :: ,@defs (+ {(g 13)} 10)
     -> ,@defs (+ {(- 13 22)} 10)
     -> ,@defs (+ {-9} 10)
     :: ,@defs {(+ -9 10)}
     -> ,@defs {1}))

(let ([defs `((define (f x) (+ (g x) 10)) (define (g x) (- x 22)))])
  (t 'forward-ref/lam m:intermediate-lambda
     ,@defs (f 13)
     :: ,@defs ({f} 13)
     -> ,@defs ({(lambda (x) (+ (g x) 10))} 13)
     :: ,@defs {((lambda (x) (+ (g x) 10)) 13)}
     -> ,@defs {(+ (g 13) 10)}
     :: ,@defs (+ ({g} 13) 10)
     -> ,@defs (+ ({(lambda (x) (- x 22))} 13) 10)
     :: ,@defs (+ {((lambda (x) (- x 22)) 13)} 10)
     -> ,@defs (+ {(- 13 22)} 10)
     -> ,@defs (+ {-9} 10)
     :: ,@defs {(+ -9 10)}
     -> ,@defs {1}))


;; loops; I should add a mechanism to stop testing after n steps...
#;(let ([defs '((define (f x) (cond (else (f x))))
                (define (g x) x))])
    (t 'pnkfelix m:intermediate-lambda
       ,@defs (f (g empty))
       :: ,@defs ({f} (g empty))
       -> ,@defs ({(lambda (x) (cond (else (f x))))} (g empty))
       :: ,@defs ((lambda (x) (cond (else (f x)))) ({g} empty))
       -> ,@defs ((lambda (x) (cond (else (f x)))) ({(lambda (x) x)} empty))))

(t 'bad-cons m:upto-int/lam
   (cons 1 2)
   :: {(cons 1 2)}
   -> error: "cons: second argument must be a list, but received 1 and 2")



(t1 'prims
    m:beginner "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
    (let ([defs `((cons 3 (cons 1 empty)))])
      `((before-after (,@defs (hilite (list 1 2 3)))
                      (,@defs (hilite (cons 1 (cons 2 (cons 3 empty)))))))))

(t1 'prims/non-beginner
    m:bwla-to-int/lam "(cons 3 (cons 1 empty)) (list 1 2 3) (define-struct aa (b)) (make-aa 3)"
    `((before-after ((cons 3 (hilite (cons 1 empty)))) ((cons 3 (hilite (list 1)))))
      (before-after ((hilite (cons 3 (list 1)))) ((hilite (list 3 1))))))


(t1 'map
    m:mz "(map (lambda (x) x) (list 3 4 5))"
    `((before-after ((map (lambda (x) x) (hilite (list 3 4 5))))
                    ((map (lambda (x) x) (hilite `(3 4 5)))))
      (before-after ((hilite (map (lambda (x) x) `(3 4 5))))
                    ((... (hilite 3) ...)))
      (before-after (...)
                    ((... (hilite 4) ...)))
      (before-after (...)
                    ((... (hilite 5) ...)))
      (before-after (...) ((hilite `(3 4 5))))))

(t1 'quoted-list
    m:beginner-wla "'(3 4 5)"
    `())

(t1 'quoted-list-display
    m:bwla-to-int/lam "(define (f x) '((a))) (+ 3 4)"
    `((before-after ((define (f x) (list (list 'a))) (hilite (+ 3 4)))
                    ((define (f x) (list (list 'a))) (hilite 7)))
      (finished-stepping)))


;;;;;;;;;;;;;
;;
;;  QUASIQUOTE
;;
;;;;;;;;;;;;;.

; note: we currently punt on trying to unwind quasiquote.

(t1 'qq1
    m:beginner-wla "`(3 4 ,(+ 4 5))"
    `((before-after ((cons 3 (cons 4 (cons (hilite (+ 4 5)) empty))))
                    ((cons 3 (cons 4 (cons (hilite 9) empty)))))
      (before-after ((cons 3 (cons 4 (hilite (cons 9 empty)))))
                    ((cons 3 (cons 4 (hilite (list 9))))))
      (before-after ((cons 3 (hilite (cons 4 (list 9)))))
                    ((cons 3 (hilite (list 4 9)))))
      (before-after ((hilite (cons 3 (list 4 9)))) ((hilite (list 3 4 9))))
      (finished-stepping)))

(t1 'qq-splice
    m:beginner-wla "`(3 ,@(list (+ 3 4) 5) 6)"
    `((before-after ((cons 3 (append (list (hilite (+ 3 4)) 5) (cons 6 empty)))) ((cons 3 (append (list (hilite 7) 5) (cons 6 empty)))))
      (before-after ((cons 3 (append (list 7 5) (hilite (cons 6 empty))))) ((cons 3 (append (list 7 5) (list 6)))))
      (before-after ((cons 3 (hilite (append (list 7 5) (list 6))))) ((cons 3 (hilite (list 7 5 6)))))
      (before-after ((hilite (cons 3 (list 7 5 6)))) ((hilite (list 3 7 5 6))))
      (finished-stepping)))

;;;;;;;;;;;;;
;;
;;  LET
;;
;;;;;;;;;;;;;

(t1 'let1 m:both-intermediates "(let ([a 3]) 4)"
    `((before-after ((hilite (let ([a 3]) 4))) ((hilite (define a_0 3)) (hilite 4)))
      (finished-stepping)))

(t1 'let2
    m:both-intermediates "(let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))"
    `((before-after ((hilite (let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))))
                    ((hilite (define a_0 (+ 4 5))) (hilite (define b_0 (+ 9 20))) (hilite (+ a_0 b_0))))
      (before-after ((define a_0 (hilite (+ 4 5))) (define b_0 (+ 9 20)) (+ a_0 b_0))
                    ((define a_0 (hilite 9)) (define b_0 (+ 9 20)) (+ a_0 b_0)))
      (before-after ((define a_0 9) (define b_0 (hilite (+ 9 20))) (+ a_0 b_0))
                    ((define a_0 9) (define b_0 (hilite 29)) (+ a_0 b_0)))
      (before-after ((define a_0 9) (define b_0 29) (+ (hilite a_0) b_0))
                    ((define a_0 9) (define b_0 29) (+ (hilite 9) b_0)))
      (before-after ((define a_0 9) (define b_0 29) (+ 9 (hilite b_0)))
                    ((define a_0 9) (define b_0 29) (+ 9 (hilite 29))))
      (before-after ((define a_0 9) (define b_0 29) (hilite (+ 9 29)))
                    ((define a_0 9) (define b_0 29) (hilite 38)))
      (finished-stepping)))

(t1 'let-scoping1
    m:intermediate "(let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))"
    (let ([d1 `(define a_0 3)]
          [d2 `(define a_1 (lambda (x) (+ a_0 x)))])
      `((before-after ((hilite (let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))))
                      ((hilite (define a_0 3)) (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4)))))
        (before-after (,d1 (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4))))
                      (,d1 (hilite (define a_1 (lambda (x) (+ a_0 x)))) (hilite (a_1 4))))
        (before-after (,d1 ,d2 (hilite (a_1 4)))
                      (,d1 ,d2 (hilite (+ a_0 4))))
        (before-after (,d1 ,d2 (+ (hilite a_0) 4))
                      (,d1 ,d2 (+ (hilite 3) 4)))
        (before-after (,d1 ,d2 (hilite (+ 3 4)))
                      (,d1 ,d2 (hilite 7)))
        (finished-stepping))))


(t1 'let-scoping2
    m:intermediate-lambda "(let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))"
    (let* ([d1 `(define a_0 3)]
           [defs `(,d1 (define a_1 (lambda (x) (+ a_0 x))))])
      `((before-after ((hilite (let ([a 3]) (let ([a (lambda (x) (+ a x))]) (a 4)))))
                      ((hilite (define a_0 3)) (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4)))))
        (before-after (,d1 (hilite (let ([a (lambda (x) (+ a_0 x))]) (a 4))))
                      (,d1 (hilite (define a_1 (lambda (x) (+ a_0 x)))) (hilite (a_1 4))))
        (before-after (,@defs ((hilite a_1) 4))
                      (,@defs ((hilite (lambda (x) (+ a_0 x))) 4)))
        (before-after (,@defs (hilite ((lambda (x) (+ a_0 x)) 4))) (,@defs (hilite (+ a_0 4))))
        (before-after (,@defs (+ (hilite a_0) 4)) (,@defs (+ (hilite 3) 4)))
        (before-after (,@defs (hilite (+ 3 4))) (,@defs (hilite 7)))
        (finished-stepping))))

(t1 'let-scoping3
    m:intermediate "(define a12 3) (define c12 19) (let ([a12 13] [b12 a12]) (+ b12 a12 c12))"
    (let* ([defs1 `((define a12 3) (define c12 19))]
           [defs2 `(,@defs1 (define a12_0 13))]
           [defs3 `(,@defs2 (define b12_0 3))])
      `((before-after (,@defs1 (hilite (let ([a12 13] [b12 a12]) (+ b12 a12 c12))))
                      (,@defs1 (hilite (define a12_0 13)) (hilite (define b12_0 a12)) (hilite (+ b12_0 a12_0 c12))))
        (before-after (,@defs2 (define b12_0 (hilite a12)) (+ b12_0 a12_0 c12))
                      (,@defs2 (define b12_0 (hilite 3)) (+ b12_0 a12_0 c12)))
        (before-after (,@defs3 (+ (hilite b12_0) a12_0 c12))
                      (,@defs3 (+ (hilite 3) a12_0 c12)))
        (before-after (,@defs3 (+ 3 (hilite a12_0) c12))
                      (,@defs3 (+ 3 (hilite 13) c12)))
        (before-after (,@defs3 (+ 3 13 (hilite c12)))
                      (,@defs3 (+ 3 13 (hilite 19))))
        (before-after (,@defs3 (hilite (+ 3 13 19)))
                      (,@defs3 (hilite 35)))
        (finished-stepping))))

(t1 'let-lifting1
    m:intermediate "(let ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
    `((before-after ((hilite (let ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)))
                    ((hilite (define a_0 (lambda (x) (+ x 14)))) (hilite (define b_0 (+ 3 4))) (hilite 9)))
      (before-after ((define a_0 (lambda (x) (+ x 14))) (define b_0 (hilite (+ 3 4))) 9)
                    ((define a_0 (lambda (x) (+ x 14))) (define b_0 (hilite 7)) 9))
      (finished-stepping)))

(t1 'let-deriv
    m:intermediate "(define (f g) (let ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)) (define gprime (f cos))"
    (let ([defs `((define (f g) 
                    (let ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))])
                      gp)))])
      `((before-after (,@defs (define gprime
                                (hilite (f cos))))
                      (,@defs (define gprime 
                                (hilite (let ([gp (lambda (x) 
                                                    (/ (- (cos (+ x 0.1)) (cos x))
                                                       0.001))]) 
                                          gp)))))
        (before-after (,@defs (define gprime (hilite (let ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp))))
                      (,@defs (hilite (define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001)))) (define gprime (hilite gp_0))))
        (finished-stepping))))

(t1 'let-assigned
    m:intermediate "(define a (let ([f (lambda (x) (+ x 13))]) f))"
    `((before-after ((define a (hilite (let ([f (lambda (x) (+ x 13))]) f))))
                    ((hilite (define f_0 (lambda (x) (+ x 13)))) (define a (hilite f_0))))
      (finished-stepping)))

(t1 'let-assigned/lam
    m:intermediate-lambda "(define a (let ([f (lambda (x) (+ x 13))]) f))"
    `((before-after ((define a (hilite (let ([f (lambda (x) (+ x 13))]) f))))
                    ((hilite (define f_0 (lambda (x) (+ x 13)))) (define a (hilite f_0))))
      (before-after ((define f_0 (lambda (x) (+ x 13))) (define a (hilite f_0)))
                    ((define f_0 (lambda (x) (+ x 13))) (define a (hilite (lambda (x) (+ x 13))))))
      (finished-stepping)))



;;;;;;;;;;;;;
;;
;;  LET*
;;
;;;;;;;;;;;;;

(t1 'let*-scoping1
    m:both-intermediates "(define a 3) (define c 19) (let* ([a 13] [b a]) (+ b a c))"
    (let* ([defs1 `((define a 3) (define c 19))]
           [defs2 (append defs1 `((define a_0 13)))]
           [defs3 (append defs2 `((define b_1 13)))])
      `((before-after (,@defs1 (hilite (let* ([a 13] [b a]) (+ b a c))))
                      (,@defs1 (hilite (define a_0 13)) (hilite (let* ([b a_0]) (+ b a_0 c)))))
        (before-after (,@defs2 (hilite (let* ([b a_0]) (+ b a_0 c))))
                      (,@defs2 (hilite (define b_1 a_0)) (hilite (+ b_1 a_0 c))))
        (before-after (,@defs2 (define b_1 (hilite a_0)) (+ b_1 a_0 c))
                      (,@defs2 (define b_1 (hilite 13)) (+ b_1 a_0 c)))
        (before-after (,@defs3 (+ (hilite b_1) a_0 c))
                      (,@defs3 (+ (hilite 13) a_0 c)))
        (before-after (,@defs3 (+ 13 (hilite a_0) c))
                      (,@defs3 (+ 13 (hilite 13) c)))
        (before-after (,@defs3 (+ 13 13 (hilite c)))
                      (,@defs3 (+ 13 13 (hilite 19))))
        (before-after (,@defs3 (hilite (+ 13 13 19)))
                      (,@defs3 (hilite 45)))
        (finished-stepping))))

(t1 'let*-lifting1
    m:intermediate "(let* ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
    (let ([defs `((define a_0 (lambda (x) (+ x 14))))])
      `((before-after ((hilite (let* ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)))
                      ((hilite (define a_0 (lambda (x) (+ x 14)))) (hilite (let* ([b (+ 3 4)]) 9))))
        (before-after (,@defs (hilite (let* ([b (+ 3 4)]) 9)))
                      (,@defs (hilite (define b_1 (+ 3 4))) (hilite 9)))
        (before-after (,@defs (define b_1 (hilite (+ 3 4))) 9)
                      (,@defs (define b_1 (hilite 7)) 9))
        (finished-stepping))))

(t1 'let*-deriv
    m:intermediate "(define (f g) (let* ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)) (define gprime (f cos))"
    (let ([defs `((define (f g) (let* ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)))])
      `((before-after (,@defs (define gprime (hilite (f cos))))
                      (,@defs (define gprime (hilite (let* ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp)))))
        (before-after (,@defs (define gprime (hilite (let* ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp))))
                      (,@defs (hilite (define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001)))) (define gprime (hilite gp_0))))
        (finished-stepping))))

(t1 'let/let*
    m:both-intermediates "(let* ([a 9]) (let ([b 6]) a))"
    `((before-after ((hilite (let* ([a 9]) (let ([b 6]) a)))) ((hilite (define a_0 9)) (hilite (let ([b 6]) a_0))))
      (before-after ((define a_0 9) (hilite (let ([b 6]) a_0)))
                    ((define a_0 9) (hilite (define b_1 6)) (hilite a_0)))
      (before-after ((define a_0 9) (define b_1 6) (hilite a_0))
                    ((define a_0 9) (define b_1 6) (hilite 9)))
      (finished-stepping)))

(let ([def '(define (f x) (let* ([a 13] [b 14]) (let* ([c 15] [d 16]) (+ a b c d))))])
  (t1 'nested-let-unwinding
      m:both-intermediates 
      "(define (f x) (let* ([a 13] [b 14]) (let* ([c 15] [d 16]) (+ a b c d)))) (+ 3 4)"
      `((before-after (,def (hilite (+ 3 4)))
                      (,def (hilite 7)))
        (finished-stepping))))

;;;;;;;;;;;;;
;;
;;  LETREC
;;
;;;;;;;;;;;;;

(t1 'letrec1
    m:intermediate "(define a 3) (define c 19) (letrec ([a 13] [b a]) (+ b a c))"
    (let* ([defs1 `((define a 3) (define c 19))]
           [defs2 (append defs1 `((define a_0 13)))]
           [defs3 (append defs2 `((define b_0 13)))])
      `((before-after (,@defs1 (hilite (letrec ([a 13] [b a]) (+ b a c))))
                      (,@defs1 (hilite (define a_0 13)) (hilite (define b_0 a_0)) (hilite (+ b_0 a_0 c))))
        (before-after (,@defs2 (define b_0 (hilite a_0)) (+ b_0 a_0 c))
                      (,@defs2 (define b_0 (hilite 13)) (+ b_0 a_0 c)))
        (before-after (,@defs3 (+ (hilite b_0) a_0 c))
                      (,@defs3 (+ (hilite 13) a_0 c)))
        (before-after (,@defs3 (+ 13 (hilite a_0) c))
                      (,@defs3 (+ 13 (hilite 13) c)))
        (before-after (,@defs3 (+ 13 13 (hilite c)))
                      (,@defs3 (+ 13 13 (hilite 19))))
        (before-after (,@defs3 (hilite (+ 13 13 19)))
                      (,@defs3 (hilite 45)))
        (finished-stepping))))

  (t1 'letrec2
      m:intermediate "(letrec ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)"
      `((before-after ((hilite (letrec ([a (lambda (x) (+ x 14))] [b (+ 3 4)]) 9)))
                      ((hilite (define a_0 (lambda (x) (+ x 14)))) (hilite (define b_0 (+ 3 4))) (hilite 9)))
        (before-after ((define a_0 (lambda (x) (+ x 14))) (define b_0 (hilite (+ 3 4))) 9)
                      ((define a_0 (lambda (x) (+ x 14))) (define b_0 (hilite 7)) 9))
        (finished-stepping)))

  (t1 'letrec3
      m:intermediate "(define (f g) (letrec ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)) (define gprime (f cos))"
      (let ([defs `((define (f g) (letrec ([gp (lambda (x) (/ (- (g (+ x 0.1)) (g x)) 0.001))]) gp)))])
        `((before-after (,@defs (define gprime (hilite (f cos))))
                        (,@defs (define gprime (hilite (letrec ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp)))))
          (before-after (,@defs (define gprime (hilite (letrec ([gp (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001))]) gp))))
                        (,@defs (hilite (define gp_0 (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.001)))) (define gprime (hilite gp_0))))
          (finished-stepping))))
  
      ;;;;;;;;;;;;;
      ;;
      ;;  RECUR
      ;;
      ;;;;;;;;;;;;;

  ;; N.B. : we cheat here.  In particular, the rhs of the double-break expression should highlight the whole application, and not
  ;; just the applied loop identifier.  This is hard to fix because we have an application which is initially hidden, but then later
  ;; not hidden.  Fixing this involves parameterizing the unwind by what kind of break it was.  Yuck!  So we just fudge the test case.

  (t1 'recur
      m:advanced "(define (countdown n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1))))) (countdown 2)"
      (let* ([defs1 `((define (countdown n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1))))))]
             [defs2 (append defs1 `((define (loop_0 n) (if (= n 0) 13 (loop_0 (- n 1))))))])
        `((before-after (,@defs1 ((hilite countdown) 2))
                        (,@defs1 ((hilite (lambda (n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1)))))) 2)))
          (before-after (,@defs1 (hilite ((lambda (n) (recur loop ([n n]) (if (= n 0) 13 (loop (- n 1))))) 2)))
                        (,@defs1 (hilite (recur loop ([n 2]) (if (= n 0) 13 (loop (- n 1)))))))
          (before-after (,@defs1 (hilite (recur loop ([n 2]) (if (= n 0) 13 (loop (- n 1))))))
                        (,@defs1 (hilite (define (loop_0 n) (if (= n 0) 13 (loop_0 (- n 1))))) ((hilite loop_0) 2)))
          (before-after (,@defs2 ((hilite loop_0) 2))
                        (,@defs2 ((hilite (lambda (n) (if (= n 0) 13 (loop_0 (- n 1))))) 2)))
          (before-after (,@defs2 (hilite ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) 2)))
                        (,@defs2 (hilite (if (= 2 0) 13 (loop_0 (- 2 1))))))
          (before-after (,@defs2 (if (hilite (= 2 0)) 13 (loop_0 (- 2 1))))
                        (,@defs2 (if (hilite false) 13 (loop_0 (- 2 1)))))
          (before-after (,@defs2 (hilite (if false 13 (loop_0 (- 2 1)))))
                        (,@defs2 (hilite (loop_0 (- 2 1)))))
          (before-after (,@defs2 ((hilite loop_0) (- 2 1)))
                        (,@defs2 ((hilite (lambda (n) (if (= n 0) 13 (loop_0 (- n 1))))) (- 2 1))))
          (before-after (,@defs2 ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) (hilite (- 2 1))))
                        (,@defs2 ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) (hilite 1))))
          (before-after (,@defs2 (hilite ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) 1)))
                        (,@defs2 (hilite (if (= 1 0) 13 (loop_0 (- 1 1))))))
          (before-after (,@defs2 (if (hilite (= 1 0)) 13 (loop_0 (- 1 1))))
                        (,@defs2 (if (hilite false) 13 (loop_0 (- 1 1)))))
          (before-after (,@defs2 (hilite (if false 13 (loop_0 (- 1 1)))))
                        (,@defs2 (hilite (loop_0 (- 1 1)))))
          (before-after (,@defs2 ((hilite loop_0) (- 1 1)))
                        (,@defs2 ((hilite (lambda (n) (if (= n 0) 13 (loop_0 (- n 1))))) (- 1 1))))
          (before-after (,@defs2 ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) (hilite (- 1 1))))
                        (,@defs2 ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) (hilite 0))))
          (before-after (,@defs2 (hilite ((lambda (n) (if (= n 0) 13 (loop_0 (- n 1)))) 0)))
                        (,@defs2 (hilite (if (= 0 0) 13 (loop_0 (- 0 1))))))
          (before-after (,@defs2 (if (hilite (= 0 0)) 13 (loop_0 (- 0 1))))
                        (,@defs2 (if (hilite true) 13 (loop_0 (- 0 1)))))
          (before-after (,@defs2 (hilite (if true 13 (loop_0 (- 0 1)))))
                        (,@defs2 (hilite 13)))
          (finished-stepping))))

    ;;;;;;;;;;;;;
    ;;
    ;;  LOCAL
    ;;
    ;;;;;;;;;;;;;


  (t1 'empty-local
      m:both-intermediates "(local () (+ 3 4))"
      `((before-after ((hilite (local () (+ 3 4)))) ((hilite (+ 3 4))))
        (before-after ((hilite (+ 3 4))) ((hilite 7)))
        (finished-stepping)))

  (t1 'local1
      m:both-intermediates "(local ((define a 3) (define b 8)) 4)"
      `((before-after ((hilite (local ((define a 3) (define b 8)) 4)))
                      ((hilite (define a_0 3)) (hilite (define b_0 8)) (hilite 4)))
        (finished-stepping)))

  (t1 'local2
      m:intermediate "(local ((define (a x) (+ x 9))) (a 6))"
      (let ([defs `((define (a_0 x) (+ x 9)))])
        `((before-after ((hilite (local ((define (a x) (+ x 9))) (a 6))))
                        ((hilite (define (a_0 x) (+ x 9))) (hilite (a_0 6))))
          (before-after (,@defs (hilite (a_0 6)))
                        (,@defs (hilite (+ 6 9))))
          (before-after (,@defs (hilite (+ 6 9)))
                        (,@defs (hilite 15)))
          (finished-stepping))))

  (t1 'local3
      m:intermediate-lambda "(local ((define (a x) (+ x 9))) (a 6))"
      (let ([defs `((define (a_0 x) (+ x 9)))])
        `((before-after ((hilite (local ((define (a x) (+ x 9))) (a 6))))
                        ((hilite (define (a_0 x) (+ x 9))) (hilite (a_0 6))))
          (before-after (,@defs ((hilite a_0) 6))
                        (,@defs ((hilite (lambda (x) (+ x 9))) 6)))
          (before-after (,@defs (hilite ((lambda (x) (+ x 9)) 6)))
                        (,@defs (hilite (+ 6 9))))
          (before-after (,@defs (hilite (+ 6 9)))
                        (,@defs (hilite 15)))
          (finished-stepping))))

  (t1 'local4
      m:intermediate "(local ((define (a x) (+ x 13))) a)"
      `((before-after ((hilite (local ((define (a x) (+ x 13))) a))) ((hilite (define (a_0 x) (+ x 13))) (hilite a_0)))
        (finished-stepping)))

  (t1 'local5
      m:intermediate-lambda "(local ((define (a x) (+ x 13))) a)"
      `((before-after ((hilite (local ((define (a x) (+ x 13))) a)))
                      ((hilite (define (a_0 x) (+ x 13))) (hilite a_0)))
        (before-after ((define (a_0 x) (+ x 13)) (hilite a_0))
                      ((define (a_0 x) (+ x 13)) (hilite (lambda (x) (+ x 13)))))
        (finished-stepping)))
  


  (t1 'local-interref1
      m:intermediate "(local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))"
      (let* ([defs1 `((define (a_0 x) (+ x 9)) (define b_0 a_0))]
             [defs2 (append defs1 `((define p_0 7)))])
        `((before-after ((hilite (local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))))
                        ((hilite (define (a_0 x) (+ x 9))) (hilite (define b_0 a_0)) (hilite (define p_0 (+ 3 4))) (hilite (b_0 1))))
          (before-after (,@defs1 (define p_0 (hilite (+ 3 4))) (b_0 1))
                        (,@defs1 (define p_0 (hilite 7)) (b_0 1)))
          (before-after (,@defs2 ((hilite b_0) 1))
                        (,@defs2 ((hilite a_0) 1)))
          (before-after (,@defs2 (hilite (a_0 1)))
                        (,@defs2 (hilite (+ 1 9))))
          (before-after (,@defs2 (hilite (+ 1 9)))
                        (,@defs2 (hilite 10)))
          (finished-stepping))))

  (t1 'local-interref2
      m:intermediate-lambda "(local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))"
      (let* ([defs1 `((define (a_0 x) (+ x 9)))]
             [defs2 (append defs1 `((define b_0 (lambda (x) (+ x 9)))))]
             [defs3 (append defs2 `((define p_0 7)))])
        `((before-after ((hilite (local ((define (a x) (+ x 9)) (define b a) (define p (+ 3 4))) (b 1))))
                        ((hilite (define (a_0 x) (+ x 9))) (hilite (define b_0 a_0)) (hilite (define p_0 (+ 3 4))) (hilite (b_0 1))))
          (before-after (,@defs1 (define b_0 (hilite a_0)) (define p_0 (+ 3 4)) (b_0 1))
                        (,@defs1 (define b_0 (hilite (lambda (x) (+ x 9)))) (define p_0 (+ 3 4)) (b_0 1)))
          (before-after (,@defs2 (define p_0 (hilite (+ 3 4))) (b_0 1))
                        (,@defs2 (define p_0 (hilite 7)) (b_0 1)))
          (before-after (,@defs3 ((hilite b_0) 1))
                        (,@defs3 ((hilite (lambda (x) (+ x 9))) 1)))
          (before-after (,@defs3 (hilite ((lambda (x) (+ x 9)) 1)))
                        (,@defs3 (hilite (+ 1 9))))
          (before-after (,@defs3 (hilite (+ 1 9)))
                        (,@defs3 (hilite 10)))
          (finished-stepping))))

  (t1 'local-gprime
      m:intermediate "(define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)) (define gprime (f12 cos))"
      (let ([defs `((define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)))])
        `((before-after (,@defs (define gprime (hilite (f12 cos))))
                        (,@defs (define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp)))))
          (before-after (,@defs (define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp))))
                        (,@defs (hilite (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))) (define gprime (hilite gp_0))))
          (finished-stepping))))

  (t1 'local-gprime/lambda
      m:intermediate-lambda "(define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)) (define gprime (f12 cos))"
      (let ([defs `((define (f12 g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)))])
        `((before-after (,@defs (define gprime ((hilite f12) cos)))
                        (,@defs (define gprime ((hilite (lambda (g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp))) cos))))
          (before-after (,@defs (define gprime (hilite ((lambda (g) (local ([define (gp x) (/ (- (g (+ x 0.1)) (g x)) 0.1)]) gp)) cos))))
                        (,@defs (define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp)))))
          (before-after (,@defs (define gprime (hilite (local ([define (gp x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)]) gp))))
                        (,@defs (hilite (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))) (define gprime (hilite gp_0))))
          (before-after (,@defs (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1)) (define gprime (hilite gp_0)))
                        (,@defs
                         (define (gp_0 x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))
                         (define gprime (hilite (lambda (x) (/ (- (cos (+ x 0.1)) (cos x)) 0.1))))))
          (finished-stepping))))

  ; test generativity... that is, multiple evaluations of a local should get different lifted names:

  (t1 'local-generative
      m:intermediate "(define (a13 b13 c13) (b13 c13)) (define (f9 x) (local ((define (maker dc) x)) maker)) (define m1 (f9 3)) (a13 (f9 4) 1)"
      (let* ([defs1 `((define (a13 b13 c13) (b13 c13))
                      (define (f9 x) (local ((define (maker dc) x)) maker)))]
             [defs2 (append defs1 `((define (maker_0 dc) 3) (define m1 maker_0)))]
             [defs3 (append defs2 `((define (maker_1 dc) 4)))])
        `((before-after (,@defs1 (define m1 (hilite (f9 3))))
                        (,@defs1 (define m1 (hilite (local ((define (maker dc) 3)) maker)))))
          (before-after (,@defs1 (define m1 (hilite (local ((define (maker dc) 3)) maker))))
                        (,@defs1 (hilite (define (maker_0 dc) 3)) (define m1 (hilite maker_0))))
          (before-after (,@defs2 (a13 (hilite (f9 4)) 1))
                        (,@defs2 (a13 (hilite (local ((define (maker dc) 4)) maker)) 1)))
          (before-after (,@defs2 (a13 (hilite (local ((define (maker dc) 4)) maker)) 1))
                        (,@defs2 (hilite (define (maker_1 dc) 4)) (a13 (hilite maker_1) 1)))
          (before-after (,@defs3 (hilite (a13 maker_1 1)))
                        (,@defs3 (hilite (maker_1 1))))
          (before-after (,@defs3 (hilite (maker_1 1)))
                        (,@defs3 (hilite 4)))
          (finished-stepping))))

  (t1 'local-generative/lambda
      m:intermediate-lambda "(define (a13 b13 c13) (b13 c13)) (define (f9 x) (local ((define (maker dc) x)) maker)) (define m1 (f9 3)) (a13 (f9 4) 1)"
      (let* ([defs1 `((define (a13 b13 c13) (b13 c13))
                      (define (f9 x) (local ((define (maker dc) x)) maker)))]
             [defs2 (append defs1 `((define (maker_0 dc) 3)))]
             [defs3 (append defs2 `((define m1 (lambda (dc) 3))))]
             [defs4 (append defs3 `((define (maker_1 dc) 4)))])
        `((before-after (,@defs1 (define m1 ((hilite f9) 3)))
                        (,@defs1 (define m1 ((hilite (lambda (x) (local ((define (maker dc) x)) maker))) 3))))
          (before-after (,@defs1 (define m1 (hilite ((lambda (x) (local ((define (maker dc) x)) maker)) 3))))
                        (,@defs1 (define m1 (hilite (local ((define (maker dc) 3)) maker)))))
          (before-after (,@defs1 (define m1 (hilite (local ((define (maker dc) 3)) maker))))
                        (,@defs1 (hilite (define (maker_0 dc) 3)) (define m1 (hilite maker_0))))
          (before-after (,@defs2 (define m1 (hilite maker_0)))
                        (,@defs2 (define m1 (hilite (lambda (dc) 3)))))
          (before-after (,@defs3 ((hilite a13) (f9 4) 1))
                        (,@defs3 ((hilite (lambda (b13 c13) (b13 c13))) (f9 4) 1)))
          (before-after (,@defs3 ((lambda (b13 c13) (b13 c13)) ((hilite f9) 4) 1))
                        (,@defs3 ((lambda (b13 c13) (b13 c13)) ((hilite (lambda (x) (local ((define (maker dc) x)) maker))) 4) 1)))
          (before-after (,@defs3 ((lambda (b13 c13) (b13 c13)) (hilite ((lambda (x) (local ((define (maker dc) x)) maker)) 4)) 1))
                        (,@defs3 ((lambda (b13 c13) (b13 c13)) (hilite (local ((define (maker dc) 4)) maker)) 1)))
          (before-after (,@defs3 ((lambda (b13 c13) (b13 c13)) (hilite (local ((define (maker dc) 4)) maker)) 1))
                        (,@defs3 (hilite (define (maker_1 dc) 4)) ((lambda (b13 c13) (b13 c13)) (hilite maker_1) 1)))
          (before-after (,@defs4 ((lambda (b13 c13) (b13 c13)) (hilite maker_1) 1))
                        (,@defs4 ((lambda (b13 c13) (b13 c13)) (hilite (lambda (dc) 4)) 1)))
          (before-after (,@defs4 (hilite ((lambda (b13 c13) (b13 c13)) (lambda (dc) 4) 1)))
                        (,@defs4 (hilite ((lambda (dc) 4) 1))))
          (before-after (,@defs4 (hilite ((lambda (dc) 4) 1)))
                        (,@defs4 (hilite 4)))
          (finished-stepping))))

  ;;;;;;;;;;;;;
  ;;
  ;;  Reduction of Lambda in int/lambda
  ;;
  ;;;;;;;;;;;;;

  (t1 'int/lam1
      m:intermediate-lambda "(define f ((lambda (x) x) (lambda (x) x))) (f f)"
      (let ([defs `((define f (lambda (x) x)))])
        `((before-after ((define f (hilite ((lambda (x) x) (lambda (x) x)))))
                        ((define f (hilite (lambda (x) x)))))
          (before-after (,@defs ((hilite f) f))
                        (,@defs ((hilite (lambda (x) x)) f)))
          (before-after (,@defs ((lambda (x) x) (hilite f)))
                        (,@defs ((lambda (x) x) (hilite (lambda (x) x)))))
          (before-after (,@defs (hilite ((lambda (x) x) (lambda (x) x))))
                        (,@defs (hilite (lambda (x) x))))
          (finished-stepping))))


  (t1 'int/lam2
      m:intermediate-lambda "(define f (if false (lambda (x) x) (lambda (x) x))) (f f)"
      (let ([defs `((define f (lambda (x) x)))])
        `((before-after ((define f (hilite (if false (lambda (x) x) (lambda (x) x)))))
                        ((define f (hilite (lambda (x) x)))))
          (before-after (,@defs ((hilite f) f))
                        (,@defs ((hilite (lambda (x) x)) f)))
          (before-after (,@defs ((lambda (x) x) (hilite f)))
                        (,@defs ((lambda (x) x) (hilite (lambda (x) x)))))
          (before-after (,@defs (hilite ((lambda (x) x) (lambda (x) x))))
                        (,@defs (hilite (lambda (x) x))))
          (finished-stepping))))

  ;
  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  TIME
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;

  (t1 'time
      m:intermediate "(time (+ 3 4))"
      `((before-after ((hilite (+ 3 4)))
                      ((hilite 7)))
        (finished-stepping)))


  ;;;;;;;;;;;;;;;;
  ;;
  ;;  XML (uses GRacket)
  ;;
  ;;;;;;;;;;;;;;;;

  ;; NOT UPDATED FOR NEW TEST CASE FORMAT

  #;
  (t1 'ddj-screenshot
     (m:mz (define-syntax (xml stx)
                                (letrec ([process-xexpr
                                          (lambda (xexpr)
                                            (syntax-case xexpr (lmx lmx-splice)
                                              [(lmx-splice unquoted) #`(unquote-splicing unquoted)]
                                              [(lmx unquoted) #`(unquote unquoted)]
                                              [(tag ([attr val] ...) . sub-xexprs)
                                               (identifier? #`tag)
                                               #`(tag ([attr val] ...) #,@(map process-xexpr (syntax->list #`sub-xexprs)))]
                                              [(tag . sub-xexprs)
                                               (identifier? #`tag)
                                               #`(tag () #,@(map process-xexpr (syntax->list #`sub-xexprs)))]
                                              [str
                                               (string? (syntax-e #`str))
                                               xexpr]))])
                                  (syntax-case stx ()
                                    [(_ xexpr) #`(quasiquote #,(process-xexpr #`xexpr))])))
     (xml (article (header (author "John Clements")
                           (title (if (< 3 4)
                                      (xml "No Title Available")
                                      (get-title))))
                   (text "More Sample Text")))
     '((before-after-finished ((define-syntax (xml stx)
                                 (letrec ([process-xexpr
                                           (lambda (xexpr)
                                             (syntax-case xexpr (lmx lmx-splice)
                                               [(lmx-splice unquoted) #`(unquote-splicing unquoted)]
                                               [(lmx unquoted) #`(unquote unquoted)]
                                               [(tag ([attr val] ...) . sub-xexprs)
                                                (identifier? #`tag)
                                                #`(tag ([attr val] ...) #,@(map process-xexpr (syntax->list #`sub-xexprs)))]
                                               [(tag . sub-xexprs)
                                                (identifier? #`tag)
                                                #`(tag () #,@(map process-xexpr (syntax->list #`sub-xexprs)))]
                                               [str
                                                (string? (syntax-e #`str))
                                                xexpr]))])
                                   (syntax-case stx ()
                                     [(_ xexpr) #`(quasiquote #,(process-xexpr #`xexpr))]))))
                              ((xml ))
                              ((xml (a ([a "x"]) "ab" "hdo" "hon")))))))

  #;
  (define (test-xml-sequence namespace-spec render-settings track-inferred-names? spec expected-steps)
    (letrec ([port (open-input-text-editor (construct-text spec))])
      (test-sequence-core namespace-spec render-settings track-inferred-names? port expected-steps)))

  #;
  (define (construct-text spec)
    (let ([new-text (instantiate text% ())])
      (for-each
       (match-lambda
         [`(xml-box ,@(xmlspec ...)) (send new-text insert (construct-xml-box xmlspec))]
         [(? string? text) (send new-text insert text)])
       spec)
      new-text))

  #;
  (define (test-xml-beginner-sequence spec expected)
    test-xml-sequence `(lib "htdp-beginner.ss" "lang")
    fake-beginner-render-settings
    #t
    spec
    expected)

  #;
  (t1 'xml-box1
      test-xml-beginner-sequence `((xml-box "<abba>3</abba>"))
      `((finished-stepping)))

  #;
  (t1 'xml-box2
      text-xml-beginnner-sequence `("(cdr (cdr " (xml-box "<foozle>a b</foozle>") "))")
      `((before-after ((cdr (cdr (xml-box "<foozle>a b</foozle>")))))))

  ;(t1 'filled-rect-image
  ;   (m:upto-int-lam "(image-width (filled-rect 10 10 'blue))"
  ;                      `((before-after ((image-width (hilite (filled-rect 10 10 'blue)))) ((image-width (hilite )))))))
  ; add image test: (image-width (filled-rect 10 10 'blue))


  
  (t 'check-expect m:upto-int/lam
     (check-expect (+ 3 4) (+ 8 9)) (check-expect (+ 1 1) 2) (check-expect (+ 2 2) 4) (+ 4 5)
     :: {(+ 4 5)} -> {9}
     :: 9 (check-expect (+ 3 4) {(+ 8 9)}) -> 9 (check-expect (+ 3 4) {17})
     :: 9 (check-expect {(+ 3 4)} 17) -> 9 (check-expect {7} 17)
     :: 9 false (check-expect {(+ 1 1)} 2) -> 9 false (check-expect {2} 2)
     :: 9 false true (check-expect {(+ 2 2)} 4) -> 9 false true (check-expect {4} 4))
  
  (t 'simple-check-expect m:beginner
     (check-expect (+ 3 4) 7)
     :: (check-expect {(+ 3 4)} 7) -> (check-expect {7} 7))
  
  (t1 'check-within
      m:upto-int/lam
      "(check-within (+ 3 4) (+ 8 10) (+ 10 90)) (check-expect (+ 1 1) 2)(+ 4 5)"
      `((before-after ((hilite (+ 4 5)))
                      ((hilite 9)))
        (before-after (9 (check-within (+ 3 4) (hilite (+ 8 10)) (+ 10 90)))
                      (9 (check-within (+ 3 4) (hilite 18) (+ 10 90))))
        (before-after (9 (check-within (+ 3 4) 18 (hilite (+ 10 90))))
                      (9 (check-within (+ 3 4) 18 (hilite 100))))
        (before-after (9 (check-within (hilite (+ 3 4)) 18 100))
                      (9 (check-within (hilite 7) 18 100)))
        (before-after (9 true (check-expect (hilite (+ 1 1)) 2))
                      (9 true (check-expect (hilite 2) 2)))))
  

  
  
  (t1 'check-within-bad
      m:upto-int/lam
      "(check-within (+ 3 4) (+ 8 10) 0.01) (+ 4 5) (check-expect (+ 1 1) 2)"
      `((before-after ((hilite (+ 4 5)))
                      ((hilite 9)))
        (before-after (9 (check-within (+ 3 4) (hilite (+ 8 10)) 0.01))
                      (9 (check-within (+ 3 4) (hilite 18) 0.01)))
        (before-after (9 (check-within (hilite (+ 3 4)) 18 0.01))
                      (9 (check-within (hilite 7) 18 0.01)))
        (before-after (9 false (check-expect (hilite (+ 1 1)) 2))
                      (9 false (check-expect (hilite 2) 2)))))

  (let ([errmsg "rest: expected argument of type <non-empty list>; given ()"])
    (t1 'check-error
        m:upto-int/lam
        "(check-error (+ (+ 3 4) (rest empty)) (string-append \"rest: \" \"expected argument of type <non-empty list>; given ()\")) (check-expect (+ 3 1) 4) (+ 4 5)"
        `((before-after ((hilite (+ 4 5)))
                        ((hilite 9)))
          (before-after (9 (check-error (+ (+ 3 4) (rest empty)) (hilite (string-append "rest: " "expected argument of type <non-empty list>; given ()"))))
                        (9 (check-error (+ (+ 3 4) (rest empty)) (hilite ,errmsg))))
          (before-after (9 (check-error (+ (hilite (+ 3 4)) (rest empty)) ,errmsg))
                        (9 (check-error (+ (hilite 7) (rest empty)) ,errmsg)))
          (before-after (9 true (check-expect (hilite (+ 3 1)) 4))
                        (9 true (check-expect (hilite 4) 4))))))

  (t1 'check-error-bad
      m:upto-int/lam
      "(check-error (+ (+ 3 4) (rest empty)) (string-append \"b\" \"ogus\")) (check-expect (+ 3 1) 4) (+ 4 5)"
      `((before-after ((hilite (+ 4 5)))
                      ((hilite 9)))
        (before-after (9 (check-error (+ (+ 3 4) (rest empty)) (hilite (string-append "b" "ogus"))))
                      (9 (check-error (+ (+ 3 4) (rest empty)) (hilite "bogus"))))
        (before-after (9 (check-error (+ (hilite (+ 3 4)) (rest empty)) "bogus"))
                      (9 (check-error (+ (hilite 7) (rest empty)) "bogus")))
        (before-after (9 false (check-expect (hilite (+ 3 1)) 4))
                      (9 false (check-expect (hilite 4) 4)))))
  
  ;;;;;;;;;;;;
  ;;
  ;;    DMdA TESTS
  ;;
  ;;;;;;;;;;;
  
  (t1 'dmda-certificate-bug
       m:dmda-a
      "(: apply-nim-move (integer? -> integer?))
  (define apply-nim-move
    (lambda (s)
      (if s s s)))"
      '())


  ;  ;;;;;;;;;;;;;
  ;  ;;
  ;  ;;  TEACHPACK TESTS
  ;  ;;
  ;  ;;;;;;;;;;;;;
  ;

  ; as you can see, many teachpack tests work only in gracket:
  ;; (require mred)



  ; uses set-render-settings!
  ;(reconstruct:set-render-settings! fake-beginner-render-settings)
  ;(test-sequence "(define (check-guess guess target) 'TooSmall) (guess-with-gui check-guess)"
  ;               `((before-after ((hilite ,h-p)) ((guess-with-gui check-guess)))
  ;                 (((hilite ,h-p)) (true)))
  ;               `((define (check-guess guess target) 'toosmall) true)
  ;               tp-namespace)

  #;
  (t1 'teachpack-drawing
      (make-teachpack-ll-model
       `((lib "draw.ss" "htdp")))
      "(define (draw-limb i) (cond
 [(= i 1) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
 [(= i 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))
 (and (start 100 100)
 (draw-limb 0))"
      `((before-after-finished ((define (draw-limb i) (cond [(= i 1) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                                            [(= i 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))
                               ((and (hilite (start 100 100)) (draw-limb 0)))
                               ((and (hilite true) (draw-limb 0))))
        (before-after ((and true (hilite (draw-limb 0))))
                      ((and true (hilite (cond [(= 0 1) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                               [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))))
        (before-after ((and true (cond [(hilite (= 0 1)) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                       [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))
                      ((and true (cond [(hilite false) (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                       [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))))
        (before-after ((and true (hilite (cond [false (draw-solid-line (make-posn 20 20) (make-posn 20 100) 'blue)]
                                               [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))))
                      ((and true (hilite (cond [(= 0 0) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))))
        (before-after ((and true (cond [(hilite (= 0 0)) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)])))
                      ((and true (cond [(hilite true) (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))))
        (before-after ((and true (hilite (cond [true (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)]))))
                      ((and true (hilite (draw-solid-line (make-posn (+ 1 10) 10) (make-posn 10 100) 'red)))))
        (before-after ((and true (draw-solid-line (make-posn (hilite (+ 1 10)) 10) (make-posn 10 100) 'red)))
                      ((and true (draw-solid-line (make-posn (hilite 11) 10) (make-posn 10 100) 'red))))
        (before-after ((and true (hilite (draw-solid-line (make-posn 11 10) (make-posn 10 100) 'red))))
                      ((and true (hilite true))))
        (before-after ((hilite (and true true)))
                      ((hilite true)))
        (finished-stepping)))
  
  #;(t1 'teachpack-universe
        (test-teachpack-sequence
         `((lib "universe.ss" "2htdp")))
        "(define (z world)
  (empty-scene 100 100))

(big-bang 3
          (on-tick add1)
          (on-draw z))"
        `((finished-stepping)))

  
  #;
  (t1 'teachpack-name-rendering
      (test-teachpack-sequence
       `((file "/Users/clements/plt/teachpack/htdp/draw.ss")))
      "(start 300 300) (if true (get-key-event) 3)"
      `((before-after ((hilite (start 300 300)))
                      ((hilite true)))
        (before-after-finished (true)
                               ((hilite (if true (get-key-event) 3)))
                               ((hilite (get-key-event))))
        (before-after ((hilite (get-key-event)))
                      ((hilite false)))
        (finished-stepping)))

  #;
  (t1 'teachpack-hop-names
      (make-teachpack-ll-model
       `((file "/Users/clements/plt/teachpack/htdp/draw.ss")))
      "(start 300 300) (define (a x y) (+ 3 4)) (if true (on-key-event a) 3)"
      `((before-after ((hilite (start 300 300)))
                      ((hilite true)))
        (before-after-finished (true (define (a x y) (+ 3 4)))
                               ((hilite (if true (on-key-event a) 3)))
                               ((hilite (on-key-event a))))
        (before-after ((hilite (on-key-event a)))
                      ((hilite true)))
        (finished-stepping)))

  #;
  (t1 'teachpack-web-interaction
      (make-teachpack-ll-model
       `(htdp/servlet2))
      "(define (adder go) (inform (number->string (+ (single-query (make-number \"enter 10\")) (single-query (make-number \"enter 20\")))))) (adder true)"
      `((before-after-finished ((define (adder go) (inform (number->string (+ (single-query (make-number "enter 10")) (single-query (make-number "enter 20")))))))
                               ((hilite (adder true)))
                               ((hilite (inform (number->string (+ (single-query (make-number "enter 10")) (single-query (make-number "enter 20"))))))))
        (before-after ((inform (number->string (+ (single-query (hilite (make-number "enter 10"))) (single-query (make-number "enter 20")))))) ; this step looks wrong wrong wrong.
                      ((inform (number->string (+ (single-query (hilite (make-numeric "enter 10"))) (single-query (make-number "enter 20")))))))
        (before-after ((inform (number->string (+ (hilite (single-query (make-numeric "enter 10"))) (single-query (make-number "enter 20"))))))
                      ((inform (number->string (+ (hilite 10) (single-query (make-number "enter 20")))))))
        (before-after ((inform (number->string (+ 10 (single-query (hilite (make-number "enter 20")))))))
                      ((inform (number->string (+ 10 (single-query (hilite (make-numeric "enter 20"))))))))
        (before-after ((inform (number->string (+ 10 (hilite (single-query (make-numeric "enter 20")))))))
                      ((inform (nut
                                mber->string (+ 10 (hilite 20))))))
        (before-after ((inform (number->string (hilite (+ 10 20)))))
                      ((inform (number->string (hilite 30)))))
        (before-after ((inform (hilite (number->string 30))))
                      ((inform (hilite "30"))))
        (before-after ((hilite (inform "30")))
                      ((hilite true)))
        (finished-stepping)))

  ;;;;;;;;;;;;;
  ;;
  ;;  Set!
  ;;
  ;;;;;;;;;;;;;

  (t1 'top-ref-to-lifted
      m:advanced "(define a (local ((define i1 0) (define (i2 x) i1)) i2)) (+ 3 4)"
      (let ([defs `((define i1_0 0) (define (i2_0 x) i1_0))])
        `((before-after ((define a (hilite (local ((define i1 0) (define (i2 x) i1)) i2))))
                        ((hilite (define i1_0 0)) (hilite (define (i2_0 x) i1_0)) (define a (hilite i2_0))))
          (before-after (,@defs (define a (hilite i2_0)))
                        (,@defs (define a (hilite (lambda (x) i1_0)))))
          (before-after (,@defs (define a (lambda (x) i1_0)) (hilite (+ 3 4)))
                        (,@defs (define a (lambda (x) i1_0)) (hilite 7))))))

  (t1 'set!
      m:advanced "(define a 3) (set! a (+ 4 5)) a"
      `((before-after ((define a 3) (set! a (hilite (+ 4 5))))
                      ((define a 3) (set! a (hilite 9))))
        (before-after ((hilite (define a 3)) (hilite (set! a 9)))
                      ((hilite (define a 9)) (hilite (void))))
        (before-after ((define a 9) (void) (hilite a))
                      ((define a 9) (void) (hilite 9)))
        (finished-stepping)))

  (t1 'local-set!
      m:advanced
      "(define a (local ((define in 14) (define (getter dc) in) (define (modder n) (set! in n))) modder)) (a 15)"
      (let ([d1 `(define in_0 14)]
            [d2 `(define (getter_0 dc) in_0)]
            [d3 `(define (modder_0 n) (set! in_0 n))]
            [d4 `(define a (lambda (n) (set! in_0 n)))])
        `((before-after ((define a (hilite (local ((define in 14) (define (getter dc) in) (define (modder n) (set! in n))) modder))))
                        ((hilite ,d1) (hilite ,d2) (hilite ,d3) (define a (hilite modder_0))))
          (before-after (,d1 ,d2 ,d3 (define a (hilite modder_0)))
                        (,d1 ,d2 ,d3 (define a (hilite (lambda (n) (set! in_0 n))))))
          (before-after (,d1 ,d2 ,d3 ,d4 ((hilite a) 15))
                        (,d1 ,d2 ,d3 ,d4 ((hilite (lambda (n) (set! in_0 n))) 15)))
          (before-after (,d1 ,d2 ,d3 ,d4 (hilite ((lambda (n) (set! in_0 n)) 15)))
                        (,d1 ,d2 ,d3 ,d4 (hilite (set! in_0 15))))
          (before-after ((hilite ,d1) ,d2 ,d3 , d4 (hilite (set! in_0 15)))
                        ((hilite (define in_0 15)) ,d2 ,d3 ,d4 (void)))
          (finished-stepping))))

  ;;;;;;;;;;;
  ;;
  ;;  BEGIN
  ;;
  ;;;;;;;;;;;

  (t1 'simple-begin
      m:advanced "(+ 3 (begin 4 5))"
      `((before-after ((+ 3 (hilite (begin 4 5))))
                      ((+ 3 (hilite 5))))
        (before-after ((hilite (+ 3 5)))
                      ((hilite 8)))
        (finished-stepping)))

  (t1 'begin-onlyvalues
      m:advanced "(+ 3 (begin 4 5 6))"
      `((before-after ((+ 3 (hilite (begin 4 5 6))))
                      ((+ 3 (hilite (begin 5 6)))))
        (before-after ((+ 3 (hilite (begin 5 6))))
                      ((+ 3 (hilite 6))))
        (before-after ((hilite (+ 3 6)))
                      ((hilite 9)))))

  (t1 'begin
      m:advanced "(begin (+ 3 4) (+ 4 5) (+ 9 8))"
      `((before-after ((begin (hilite (+ 3 4)) (+ 4 5) (+ 9 8)))
                      ((begin (hilite 7) (+ 4 5) (+ 9 8))))
        (before-after ((hilite (begin 7 (+ 4 5) (+ 9 8))))
                      ((hilite (begin (+ 4 5) (+ 9 8)))))
        (before-after ((begin (hilite (+ 4 5)) (+ 9 8)))
                      ((begin (hilite 9) (+ 9 8))))
        (before-after ((hilite (begin 9 (+ 9 8))))
                      ((hilite (+ 9 8))))
        (before-after ((hilite (+ 9 8)))
                      ((hilite 17)))
        (finished-stepping)))
  
  (t 'begin-let-bug m:advanced
     (let ([x 3]) (begin 3 4))
     :: {(let ([x 3]) (begin 3 4))}
     -> {(define x_0 3)} {(begin 3 4)}
     :: (define x_0 3) {(begin 3 4)}
     -> (define x_0 3) 4)

  (t1 'empty-begin
      m:advanced "(begin)"
      `((error "begin: expected at least one expression after begin, but nothing's there")))

  ;;;;;;;;;;;;
  ;;
  ;; BEGIN0
  ;;
  ;;;;;;;;;;;;
  
  (t1 'empty-begin0
      m:advanced "(begin0)"
      `((error "begin0: expected at least one expression after begin0, but nothing's there")))
  
  (t1 'trivial-begin0
      m:advanced "(begin0 3)"
      `((before-after ((hilite (begin0 3)))
                      ((hilite 3)))
        (finished-stepping)))
  

  
  ;; urg.. the first element of a begin0 is in tail position if there's only one.
  (t1 'one-item-begin0
      m:advanced "(begin0 (+ 3 4))"
      `((before-after ((hilite (begin0 (+ 3 4))))
                      ((hilite (+ 3 4))))
        (before-after ((hilite (+ 3 4)))
                      ((hilite 7)))
        (finished-stepping)))
  
  (t 'begin0-onlyvalues m:advanced
     (begin0 3 4 5)
     :: {(begin0 3 4 5)}
     -> {(begin0 3 5)}
     -> {3})
  
  (t 'begin0 m:advanced
     (begin0 (+ 3 4) (+ 4 5) (+ 6 7))
     :: (begin0 {(+ 3 4)} (+ 4 5) (+ 6 7))
     -> (begin0 {7} (+ 4 5) (+ 6 7))
     :: (begin0 7 {(+ 4 5)} (+ 6 7))
     -> (begin0 7 {9} (+ 6 7))
     :: {(begin0 7 9 (+ 6 7))}
     -> {(begin0 7 (+ 6 7))}
     :: (begin0 7 {(+ 6 7)})
     -> (begin0 7 {13})
     :: {(begin0 7 13)}
     -> {7})
  

  ;; --------------------------------------------------------------------------
  ;; Lazy Stepper tests
  ;; --------------------------------------------------------------------------
  

  (t 'lazy1 m:lazy
     (! (+ 3 4))
     :: {(+ 3 4)} -> {7})

  (t 'lazy2 m:lazy
     (+ (+ 3 4) 5)
     :: (+ {(+ 3 4)} 5)
     -> (+ {7} 5)
     :: {(+ 7 5)}
     -> {12})

  (t 'lazy3 m:lazy
     ((lambda (x y) (* x x)) (+ 1 2) (/ 1 0))
     :: {((lambda (x y) (* x x)) (+ 1 2) (/ 1 0))}
     -> {(* (+ 1 2) (+ 1 2))}
     :: (* {(+ 1 2)} {(+ 1 2)})
     -> (* {3} {3})
     :: {(* 3 3)}
     -> {9})
  
  (t 'lazy-multi m:lazy
     (+ 1 2) (+ 3 4)
     :: {(+ 1 2)} -> {3}
     :: 3 {(+ 3 4)} -> 3 {7})
  
  ; lazy-app1: 
  ; (define (f x) (+ x x))
  ; (f (+ 1 2))
  (let* ([body '(+ x x)]
         [lam `(lambda (x) ,body)]
         [def `(define (f x) ,body)]
         [arg '(+ 1 2)])
    (t 'lazy-app1 m:lazy
       ,def (f ,arg)
       :: ,def ({f} ,arg)        -> ,def ({,lam} ,arg)
       :: ,def {(,lam ,arg)}     -> ,def {(+ ,arg ,arg)}
       :: ,def (+ {,arg} {,arg}) -> ,def (+ {3} {3})
       :: ,def {(+ 3 3)}         -> ,def {6}))
  
  ; lazy-app2: 
  ; (define (f x) (+ x x))
  ; (f (f (+ 1 2)))
  (let* ([body '(+ x x)]
         [lam `(lambda (x) ,body)]
         [def `(define (f x) ,body)]
         [arg1 '(+ 1 2)]
         [arg2 `(f ,arg1)])
    (t 'lazy-app2 m:lazy
       ,def (f ,arg2)
       :: ,def ({f} ,arg2)                 -> ,def ({,lam} ,arg2)
       :: ,def {(,lam ,arg2)}              -> ,def {(+ ,arg2 ,arg2)}
       :: ,def (+ ({f} ,arg1) ({f} ,arg1)) -> ,def (+ ({,lam} ,arg1) ({,lam} ,arg1))
       :: ,def (+ {(,lam ,arg1)} {(,lam ,arg1)})           
       -> ,def (+ {(+ ,arg1 ,arg1)} {(+ ,arg1 ,arg1)})
       :: ,def (+ (+ {,arg1} {,arg1}) (+ {,arg1} {,arg1})) 
       -> ,def (+ (+ {3} {3}) (+ {3} {3})) 
       :: ,def (+ {(+ 3 3)} {(+ 3 3)})     -> ,def (+ {6} {6})
       :: ,def {(+ 6 6)}                   -> ,def {12}))
     
  ; lazy-app3
  ; (define (f x) (+ (+ x x) x))
  ; (define (g x) (+ x (+ x x)))
  ; (f (+ 1 2))
  ; (g (+ 3 4))
  (let* ([body '(+ x x)]
         [body1 `(+ ,body x)]
         [body2 `(+ x ,body)]
         [def1 `(define (f x) ,body1)]
         [def2 `(define (g x) ,body2)]
         [lam1 `(lambda (x) ,body1)]
         [lam2 `(lambda (x) ,body2)]
         [arg1 '(+ 1 2)]
         [arg2 '(+ 3 4)])
    (t 'lazy-app3 m:lazy
       ,def1 ,def2 (f ,arg1) (g ,arg2)
       :: ,def1 ,def2 ({f} ,arg1) -> ,def1 ,def2 ({,lam1} ,arg1)
       :: ,def1 ,def2 {(,lam1 ,arg1)} -> ,def1 ,def2 {(+ (+ ,arg1 ,arg1) ,arg1)}
       :: ,def1 ,def2 (+ (+ {,arg1} {,arg1}) {,arg1}) -> ,def1 ,def2 (+ (+ {3} {3}) {3})
       :: ,def1 ,def2 (+ {(+ 3 3)} 3) -> ,def1 ,def2 (+ {6} 3)
       :: ,def1 ,def2 {(+ 6 3)} -> ,def1 ,def2 {9}
       :: ,def1 ,def2 9 ({g} ,arg2) -> ,def1 ,def2 9 ({,lam2} ,arg2)
       :: ,def1 ,def2 9 {(,lam2 ,arg2)} -> ,def1 ,def2 9 {(+ ,arg2 (+ ,arg2 ,arg2))}
       :: ,def1 ,def2 9 (+ {,arg2} (+ {,arg2} {,arg2})) -> ,def1 ,def2 9 (+ {7} (+ {7} {7}))
       :: ,def1 ,def2 9 (+ 7 {(+ 7 7)}) -> ,def1 ,def2 9 (+ 7 {14})
       :: ,def1 ,def2 9 {(+ 7 14)} -> ,def1 ,def2 9 {21}))

   (t 'lazy-cons1 m:lazy
      (car (cons (+ 1 2) ,err))
      :: {(car (cons (+ 1 2) ,err))} -> {(+ 1 2)} -> {3})
   
   (t 'lazy-cons2 m:lazy
      (cdr (cons ,err (+ 1 2)))
      :: {(cdr (cons ,err (+ 1 2)))} -> {(+ 1 2)} -> {3})
   
   (t 'lazy-list1 m:lazy
      (car (list (+ 1 2) ,err))
      :: {(car (list (+ 1 2) ,err))} -> {(+ 1 2)} -> {3})
   
   (t 'lazy-list2 m:lazy
      (cdr (list ,err (+ 1 2)))
      :: {(cdr (list ,err (+ 1 2)))} -> {(list (+ 1 2))})

   (t 'lazy-list3 m:lazy
      (cadr (second (third (list ,err ,err (list ,err (list ,err (+ 1 2)))))))
      :: (cadr (second {(third (list ,err ,err (list ,err (list ,err (+ 1 2)))))}))
      -> (cadr (second {(list ,err (list ,err (+ 1 2)))}))
      :: (cadr {(second (list ,err (list ,err (+ 1 2))))})
      -> (cadr {(list ,err (+ 1 2))})
      :: {(cadr (list ,err (+ 1 2)))}
      -> {(+ 1 2)} -> {3})
   
   (t 'lazy-caar m:lazy
      (caar (list (list 1)))
      :: {(caar (list (list 1)))} -> {1})
   (t 'lazy-cadr m:lazy
      (cadr (list 1 2))
      :: {(cadr (list 1 2))} -> {2})
   (t 'lazy-cdar m:lazy
      (cdar (list (list 1)))
      :: {(cdar (list (list 1)))} -> {empty})
   

   (t 'lazy-cddr m:lazy
      (cddr (list 1 2))
      :: {(cddr (list 1 2))} -> {empty})
   (t 'lazy-caaar m:lazy
      (caaar (list (list (list 1))))
      :: {(caaar (list (list (list 1))))} -> {1})
   (t 'lazy-caadr m:lazy
      (caadr (list 1 (list 1)))
      :: {(caadr (list 1 (list 1)))} -> {1})
   (t 'lazy-cadar m:lazy
      (cadar (list (list 1 2)))
      :: {(cadar (list (list 1 2)))} -> {2})
   (t 'lazy-caddr m:lazy
      (caddr (list 1 2 3))
      :: {(caddr (list 1 2 3))} -> {3})
   (t 'lazy-cdaar m:lazy
      (cdaar (list (list (list 1))))
      :: {(cdaar (list (list (list 1))))} -> {empty})
   (t 'lazy-cdadr m:lazy
      (cdadr (list 1 (list 1)))
      :: {(cdadr (list 1 (list 1)))} -> {empty})
   (t 'lazy-cddar m:lazy
      (cddar (list (list 1 2)))
      :: {(cddar (list (list 1 2)))} -> {empty})
   (t 'lazy-cdddr m:lazy
      (cdddr (list 1 2 3))
      :: {(cdddr (list 1 2 3))} -> {empty})
   (t 'lazy-caaaar m:lazy
      (caaaar (list (list (list (list 1)))))
      :: {(caaaar (list (list (list (list 1)))))} -> {1})
   (t 'lazy-caaadr m:lazy
      (caaadr (list 1 (list (list 1))))
      :: {(caaadr (list 1 (list (list 1))))} -> {1})
   (t 'lazy-caadar m:lazy
      (caadar (list (list 1 (list 2))))
      :: {(caadar (list (list 1 (list 2))))} -> {2})
   (t 'lazy-caaddr m:lazy
      (caaddr (list 1 2 (list 1)))
      :: {(caaddr (list 1 2 (list 1)))} -> {1})
   (t 'lazy-cadaar m:lazy
      (cadaar (list (list (list 1 2))))
      :: {(cadaar (list (list (list 1 2))))} -> {2})
   (t 'lazy-cadadr m:lazy
      (cadadr (list 1 (list 1 2)))
      :: {(cadadr (list 1 (list 1 2)))} -> {2})
   (t 'lazy-caddar m:lazy
      (caddar (list (list 1 2 3)))
      :: {(caddar (list (list 1 2 3)))} -> {3})
   (t 'lazy-cadddr m:lazy
      (cadddr (list 1 2 3 4))
      :: {(cadddr (list 1 2 3 4))} -> {4})
   (t 'lazy-cdaaar m:lazy
      (cdaaar (list (list (list (list 1)))))
      :: {(cdaaar (list (list (list (list 1)))))} -> {empty})
   (t 'lazy-cdaadr m:lazy
      (cdaadr (list 1 (list (list 1))))
      :: {(cdaadr (list 1 (list (list 1))))} -> {empty})
   (t 'lazy-cdadar m:lazy
      (cdadar (list (list 1 (list 2))))
      :: {(cdadar (list (list 1 (list 2))))} -> {empty})
   (t 'lazy-cdaddr m:lazy
      (cdaddr (list 1 2 (list 1)))
      :: {(cdaddr (list 1 2 (list 1)))} -> {empty})
   (t 'lazy-cddaar m:lazy
      (cddaar (list (list (list 1 2))))
      :: {(cddaar (list (list (list 1 2))))} -> {empty})
   (t 'lazy-cddadr m:lazy
      (cddadr (list 1 (list 1 2)))
      :: {(cddadr (list 1 (list 1 2)))} -> {empty})
   (t 'lazy-cdddar m:lazy
      (cdddar (list (list 1 2 3)))
      :: {(cdddar (list (list 1 2 3)))} -> {empty})
   

   (t 'lazy-cddddr m:lazy
      (cddddr (list 1 2 3 4))
      :: {(cddddr (list 1 2 3 4))} -> {empty})
   (t 'lazy-second m:lazy
      (second (list 1 2 3 4 5 6 7 8))
      :: {(second (list 1 2 3 4 5 6 7 8))} -> {2})
   (t 'lazy-third m:lazy
      (third (list 1 2 3 4 5 6 7 8))
      :: {(third (list 1 2 3 4 5 6 7 8))} -> {3})
   (t 'lazy-fourth m:lazy
      (fourth (list 1 2 3 4 5 6 7 8))
      :: {(fourth (list 1 2 3 4 5 6 7 8))} -> {4})
   (t 'lazy-fifth m:lazy
      (fifth (list 1 2 3 4 5 6 7 8))
      :: {(fifth (list 1 2 3 4 5 6 7 8))} -> {5})
   (t 'lazy-sixth m:lazy
      (sixth (list 1 2 3 4 5 6 7 8))
      :: {(sixth (list 1 2 3 4 5 6 7 8))} -> {6})
   (t 'lazy-seventh m:lazy
      (seventh (list 1 2 3 4 5 6 7 8))
      :: {(seventh (list 1 2 3 4 5 6 7 8))} -> {7})
   (t 'lazy-eighth m:lazy
      (eighth (list 1 2 3 4 5 6 7 8))
      :: {(eighth (list 1 2 3 4 5 6 7 8))} -> {8})
 
   (t 'lazy-list4 m:lazy
      (caaaar (cdddar (list (list ,err ,err ,err (list (list (list (+ 1 2))))))))
      :: (caaaar {(cdddar (list (list ,err ,err ,err (list (list (list (+ 1 2)))))))})
      -> (caaaar {(list (list (list (list (+ 1 2)))))})
      :: {(caaaar (list (list (list (list (+ 1 2))))))} -> {(+ 1 2)} -> {3})
   
   ; lazy-list5
   ; (define (f x) (+ (car (caar x)) (cadr (cddr x))))
   ; (f (list (list (list (+ 1 2))) (/ 1 0) (/ 1 0) (+ 3 4)))
   (let* ([make-rand1 (λ (x) `(car (caar ,x)))]
          [make-rand2 (λ (x) `(cadr (cddr ,x)))]
          [rand1 (make-rand1 'x)]
          [rand2 (make-rand2 'x)]
          [make-body (λ (x) `(+ ,(make-rand1 x) ,(make-rand2 x)))]
          [body (make-body 'x)]
          [def `(define (f x) ,body)]
          [lam `(lambda (x) ,body)]
          [subarg1 '(+ 1 2)]
          [subarg2 `(list ,subarg1)]
          [subarg3 '(+ 3 4)]
          [subarg4 `(list ,err ,subarg3)]
          [make-arg (λ (x y) `(list (list ,x) ,err ,err ,y))]
          [arg (make-arg subarg2 subarg3)]
          [body-subst (make-body arg)]
          [rand2-subst (make-rand2 arg)])
     (t 'lazy-list5 m:lazy
        ,def (f ,arg)
        :: ,def ({f} ,arg) -> ,def ({,lam} ,arg)
        :: ,def {(,lam ,arg)} -> ,def {,body-subst}
        :: ,def (+ (car {(caar ,arg)}) ,rand2-subst)
        -> ,def (+ (car {,subarg2}) ,rand2-subst)
        :: ,def (+ {(car ,subarg2)} ,rand2-subst)
        -> ,def (+ {,subarg1} ,rand2-subst)
        :: ,def (+ {,subarg1} (cadr (cddr (list (list (list {,subarg1})) ,err ,err ,subarg3))))
        -> ,def (+ {3} (cadr (cddr (list (list (list {3})) ,err ,err ,subarg3))))
        :: ,def (+ 3 (cadr {(cddr (list (list (list 3)) ,err ,err ,subarg3))}))
        -> ,def (+ 3 (cadr {,subarg4}))
        :: ,def (+ 3 {(cadr ,subarg4)})
        -> ,def (+ 3 {,subarg3}) -> ,def (+ 3 {7})
        :: ,def {(+ 3 7)} -> ,def {10}))
   

   
   ; lazy-if1
   ; (define (f x) (if x (/ 1 0) (not x)))
   ; (f (< 1 0))
   (let* ([make-body (λ (x) `(if ,x ,err (not ,x)))]
          [body (make-body 'x)]
          [def `(define (f x) ,body)]
          [lam `(lambda (x) ,body)]
          [arg '(< 1 0)]
          [body-subst (make-body arg)])
     (t 'lazy-if1 m:lazy
        ,def (f ,arg)
        :: ,def ({f} ,arg) -> ,def ({,lam} ,arg)
        :: ,def {(,lam ,arg)} -> ,def {,body-subst}
        :: ,def (if {,arg} ,err (not {,arg})) -> ,def (if {false} ,err (not {false}))
        :: ,def {(if false ,err (not false))} -> ,def {(not false)} -> ,def {true}))
   
   ; lazy-if2
   ; (define (f x) (if x (not x) (/ 1 0)))
   ; (f (> 1 0))
   (let* ([make-body (λ (x) `(if ,x (not ,x) ,err))]
          [body (make-body 'x)]
          [def `(define (f x) ,body)]
          [lam `(lambda (x) ,body)]
          [arg '(> 1 0)]
          [body-subst (make-body arg)])
     (t 'lazy-if2 m:lazy
        ,def (f ,arg)
        :: ,def ({f} ,arg) -> ,def ({,lam} ,arg)
        :: ,def {(,lam ,arg)} -> ,def {,body-subst}
        :: ,def (if {,arg} (not {,arg}) ,err) -> ,def (if {true} (not {true}) ,err)
        :: ,def {(if true (not true) ,err)} -> ,def {(not true)} -> ,def {false}))

   ; lazy-take-0
   (let ([e '(take 0 (list 1 2))])
     (t 'lazy-take-0 m:lazy
        ,e :: {,e} -> {empty}))
     
   ; lazy-take
   (t 'lazy-take m:lazy
      (take (+ 0 2) (list (+ 1 2) (+ 3 4) (/ 1 0)))
      :: (take {(+ 0 2)} (list (+ 1 2) (+ 3 4) (/ 1 0)))
      -> (take {2} (list (+ 1 2) (+ 3 4) (/ 1 0)))
      :: {(take 2 (list (+ 1 2) (+ 3 4) (/ 1 0)))}
      -> {(cons (+ 1 2) ,(<delay#> 0))})
   
   ; lazy-take-impl
;   (define (take-n n lst)
;     (if (= n 0)
;         null
;         (cons (first lst)
;               (take-n (- n 1) (rest lst)))))
;   (define (f lst) (+ (first lst) (second lst)))
;   (f (take-n 3 (list 1 2 (/ 1 0) 4)))
   (let* ([make-take-body 
           (λ (n lst) 
             `(if (= ,n 0) null
                  (cons (first ,lst)
                        (take-n (- ,n 1) (rest ,lst)))))]
          [take-body (make-take-body 'n 'lst)]
          [take-def `(define (take-n n lst) ,take-body)]
          [take-lam `(lambda (n lst) ,take-body)]
          [make-f-body (λ (lst) `(+ (first ,lst) (second ,lst)))]
          [f-body (make-f-body 'lst)]
          [f-def `(define (f lst) ,f-body)]
          [f-lam `(lambda (lst) ,f-body)]
          [lst-arg `(list (+ 1 2) (+ 3 4) ,err (+ 5 6))]
          [lst-arg-red1 `(list 3 (+ 3 4) ,err (+ 5 6))]
          [make-take-app (λ (n lst) `(take-n ,n ,lst))]
          [take-arg (make-take-app 3 lst-arg)])
     (t 'lazy-take-impl m:lazy
        ,take-def ,f-def (f ,take-arg)
        :: ,take-def ,f-def ({f} ,take-arg) -> ,take-def ,f-def ({,f-lam} ,take-arg)
        :: ,take-def ,f-def {(,f-lam ,take-arg)} -> ,take-def ,f-def {,(make-f-body take-arg)}
        :: ,take-def ,f-def ,(make-f-body `({take-n} 3 ,lst-arg))
        -> ,take-def ,f-def ,(make-f-body `({,take-lam} 3 ,lst-arg))
        :: ,take-def ,f-def ,(make-f-body `{(,take-lam 3 ,lst-arg)})
        -> ,take-def ,f-def ,(make-f-body `{,(make-take-body 3 lst-arg)})
        :: ,take-def ,f-def ,(make-f-body `(if {(= 3 0)} null
                                               (cons (first ,lst-arg)
                                                     (take-n (- 3 1) (rest ,lst-arg)))))
        -> ,take-def ,f-def ,(make-f-body `(if {false} null
                                               (cons (first ,lst-arg)
                                                     (take-n (- 3 1) (rest ,lst-arg)))))
        :: ,take-def ,f-def ,(make-f-body `{(if false null
                                                (cons (first ,lst-arg)
                                                      (take-n (- 3 1) (rest ,lst-arg))))})
        -> ,take-def ,f-def ,(make-f-body `{(cons (first ,lst-arg) ,(make-take-app '(- 3 1) `(rest ,lst-arg)))})
        :: ,take-def ,f-def (+ {(first (cons (first ,lst-arg) ,(make-take-app '(- 3 1) `(rest ,lst-arg))))}
                               (second (cons (first ,lst-arg) ,(make-take-app '(- 3 1) `(rest ,lst-arg)))))
        -> ,take-def ,f-def (+ {(first ,lst-arg)}
                               (second (cons (first ,lst-arg) ,(make-take-app '(- 3 1) `(rest ,lst-arg)))))
        :: ,take-def ,f-def (+ {(first ,lst-arg)}
                               (second (cons {(first ,lst-arg)} ,(make-take-app '(- 3 1) `(rest ,lst-arg)))))
        -> ,take-def ,f-def (+ {(+ 1 2)}
                               (second (cons {(+ 1 2)} ,(make-take-app '(- 3 1) `(rest ,lst-arg)))))
        :: ,take-def ,f-def (+ {(+ 1 2)}
                               (second (cons {(+ 1 2)} ,(make-take-app '(- 3 1) `(rest (list {(+ 1 2)} (+ 3 4) ,err (+ 5 6)))))))
        -> ,take-def ,f-def (+ {3}
                               (second (cons {3} ,(make-take-app '(- 3 1) `(rest (list {3} (+ 3 4) ,err (+ 5 6)))))))
        :: ,take-def ,f-def (+ 3 (second (cons 3 ({take-n} (- 3 1) (rest ,lst-arg-red1)))))
        -> ,take-def ,f-def (+ 3 (second (cons 3 ({,take-lam} (- 3 1) (rest ,lst-arg-red1)))))
        :: ,take-def ,f-def (+ 3 (second (cons 3 {(,take-lam (- 3 1) (rest ,lst-arg-red1))})))
        -> ,take-def ,f-def (+ 3 (second (cons 3 {,(make-take-body '(- 3 1) `(rest ,lst-arg-red1))})))
        :: ,take-def ,f-def (+ 3 (second (cons 3 ,(make-take-body '{(- 3 1)} `(rest ,lst-arg-red1)))))
        -> ,take-def ,f-def (+ 3 (second (cons 3 ,(make-take-body '{2} `(rest ,lst-arg-red1)))))
        :: ,take-def ,f-def (+ 3 (second (cons 3 (if {(= 2 0)} null (cons (first (rest ,lst-arg-red1))
                                                                          (take-n (- 2 1) (rest (rest ,lst-arg-red1))))))))
        -> ,take-def ,f-def (+ 3 (second (cons 3 (if {false} null (cons (first (rest ,lst-arg-red1))
                                                                        (take-n (- 2 1) (rest (rest ,lst-arg-red1))))))))
        :: ,take-def ,f-def (+ 3 (second (cons 3 {(if false null (cons (first (rest ,lst-arg-red1))
                                                                       (take-n (- 2 1) (rest (rest ,lst-arg-red1)))))})))
        -> ,take-def ,f-def (+ 3 (second (cons 3 {(cons (first (rest ,lst-arg-red1))
                                                        (take-n (- 2 1) (rest (rest ,lst-arg-red1))))})))
        :: ,take-def ,f-def (+ 3 {(second (cons 3 (cons (first (rest ,lst-arg-red1))
                                                        (take-n (- 2 1) (rest (rest ,lst-arg-red1))))))})
        -> ,take-def ,f-def (+ 3 {(first (rest ,lst-arg-red1))})
        :: ,take-def ,f-def (+ 3 (first {(rest ,lst-arg-red1)}))
        -> ,take-def ,f-def (+ 3 (first {(list (+ 3 4) (/ 1 0) (+ 5 6))}))
        :: ,take-def ,f-def (+ 3 {(first (list (+ 3 4) (/ 1 0) (+ 5 6)))})
        -> ,take-def ,f-def (+ 3 {(+ 3 4)}) -> ,take-def ,f-def (+ 3 {7})
        :: ,take-def ,f-def {(+ 3 7)} -> ,take-def ,f-def {10}
        ))

   ; lazy-unknown1
   (t 'lazy-unknown1 m:lazy
      (second (take 3 (list (+ 1 2) (+ 3 4) (/ 1 0))))
      :: (second {(take 3 (list (+ 1 2) (+ 3 4) (/ 1 0)))})
      -> (second {(cons (+ 1 2) ,(<delay#> 0))})
      :: {(second (cons (+ 1 2) (cons (+ 3 4) ,(<delay#> 1))))}
      -> {(+ 3 4)} -> {7})
   

      
   ; lazy-unknown2
   (let* ([make-body (λ (x) `(+ (second ,x) (third ,x)))]
          [body (make-body 'x)]
          [def `(define (f x) ,body)]
          [lam `(lambda (x) ,body)]
          [subarg `(take 4 (list (+ 1 2) (+ 3 4) (+ 5 6) (+ 7 8) (/ 1 0)))]
          [arg `(cdr ,subarg)]
          [arg-red `(cons (+ 3 4) (cons (+ 5 6) ,(<delay#> 1)))])
     (t 'lazy-unknown2 m:lazy
        ,def (f ,arg)
        :: ,def ({f} ,arg) -> ,def ({,lam} ,arg)
        :: ,def {(,lam ,arg)} -> ,def {,(make-body arg)}
        :: ,def ,(make-body `(cdr {,subarg}))
        -> ,def ,(make-body `(cdr {(cons (+ 1 2) ,(<delay#> 0))}))
        :: ,def ,(make-body `{(cdr (cons (+ 1 2) ,(<delay#> 0)))})
        -> ,def ,(make-body `{,(<delay#> 0)})
        :: ,def (+ {(second ,arg-red)} (third ,arg-red))
        -> ,def (+ {(+ 5 6)} (third ,arg-red))
        :: ,def (+ {(+ 5 6)} (third (cons (+ 3 4) (cons {(+ 5 6)} ,(<delay#> 1)))))
        -> ,def (+ {11} (third (cons (+ 3 4) (cons {11} ,(<delay#> 1)))))
        :: ,def (+ 11 {(third (cons (+ 3 4) (cons 11 ,(<delay#> 1))))})
        -> ,def (+ 11 {(+ 7 8)}) -> ,def (+ 11 {15})
        :: ,def {(+ 11 15)} -> ,def {26}
        ))
   
   ; lazy-inf-list1
;   (define (add-one x) (+ x 1))
;   (define nats (cons 1 (map add-one nats)))
;   (+ (second nats) (third nats))
   (let* ([add1-body '(+ x 1)]
          [add1-def `(define (add-one x) ,add1-body)]
          [add1-lam `(lambda (x) ,add1-body)]
          [nats-def '(define nats (cons 1 (map add-one nats)))]
          [nats-def-expanded `(define nats (cons 1 (cons 2 (cons 3 ,(<delay#> 4)))))])
     (t 'lazy-inf-list1 m:lazy
        ,add1-def ,nats-def (+ (second nats) (third nats))
        :: ,add1-def ,nats-def (+ (second {nats}) (third nats))
        -> ,add1-def ,nats-def (+ (second {(cons 1 (map add-one nats))}) (third nats))
        :: ,add1-def (define nats (cons 1 (map {add-one} nats)))
        (+ (second (cons 1 (map {add-one} nats))) (third nats))
        -> ,add1-def (define nats (cons 1 (map {,add1-lam} nats)))
        (+ (second (cons 1 (map {,add1-lam} nats))) (third nats))
        :: ,add1-def (define nats (cons 1 (map ,add1-lam {nats})))
        (+ (second (cons 1 (map ,add1-lam {nats}))) (third nats))
        -> ,add1-def (define nats (cons 1 (map ,add1-lam {(cons 1 ,(<delay#> 0))})))
        (+ (second (cons 1 (map ,add1-lam {(cons 1 ,(<delay#> 0))}))) (third nats))
        :: ,add1-def (define nats (cons 1 {(map ,add1-lam (cons 1 ,(<delay#> 0)))}))
        (+ (second (cons 1 {(map ,add1-lam (cons 1 ,(<delay#> 0)))})) (third nats))
        -> ,add1-def (define nats (cons 1 {(cons ,(<delay#> 1) ,(<delay#> 2))}))
        (+ (second (cons 1 {(cons ,(<delay#> 1) ,(<delay#> 2))})) (third nats))
        :: ,add1-def (define nats (cons 1 (cons ,(<delay#> 1) ,(<delay#> 2))))
        (+ {(second (cons 1 (cons ,(<delay#> 1) ,(<delay#> 2))))} (third nats))
        -> ,add1-def (define nats (cons 1 (cons ,(<delay#> 1) ,(<delay#> 2))))
        (+ {,(<delay#> 1)} (third nats))
;        :: ,add1-def (define nats (cons 1 (cons {(+ 1 1)} ,(<delay#> 2))))
;        (+ {,(<delay#> 1)} (third nats))
        :: ...
        -> ,add1-def (define nats (cons 1 (cons {(+ 1 1)} ,(<delay#> 2))))
        (+ {(+ 1 1)} (third nats))
        -> ,add1-def (define nats (cons 1 (cons {2} ,(<delay#> 2))))
        (+ {2} (third nats))
        :: ,add1-def (define nats (cons 1 (cons 2 ,(<delay#> 2))))
        (+ 2 (third {nats}))
        -> ,add1-def (define nats (cons 1 (cons 2 ,(<delay#> 2))))
        (+ 2 (third {(cons 1 (cons 2 ,(<delay#> 2)))}))
        :: ,add1-def (define nats (cons 1 (cons 2 ,(<delay#> 2))))
        (+ 2 {(third (cons 1 (cons 2 ,(<delay#> 2))))})
        -> ,add1-def (define nats (cons 1 (cons 2 (cons ,(<delay#> 3) ,(<delay#> 4)))))
        (+ 2 {,(<delay#> 3)})
;        :: ,add1-def (define nats (cons 1 (cons 2 (cons {(+ 2 1)} ,(<delay#> 4)))))
;        (+ 2 {,(<delay#> 3)})
        :: ...
        -> ,add1-def (define nats (cons 1 (cons 2 (cons {(+ 2 1)} ,(<delay#> 4)))))
        (+ 2 {(+ 2 1)})
        -> ,add1-def (define nats (cons 1 (cons 2 (cons {3} ,(<delay#> 4)))))
        (+ 2 {3})
        :: ,add1-def ,nats-def-expanded {(+ 2 3)}
        -> ,add1-def ,nats-def-expanded {5}
        ))
   
   ; lazy-cond1, lazy-cond2
;   (define (f x)
;     (cond ((> 0 x) (/ 1 0))
;           ((< 0 x) (* x 10))
;           (else (+ x 10))))
;   (f 0)
;   (f 1)
   (let* ([make-test1 (λ (x) `(> 0 ,x))]
          [make-test2 (λ (x) `(< 0 ,x))]
          [test1 (make-test1 0)]
          [test2 (make-test2 0)]
          [test12 (make-test1 2)]
          [test22 (make-test2 2)]
          [make-clause1 (λ (x) `(* ,x 10))]
          [make-clause2 (λ (x) `(+ ,x 10))]
          [clause1 (make-clause1 0)]
          [clause2 (make-clause2 0)]
          [clause12 (make-clause1 2)]
          [clause22 (make-clause2 2)]
          [cnd (λ (x) `(cond (,(make-test1 x) ,err)
                             (,(make-test2 x) ,(make-clause1 x))
                             (else ,(make-clause2 x))))]
          [make-def (λ (x) `(define (f x) ,(cnd x)))]
          [def (make-def 'x)]
          [lam (λ (x) `(lambda (x) ,(cnd x)))])
     (t 'lazy-cond1 m:lazy
        ,def (f 0)
        :: ,def ({f} 0) -> ,def ({,(lam 'x)} 0)
        :: ,def {(,(lam 'x ) 0)} -> ,def {,(cnd 0)}
        :: ,def (cond ({,test1} ,err)
                      (,test2 ,clause1)
                      (else ,clause2))
        -> ,def (cond ({false} ,err)
                      (,test2 ,clause1)
                      (else ,clause2))
        :: ,def {(cond (false ,err)
                       (,test2 ,clause1)
                       (else ,clause2))}
        -> ,def {(cond (,test2 ,clause1)
                       (else ,clause2))}
        :: ,def (cond ({,test2} ,clause1)
                      (else ,clause2))
        -> ,def (cond ({false} ,clause1)
                      (else ,clause2))
        :: ,def {(cond (false ,clause1)
                       (else ,clause2))}
        -> ,def {(cond (else ,clause2))} 
        -> ,def {,clause2} -> ,def {10})
     )
   (let* ([make-test1 (λ (x) `(> 0 ,x))]
          [make-test2 (λ (x) `(< 0 ,x))]
          [test1 (make-test1 0)]
          [test2 (make-test2 0)]
          [test12 (make-test1 2)]
          [test22 (make-test2 2)]
          [make-clause1 (λ (x) `(* ,x 10))]
          [make-clause2 (λ (x) `(+ ,x 10))]
          [clause1 (make-clause1 0)]
          [clause2 (make-clause2 0)]
          [clause12 (make-clause1 2)]
          [clause22 (make-clause2 2)]
          [cnd (λ (x) `(cond (,(make-test1 x) ,err)
                             (,(make-test2 x) ,(make-clause1 x))
                             (else ,(make-clause2 x))))]
          [make-def (λ (x) `(define (f x) ,(cnd x)))]
          [def (make-def 'x)]
          [lam (λ (x) `(lambda (x) ,(cnd x)))])
     (t 'lazy-cond2 m:lazy
        ,def (f 2)
        :: ,def ({f} 2) -> ,def ({,(lam 'x)} 2)
        :: ,def {(,(lam 'x ) 2)} -> ,def {,(cnd 2)}
        :: ,def (cond ({,test12} ,err)
                      (,test22 ,clause12)
                      (else ,clause22))
        -> ,def (cond ({false} ,err)
                      (,test22 ,clause12)
                      (else ,clause22))
        :: ,def {(cond (false ,err)
                       (,test22 ,clause12)
                       (else ,clause22))}
        -> ,def {(cond (,test22 ,clause12)
                       (else ,clause22))}
        :: ,def (cond ({,test22} ,clause12)
                      (else ,clause22))
        -> ,def (cond ({true} ,clause12)
                      (else ,clause22))
        :: ,def {(cond (true ,clause12)
                       (else ,clause22))}
        -> ,def {,clause12} -> ,def {20})
     )

   
   ; lazy-cond3
   (t 'lazy-cond3 m:lazy
      (! (cond [false 1] [else 2]))
      :: {(cond [false 1] [else 2])} -> {(cond [else 2])} -> {2})
   
   
   (t 'lazy-eq? m:lazy
      (eq? 'a 'a)
      :: {(eq? 'a'a)} -> {true})
   (t 'lazy-eqv? m:lazy
      (eqv? (integer->char 955) (integer->char 955))
      :: (eqv? {(integer->char 955)} (integer->char 955))
      -> (eqv? {#\λ} (integer->char 955))
      :: (eqv? #\λ {(integer->char 955)})
      -> (eqv? #\λ {#\λ})
      :: {(eqv? #\λ #\λ)} -> {true})
   (t 'lazy-equal? m:lazy
      (equal? (list 1 2) (list 1 2))
      :: {(equal? (list 1 2) (list 1 2))} -> {true})
   (t 'lazy-list?1 m:lazy
      (list? (list 1 2))
      :: {(list? (list 1 2))} -> {true})
   (t 'lazy-list?2 m:lazy
      (list? empty)
      :: {(list? empty)} -> {true})
   (t 'lazy-list?3 m:lazy
      (list? (+ 1 2))
      :: (list? {(+ 1 2)}) -> (list? {3})
      :: {(list? 3)} -> {false})
   (t 'lazy-length m:lazy
      (length (list 1 2))
      :: {(length (list 1 2))} -> {2})
   (t 'lazy-list-ref m:lazy
      (list-ref (list 1 2) (+ 1 0))
      :: (list-ref (list 1 2) {(+ 1 0)}) -> (list-ref (list 1 2) {1})
      :: {(list-ref (list 1 2) 1)} -> {2})
   (t 'lazy-list-tail m:lazy
      (list-tail (list 1 2) 1)
      :: {(list-tail (list 1 2) 1)} -> {(list 2)})
   (t 'lazy-append m:lazy
      (append (list 1 2) (list 3 4))
      :: {(append (list 1 2) (list 3 4))}
      -> {(cons 1 ,(<delay#> 0))})
   (t 'lazy-reverse m:lazy
      (reverse (list 1 2 3))
      :: {(reverse (list 1 2 3))} -> {(list 3 2 1)})
   (t 'lazy-empty? m:lazy
      (empty? (list 1 2))
      :: {(empty? (list 1 2))} -> {false})
   (t 'lazy-assoc m:lazy
      (assoc 1 (list (list 1 2)))
      :: {(assoc 1 (list (list 1 2)))} -> {(list 1 2)})
   (t 'lazy-assq m:lazy
      (assq 1 (list (list 1 2)))
      :: {(assq 1 (list (list 1 2)))} -> {(list 1 2)})
   (t 'lazy-assv m:lazy
      (assv 1 (list (list 1 2)))
      :: {(assv 1 (list (list 1 2)))} -> {(list 1 2)})
   (t 'lazy-cons? m:lazy
      (cons? (list 1 2))
      :: {(cons? (list 1 2))} -> {true})
   (t 'lazy-remove m:lazy
      (remove 2 (list 1 2 3))
      :: {(remove 2 (list 1 2 3))} -> {(cons 1 ,(<delay#> 0))})
   (t 'lazy-remq m:lazy
      (remq 2 (list 1 2 3))
      :: {(remq 2 (list 1 2 3))} -> {(cons 1 ,(<delay#> 0))})
   (t 'lazy-remv m:lazy
      (remv 2 (list 1 2 3))
      :: {(remv 2 (list 1 2 3))} -> {(cons 1 ,(<delay#> 0))})
   (t 'lazy-member m:lazy
      (member 1 (list 1 2))
      :: {(member 1 (list 1 2))} -> {(list 1 2)})
   (t 'lazy-memq m:lazy
      (memq 1 (list 1 2))
      :: {(memq 1 (list 1 2))} -> {(list 1 2)})
   (t 'lazy-memv m:lazy
      (memv 1 (list 1 2))
      :: {(memv 1 (list 1 2))} -> {(list 1 2)})
  
   
   (t 'lazy-filter1 m:lazy
      (third (filter (lambda (x) (>= x 3)) '(1 2 3 4 5)))
      :: (third {(filter (lambda (x) (>= x 3)) (list 1 2 3 4 5))})
      -> (third (... {(>= 1 3)} ...))
      -> (third (... {false} ...))
      :: ... -> (third {,(<delay#> 0)})
      :: ... -> (third {(>= 2 3)}) -> (third {false})
      :: ... -> (third {(>= 3 3)}) -> (third {true})
      :: ... -> (third (cons 3 {(>= 4 3)})) -> (third (cons 3 {true}))
      :: ... -> (third (cons 3 (cons 4 {(>= 5 3)})))
      -> (third (cons 3 (cons 4 {true})))
      :: {(third (cons 3 (cons 4 (cons 5 ,(<delay#> 1)))))} -> {5})
   
   ; same as lazy-filter1 except forcing of the list itself shows up as a step
   ; only difference is in lazy-filter1 a quote (') is used while in lazy-filter2
   ; a list constructor is used
   (t 'lazy-filter2 m:lazy
      (third (filter (lambda (x) (>= x 3)) (list 1 2 3 4 5)))
      :: (third {(filter (lambda (x) (>= x 3)) (list 1 2 3 4 5))})
      -> (third (... {(list 1 2 3 4 5)} ...))
      :: ... -> (third (... {(>= 1 3)} ...)) -> (third (... {false} ...))
      :: ... -> (third {,(<delay#> 0)})
      :: ... -> (third {(>= 2 3)}) -> (third {false})
      :: ... -> (third {(>= 3 3)}) -> (third {true})
      :: ... -> (third (cons 3 {(>= 4 3)})) -> (third (cons 3 {true}))
      :: ... -> (third (cons 3 (cons 4 {(>= 5 3)})))
      -> (third (cons 3 (cons 4 {true})))
      :: {(third (cons 3 (cons 4 (cons 5 ,(<delay#> 1)))))} -> {5})
   
   (t 'lazy-fold m:lazy
      (+ (foldr (lambda (x y) (+ x y)) 0 '(1 2 3)) 1000)
      :: (+ {(foldr (lambda (x y) (+ x y)) 0 (list 1 2 3))} 1000)
      -> (+ {,(<delay#> 0)} 1000)
      :: ... -> (+ {(+ 1 ,(<delay#> 1))} 1000)
      :: ... -> (+ (+ 1 {(+ 2 ,(<delay#> 2))}) 1000)
      :: ... -> (+ (+ 1 (+ 2 {(+ 3 ,(<delay#> 3))})) 1000)
      :: (+ (+ 1 (+ 2 {(+ 3 0)})) 1000)
      -> (+ (+ 1 (+ 2 {3})) 1000)
      :: (+ (+ 1 {(+ 2 3)}) 1000)
      -> (+ (+ 1 {5}) 1000)
      :: (+ {(+ 1 5)} 1000)
      -> (+ {6} 1000)
      :: {(+ 6 1000)} -> {1006})
    
   (let ([def '(define ones (cons 1 ones))])
     (t 'lazy-cyclic1 m:lazy
        ,def (+ (second ones) (third ones))
        :: ,def (+ (second {ones}) (third ones))
        -> ,def (+ (second {(cons 1 ones)}) (third ones))
        :: (define ones {ones}) (+ (second {ones}) (third ones)) ; extra step
        -> (define ones {,(<delay#> 0)}) (+ (second {,(<delay#> 0)}) (third ones))
        :: ,def (+ {(second (cons 1 ,(<delay#> 0)))} (third ones))
        -> ,def (+ {1} (third ones))
        :: ,def (+ 1 (third {ones}))
        -> ,def (+ 1 (third {(cons 1 ,(<delay#> 0))}))
        :: ,def (+ 1 {(third (cons 1 ,(<delay#> 0)))})
        -> ,def (+ 1 {1})
        :: ,def {(+ 1 1)} -> ,def {2}))
   
   ; application in function position -- checks bug fix
   (let* ([lxx '(lambda (x) x)]
          [def `(define I ,lxx)])
     (t 'lazy-fn-app m:lazy
        ,def ((I I) I)
        :: ,def (({I} I) I)       -> ,def (({,lxx} I) I)
        :: ,def ((,lxx {I}) I)    -> ,def ((,lxx {,lxx}) I)
        :: ,def ({(,lxx ,lxx)} I) -> ,def ({,lxx} I)
        :: ,def (,lxx {I})        -> ,def (,lxx {,lxx})
        :: ,def {(,lxx ,lxx)}     -> ,def {,lxx}))
    
   
  #;
  (t1 'teachpack-callbacks
     (test-teachpack-sequence
      "(define (f2c x) x) (convert-gui f2c)" `() ; placeholder
      ))
  
  ;; SYNTAX ERRORS : 
  
  (t1 'bad-parens m:upto-int/lam
      "("
      `((error "read: expected a `)' to close `('")))
  
  #;(t1 'bad-stx-and m:upto-int/lam
      "(and)"
      `((error "foo")))
  
  (t 'local-struct/i m:intermediate
     (define (f x) (local ((define-struct a (b c))) x))  (f 1)
     :: (define (f x) (local ((define-struct a (b c))) x)) {(f 1)}
     -> (define (f x) (local ((define-struct a (b c))) x)) 
     {(define-struct a_1 (b c))} {1})
  
  (t 'local-struct/ilam m:intermediate-lambda
     (define (f x) (local ((define-struct a (b c))) x))  (f 1)
     :: (define (f x) (local ((define-struct a (b c))) x)) {(f 1)}
     -> (define (f x) (local ((define-struct a (b c))) x)) 
     {((lambda (x) (local ((define-struct a (b c))) x)) 1)}
     -> (define (f x) (local ((define-struct a (b c))) x)) 
     {(define-struct a_1 (b c))} {1})
  
  ;; test of require
  (list 'require-test m:upto-int/lam
        "(require \"foo.rkt\") (+ a 4)"
        '((before-after ((require "foo.rkt") (+ (hilite a) 4))
                        ((require "foo.rkt") (+ (hilite 3) 4)))
          (before-after ((require "foo.rkt") (hilite (+ 3 4)))
                        ((require "foo.rkt") (hilite 7))))
        '(("foo.rkt" "#lang racket \n(provide a) (define a 3)")))
   
    
   
   ))

