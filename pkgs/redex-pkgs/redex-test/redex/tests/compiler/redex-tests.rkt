#lang racket
(require (only-in redex term)
         redex/private/lang-struct
         redex/private/matcher
         redex/private/compiler/match
         redex/private/compiler/redextomatrix
         (only-in "../test-util.rkt" equal/bindings?)
         mzlib/list)

(define (make-test-mtch a b c) (make-mtch a (build-flat-context b) c))

(define (build-context c)
  (let loop ([c c])
    (cond
      [(eq? c the-hole) the-hole]
      [(pair? c) (build-cons-context (loop (car c)) (loop (cdr c)))]
      [(or (null? c)
           (number? c)
           (symbol? c))
       (build-flat-context c)]
      [else (error 'build-context "unknown ~s" c)])))

(define no-context #f)
(define in-context #t)
(define context-match (make-parameter no-context))

(define natural?
  (λ (x) (and 
          (exact-integer? x) 
          (not (negative? x)))))

(define (variable-prefix? x y) 
  (let* ([prefix-str (symbol->string x)]
         [prefix-len (string-length prefix-str)])
    (and (symbol? y)
         (let ([str (symbol->string y)])
           (and ((string-length str) . >= . prefix-len)
                (string=? (substring str 0 prefix-len) prefix-str))))))

(define (run-match-test xs ys)
  (cond
    [(and (not xs) (not ys)) #t]
    [(and (list? xs)
          (list? ys))
     (and (andmap (λ (x) (memf (λ (y) (equal/bindings? x y)) ys)) xs)
          (andmap (λ (y) (memf (λ (x) (equal/bindings? x y)) xs)) ys)
          (= (length xs) (length ys)))]
    [else #f]))

(define empty-lang (test-redex-match '(define-language empty)))
(define x-lang (test-redex-match '(define-language x-lang (x (variable-except x)))))
(define ab-lang (test-redex-match '(define-language ab-lang (aa a) (bb b))))
(define xab-lang (test-redex-match '(define-language xab-lang 
                                      (exp (+ exp exp)
                                           number)
                                      (ctxt (+ ctxt exp)
                                            (+ exp ctxt)
                                            hole)
                                      (ec-one (+ (hole xx) exp)
                                              (+ exp (hole xx)))
                                      (same-in-nt ((name x any) (name x any)))
                                      (forever-list (forever-list forever-list ...)
                                                    x)
                                      (lsts ()
                                            (x)
                                            x
                                            #f)
                                      (split-out split-out2)
                                      (split-out2 number)
                                      (simple simple-rhs)
                                      (nesting-names (a (name x nesting-names))
                                                     b)
                                      (var variable-not-otherwise-mentioned)
                                      (underscore exp_1)
                                      )
                                   ))
(define nany-lang (test-redex-match '(define-language nany (n any number))))

; test-empty : sexp[pattern] sexp[term] answer -> void
;; returns #t if pat matching exp with the empty language produces ans.
(define (test-empty pat exp ans)
  (let ((a ((empty-lang pat) exp)))
    (unless (run-match-test ans (if (empty? a) #f a)) (printf "Pattern: ~s Input: ~s ==> Got: ~s Expected: ~s\n" pat exp a ans))
    )
  )

;; test-x : sexp[pattern] sexp[term] answer -> void
;; returns #t if pat matching exp with the x language produces ans.
(define (test-x pat exp ans)
  (let ((a ((x-lang pat) exp)))
    (unless (run-match-test ans (if (empty? a) #f a)) (printf "Pattern: ~s Input: ~s ==> Got: ~s Expected: ~s\n" pat exp a ans))
    )
  )

;; test-ab : sexp[pattern] sexp[term] answer -> void
;; returns #t if pat matching exp with the ab language produces ans.
(define (test-ab pat exp ans)
  (let ((a ((ab-lang pat) exp)))
    (unless (run-match-test ans (if (empty? a) #f a)) (printf "Pattern: ~s Input: ~s ==> Got: ~s Expected: ~s\n" pat exp a ans))
    )
  )

;; test-xab : sexp[pattern] sexp[term] answer -> void
;; returns #t if pat matching exp with the xab language produces ans.
(define (test-xab pat exp ans)
  (let ((a ((xab-lang pat) exp)))
    (unless (run-match-test ans (if (empty? a) #f a)) (printf "Pattern: ~s Input: ~s ==> Got:\n~s\nExpected:\n~s\n\n" pat exp a ans))
    )
  )

(define (test-nany pat exp ans)
  (let ((a ((nany-lang pat) exp)))
    (unless (run-match-test ans (if (empty? a) #f a)) (printf "Pattern: ~s Input: ~s ==> Got:\n~s\nExpected:\n~s\n\n" pat exp a ans))
    )
  )

(define (test)
  (print-struct #t)
  (test-empty 'any 1 (list (make-test-mtch (make-bindings (list (make-bind 'any 1))) 1 'none)))
  (test-empty 'any 'true (list (make-test-mtch (make-bindings (list (make-bind 'any 'true))) 'true 'none)))
  (test-empty 'any "a" (list (make-test-mtch (make-bindings (list (make-bind 'any "a"))) "a" 'none)))
  (test-empty 'any '(a b) (list (make-test-mtch (make-bindings (list (make-bind 'any '(a b)))) '(a b) 'none)))
  (test-empty 'any #t (list (make-test-mtch (make-bindings (list (make-bind 'any #t))) #t 'none)))
  (test-empty 1 1 (list (make-test-mtch (make-bindings null) 1 'none)))
  (test-empty 1 '() #f)
  (test-empty 99999999999999999999999999999999999999999999999
              99999999999999999999999999999999999999999999999
              (list (make-test-mtch (make-bindings null) 
                                    99999999999999999999999999999999999999999999999
                                    'none)))
  (test-empty 99999999999999999999999999999999999999999999999
              '()
              #f)
  (test-empty 'x 'x (list (make-test-mtch (make-bindings null) 'x 'none)))
  (test-empty 'x '() #f)
  (test-empty 1 2 #f)
  (test-empty "a" "b" #f)
  (test-empty "a" '(x) #f)
  (test-empty "a" '() #f)
  (test-empty "a" "a" (list (make-test-mtch (make-bindings null) "a" 'none)))
  (test-empty 'number 1 (list (make-test-mtch (make-bindings (list (make-bind 'number 1))) 1 'none)))
  (test-empty 'number 'x #f)
  (test-empty 'number '() #f)
  (test-empty 'natural 1 (list (make-test-mtch (make-bindings (list (make-bind 'natural 1))) 1 'none)))
  (test-empty 'natural 'x #f)
  (test-empty 'natural '() #f)
  (test-empty 'natural -1 #f)
  (test-empty 'natural 1.0 #f)
  (test-empty 'integer -1 (list (make-test-mtch (make-bindings (list (make-bind 'integer -1))) -1 'none)))
  (test-empty 'integer 'x #f)
  (test-empty 'integer '() #f)
  (test-empty 'integer 1.0 #f)
  (test-empty 'real 1.1 (list (make-test-mtch (make-bindings (list (make-bind 'real 1.1))) 1.1 'none)))
  (test-empty 'real 'x #f)
  (test-empty 'real '() #f)
  (test-empty 'real 2+3i #f)
  (test-empty 'string "a" (list (make-test-mtch (make-bindings (list (make-bind 'string "a"))) "a" 'none)))
  (test-empty 'string 1 #f)
  (test-empty 'string '() #f)
  (test-empty 'variable 'x (list (make-test-mtch (make-bindings (list (make-bind 'variable 'x))) 'x 'none)))
  (test-empty 'variable 1 #f)
  (test-empty '(variable-except x) 1 #f)
  (test-empty '(variable-except x) 'x #f)
  (test-empty '(variable-except x) 'y (list (make-test-mtch (make-bindings null) 'y 'none)))
  
  (test-x 'x 'y (list (make-mtch (make-bindings (list (make-bind 'x 'y))) 'y 'none)))
  ; added to replace:
  #;(test-lang 'x 'y (list (make-mtch (make-bindings (list (make-bind 'x 'y))) 'y 'none))
               (list (make-nt 'x (list (make-rhs '(variable-except x))))))
  
  (test-empty '(variable-prefix x:) 'x: (list (make-test-mtch (make-bindings null) 'x: 'none)))
  (test-empty '(variable-prefix x:) 'x:x (list (make-test-mtch (make-bindings null) 'x:x 'none)))
  (test-empty '(variable-prefix x:) ': #f)
  (test-empty '(variable-prefix x:) '() #f)
  (test-empty 'hole 1 #f)
  (test-empty `hole
              the-hole
              (list (make-test-mtch (make-bindings (list)) the-hole 'none)))
  (test-empty '(in-hole (hole 2) 1)
              '(1 2)
              (list (make-test-mtch (make-bindings (list)) `(1 2) 'none)))
  
  (test-empty '(in-hole (name E_1 ((hide-hole hole) hole)) x)
              `(,the-hole x)
              (list (make-test-mtch (make-bindings (list (make-bind 'E_1 `(,the-not-hole ,the-hole)))) 
                                    `(,the-hole x)
                                    'none)))
  (test-empty '(name x number) 1 (list (make-test-mtch (make-bindings (list (make-bind 'x 1) (make-bind 'number 1))) 1 'none)))
  (test-empty 'number_x 1 (list (make-test-mtch (make-bindings (list (make-bind 'number_x 1))) 1 'none)))
  (test-empty 'string_y "b" (list (make-test-mtch (make-bindings (list (make-bind 'string_y "b"))) "b" 'none)))
  (test-empty 'any_z '(a b) (list (make-test-mtch (make-bindings (list (make-bind 'any_z '(a b)))) '(a b) 'none)))
  
  ; We don't have _!_ yet
  
  ;    (test-empty '(name x_!_1 number) 1 (list (make-test-mtch (make-bindings (list (make-bind 'number 1))) 1 'none)))
  ;    (test-empty '((name x_!_1 number) (name x_!_1 number)) '(1 1) #f)
  ;    (test-empty '((name x_!_1 number_a) (name x_!_1 number_b)) '(1 2) 
  ;                (list (make-test-mtch (make-bindings (list (make-bind 'number_a 1)
  ;                                                           (make-bind 'number_b 2)))
  ;                                      '(1 2) 
  ;                                      'none)))
  ;    (test-empty '(number_!_1 number_!_1) '(1 1) #f)
  ;    (test-empty '(number_!_1 number_!_1) '(1 2) (list (make-test-mtch (make-bindings (list)) '(1 2) 'none)))
  ;    (test-empty '(number_!_1 ...) '(1 2) (list (make-test-mtch (make-bindings (list)) '(1 2) 'none)))
  ;    (test-empty '(number_!_1 ...) '(1 2 3 4 5) (list (make-test-mtch (make-bindings (list)) '(1 2 3 4 5) 'none)))
  ;    (test-empty '(number_!_1 ...) '(1 2 3 1 5) (list (make-test-mtch (make-bindings (list)) '(1 2 3 1 5) 'none)))
  ;    (test-empty '((number_!_1 ...) (number_!_1 ...)) 
  ;                '((1 2 3 1 5) (1 2 3 1 5))
  ;                #f)
  ;    (test-empty '((number_!_1 ...) (number_!_1 ...)) 
  ;                '((17 2 3 1 5) (1 2 3 1 5))
  ;                (list (make-test-mtch (make-bindings (list)) '((17 2 3 1 5) (1 2 3 1 5)) 'none)))
  ;    (test-empty '((number_!_1 number_!_1) ... number_!_1 ...) '((1 1) (2 2) 1 3) #f)
  ;    (test-empty '((number_!_1 number_!_1) ... number_!_1 ...) '((1 1) (2 3) 1 2) #f)
  ;    (test-empty '((number_!_1 number_!_1) ... number_!_1 ...)
  ;                '((1 1) (2 3) 1 4)
  ;                (list (make-test-mtch (make-bindings (list)) '((1 1) (2 3) 1 4) 'none)))
  
  ; cases based on "test-ellipses"
  ; (test-ellipses '(a) '(a))
  (test-empty '(a) '(a) (list (make-test-mtch (make-bindings null) '(a) 'none)))
  
  ; (test-ellipses '(a ...) `(,(make-repeat 'a '() #f #f)))
  (test-empty '(a ...) '() (list (make-test-mtch (make-bindings null) '() 'none)))
  (test-empty '(a ...) '(a) (list (make-test-mtch (make-bindings null) '(a) 'none)))
  (test-empty '(a ...) '(a a a) (list (make-test-mtch (make-bindings null) '(a a a) 'none)))
  (test-empty '(a ...) '(a b a) #f) 
  
  ; (test-ellipses '((a ...) ...) `(,(make-repeat '(a ...) '() #f #f)))
  (test-empty '((a ...) ...) '() (list (make-test-mtch (make-bindings null) '() 'none)))
  (test-empty '((a ...) ...) '(()) (list (make-test-mtch (make-bindings null) '(()) 'none)))
  (test-empty '((a ...) ...) '(() (a) (a a a)) (list (make-test-mtch (make-bindings null) '(() (a) (a a a)) 'none)))
  (test-empty '((a ...) ...) '((a a a a a) () () (a a)) (list (make-test-mtch (make-bindings null) '((a a a a a) () () (a a)) 'none)))
  (test-empty '((a ...) ...) '((a a a a a) () () (b a)) #f)
  
  ; (test-ellipses '(a ... b c ...) `(,(make-repeat 'a '() #f #f) b ,(make-repeat 'c '() #f #f)))
  (test-empty '(a ... b c ...) '(b) (list (make-test-mtch (make-bindings null) '(b) 'none)))
  (test-empty '(a ... b c ...) '(a b) (list (make-test-mtch (make-bindings null) '(a b) 'none)))
  (test-empty '(a ... b c ...) '(b c) (list (make-test-mtch (make-bindings null) '(b c) 'none)))
  (test-empty '(a ... b c ...) '(a a a b) (list (make-test-mtch (make-bindings null) '(a a a b) 'none)))
  (test-empty '(a ... b c ...) '(b c c c) (list (make-test-mtch (make-bindings null) '(b c c c) 'none)))
  (test-empty '(a ... b c ...) '(a a a b c c) (list (make-test-mtch (make-bindings null) '(a a a b c c) 'none)))
  (test-empty '(a ... b c ...) '(a a a b b c c) #f)
  
  ; (test-ellipses '((name x a) ...) `(,(make-repeat '(name x a) (list (make-bind 'x '())) #f #f))) 
  (test-empty '((name x a) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'x '()))) '() 'none)))
  (test-empty '((name x a) ...) '(a) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a)))) '(a) 'none)))
  (test-empty '((name x a) ...) '(a a a) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a a a)))) '(a a a) 'none)))
  (test-empty '((name x a) ...) '(a b a) #f)
  
  ;    (test-ellipses '((name x (a ...)) ...)
  ;                   `(,(make-repeat '(name x (a ...)) (list (make- bind 'x '())) #f #f)))
  (test-empty '((name x (a ...)) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'x '()))) '() 'none)))
  (test-empty '((name x (a ...)) ...) '(()) (list (make-test-mtch (make-bindings (list (make-bind 'x '(())))) '(()) 'none)))
  (test-empty '((name x (a ...)) ...) '(() (a) (a a a)) (list (make-test-mtch (make-bindings (list (make-bind 'x '(() (a) (a a a))))) '(() (a) (a a a)) 'none)))
  (test-empty '((name x (a ...)) ...) '((a a a a a) () () (a a)) (list (make-test-mtch (make-bindings (list (make-bind 'x '((a a a a a) () () (a a))))) '((a a a a a) () () (a a)) 'none)))
  (test-empty '((name x (a ...)) ...) '((a a a a a) () () (b a)) #f)
  
  ;    (test-ellipses '(((name x a) ...) ...)
  ;                   `(,(make-repeat '((name x a) ...) (list (make-bind 'x '())) #f #f)))
  (test-empty '(((name x a) ...) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'x '()))) '() 'none)))
  (test-empty '(((name x a) ...) ...) '(()) (list (make-test-mtch (make-bindings (list (make-bind 'x '(())))) '(()) 'none)))
  (test-empty '(((name x a) ...) ...) '(() (a) (a a a)) (list (make-test-mtch (make-bindings (list (make-bind 'x '(() (a) (a a a))))) '(() (a) (a a a)) 'none)))
  (test-empty '(((name x a) ...) ...) '((a a a a a) () () (a a)) (list (make-test-mtch (make-bindings (list (make-bind 'x '((a a a a a) () () (a a))))) '((a a a a a) () () (a a)) 'none)))
  (test-empty '(((name x a) ...) ...) '((a a a a a) () () (b a)) #f)
  
  ;    (test-ellipses '((1 (name x a)) ...)
  ;                   `(,(make-repeat '(1 (name x a)) (list (make-bind 'x '())) #f #f)))
  (test-empty '((1 (name x a)) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'x '()))) '() 'none)))
  (test-empty '((1 (name x a)) ...) '((1 a)) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a)))) '((1 a)) 'none)))
  (test-empty '((1 (name x a)) ...) '((1 a) (1 a) (1 a)) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a a a)))) '((1 a) (1 a) (1 a)) 'none)))
  (test-empty '((1 (name x a)) ...) '((1 a) (2 a) (1 a)) #f)
  (test-empty '((1 (name x a)) ...) '((1 a) (1 b) (1 a)) #f)
  
  ;    (test-ellipses '((any (name x a)) ...)
  ;                   `(,(make-repeat '(any (name x a)) (list (make-bind 'any '())
  ;                                                           (make-bind 'x '())) 
  ;                                   #f #f)))
  (test-empty '((any (name x a)) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'any '()) (make-bind 'x '()))) '() 'none)))
  (test-empty '((any (name x a)) ...) '((b a)) (list (make-test-mtch (make-bindings (list (make-bind 'any '(b)) (make-bind 'x '(a)))) '((b a)) 'none)))
  (test-empty '((any (name x a)) ...) '((1 a) (2 a) (3 a)) (list (make-test-mtch (make-bindings (list (make-bind 'any '(1 2 3)) (make-bind 'x '(a a a)))) '((1 a) (2 a) (3 a)) 'none)))
  (test-empty '((any (name x a)) ...) '((1 a) (2 b) (3 a)) #f)
  
  ;    (test-ellipses '((number (name x a)) ...)
  ;                   `(,(make-repeat '(number (name x a)) (list (make-bind 'number '())
  ;                                                              (make-bind 'x '())) 
  ;                                   #f #f)))
  (test-empty '((number (name x a)) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'number '()) (make-bind 'x '()))) '() 'none)))
  (test-empty '((number (name x a)) ...) '((2 a)) (list (make-test-mtch (make-bindings (list (make-bind 'number '(2)) (make-bind 'x '(a)))) '((2 a)) 'none)))
  (test-empty '((number (name x a)) ...) '((1 a) (2 a) (3 a)) (list (make-test-mtch (make-bindings (list (make-bind 'number '(1 2 3)) (make-bind 'x '(a a a)))) '((1 a) (2 a) (3 a)) 'none)))
  (test-empty '((number (name x a)) ...) '((1 a) (2 b) (3 a)) #f)
  (test-empty '((number (name x a)) ...) '((1 a) (b a) (3 a)) #f)
  
  ;    (test-ellipses '((variable (name x a)) ...)
  ;                   `(,(make-repeat '(variable (name x a)) (list (make-bind 'variable '())
  ;                                                                (make-bind 'x '()))
  ;                                   #f #f)))
  (test-empty '((variable (name x a)) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'variable '()) (make-bind 'x '()))) '() 'none)))
  (test-empty '((variable (name x a)) ...) '((x a)) (list (make-test-mtch (make-bindings (list (make-bind 'variable '(x)) (make-bind 'x '(a)))) '((x a)) 'none)))
  (test-empty '((variable (name x a)) ...) '((x a) (y a) (z a)) (list (make-test-mtch (make-bindings (list (make-bind 'variable '(x y z)) (make-bind 'x '(a a a)))) '((x a) (y a) (z a)) 'none)))
  (test-empty '((variable (name x a)) ...) '((x a) (y b) (z a)) #f)
  (test-empty '((variable (name x a)) ...) '((1 a) (b a) (c a)) #f)
  
  ;    (test-ellipses '(((name x a) (name y b)) ...)
  ;                   `(,(make-repeat '((name x a) (name y b)) (list (make-bind 'x '()) (make-bind 'y '())) #f #f)))
  (test-empty '(((name x a) (name y b)) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'y '()) (make-bind 'x '()))) '() 'none)))
  (test-empty '(((name x a) (name y b)) ...) '((a b)) (list (make-test-mtch (make-bindings (list (make-bind 'y '(b)) (make-bind 'x '(a)))) '((a b)) 'none)))
  (test-empty '(((name x a) (name y b)) ...) '((a b) (a b)) (list (make-test-mtch (make-bindings (list (make-bind 'y '(b b)) (make-bind 'x '(a a)))) '((a b) (a b)) 'none)))
  (test-empty '(((name x a) (name y b)) ...) '((a b) (b a)) #f)
  (test-empty '(((name x a) (name y b)) ...) '(a b) #f)
  (test-empty '(((name x a) (name y b)) ...) '(a b a b) #f)
  (test-empty '(((name x a) (name y b)) ...) '(a c) #f)
  
  ;    (test-ellipses '((name x (name y b)) ...)
  ;                   `(,(make-repeat '(name x (name y b)) (list (make-bind 'x '()) (make-bind 'y '())) #f #f)))
  (test-empty '((name x (name y b)) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'y '()) (make-bind 'x '()))) '() 'none)))
  (test-empty '((name x (name y b)) ...) '(b) (list (make-test-mtch (make-bindings (list (make-bind 'y '(b)) (make-bind 'x '(b)))) '(b) 'none)))
  (test-empty '((name x (name y b)) ...) '(b b b) (list (make-test-mtch (make-bindings (list (make-bind 'y '(b b b)) (make-bind 'x '(b b b)))) '(b b b) 'none)))
  (test-empty '((name x (name y b)) ...) '(b b a) #f)
  
  ;    (test-ellipses '((in-hole (name x a) (name y b)) ...)
  ;                   `(,(make-repeat '(in-hole (name x a) (name y b)) 
  ;                                  (list (make-bind 'y '()) (make-bind 'x '())) #f #f)))
  (test-empty '((in-hole (name x a) (name y b)) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'y '()) (make-bind 'x '()))) '() 'none)))
  (test-empty '((in-hole (name x a) (name y b)) ...) '(a) #f)
  
  ; We don't have ..._ or ..._!_ yet 
  ;    (test-ellipses '(a ..._1)
  ;                   `(,(make-repeat 'a (list) '..._1 #f)))
  ;    (test-ellipses '(a ..._!_1)
  ;                   `(,(make-repeat 'a (list) '..._!_1 #t)))
  
  
  (test-empty '(((name x 5) (name y 5)) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'x '()) (make-bind 'y '()))) '() 'none)))
  (test-empty '(((name x 5) (name y 5)) ...) '((5 5)) (list (make-test-mtch (make-bindings (list (make-bind 'x '(5)) (make-bind 'y '(5)))) '((5 5)) 'none)))
  (test-empty '(((name x 5) (name y 5)) ...) '((5 5) (5 5) (5 5)) (list (make-test-mtch (make-bindings (list (make-bind 'y '(5 5 5)) (make-bind 'x '(5 5 5)))) '((5 5) (5 5) (5 5)) 'none)))
  (test-empty '(((name x 5) (name y 5)) ...) '((5 6)) #f)
  
  (test-empty '((number number) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'number '()))) '() 'none)))
  (test-empty '((number number) ...) '((5 5)) (list (make-test-mtch (make-bindings (list (make-bind 'number '(5)))) '((5 5)) 'none)))
  (test-empty '((number number) ...) '((1 1) (2 2) (3 3)) (list (make-test-mtch (make-bindings (list (make-bind 'number '(1 2 3)))) '((1 1) (2 2) (3 3)) 'none)))
  (test-empty '((number number) ...) '((1 2)) #f)
  
  (test-empty '(number ... number ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'number '()))) '() 'none)))
  (test-empty '(number ... number ...) '(1 1) (list (make-test-mtch (make-bindings (list (make-bind 'number '(1)))) '(1 1) 'none)))
  (test-empty '(number ... number ...) '(1 2 1 2) (list (make-test-mtch (make-bindings (list (make-bind 'number '(1 2)))) '(1 2 1 2) 'none)))
 (test-empty '(number ... number ...) '(1 2) #f)
  
;  (test-empty '((number ... number ...) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'number '()))) '() 'none)))
;  (test-empty '((number ... number ...) ...) '(()) (list (make-test-mtch (make-bindings (list (make-bind 'number '(())))) '(()) 'none)))
;  (test-empty '((number ... number ...) ...) '((1 1)) (list (make-test-mtch (make-bindings (list (make-bind 'number '((1))))) '((1 1)) 'none)))
;  (test-empty '((number ... number ...) ...) '((1 1) (2 2) (1 2 3 1 2 3) (4 5 6 4 5 6)) (list (make-test-mtch (make-bindings 
;                                                                                                               (list (make-bind 'number '((1) (2) (1 2 3) (4 5 6)))))
;                                                                                                              '((1 1) (2 2) (1 2 3 1 2 3) (4 5 6 4 5 6))
;                                                                                                              'none)))
;  (test-empty '((number ... number ...) ...) '((1 1) (2 2) (1 2 3 1 2 3) (4 5 6 4 5 -6)) #f)
;  
;  (test-empty '((name number any) (number ... number ...) ...) '(()) (list (make-test-mtch (make-bindings (list (make-bind 'any '()) (make-bind 'number '()))) '(()) 'none)))
;  (test-empty '((name number any) (number ... number ...) ...) '(((1)) (1 1)) (list (make-test-mtch (make-bindings (list (make-bind 'any '((1))) (make-bind 'number '((1))))) '(((1)) (1 1)) 'none)))
;  (test-empty '((name number any) (number ... number ...) ...) '(((1)) (1 2)) #f)
;  (test-empty '((name number any) (number ... number ...) ...) '(((2)) (1 1)) #f)
;  
;  (test-empty '((number ... number ...) ... (name number any)) '(()) (list (make-test-mtch (make-bindings (list (make-bind 'any '()) (make-bind 'number '()))) '(()) 'none)))
;  (test-empty '((number ... number ...) ... (name number any)) '((1 1) ((1))) (list (make-test-mtch (make-bindings (list (make-bind 'any '((1))) (make-bind 'number '((1))))) '((1 1) ((1))) 'none)))
;  (test-empty '((number ... number ...) ... (name number any)) '((1 2) ((1))) #f)
;  (test-empty '((number ... number ...) ... (name number any)) '((1 1) ((2))) #f)
  
;  ; causes an error
;  #;(test-empty '((number ... number ...) ... (number ... number ...) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'number '()))) '() 'none)))
;  
  (test-empty '() '() (list (make-test-mtch (make-bindings null) '() 'none)))
  (test-empty '(a) '(a) (list (make-test-mtch (make-bindings null) '(a) 'none)))
  (test-empty '(a) '(b) #f)
  (test-empty '(a b) '(a b) (list (make-test-mtch (make-bindings null) '(a b) 'none)))
  (test-empty '(a b) '(a c) #f)
  (test-empty '() 1 #f)
  (test-empty '(#f x) '(#f x) (list (make-test-mtch (make-bindings null) '(#f x) 'none)))
  (test-empty '(#f (name y any)) '(#f) #f)
  (test-empty '(in-hole (z hole) a) '(z a) (list (make-test-mtch (make-bindings (list)) '(z a) 'none)))
  (test-empty '(in-hole (z hole) (in-hole (x hole) a)) 
              '(z (x a))
              (list (make-test-mtch (make-bindings (list)) '(z (x a)) 'none)))
  
  #;(run-test/cmp 'in-hole-zero-holes 
                  (with-handlers ([exn:fail? (λ (e) (regexp-match #rx"zero holes" (exn-message e)))])
                    (test-empty '(in-hole (1 2) 2) '(1 2) 'never-gets-here)
                    'should-have-raised-an-exception)
                  '("zero holes")
                  equal?)
  
  
  (test-empty '(in-hole (in-hole (x hole) hole) y)
              '(x y)
              (list (make-test-mtch (make-bindings (list)) '(x y) 'none)))
  
  (test-empty '(number number) '(1 1) (list (make-test-mtch (make-bindings (list (make-bind 'number 1))) '(1 1) 'none)))
  (test-empty '((name x number) (name x number)) '(1 1) (list (make-test-mtch (make-bindings (list (make-bind 'x 1) (make-bind 'number 1))) '(1 1) 'none)))
  (test-empty '((name x number_q) (name x number_r)) '(1 1) (list (make-test-mtch (make-bindings (list (make-bind 'x 1) 
                                                                                                       (make-bind 'number_q 1)
                                                                                                       (make-bind 'number_r 1)))
                                                                                  '(1 1)
                                                                                  'none)))
  (test-empty '(number number) '(1 2) #f)
  (test-empty '((name x number) (name x number)) '(1 2) #f)
  (test-empty '((name x number_q) (name x number_r)) '(1 2) #f)
  
  (test-empty '(a ...) '() (list (make-test-mtch (make-bindings empty) '() 'none)))
  (test-empty '(a ...) '(a) (list (make-test-mtch (make-bindings empty) '(a) 'none)))
  (test-empty '(a ...) '(a a) (list (make-test-mtch (make-bindings empty) '(a a) 'none)))
  (test-empty '((name x a) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'x '()))) '() 'none)))
  (test-empty '((name x a) ...) '(a) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a)))) '(a) 'none)))
  (test-empty '((name x a) ...) '(a a) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a a)))) '(a a) 'none)))
  (test-empty '(hole ...) '() (list (make-test-mtch (make-bindings empty) '() 'none)))    
  (test-empty '(b ... a ...) '() (list (make-test-mtch (make-bindings empty) '() 'none)))
  (test-empty '(b ... a ...) '(a) (list (make-test-mtch (make-bindings empty) '(a) 'none)))
  (test-empty '(b ... a ...) '(b) (list (make-test-mtch (make-bindings empty) '(b) 'none)))
  (test-empty '(b ... a ...) '(b a) (list (make-test-mtch (make-bindings empty) '(b a) 'none)))
  (test-empty '(b ... a ...) '(b b a a) (list (make-test-mtch (make-bindings empty) '(b b a a) 'none)))
  (test-empty '(b ... a ...) '(a a) (list (make-test-mtch (make-bindings empty) '(a a) 'none)))
  (test-empty '(b ... a ...) '(b b) (list (make-test-mtch (make-bindings empty) '(b b) 'none)))
  
  ;    (test-empty '(a ..._1 a ..._2) 
  ;                '(a) 
  ;                (list (make-test-mtch (make-bindings (list (make-bind '..._1 1) (make-bind '..._2 0))) '(a) 'none)
  ;                      (make-test-mtch (make-bindings (list (make-bind '..._1 0) (make-bind '..._2 1))) '(a) 'none)))
  ;    (test-empty '(a ..._1 a ..._1) '(a) #f)
  ;    (test-empty '(a ..._1 a ..._1)
  ;                '(a a) 
  ;                (list (make-test-mtch (make-bindings (list (make-bind '..._1 1))) '(a a) 'none)))
  ;
  ;    (test-empty '((name x a) ..._!_1 (name y a) ..._!_1) 
  ;                '(a a) 
  ;                (list (make-test-mtch (make-bindings (list (make-bind 'x '()) (make-bind 'y '(a a)))) '(a a) 'none)
  ;                      (make-test-mtch (make-bindings (list (make-bind 'x '(a a)) (make-bind 'y '()))) '(a a) 'none)))
  ;    
  ;    (test-empty '((name y b) ... (name x a) ...) '() 
  ;                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
  ;                                                      (make-bind 'y '())))
  ;                                 '()
  ;                                 'none)))
  ;    (test-empty '((name y b) ... (name x a) ...) '(a)
  ;                (list (make-test-mtch (make-bindings (list (make-bind 'x '(a))
  ;                                                      (make-bind 'y '())))
  ;                                 '(a)
  ;                                 'none)))
  ;    (test-empty '((name y b) ... (name x a) ...) '(b) 
  ;                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
  ;                                                      (make-bind 'y '(b))))
  ;                                 '(b)
  ;                                 'none)))
  ;    (test-empty '((name y b) ... (name x a) ...) '(b b a a) 
  ;                (list (make-test-mtch (make-bindings (list (make-bind 'x '(a a))
  ;                                                      (make-bind 'y '(b b))))
  ;                                 '(b b a a)
  ;                                 'none)))
  ;    (test-empty '((name y a) ... (name x a) ...) '(a) 
  ;                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
  ;                                                      (make-bind 'y '(a))))
  ;                                 '(a)
  ;                                 'none)
  ;                      (make-test-mtch (make-bindings (list (make-bind 'x '(a))
  ;                                                      (make-bind 'y '())))
  ;                                 '(a)
  ;                                 'none)))
  ;    (test-empty '((name y a) ... (name x a) ...) '(a a) 
  ;                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
  ;                                                      (make-bind 'y '(a a))))
  ;                                 '(a a)
  ;                                 'none)
  ;                      (make-test-mtch (make-bindings (list (make-bind 'x '(a))
  ;                                                      (make-bind 'y '(a))))
  ;                                 '(a a)
  ;                                 'none)
  ;                      (make-test-mtch (make-bindings (list (make-bind 'x '(a a))
  ;                                                      (make-bind 'y '())))
  ;                                 '(a a)
  ;                                 'none)))
  #;(test-ab '(bb_y ... aa_x ...) '() 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                        (make-bind 'bb_y '())))
                                   '()
                                   'none)))
  #;(test-ab '(bb_y ... aa_x ...) '(a)
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a))
                                                        (make-bind 'bb_y '())))
                                   '(a) 
                                   'none)))
  #;(test-ab '(bb_y ... aa_x ...) '(b) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                        (make-bind 'bb_y '(b))))
                                   '(b)
                                   'none)))
  #;(test-ab '(bb_y ... aa_x ...) '(b b a a) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a a))
                                                        (make-bind 'bb_y '(b b))))
                                   '(b b a a)
                                   'none)))
  #;(test-ab '(aa_y ... aa_x ...) '(a) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                        (make-bind 'aa_y '(a))))
                                   '(a)
                                   'none)
                   (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a))
                                                        (make-bind 'aa_y '())))
                                   '(a)
                                   'none)))
  #;(test-ab '(aa_y ... aa_x ...) '(a a) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                        (make-bind 'aa_y '(a a))))
                                   '(a a)
                                   'none)
                   (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a))
                                                        (make-bind 'aa_y '(a))))
                                   '(a a)
                                   'none)
                   (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a a))
                                                        (make-bind 'aa_y '())))
                                   '(a a)
                                   'none)))
  
  (test-empty '((name x number) ...) '(1 2) (list (make-test-mtch (make-bindings (list (make-bind 'x '(1 2)) (make-bind 'number '(1 2)))) '(1 2) 'none)))
  
  (test-empty '(a ...) '(b) #f)
  (test-empty '(a ... b ...) '(c) #f)
  (test-empty '(a ... b) '(b c) #f)
  (test-empty '(a ... b) '(a b c) #f)
  
  
  (test-nany '(n n ...) '((1 1) 1 1) (list (make-test-mtch (make-bindings (list (make-bind 'n '(1 1)))) '((1 1) 1 1) 'none)))
  (test-nany '(n (n ...)) '((1 1) (1 1)) (list (make-test-mtch (make-bindings (list (make-bind 'n '(1 1)))) '((1 1) (1 1)) 'none)))
  (test-empty '((name x any) 
                ((name x number) ...))
              '((1 1) (1 1))
              (list (make-test-mtch (make-bindings (list (make-bind 'x '(1 1))
                                                         (make-bind 'any '(1 1))
                                                         (make-bind 'number '(1 1))))
                                    '((1 1) (1 1)) 
                                    'none)))
  
  (test-empty '((variable_1 variable_1) ...)
              '((x y))
              #f)
  
  
  (test-empty '(number ...) '()
              (list (make-test-mtch (make-bindings (list (make-bind 'number '()))) '() 'none)))
  (test-ab '(aa ...) '()
           (list (make-test-mtch (make-bindings (list (make-bind 'aa '()))) '() 'none)))
  
  
  ;; testing block-in-hole
  (test-empty '(hide-hole a) 'b #f)
  (test-empty '(hide-hole a) 'a (list (make-test-mtch (make-bindings '()) 'a 'none)))
  (test-empty '(hide-hole a) '(block-in-hole a) #f)
  (test-empty '(in-hole (x (hide-hole hole)) 1) '(x 1) #f)
  (test-empty '(in-hole (x hole) 1) '(x 1) (list (make-test-mtch (make-bindings '()) '(x 1) 'none)))
  (test-empty '(in-hole ((hole #f) (hide-hole hole)) junk)
              '(junk junk2)
              #f)
  
  (test-xab 'lsts '() (list (make-test-mtch (make-bindings (list (make-bind 'lsts '()))) '() 'none)))
  (test-xab 'lsts '(x) (list (make-test-mtch (make-bindings (list (make-bind 'lsts '(x)))) '(x) 'none)))
  (test-xab 'lsts 'x (list (make-test-mtch (make-bindings (list (make-bind 'lsts 'x))) 'x 'none)))
  (test-xab 'lsts #f (list (make-test-mtch (make-bindings (list (make-bind 'lsts #f))) #f 'none)))
  (test-xab 'split-out '1 (list (make-test-mtch (make-bindings (list (make-bind 'split-out 1))) '1 'none)))
  
  (test-xab 'exp 1 (list (make-test-mtch (make-bindings (list (make-bind 'exp 1))) 1 'none)))
  (test-xab 'exp '(+ 1 2) (list (make-test-mtch (make-bindings (list (make-bind 'exp '(+ 1 2)))) '(+ 1 2) 'none)))
  (test-xab '(in-hole ctxt any)
            '1
            (list (make-test-mtch (make-bindings (list (make-bind 'ctxt the-hole) (make-bind 'any 1))) 1 'none)))
  (test-xab '(in-hole ctxt (name x any))
            '1
            (list (make-test-mtch (make-bindings (list (make-bind 'ctxt the-hole) (make-bind 'x 1) (make-bind 'any 1))) 1 'none)))
  
  (test-xab '(in-hole (name c ctxt) (name x any))
            '(+ 1 2)
            (list (make-test-mtch (make-bindings (list (make-bind 'ctxt (build-context the-hole))
                                                       (make-bind 'c (build-context the-hole))
                                                       (make-bind 'x '(+ 1 2))
                                                       (make-bind 'any '(+ 1 2))))
                                  '(+ 1 2) 'none)
                  (make-test-mtch (make-bindings (list (make-bind 'ctxt (build-context `(+ ,the-hole 2)))
                                                       (make-bind 'c (build-context `(+ ,the-hole 2)))
                                                       (make-bind 'x 1)
                                                       (make-bind 'any 1)))
                                  '(+ 1 2) 'none)
                  (make-test-mtch (make-bindings (list (make-bind 'ctxt (build-context `(+ 1 ,the-hole)))
                                                       (make-bind 'c (build-context `(+ 1 ,the-hole)))
                                                       (make-bind 'x 2)
                                                       (make-bind 'any 2))) 
                                  '(+ 1 2) 'none)))
  (test-xab '(in-hole (name c ctxt) (name i (+ number_1 number_2)))
            '(+ (+ 1 2) (+ 3 4))
            (list (make-test-mtch 
                   (make-bindings (list (make-bind 'i '(+ 1 2))
                                        (make-bind 'number_1 1)
                                        (make-bind 'number_2 2)
                                        (make-bind 'ctxt (build-context `(+ ,the-hole (+ 3 4))))
                                        (make-bind 'c (build-context `(+ ,the-hole (+ 3 4))))))
                   '(+ (+ 1 2) (+ 3 4))
                   'none)
                  (make-test-mtch (make-bindings (list (make-bind 'i '(+ 3 4)) 
                                                       (make-bind 'number_1 3)
                                                       (make-bind 'number_2 4)
                                                       (make-bind 'ctxt `(+ (+ 1 2) ,the-hole))
                                                       (make-bind 'c `(+ (+ 1 2) ,the-hole))))
                                  '(+ (+ 1 2) (+ 3 4))
                                  'none)))
  
  (test-empty '(in-hole ((z hole)) (name x any))
              '((z a))
              (list (make-test-mtch (make-bindings (list (make-bind 'x 'a) (make-bind 'any 'a))) '((z a)) 'none)))
  ; in-hole-name bug
  (test-empty '(in-hole (name c (z ... hole z ...)) any)
              '(z z)
              (list 
               (make-test-mtch (make-bindings (list (make-bind 'c `(z ,the-hole)) (make-bind 'any 'z))) '(z z) 'none)
               (make-test-mtch (make-bindings (list (make-bind 'c `(,the-hole z)) (make-bind 'any 'z))) '(z z) 'none)))
  (test-empty '(in-hole (name c (z ... hole z ...)) any)
              '(z z z)
              (list 
               (make-test-mtch (make-bindings (list (make-bind 'c `(z z ,the-hole)) (make-bind 'any 'z))) '(z z z) 'none)
               (make-test-mtch (make-bindings (list (make-bind 'c `(z ,the-hole z)) (make-bind 'any 'z))) '(z z z) 'none)
               (make-test-mtch (make-bindings (list (make-bind 'c `(,the-hole z z)) (make-bind 'any 'z))) '(z z z) 'none)))
  
  (test-empty '(z (in-hole (name c (z hole)) a))
              '(z (z a))
              (list 
               (make-test-mtch (make-bindings (list (make-bind 'c `(z ,the-hole))))
                               '(z (z a))
                               'none)))
  
  (test-empty '(a (in-hole (name c1 (b (in-hole (name c2 (c hole)) d) hole)) e))
              '(a (b (c d) e))
              (list 
               (make-test-mtch (make-bindings (list (make-bind 'c2 `(c ,the-hole))
                                                    (make-bind 'c1 `(b (c d) ,the-hole))))
                               '(a (b (c d) e))
                               'none)))
  
  (test-empty '(in-hole (in-hole hole hole) a)
              'a
              (list (make-test-mtch (make-bindings (list)) 'a 'none)))
  
  (test-empty '(a (b (in-hole (name c1 (in-hole (name c2 (c hole)) (d hole))) e)))
              '(a (b (c (d e))))
              (list 
               (make-test-mtch (make-bindings (list (make-bind 'c1 `(c (d ,the-hole)))
                                                    (make-bind 'c2 `(c ,the-hole))))
                               '(a (b (c (d e))))
                               'none)))
  
  (test-empty '(in-hole (name c1 (in-hole (name c2 (c hole)) (d hole))) e)
              '(c (d e))
              (list 
               (make-test-mtch (make-bindings (list (make-bind 'c1 `(c (d ,the-hole)))
                                                    (make-bind 'c2 `(c ,the-hole))))
                               '(c (d e))
                               'none)))
  
  (test-empty '(in-hole (in-hole (c hole) (d hole)) e)
              '(c (d e))
              (list (make-test-mtch (make-bindings null) '(c (d e)) 'none)))
  
  (test-empty `(+ 1 (side-condition any (lambda (bindings) #t) #t))
              '(+ 1 b)
              (list (make-test-mtch (make-bindings (list (make-bind 'any 'b))) '(+ 1 b) 'none)))
  (test-empty `(+ 1 (side-condition any (lambda (bindings) #f) #f))
              '(+ 1 b)
              #f)
  
  (test-empty `(+ 1 (side-condition b (lambda (bindings) #t) #t))
              '(+ 1 b)
              (list (make-test-mtch (make-bindings '()) '(+ 1 b) 'none)))
  (test-empty `(+ 1 (side-condition a (lambda (bindings) #t)) #t)
              '(+ 1 b)
              #f)
  
  (test-empty `(side-condition (name x any) (lambda (bindings) (eq? (term x) #;(lookup-binding bindings 'x) 'a)) (eq? (term x) 'a))
              'a
              (list 
               (make-test-mtch (make-bindings (list (make-bind 'x 'a)
                                                    (make-bind 'any 'a)))
                               'a
                               'none)))
  
  (test-empty `(+ 1 (side-condition (name x any) (lambda (bindings) (eq? (term x) #;(lookup-binding bindings 'x) 'a)) (eq? (term x) 'a)))
              '(+ 1 a)
              (list 
               (make-test-mtch (make-bindings (list (make-bind 'x 'a)
                                                    (make-bind 'any 'a)))
                               '(+ 1 a)
                               'none)))
  
  (test-empty `(side-condition (name x any) (lambda (bindings) (eq? (term x) #;(lookup-binding bindings 'x) 'a)) (eq? (term x) 'a))
              'b
              #f)
  
  (test-empty `(+ 1 (side-condition (name x any) (lambda (bindings) (eq? (term x) #;(lookup-binding bindings 'x) 'a)) (eq? (term x) 'a)))
              '(+ 1 b)
              #f) 
  
  (test-empty `(any_2 any_3 ...) '(1 2 3) (list 
               (make-test-mtch (make-bindings (list (make-bind 'any_2 1)
                                                    (make-bind 'any_3 '(2 3))))
                               '(1 2 3)
                               'none)))
  
  #;(test-empty `(side-condition ((any_1 ..._a) (any_2 ..._a))
                                 (lambda (bindings) (error 'should-not-be-called))
                                 (error 'should-not-be-called))
                '((1 2 3) (4 5))
                #f)
  
  (test-xab 'exp_1
            '(+ 1 2)
            (list (make-test-mtch (make-bindings (list (make-bind 'exp_1 '(+ 1 2)))) '(+ 1 2) 'none)))
  (test-xab '(exp_1 exp_2)
            '((+ 1 2) (+ 3 4))
            (list (make-test-mtch (make-bindings (list (make-bind 'exp_1 '(+ 1 2)) (make-bind 'exp_2 '(+ 3 4))))
                                  '((+ 1 2) (+ 3 4))
                                  'none)))
  (test-xab '(exp_1 exp_1)
            '((+ 1 2) (+ 3 4))
            #f)
  (test-xab 'nesting-names
            'b
            (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names 'b))) 'b 'none)))
  (test-xab 'nesting-names
            '(a b)
            (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names '(a b)))) '(a b) 'none)))
  (test-xab 'nesting-names
            '(a (a b))
            (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names '(a (a b))))) '(a (a b)) 'none)))
  (test-xab '((name x a) nesting-names)
            '(a (a (a b)))
            (list (make-test-mtch (make-bindings (list (make-bind 'x 'a)
                                                       (make-bind 'nesting-names '(a (a b)))))
                                  '(a (a (a b))) 'none)))
  (test-xab 'nesting-names
            '(a (a (a (a b))))
            (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names '(a (a (a (a b)))))))
                                  '(a (a (a (a b)))) 'none)))
  
  (test-xab 'same-in-nt
            '(x x)
            (list (make-test-mtch (make-bindings (list (make-bind 'same-in-nt '(x x)))) '(x x) 'none)))
  (test-xab 'same-in-nt
            '(x y)
            #f)
  
  ; We don't have cross yet  
  
  ;    (test-xab '(in-hole (cross forever-list) 1)
  ;              '(a b c)
  ;              #f)
  ;    
  ;    (test-xab '(in-hole (cross forever-list) 1)
  ;              '(1 x x)
  ;              (list (make-test-mtch (make-bindings '()) '(1 x x) 'none)))
  ;    
  ;    (test-xab '(in-hole (cross forever-list) 1)
  ;              '(x 1 x)
  ;              (list (make-test-mtch (make-bindings '()) '(x 1 x) 'none)))
  ;    
  ;    
  ;    (test-xab '(in-hole (cross simple) g)
  ;              'g
  ;              (list (make-mtch (make-bindings (list)) 'g 'none)))
  
  (test-xab 'var '+ #f)
  (test-xab 'var 'anunusedvariable (list (make-mtch (make-bindings (list (make-bind 'var 'anunusedvariable))) 'anunusedvariable 'none)))
  (test-xab 'var 'exp (list (make-mtch (make-bindings (list (make-bind 'var 'exp))) 'exp 'none)))
  (test-xab 'var 'exp_x (list (make-mtch (make-bindings (list (make-bind 'var 'exp_x))) 'exp_x 'none)))
  
  (test-xab 'underscore '(+ 1 2) (list (make-mtch (make-bindings (list (make-bind 'underscore '(+ 1 2)))) '(+ 1 2) 'none)))
  (test-xab 'underscore '2 (list (make-mtch (make-bindings (list (make-bind 'underscore 2))) 2 'none)))
  
;  ; simple boolean language
;  (define bool
;    '(define-language bool
;       (B true
;          false
;          (and B B)
;          (or B B))
;       (C (and C B)
;          (or C B)
;          hole))
;    
;    (define bool-rr
;      '(reduction-relation bool
;                           (--> (in-hole C (and false B))
;                                (in-hole C false)
;                                )
;                           (--> (in-hole C (and true B))
;                                (in-hole C B)
;                                )
;                           (--> (in-hole C (or false B))
;                                (in-hole C B))
;                           (--> (in-hole C (or true B))
;                                (in-hole C true))
;                           ))
;    
;    ; metafunction-less λv language:
;    
;    ; define-language
;    (define λv '(define-language λv
;                  (e (e e ...) (if0 e e e) x v)
;                  (v (λ (x ...) e) number * +)
;                  (E (v ... E e ...) (if0 E e e) hole)
;                  (x (variable-except if0 λ * +))))
;    
;    ; converted metafunctions
;    (define subst-vars-rr
;      '(reduction-relation 
;        λv
;        (--> ((x_1 any_1) x_1) 
;             any_1)
;        (--> ((x_1 any_1) (any_2 ...))
;             ,(map subst-vars (term (((x_1 any_1) any_2) ...))) )
;        (--> (side-condition
;              ((x_1 any_1) any_2)
;              (and (not (list? any_2)) 
;                   (not (eq? (term x_1) (term any_2)))))
;             any_2)
;        (--> ((x_1 any_1) (x_2 any_2) (x_3 any_3) ... any_4)
;             ,(subst-vars (term ((x_1 any_1) ,(subst-vars (term ((x_2 any_2) (x_3 any_3) ... any_4)))))))
;        (--> (any)
;             any)))
;    
;    (define subst-rr 
;      '(reduction-relation
;        λv
;        (--> (x_1 any_1 (λ (x_2 ... x_1 x_3 ...) any_2))
;             (λ (x_2 ... x_1 x_3 ...) any_2))
;        (--> (side-condition
;              (x_1 any_1 (λ (x_2 ...) any_2))
;              (not (memq (term x_1) (term (x_2 ...)))))
;             ,(term-let ([(x_new ...) (variables-not-in (term (x_1 any_1 any_2)) (term (x_2 ...)))])
;                        (term
;                         (λ (x_new ...) ,(subst (term (x_1 any_1 ,(subst-vars (term ((x_2 x_new) ... any_2))))) )))) )
;        (--> (x_1 any_1 x_1) 
;             any_1)
;        (--> (side-condition
;              (x_1 any_1 x_2)
;              (not (eq? (term x_1) (term x_2))))
;             x_2)
;        (--> (x any (e_1 e_2 ...))
;             (,(subst (term (x any e_1))) ,@(map subst (term ((x any e_2) ...)))) )
;        (--> (x any (if0 e_1 e_2 e_3))
;             (if0 ,(subst (term (x any e_1))) ,(subst (term (x any e_2))) ,(subst (term (x any e_3)))))
;        (--> (x any number)
;             number)
;        (--> (x any +)
;             +)
;        (--> (x any *)
;             *)))
;    
;    (define subst-n-rr '(reduction-relation 
;                         λv
;                         (--> ((x_1 any_1) (x_2 any_2) ... any_3)
;                              ,(subst (term (x_1 any_1 ,(subst-n (term ((x_2 any_2) ... any_3)))))))
;                         (--> (any_3)
;                              any_3)))
;    
;    ; reduction-relation
;    (define λv-rr '(reduction-relation 
;                    λv
;                    (--> (in-hole E (* number_1 number_2)) 
;                         (in-hole E ,(* (term number_1) (term number_2))))
;                    (--> (in-hole E (+ number_1 number_2)) 
;                         (in-hole E ,(+ (term number_1) (term number_2))))
;                    (--> (in-hole E (if0 0 e_1 e_2))
;                         (in-hole E e_1))
;                    (--> (in-hole E (if0 (side-condition number_1 (not (zero? (term number_1)))) e_1 e_2))
;                         (in-hole E e_2))
;                    (--> (in-hole E (side-condition ((λ (x ...) e) v ...)
;                                                    (= (length (term (x ...)))
;                                                       (length (term (v ...))))))
;                         (in-hole E ,(subst-n (term ((x v) ... e)))))))
  )
  (test)
