#lang scheme

(require "test-util.ss"
         "reduction-semantics.ss"
         "matcher.ss"
         "term.ss"
         "rg.ss")

(reset-count)



;; to-table : hash-table -> assoc
;; extracts the hash-table's mapping in a deterministic way
(define (to-table ht)
  (sort (hash-map ht cons)
        (λ (x y) (string<=? (format "~a" (car x)) (format "~a" (car y))))))

(let ()
  (define-language lc
    (e x (e e) (λ (x) e))
    (x variable))
  (test (to-table (find-base-cases lc))
        '((e . (1 2 2)) (x . (0)))))

(let ()
  (define-language lc
    (e (e e)))
  (test (to-table (find-base-cases lc))
        '((e . (inf)))))

(let ()
  (define-language lc
    (a 1 2 3)
    (b a (a b)))
  (test (to-table (find-base-cases lc))
        '((a . (0 0 0)) (b . (1 2)))))

(let ()
  (define-language lc
    (e (e e)
       (+ e e)
       x
       v)
    (v (λ (x) e)
       number)
    (x variable))
  (test (to-table (find-base-cases lc))
        '((e . (2 2 1 1)) (v . (2 0)) (x . (0)))))

(let ()
  (define-language lang
    (e number x y)
    (x variable)
    (y y))
  (test (min-prods (car (compiled-lang-lang lang)) (find-base-cases lang))
        (list (car (nt-rhs (car (compiled-lang-lang lang)))))))

(let ()
  (define-language lang
    (a (side-condition "strin_g" #t) 1/2 #t)
    (b ()))
  (let* ([literals (sort (lang-literals lang) string<=?)]
         [chars (sort (unique-chars literals) char<=?)])
    (test literals '("1/2" "side-condition" "strin_g"))
    (test chars '(#\- #\/ #\1 #\2 #\c #\d #\e #\g #\i #\n #\o #\r #\s #\t))))

(define (make-random nums)
  (let ([nums (box nums)])
    (λ (m)
      (cond [(null? (unbox nums)) (error 'make-random "out of numbers")]
            [(>= (car (unbox nums)) m) (error 'make-random "number too large")]
            [else (begin0 (car (unbox nums)) (set-box! nums (cdr (unbox nums))))]))))

(test (pick-from-list '(a b c) (make-random '(1))) 'b)

(test (pick-length (make-random '(1 1 1 0))) 3)

(let ()
  (define-language lang
    (a bcd cbd))
  (let* ([lits (sort (lang-literals lang) string<=?)]
         [chars (sort (unique-chars lits) char<=?)])
    (test (pick-char 0 chars (make-random '(1))) #\c)
    (test (pick-char 50 chars (make-random '(1 1))) #\c)
    (test (pick-char 50 chars (make-random '(0 65))) #\a)
    (test (pick-char 500 chars (make-random '(0 1 65))) #\a)
    (test (pick-char 500 chars (make-random '(0 0 3))) #\⇒)
    (test (pick-char 2000 chars (make-random '(0 0 1 3))) #\⇒)
    (test (pick-char 2000 chars (make-random '(0 0 0 1))) (integer->char #x4E01))
    (test (pick-char 50 chars (make-random `(0 ,(- (char->integer #\_) #x20)))) #\`)
    (test (random-string chars lits 3 0 (make-random '(0 1))) "cbd")
    (test (random-string chars lits 3 0 (make-random '(1 2 1 0))) "dcb")
    (test (pick-string chars lits 0 (make-random '(1 1 1 0 1 2 1 0))) "dcb")
    (test (pick-var chars lits null 0 (make-random '(0 0 1 1 2 1 0))) 'dcb)
    (test (pick-var chars lits '(x) 0 (make-random '(1 0))) 'x)))

(let ()
  (define-language empty)
  (let* ([lits (sort (lang-literals empty) string<=?)]
         [chars (sort (unique-chars lits) char<=?)])
    (test (pick-char 0 chars (make-random '(65))) #\a)
    (test (random-string chars lits 1 0 (make-random '(65))) "a")))

(define (rhs-matching pat prods)
  (cond [(null? prods) (error 'rhs-matching "no rhs matching ~s" pat)]
        [(equal? (rhs-pattern (car prods)) pat) (car prods)]
        [else (rhs-matching pat (cdr prods))]))

(define-syntax exn:fail-message
  (syntax-rules ()
    [(_ expr)
     (with-handlers ([exn:fail? exn-message])
       (begin
         expr
         (let ()
           (define-struct exn-not-raised ())
           (make-exn-not-raised))))]))

(let ()
  (define-language l (a (a b) (a b c) c))
  (test (rhs-matching '(a b c) (nt-rhs (car (compiled-lang-lang l))))
        (cadr (nt-rhs (car (compiled-lang-lang l)))))
  (test (exn:fail-message (rhs-matching '(a c) (nt-rhs (car (compiled-lang-lang l)))))
        #rx"no rhs matching"))

(define (select-pattern pat)
  (λ (prods . _) (rhs-matching pat prods)))

(define (patterns . ps) (map select-pattern ps))

(define (iterator name items)
  (let ([bi (box items)])
    (λ () 
      (if (null? (unbox bi))
          (error name "empty")
          (begin0 (car (unbox bi)) (set-box! bi (cdr (unbox bi))))))))

(let ([iter (iterator 'test-iterator '(a b))])
  (test (iter) 'a)
  (test (iter) 'b)
  (test (exn:fail-message (iter)) #rx"empty"))

(define (decisions #:var [var pick-var] 
                   #:nt [nt pick-nt]
                   #:str [str pick-string]
                   #:num [num pick-from-list]
                   #:any [any pick-any]
                   #:seq [seq pick-length])
  (define-syntax decision
    (syntax-rules ()
      [(_ d) (if (procedure? d) (λ () d) (iterator (quote d) d))]))
  (unit (import) (export decisions^)
        (define next-variable-decision (decision var))
        (define next-non-terminal-decision (decision nt))
        (define next-number-decision (decision num))
        (define next-string-decision (decision str))
        (define next-any-decision (decision any))
        (define next-sequence-decision (decision seq))))

(let ()
  (define-language lc
    (e (e e) x (λ (x) e) #:binds x e)
    (x (variable-except λ)))
  
  ;; Generate (λ (x) x)
  (test 
   (generate 
    lc e 1 0
    (decisions #:var (list (λ _ 'x) (λ _'x))
               #:nt (patterns '(λ (x) e)
                              '(variable-except λ)
                              'x
                              '(variable-except λ)))) 
   '(λ (x) x))
  
  ;; Generate pattern that's not a non-terminal
  (test 
   (generate 
    lc (x_1 x_1) 1 0
    (decisions #:var (list (λ _ 'x)))) 
   '(x x))
  
  ;; Minimum rhs is chosen with zero size
  (test 
   (let/ec k
     (generate 
      lc e 0 0
      (decisions #:nt (list (λ (prods . _) (k (map rhs-pattern prods))))))) 
   '(x))
  
  ;; Size decremented
  (let ([size 5])
    (test 
     (let/ec k
       (generate 
        lc e size 0
        (decisions #:nt (list (select-pattern 'x) (λ (p b s) (k s)))))) 
     (sub1 size))))

;; Detection of binding kludge
(let ()
  (define-language postfix
    (e (e e) x (e (x) λ) #:binds x e)
    (x (variable-except λ)))
  (test 
   (exn:fail-message 
     (generate 
      postfix e 2 0
      (decisions #:var (list (λ _ 'x) (λ _ 'y))
                 #:nt (patterns '(e (x) λ)
                                'x
                                '(variable-except λ)
                                '(variable-except λ)))))
   #rx"kludge"))

;; variable-except pattern
(let ()
  (define-language var
    (e (variable-except x y)))
  (test
   (generate
    var e 2 0
    (decisions #:nt (patterns '(variable-except x y))
               #:var (list (λ _ 'x) (λ _ 'y) (λ _ 'x) (λ _ 'z))))
   'z))

(let ()
  (define-language lang
    (e (number number ... "foo" ... "bar" #t ...)))
  (test
   (generate 
    lang e 2 0
    (decisions #:num (build-list 3 (λ (n) (λ (_) n)))
               #:seq (list (λ () 2) (λ () 3) (λ () 1))))
   `(0 1 2 "foo" "foo" "foo" "bar" #t)))

(let ()
  (define-language lc
    (e (λ (x ...) e) #:binds x e
       (e e)
       x)
    (x (variable-except λ)))
  
  ;; x and y bound in body
  (test 
   (let/ec k 
     (generate 
      lc e 10 0
      (decisions #:var (list (λ _ 'x) (λ _ 'y) (λ (c l b a) (k b)))
                 #:nt (patterns '(λ (x ...) e)
                                '(variable-except λ)
                                '(variable-except λ)
                                'x
                                '(variable-except λ))
                 #:seq (list (λ () 2)))))
   '(y x)))

(let ()
  (define-language lang (e (variable-prefix pf)))
  (test 
   (generate
    lang e 5 0
    (decisions #:var (list (λ _ 'x))
               #:nt (patterns '(variable-prefix pf))))
   'pfx))

(let ()
  (define-language lang (x variable literal))
  (test (is-nt? lang 'x) #t)
  (test (is-nt? lang 'x_1) #t)
  (test (is-nt? lang 'x_!_1) #t)
  (test (is-nt? lang 'y) #f))

(let ()
  (define-language lang
    (e number (e_1 e_2 e e_1 e_2)))
  (test
   (generate
    lang e 5 0
    (decisions #:nt (patterns '(e_1 e_2 e e_1 e_2) 
                              'number
                              'number
                              'number)
               #:num (list (λ _ 2) (λ _ 3) (λ _ 4))))
   '(2 3 4 2 3)))

(let ()
  (define-language lang
    (e (x x_1 x_1) #:binds x x_1
       (x variable_1) #:binds x variable_1)
    (x variable))
  (test
   (let/ec k
     (generate
      lang e 5 0
      (decisions #:var (list (λ _ 'x) (λ (c l b a) (k b)))
                 #:nt (patterns '(x x_1 x_1) 
                                'variable
                                'variable))))
   '(x))
  (test
   (let/ec k
     (generate
      lang e 5 0
      (decisions #:var (list (λ _ 'x) (λ (c l b a) (k b)))
                 #:nt (patterns '(x variable_1) 'variable))))
   '(x)))

(let ()
  (define-language lang
    (e (number_!_1 number_!_2 number_!_1 number_!_2)))
  (test
   (generate
    lang e 5 0
    (decisions #:nt (patterns '(number_!_1 number_!_2 number_!_1 number_!_2))
               #:num (list (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 2) (λ _ 3))))
   '(1 1 2 3)))

(let ()
  (define-language lang
    (a (b_!_1 b_!_1 b_!_1))
    (b 1 2))
  (test
   (exn:fail-message (generate lang a 5000 0))
   #rx"unable"))

(let ()
  (define-language lang
    (e (x_!_1 ...))
    (x variable))
  (test
   (generate
    lang e 5 0
    (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'y) (λ _ 'x) (λ _ 'y) (λ _ 'z))
               #:nt (patterns '(x_!_1 ...)
                              'variable
                              'variable
                              'variable
                              'variable
                              'variable
                              'variable)
               #:seq (list (λ _ 3))))
   '(x y z)))

(let ()
  (define-language lang
    (e string))
  (test
   (let/ec k 
     (generate 
      lang e 5 0 
      (decisions #:str (list (λ (c l a) (k (cons (sort c char<=?) (sort l string<=?))))))))
   (cons '(#\g #\i #\n #\r #\s #\t)
         '("string"))))

(let ()
  (define-language lang
    (a 43)
    (b (side-condition a_1 (odd? (term a_1))))
    (c (side-condition a_1 (even? (term a_1))))
    (d (side-condition (x_1 x_1 x) (not (eq? (term x_1) 'x))) #:binds x_1 x)
    (e (side-condition (x_1 x_!_2 x_!_2) (not (eq? (term x_1) 'x))))
    (x variable))
  (test (generate lang b 5 0) 43)
  (test (exn:fail-message (generate lang c 5 0))
        #rx"unable to generate")
  (test ; binding works for with side-conditions failure/retry
   (let/ec k
     (generate
      lang d 5 0
      (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'y) (λ (c l b a) (k b))))))
   '(y))
  (test ; mismatch patterns work with side-condition failure/retry
   (generate
    lang e 5 0
    (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'y) (λ _ 'y) (λ _ 'x) (λ _ 'y))))
   '(y x y))
  (test ; generate compiles side-conditions in pattern
   (generate lang (side-condition x_1 (not (eq? (term x_1) 'x))) 5 0
             (decisions #:var (list (λ _ 'x) (λ _ 'y))))
   'y))

(let ()
  (define-language lang
    (a (name x b))
    (b 4)
    (c (side-condition (name x d) (zero? (term x))))
    (d 2 1 0)
    (e ((side-condition (name d_1 d) (zero? (term d_1))) d_1))
    (f ((side-condition d_1 (zero? (term d_1))) (name d_1 d))))
  (test (generate lang a 5 0) 4)
  (test (generate lang c 5 0) 0)
  (test (generate lang e 5 0) '(0 0))
  (test (generate lang f 5 0) '(0 0)))

(let ()
  (define-language lang
    (a number (+ a a))
    (A hole (+ a A) (+ A a))
    (B (6 (hole h)))
    (C hole)
    (d (x (in-hole C y)) #:binds x y)
    (x variable)
    (y variable))
  (test 
   (generate 
    lang (in-hole A number ) 5 0
    (decisions 
     #:nt (patterns '(+ a A) '(+ a a) 'number 'number '(+ A a) 'hole '(+ a a) 'number 'number)
     #:num (build-list 5 (λ (x) (λ (_) x)))))
   '(+ (+ 0 1) (+ 2 (+ 3 4))))
  
  (test (generate lang (in-hole (in-hole (1 hole) hole) 5) 5 0) '(1 5))
  (test (generate lang hole 5 0) (term hole))
  (test (generate lang (hole h) 5 0) (term (hole h)))
  (test (generate lang (variable_1 (in-hole C variable_1)) 5 0
                  (decisions #:var (list (λ _ 'x) (λ _ 'y) (λ _ 'x))))
        '(x x))
  (test (generate lang (variable_!_1 (in-hole C variable_!_1) variable_!_1) 5 0
                  (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'y) (λ _ 'y) (λ _ 'z))))
        '(x y z))
  (test (let/ec k (generate lang d 5 0 (decisions #:var (list (λ _ 'x) (λ (c l b a) (k b))))))
        '(x)))

(let ()
  (define-language lc
      (e (e e) (+ e e) x v)
      (v (λ (x) e) number)
      (x variable-not-otherwise-mentioned))
  (test (generate lc x 5 0 (decisions #:var (list (λ _ 'λ) (λ _ '+) (λ _ 'x))))
        'x))

(let ()
  (define-language four 
    (e 4)
    (f 5))
  
  ;; `any' pattern
  (test (call-with-values (λ () (pick-any four (make-random (list 0 1)))) list)
        (list four 'f))
  (test (call-with-values (λ () (pick-any four (make-random (list 1)))) list)
        (list sexp 'sexp))
  (test (generate four any 5 0 (decisions #:any (list (λ _ (values four 'e))))) 4)
  (test (generate four any 5 0 
                  (decisions #:any (list (λ _ (values sexp 'sexp)))
                             #:nt (list (select-pattern '(sexp ...))
                                        (select-pattern 'string)
                                        (select-pattern 'string)
                                        (select-pattern 'string))
                             #:seq (list (λ _ 3))
                             #:str (list (λ _ "foo") (λ _ "bar") (λ _ "baz"))))
        '("foo" "bar" "baz")))

;; `hide-hole' pattern
(let ()
  (define-language lang
    (e (hide-hole (in-hole ((hide-hole hole) hole) 1))))
  (test (generate lang e 5 0) (term (hole 1))))

;; named ellipses
(let ()
  (define-language empty)
  (test 
   (generate empty (number ..._1 variable ..._2 number ..._1) 5 0
             (decisions #:seq (list (λ () 2) (λ () 3))
                        #:var (list (λ _ 'x) (λ _ 'y) (λ _ 'z))
                        #:num (build-list 4 (λ (x) (λ (_) x)))))
   '(0 1 x y z 2 3)))

(define (output-error-port thunk)
  (let ([port (open-output-string)])
    (parameterize ([current-error-port port])
      (thunk))
    (get-output-string port)))

(let ()
  (define-language lang
    (d 5)
    (e e 4))
  (test (check lang () 2 0 #f) "failed after 1 attempts: ()")
  (test (check lang () 2 0 #t) #t)
  (test (check lang ([x d] [y e]) 2 0 (and (eq? (term x) 5) (eq? (term y) 4))) #t)
  (test (check lang ([x d] [y e]) 2 0 #f) "failed after 1 attempts: ((x 5) (y 4))")
  (test (exn:fail-message (check lang ([x d]) 2 0 (error 'pred-raised)))
        #rx"term \\(\\(x 5\\)\\) raises"))

(print-tests-passed 'rg-test.ss)
