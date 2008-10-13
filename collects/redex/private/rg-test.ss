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
        '((e . (1 2 2)) (e-e . (0 2 2 1)) (x . (0)) (x-e . (1 2 2 2 2)) (x-x . (0)))))

(let ()
  (define-language lang
    (e (e e)))
  (test (to-table (find-base-cases lang))
        '((e . (inf)) (e-e . (0 inf inf)))))

(let ()
  (define-language lang
    (a 1 2 3)
    (b a (a_1 b_!_1)))
  (test (to-table (find-base-cases lang))
        '((a . (0 0 0)) (a-a . (0)) (a-b . (1)) (b . (1 2)) (b-b . (0)))))

(let ()
  (define-language lc
    (e (e e ...)
       (+ e e)
       x
       v)
    (v (λ (x) e)
       number)
    (x variable))
  (test (to-table (find-base-cases lc))
        '((e . (2 2 1 1)) (e-e . (0 2 2 2 2 2)) (e-v . (1))
          (v . (2 0)) (v-e . (2 2 2 2 1)) (v-v . (0 2))
          (x . (0)) (x-e . (2 2 2 2 1 3)) (x-v . (2 2)) (x-x . (0)))))

(let ()
  (define-language lang
    (e number x y)
    (x variable)
    (y y))
  (test (min-prods (car (compiled-lang-lang lang)) (find-base-cases lang))
        (list (car (nt-rhs (car (compiled-lang-lang lang)))))))

(define (make-random . nums)
  (let ([nums (box nums)])
    (λ ([m +inf.0])
      (cond [(null? (unbox nums)) (error 'make-random "out of numbers")]
            [(>= (car (unbox nums)) m) (error 'make-random "number too large")]
            [else (begin0 (car (unbox nums)) (set-box! nums (cdr (unbox nums))))]))))

(test (pick-from-list '(a b c) (make-random 1)) 'b)

(test (pick-number 3 (make-random .5)) 2)
(test (pick-number 109 (make-random 0 0 .5)) -6)
(test (pick-number 509 (make-random 0 0 1 .5 .25)) 3/7)
(test (pick-number 1009 (make-random 0 0 0 .5 1 .5)) 6.0)
(test (pick-number 2009 (make-random 0 0 0 0 2 .5 1 .5 0 0 .5))
      (make-rectangular 6.0 -6))

(let* ([lits '("bcd" "cbd")]
       [chars (sort (unique-chars lits) char<=?)])
  (test (pick-char 0 chars (make-random 1)) #\c)
  (test (pick-char 50 chars (make-random 1 1)) #\c)
  (test (pick-char 50 chars (make-random 0 65)) #\a)
  (test (pick-char 500 chars (make-random 0 1 65)) #\a)
  (test (pick-char 500 chars (make-random 0 0 3)) #\⇒)
  (test (pick-char 2000 chars (make-random 0 0 1 3)) #\⇒)
  (test (pick-char 2000 chars (make-random 0 0 0 1)) (integer->char #x4E01))
  (test (pick-char 50 chars (make-random 0 (- (char->integer #\_) #x20))) #\`)
  (test (random-string chars lits 3 0 (make-random 0 1)) "cbd")
  (test (random-string chars lits 3 0 (make-random 1 2 1 0)) "dcb")
  (test (pick-string chars lits 0 (make-random .5 1 2 1 0)) "dcb")
  (test (pick-var chars lits null 0 (make-random .01 1 2 1 0)) 'dcb)
  (test (pick-var chars lits '(x) 0 (make-random .5 0)) 'x)
  (test (pick-char 0 null (make-random 65)) #\a)
  (test (random-string null null 1 0 (make-random 65)) "a"))

(define-syntax exn:fail-message
  (syntax-rules ()
    [(_ expr)
     (with-handlers ([exn:fail? exn-message])
       (begin
         expr
         (let ()
           (define-struct exn-not-raised ())
           (make-exn-not-raised))))]))

(define (patterns . selectors) 
  (map (λ (selector) (λ (prods . _) (selector prods))) selectors))

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
                   #:num [num pick-number]
                   #:any [any pick-any]
                   #:seq [seq pick-sequence-length])
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
    (e (e e) x (λ (x) e))
    (x (variable-except λ)))
  
  ;; Generate (λ (x) x)
  (test 
   (generate/decisions 
    lc e 1 0
    (decisions #:var (list (λ _ 'x) (λ _'x))
               #:nt (patterns third first first first)))
   '(λ (x) x))
  
  ;; Generate pattern that's not a non-terminal
  (test 
   (generate/decisions 
    lc (x x x_1 x_1) 1 0
    (decisions #:var (list (λ _ 'x) (λ _ 'y)))) 
   '(x x y y))
  
  ;; Minimum rhs is chosen with zero size
  (test 
   (let/ec k
     (generate/decisions 
      lc e 0 0
      (decisions #:nt (list (λ (prods . _) (k (map rhs-pattern prods))))))) 
   '(x))
  
  ;; Size decremented
  (let ([size 5])
    (test 
     (let/ec k
       (generate/decisions 
        lc e size 0
        (decisions #:nt (list (λ (prods . _) (cadr prods)) (λ (p b s) (k s)))))) 
     (sub1 size))))

;; #:binds
(let ()
  (define-language lang
    (a (b c d) #:binds b c #:binds b d)
    (b variable)
    (c variable)
    (d variable))
  (let* ([x null]
         [prepend! (λ (c l b a) (begin (set! x (cons (car b) x)) 'x))])
    (test (begin
            (generate/decisions lang a 5 0 (decisions #:var (list (λ _ 'x) prepend! prepend!)))
            x)
          '(x x))))

;; Detection of binding kludge
(let ()
  (define-language postfix
    (e (e e) x (e (x) λ) #:binds x e)
    (x (variable-except λ)))
  (test 
   (exn:fail-message 
     (generate/decisions 
      postfix e 2 0
      (decisions #:var (list (λ _ 'x) (λ _ 'y))
                 #:nt (patterns third second first first))))
   #rx"kludge"))

;; variable-except pattern
(let ()
  (define-language var
    (e (variable-except x y)))
  (test
   (generate/decisions
    var e 2 0
    (decisions #:var (list (λ _ 'x) (λ _ 'y) (λ _ 'x) (λ _ 'z))))
   'z))

(let ()
  (define-language lang
    (a (number number ... "foo" ... "bar" #t ...))
    (b (number_1 ..._!_1 number_1 ..._1))
    (c (variable_1 ..._1 number_2 ..._1))
    (d (z_1 ... z_2 ..._!_1 (z_1 z_2) ...))
    (e (n_1 ..._!_1 n_2 ..._!_1 (n_1 n_2) ..._3))
    (f (n_1 ..._1 n_2 ..._2 n_2 ..._1))
    (g (z_1 ..._!_1 z_2 ... (z_1 z_2) ...))
    (n number)
    (z 4))
  (test
   (generate/decisions 
    lang a 2 0
    (decisions #:num (build-list 3 (λ (n) (λ (_) n)))
               #:seq (list (λ (_) 2) (λ (_) 3) (λ (_) 1))))
   `(0 1 2 "foo" "foo" "foo" "bar" #t))
  (test (generate/decisions lang b 5 0 (decisions #:seq (list (λ (_) 0))))
        null)
  (test (generate/decisions lang c 5 0 (decisions #:seq (list (λ (_) 0))))
        null)
  (test (generate/decisions lang d 5 0 (decisions #:seq (list (λ (_) 2))))
        '(4 4 4 4 (4 4) (4 4)))
  (test (exn:fail-message (generate lang e 5)) 
        #rx"generate: unable to generate pattern \\(n_1 ..._!_1 n_2 ..._!_1 \\(n_1 n_2\\) ..._3\\)")
  (test (generate/decisions lang f 5 0 (decisions #:seq (list (λ (_) 0)))) null)
  (test (generate/decisions lang ((0 ..._!_1) ... (1 ..._!_1) ...) 5 0
                  (decisions #:seq (list (λ (_) 2) (λ (_) 3) (λ (_) 4) (λ (_) 2) (λ (_) 3) (λ (_) 4)
                                         (λ (_) 2) (λ (_) 3) (λ (_) 4) (λ (_) 1) (λ (_) 3))))
        '((0 0 0) (0 0 0 0) (1 1 1)))
  (test (generate/decisions lang ((0 ..._!_1) ... (1 ..._!_1) ...) 5 0
                  (decisions #:seq (list (λ (_) 2) (λ (_) 3) (λ (_) 4) (λ (_) 2) (λ (_) 3) (λ (_) 5))))
        '((0 0 0) (0 0 0 0) (1 1 1) (1 1 1 1 1))))

(let ()
  (define-language lc
    (e (λ (x ...) e) #:binds x e
       (e e)
       x)
    (x (variable-except λ)))
  
  ;; x and y bound in body
  (test 
   (let/ec k 
     (generate/decisions 
      lc e 10 0
      (decisions #:var (list (λ _ 'x) (λ _ 'y) (λ (c l b a) (k b)))
                 #:nt (patterns first first first third first)
                 #:seq (list (λ (_) 2)))))
   '(y x)))

(let ()
  (define-language lang (e (variable-prefix pf)))
  (test 
   (generate/decisions
    lang e 5 0
    (decisions #:var (list (λ _ 'x))))
   'pfx))

(let ()
  (define-language lang (x variable literal))
  (test ((is-nt? lang) 'x) #t)
  (test ((is-nt? lang) 'y) #f))

(let ()
  (define-language lang
    (e number (e_1 e_2 e e_1 e_2)))
  (test
   (generate/decisions
    lang e 5 0
    (decisions #:nt (patterns second first first first)
               #:num (list (λ _ 2) (λ _ 3) (λ _ 4))))
   '(2 3 4 2 3)))

(let ()
  (define-language lang
    (e (x x_1 x_1) #:binds x x_1)
    (x variable))
  (test
   (let/ec k
     (generate/decisions
      lang e 5 0
      (decisions #:var (list (λ _ 'x) (λ (c l b a) (k b))))))
   '(x)))

(let ()
  (define-language lang
    (a (number_!_1 number_!_2 number_!_1))
    (b (c_!_1 c_!_1 c_!_1))
    (c 1 2))
  (test
   (generate/decisions
    lang a 5 0
    (decisions #:num (list (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 2))))
   '(1 1 2))
  (test
   (generate/decisions
    lang (number_!_1 number_!_2 number_!_1) 5 0
    (decisions #:num (list (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 2))))
   '(1 1 2))
  (test
   (exn:fail-message (generate lang b 5000))
   #rx"unable"))

(let ()
  (define-language lang
    (e string)
    (f foo bar))
  (test
   (let/ec k 
     (generate/decisions 
      lang e 5 0 
      (decisions #:str (list (λ (c l a) (k (cons (sort c char<=?) (sort l string<=?))))))))
   (cons '(#\a #\b #\f #\o #\r)
         '("bar" "foo"))))

(let ()
  (define-language lang
    (a 43)
    (b (side-condition a_1 (odd? (term a_1))))
    (c (side-condition a_1 (even? (term a_1))))
    (d (side-condition (x_1 x_1 x) (not (eq? (term x_1) 'x))) #:binds x_1 x)
    (e (side-condition (x_1 x_!_2 x_!_2) (not (eq? (term x_1) 'x))))
    (x variable))
  (test (generate lang b 5) 43)
  (test (generate lang (side-condition a (odd? (term a))) 5) 43)
  (test (exn:fail-message (generate lang c 5))
        #rx"unable to generate")
  (test ; binding works for with side-conditions failure/retry
   (let/ec k
     (generate/decisions
      lang d 5 0
      (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'y) (λ (c l b a) (k b))))))
   '(y))
  (test ; mismatch patterns work with side-condition failure/retry
   (generate/decisions
    lang e 5 0
    (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'y) (λ _ 'y) (λ _ 'x) (λ _ 'y))))
   '(y x y))
  (test ; generate compiles side-conditions in pattern
   (generate/decisions lang (side-condition x_1 (not (eq? (term x_1) 'x))) 5 0
             (decisions #:var (list (λ _ 'x) (λ _ 'y))))
   'y)
  (test ; bindings within ellipses collected properly
   (let/ec k
     (generate/decisions lang (side-condition (((number_1 3) ...) ...) (k (term ((number_1 ...) ...)))) 5 0
               (decisions #:seq (list (λ (_) 2) (λ (_) 3) (λ (_) 4))
                          #:num (build-list 7 (λ (n) (λ (_) n))))))
   '((0 1 2) (3 4 5 6))))

(let ()
  (define-language lang
    (a (name x b))
    (b 4)
    (c (side-condition (name x d) (zero? (term x))))
    (d 2 1 0)
    (e ((side-condition (name d_1 d) (zero? (term d_1))) d_1)))
  (test (generate lang a 5) 4)
  (test (generate lang c 5) 0)
  (test (generate lang e 5) '(0 0)))

(let ()
  (define-language lang
    (a number (+ a a))
    (A hole (+ a A) (+ A a))
    (C hole)
    (d (x (in-hole C y)) #:binds x y)
    (e ((in-hole (in-hole f (number_1 hole)) number_1) number_1)) 
    (f (in-hole C (number_1 hole)))
    (g (in-hole (side-condition (hole number_1) (zero? (term number_1))) number_2))
    (h ((in-hole i number_1) number_1))
    (i (number_1 (in-hole j (number_1 hole))))
    (j (in-hole (hole number_1) (number_1 hole)))
    (x variable)
    (y variable))
  
  (test 
   (generate/decisions 
    lang (in-hole A number ) 5 0
    (decisions 
     #:nt (patterns second second first first third first second first first)
     #:num (build-list 5 (λ (x) (λ (_) x)))))
   '(+ (+ 1 2) (+ 0 (+ 3 4))))
  
  (test (generate lang (in-hole (in-hole (1 hole) hole) 5) 5) '(1 5))
  (test (generate lang (hole 4) 5) (term (hole 4)))
  (test (generate/decisions lang (variable_1 (in-hole C variable_1)) 5 0
                  (decisions #:var (list (λ _ 'x) (λ _ 'y) (λ _ 'x))))
        '(x x))
  (test (generate/decisions lang (variable_!_1 (in-hole C variable_!_1)) 5 0
                  (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'x) (λ _ 'y))))
        '(x y))
  (test (let/ec k (generate/decisions lang d 5 0 (decisions #:var (list (λ _ 'x) (λ (c l b a) (k b))))))
        '(x))
  (test (generate/decisions lang e 5 0 (decisions #:num (list (λ _ 1) (λ _ 2))))
        '((2 (1 1)) 1))
  (test (generate/decisions lang g 5 0 (decisions #:num (list (λ _ 1) (λ _ 2) (λ _ 1) (λ _ 0))))
        '(1 0))
  (test (generate/decisions lang h 5 0 (decisions #:num (list (λ _ 1) (λ _ 2) (λ _ 3))))
        '((2 ((3 (2 1)) 3)) 1)))

(let ()
  (define-language lc
      (e (e e) (+ e e) x v)
      (v (λ (x) e) number)
      (x variable-not-otherwise-mentioned))
  (test (generate/decisions lc x 5 0 (decisions #:var (list (λ _ 'λ) (λ _ '+) (λ _ 'x))))
        'x))

(let ()
  (define-language four 
    (e 4)
    (f 5))
  (define-language empty)
  
  ;; `any' pattern
  (test (call-with-values (λ () (pick-any four (make-random 0 1))) list)
        (list four 'f))
  (test (call-with-values (λ () (pick-any four (make-random 1))) list)
        (list sexp 'sexp))
  (test (generate/decisions four any 5 0 (decisions #:any (list (λ _ (values four 'e))))) 4)
  (test (generate/decisions four any 5 0 
                  (decisions #:any (list (λ _ (values sexp 'sexp)))
                             #:nt (patterns fifth second second second)
                             #:seq (list (λ _ 3))
                             #:str (list (λ _ "foo") (λ _ "bar") (λ _ "baz"))))
        '("foo" "bar" "baz"))
  (test (generate/decisions empty any 5 0 (decisions #:nt (patterns first)
                                                     #:var (list (λ _ 'x))))
        'x))

;; `hide-hole' pattern
(let ()
  (define-language lang
    (e (hide-hole (in-hole ((hide-hole hole) hole) 1))))
  (test (generate lang e 5) (term (hole 1))))

(define (output-error-port thunk)
  (let ([port (open-output-string)])
    (parameterize ([current-error-port port])
      (thunk))
    (get-output-string port)))

;; `cross' pattern
(let ()
  (define-language lang
    (e x (e e) v)
    (v (λ (x) e))
    (x variable-not-otherwise-mentioned))
  (test (generate/decisions lang (cross e) 3 0 
                  (decisions #:nt (patterns fourth first first second first first first)
                             #:var (list (λ _ 'x) (λ _ 'y))))
        (term (λ (x) (hole y)))))

;; current-error-port-output : (-> (-> any) string)
(define (current-error-port-output thunk)
  (let ([p (open-output-string)])
    (parameterize ([current-error-port p])
      (thunk))
    (begin0
      (get-output-string p)
      (close-output-port p))))

;; check
(let ()
  (define-language lang
    (d 5)
    (e e 4))
  (test (current-error-port-output (λ () (check lang d 2 #f))) 
        "failed after 1 attempts:\n5\n")
  (test (check lang d #t) #t)
  (test (check lang (d e) 2 (and (eq? (term d) 5) (eq? (term e) 4))) #t)
  (test (check lang (d ...) 2 (zero? (modulo (foldl + 0 (term (d ...))) 5))) #t)
  (test (current-error-port-output (λ () (check lang (d e) 2 #f)))
        "failed after 1 attempts:\n(5 4)\n")
  (test (current-error-port-output (λ () (check lang d 2 (error 'pred-raised))))
        "failed after 1 attempts:\n5\n"))

;; check-metafunction
(let ()
  (define-language empty)
  (define-metafunction empty
    f : (side-condition number_1 (odd? (term number_1))) -> number
    [(f 1) 1]
    [(f 3) 'NaN])
  
  (define-metafunction empty
    g : number ... -> (any ...)
    [(g number_1 ... 1 number_2 ...) ()])
  
  (define-metafunction empty
    h : number -> number
    [(h any) any])
  
  (define-metafunction empty
    [(i any ...) (any ...)])
  
  ;; Dom(f) < Ctc(f)
  (test (current-error-port-output (λ () (check-metafunction f (decisions #:num (list (λ _ 2) (λ _ 5))))))
        "failed after 1 attempts:\n(5)\n")
  ;; Rng(f) > Codom(f)
  (test (current-error-port-output (λ () (check-metafunction f (decisions #:num (list (λ _ 3))))))
        "failed after 1 attempts:\n(3)\n")
  ;; LHS matches multiple ways
  (test (current-error-port-output (λ () (check-metafunction g (decisions #:num (list (λ _ 1) (λ _ 1))
                                                                          #:seq (list (λ _ 2))))))
        "failed after 1 attempts:\n(1 1)\n")
  ;; OK -- generated from Dom(h)
  (test (check-metafunction h) #t)
  ;; OK -- generated from pattern (any ...)
  (test (check-metafunction i) #t))

;; parse/unparse-pattern
(let-syntax ([test-match (syntax-rules () [(_ p x) (test (match x [p #t] [_ #f]) #t)])])
      (define-language lang (x variable))
      (let ([pattern '((x_1 number) ... 3)])
        (test-match (list 
                     (struct ellipsis 
                             ('... 
                              (list (struct binder ('x_1)) (struct binder ('number)))
                              _
                              (list (struct binder ('number)) (struct binder ('x_1)))))
                     3)
                    (parse-pattern pattern lang 'top-level))
        (test (unparse-pattern (parse-pattern pattern lang 'top-level)) pattern))
      (let ([pattern '((x_1 ..._1 x_2) ..._!_1)])
        (test-match (struct ellipsis 
                            ((struct mismatch (i_1 '..._!_1)) 
                             (list 
                              (struct ellipsis 
                                      ('..._1 
                                       (struct binder ('x_1))
                                       (struct class ('..._1))
                                       (list (struct binder ('x_1)))))
                              (struct binder ('x_2)))
                             _ 
                             (list (struct binder ('x_2)) '..._1 (struct class ('..._1)) (struct binder ('x_1)))))
                    (car (parse-pattern pattern lang 'grammar)))
        (test (unparse-pattern (parse-pattern pattern lang 'grammar)) pattern))
      (let ([pattern '((name x_1 x_!_2) ...)])
        (test-match (struct ellipsis 
                            ('... `(name x_1 ,(struct mismatch (i_2 'x_!_2))) _ 
                                  (list (struct binder ('x_1)) (struct mismatch (i_2 'x_!_2)))))
                    (car (parse-pattern pattern lang 'grammar)))
        (test (unparse-pattern (parse-pattern pattern lang 'grammar)) pattern))
      (let ([pattern '((x ...) ..._1)])
        (test-match (struct ellipsis 
                            ('..._1
                             (list 
                              (struct ellipsis 
                                      ('...
                                       (struct binder ('x))
                                       (struct class (c_1))
                                       (list (struct binder ('x))))))
                             _
                             (list (struct class (c_1)) (struct binder ('x)))))
                    (car (parse-pattern pattern lang 'top-level)))
        (test (unparse-pattern (parse-pattern pattern lang 'top-level)) pattern))
      (let ([pattern '((variable_1 ..._!_1) ...)])
        (test-match (struct ellipsis 
                            ('...
                             (list 
                              (struct ellipsis 
                                      ((struct mismatch (i_1 '..._!_1))
                                       (struct binder ('variable_1))
                                       (struct class (c_1))
                                       (list (struct binder ('variable_1))))))
                             _
                             (list (struct class (c_1)) (struct mismatch (i_1 '..._!_1)) (struct binder ('variable_1)))))
                    (car (parse-pattern pattern lang 'grammar)))
        (test (unparse-pattern (parse-pattern pattern lang 'grammar)) pattern))
      (test (parse-pattern '(cross x) lang 'grammar) '(cross x-x))
      (test (parse-pattern '(cross x) lang 'cross) '(cross x))
      (test (parse-pattern 'x lang 'grammar) 'x)
      (test (parse-pattern 'variable lang 'grammar) 'variable))

(let ()
  (define-language lang (x variable))
  (define-syntax test-class-reassignments
    (syntax-rules ()
      [(_ pattern expected)
       (test (to-table (class-reassignments (parse-pattern pattern lang 'top-level)))
             expected)]))
  
  (test-class-reassignments 
   '(x_1 ..._1 x_2 ..._2 x_2 ..._1)
   '((..._2 . ..._1)))
  (test-class-reassignments
   '((x_1 ..._1 x_1 ..._2) (x_2 ..._1 x_2 ..._2) x_3 ..._2)
   '((..._1 . ..._2) (..._2 . ..._2)))
  (test-class-reassignments
   '(x_1 ..._1 x ..._2 x_1 ..._2)
   '((..._1 . ..._2)))
  (test-class-reassignments
   '(x_1 ..._1 x_2 ..._2 (x_1 x_2) ..._3)
   '((..._1 . ..._3) (..._2 . ..._3)))
  (test-class-reassignments
   '((x_1 ..._1) ..._2 x_2 ..._3 (x_1 ..._4 x_2) ..._5)
   '((..._1 . ..._4) (..._2 . ..._5) (..._3 . ..._5)))
  (test-class-reassignments
   '((x_1 ..._1) ..._2 (x_1 ..._3) ..._4 (x_1 ..._5) ..._6)
   '((..._1 . ..._5) (..._2 . ..._6) (..._3 . ..._5) (..._4 . ..._6)))
  (test-class-reassignments
   '(x_1 ..._1 x_1 ..._2 x_2 ..._1 x_2 ..._4 x_2 ..._3)
   '((..._1 . ..._3) (..._2 . ..._3) (..._4 . ..._3)))
  (test 
   (hash-map 
    (class-reassignments (parse-pattern '(x_1 ... x_1 ..._!_1 x_1 ..._1) lang 'top-level)) 
    (λ (_ cls) cls))
   '(..._1 ..._1))
  (test-class-reassignments
   '((3 ..._1) ..._2 (4 ..._1) ..._3)
   '((..._2 . ..._3)))
  (test-class-reassignments
   '(x ..._1 x ..._2 variable ..._2 variable ..._3 variable_1 ..._3 variable_1 ..._4)
   '((..._1 . ..._4) (..._2 . ..._4) (..._3 . ..._4))))

(print-tests-passed 'rg-test.ss)
