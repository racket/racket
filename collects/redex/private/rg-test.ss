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

(test (pick-number 24 (make-random 1/5)) 3)
(test (pick-number 224 (make-random 0 0 1/5)) -5)
(test (pick-number 524 (make-random 0 0 1 1/5 1/5)) 3/4)
(test (pick-number 1624 (make-random 0 0 0 .5 1 .5)) 3.0)
(test (pick-number 2624 (make-random 0 0 0 0 1 1 1/5 1/5 2 .5 0 .5))
      (make-rectangular 7/8 -3.0))

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

(let ()
  (define-language L
    (a 5 (x a) #:binds x a)
    (b 4))
  (test (pick-nt 'a L '(x) 1 'dontcare)
        (nt-rhs (car (compiled-lang-lang L))))
  (test (pick-nt 'a L '(x) preferred-production-threshold 'dontcare (make-random 1))
        (nt-rhs (car (compiled-lang-lang L))))
  (let ([pref (car (nt-rhs (car (compiled-lang-lang L))))])
    (test (pick-nt 'a L '(x) preferred-production-threshold
                   (make-immutable-hash `((a ,pref)))
                   (make-random 0))
          (list pref)))
  (test (pick-nt 'b L null preferred-production-threshold #f)
        (nt-rhs (cadr (compiled-lang-lang L)))))

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
  (map (λ (selector) 
         (λ (name lang vars size pref-prods)
           (list (selector (nt-rhs (nt-by-name lang name))))))
       selectors))

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
                   #:seq [seq pick-sequence-length]
                   #:pref [pref pick-preferred-productions])
  (define-syntax decision
    (syntax-rules ()
      [(_ d) (if (procedure? d) (λ () d) (iterator (quote d) d))]))
  (unit (import) (export decisions^)
        (define next-variable-decision (decision var))
        (define next-non-terminal-decision (decision nt))
        (define next-number-decision (decision num))
        (define next-string-decision (decision str))
        (define next-any-decision (decision any))
        (define next-sequence-decision (decision seq))
        (define next-pref-prods-decision (decision pref))))

(define-syntax generate-term/decisions
  (syntax-rules ()
    [(_ lang pat size attempt decisions)
     (parameterize ([generation-decisions decisions])
       (generate-term lang pat size #:attempt attempt))]))

(let ()
  (define-language lc
    (e (e e) x (λ (x) e))
    (x (variable-except λ)))
  
  ;; Generate (λ (x) x)
  (test 
   (generate-term/decisions 
    lc e 1 0
    (decisions #:var (list (λ _ 'x) (λ _'x))
               #:nt (patterns third first first first)))
   '(λ (x) x))
  
  ;; Generate pattern that's not a non-terminal
  (test 
   (generate-term/decisions 
    lc (x x x_1 x_1) 1 0
    (decisions #:var (list (λ _ 'x) (λ _ 'y)))) 
   '(x x y y))
  
  ; After choosing (e e), size decremented forces each e to x.
  (test
   (generate-term/decisions 
    lc e 1 0
    (decisions #:nt (patterns first)
               #:var (list (λ _ 'x) (λ _ 'y))))
   '(x y)))

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
            (generate-term/decisions
             lang a 5 0
             (decisions #:var (list (λ _ 'x) prepend! prepend!)))
            x)
          '(x x))))

;; Detection of binding kludge
(let ()
  (define-language postfix
    (e (e e) x (e (x) λ) #:binds x e)
    (x (variable-except λ)))
  (test 
   (exn:fail-message 
     (generate-term/decisions 
      postfix e 2 0
      (decisions #:var (list (λ _ 'x) (λ _ 'y))
                 #:nt (patterns third second first first))))
   #rx"kludge"))

;; variable-except pattern
(let ()
  (define-language var
    (e (variable-except x y)))
  (test
   (generate-term/decisions
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
   (generate-term/decisions 
    lang a 2 0
    (decisions #:num (build-list 3 (λ (n) (λ (_) n)))
               #:seq (list (λ (_) 2) (λ (_) 3) (λ (_) 1))))
   `(0 1 2 "foo" "foo" "foo" "bar" #t))
  (test (generate-term/decisions lang b 5 0 (decisions #:seq (list (λ (_) 0))))
        null)
  (test (generate-term/decisions lang c 5 0 (decisions #:seq (list (λ (_) 0))))
        null)
  (test (generate-term/decisions lang d 5 0 (decisions #:seq (list (λ (_) 2))))
        '(4 4 4 4 (4 4) (4 4)))
  (test (exn:fail-message (generate-term lang e 5)) 
        #rx"generate: unable to generate pattern e")
  (test (generate-term/decisions lang f 5 0 (decisions #:seq (list (λ (_) 0)))) null)
  (test (generate-term/decisions 
         lang ((0 ..._!_1) ... (1 ..._!_1) ...) 5 0
         (decisions #:seq (list (λ (_) 2) (λ (_) 3) (λ (_) 4) (λ (_) 2) (λ (_) 3) (λ (_) 4)
                                (λ (_) 2) (λ (_) 3) (λ (_) 4) (λ (_) 1) (λ (_) 3))))
        '((0 0 0) (0 0 0 0) (1 1 1)))
  (test (generate-term/decisions 
         lang ((0 ..._!_1) ... (1 ..._!_1) ...) 5 0
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
     (generate-term/decisions 
      lc e 10 0
      (decisions #:var (list (λ _ 'x) (λ _ 'y) (λ (c l b a) (k b)))
                 #:nt (patterns first first first third first)
                 #:seq (list (λ (_) 2)))))
   '(y x)))

(let ()
  (define-language lang (e (variable-prefix pf)))
  (test 
   (generate-term/decisions
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
   (generate-term/decisions
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
     (generate-term/decisions
      lang e 5 0
      (decisions #:var (list (λ _ 'x) (λ (c l b a) (k b))))))
   '(x)))

(let ()
  (define-language lang
    (a (number_!_1 number_!_2 number_!_1))
    (b (c_!_1 c_!_1 c_!_1))
    (c 1 2))
  (test
   (generate-term/decisions
    lang a 5 0
    (decisions #:num (list (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 2))))
   '(1 1 2))
  (test
   (generate-term/decisions
    lang (number_!_1 number_!_2 number_!_1) 5 0
    (decisions #:num (list (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 2))))
   '(1 1 2))
  (test
   (exn:fail-message (generate-term lang b 5000))
   #rx"unable"))

(let ()
  (define-language lang
    (e string)
    (f foo bar))
  (test
   (let/ec k 
     (generate-term/decisions 
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
  (test (generate-term lang b 5) 43)
  (test (generate-term lang (side-condition a (odd? (term a))) 5) 43)
  (test (exn:fail-message (generate-term lang c 5))
        #rx"unable to generate")
  (test ; binding works for with side-conditions failure/retry
   (let/ec k
     (generate-term/decisions
      lang d 5 0
      (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'y) (λ (c l b a) (k b))))))
   '(y))
  (test ; mismatch patterns work with side-condition failure/retry
   (generate-term/decisions
    lang e 5 0
    (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'y) (λ _ 'y) (λ _ 'x) (λ _ 'y))))
   '(y x y))
  (test ; generate compiles side-conditions in pattern
   (generate-term/decisions 
    lang (side-condition x_1 (not (eq? (term x_1) 'x))) 5 0
    (decisions #:var (list (λ _ 'x) (λ _ 'y))))
   'y)
  (test ; bindings within ellipses collected properly
   (let/ec k
     (generate-term/decisions 
      lang (side-condition (((number_1 3) ...) ...) (k (term ((number_1 ...) ...)))) 5 0
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
  (test (generate-term lang a 5) 4)
  (test (generate-term lang c 5) 0)
  (test (generate-term lang e 5) '(0 0)))

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
   (generate-term/decisions 
    lang (in-hole A number ) 5 0
    (decisions 
     #:nt (patterns second second first first third first second first first)
     #:num (build-list 5 (λ (x) (λ (_) x)))))
   '(+ (+ 1 2) (+ 0 (+ 3 4))))
  
  (test (generate-term lang (in-hole (in-hole (1 hole) hole) 5) 5) '(1 5))
  (test (generate-term lang (hole 4) 5) (term (hole 4)))
  (test (generate-term/decisions 
         lang (variable_1 (in-hole C variable_1)) 5 0
         (decisions #:var (list (λ _ 'x) (λ _ 'y) (λ _ 'x))))
        '(x x))
  (test (generate-term/decisions 
         lang (variable_!_1 (in-hole C variable_!_1)) 5 0
         (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'x) (λ _ 'y))))
        '(x y))
  (test (let/ec k 
          (generate-term/decisions lang d 5 0 (decisions #:var (list (λ _ 'x) (λ (c l b a) (k b))))))
        '(x))
  (test (generate-term/decisions lang e 5 0 (decisions #:num (list (λ _ 1) (λ _ 2))))
        '((2 (1 1)) 1))
  (test (generate-term/decisions lang g 5 0 (decisions #:num (list (λ _ 1) (λ _ 2) (λ _ 1) (λ _ 0))))
        '(1 0))
  (test (generate-term/decisions lang h 5 0 (decisions #:num (list (λ _ 1) (λ _ 2) (λ _ 3))))
        '((2 ((3 (2 1)) 3)) 1)))

(let ()
  (define-language lc
      (e (e e) (+ e e) x v)
      (v (λ (x) e) number)
      (x variable-not-otherwise-mentioned))
  (test (generate-term/decisions lc x 5 0 (decisions #:var (list (λ _ 'λ) (λ _ '+) (λ _ 'x))))
        'x))

(let ()
  (define-language four 
    (e 4)
    (f 5))
  (define-language empty)
  
  ;; `any' pattern
  (let ([four (prepare-lang four)]
        [sexp (prepare-lang sexp)])
    (test (call-with-values (λ () (pick-any four sexp (make-random 0 1))) list)
          (list four 'f))
    (test (call-with-values (λ () (pick-any four sexp (make-random 1))) list)
          (list sexp 'sexp)))
  (test (generate-term/decisions 
         four any 5 0 (decisions #:any (list (λ (lang sexp) (values lang 'e))))) 4)
  (test (generate-term/decisions
         four any 5 0 
         (decisions #:any (list (λ (lang sexp) (values sexp 'sexp)))
                    #:nt (patterns fifth second second second)
                    #:seq (list (λ _ 3))
                    #:str (list (λ _ "foo") (λ _ "bar") (λ _ "baz"))))
        '("foo" "bar" "baz"))
  (test (generate-term/decisions
         empty any 5 0 (decisions #:nt (patterns first)
                                  #:var (list (λ _ 'x))))
        'x))

;; `hide-hole' pattern
(let ()
  (define-language lang
    (e (hide-hole (in-hole ((hide-hole hole) hole) 1))))
  (test (generate-term lang e 5) (term (hole 1))))

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
  (test (generate-term/decisions
         lang (cross e) 3 0 
         (decisions #:nt (patterns fourth first first second first first first)
                    #:var (list (λ _ 'x) (λ _ 'y))))
        (term (λ (x) (hole y)))))

;; preferred productions
(let ([make-pick-nt (λ opt (λ req (apply pick-nt (append req opt))))])
  (define-language L
    (e (+ e e) (* e e) 7))
  (let ([pats (λ (L) (nt-rhs (car (compiled-lang-lang (parse-language L)))))])
    (test 
     (generate-term/decisions
      L e 2 preferred-production-threshold
      (decisions #:pref (list (λ (L) (make-immutable-hash `((e ,(car (pats L)))))))
                 #:nt (make-pick-nt (make-random 0 0 0))))
     '(+ (+ 7 7) (+ 7 7)))
    (test
     (generate-term/decisions
      L any 2 preferred-production-threshold
      (decisions #:nt (patterns first)
                 #:var (list (λ _ 'x))
                 #:any (list (λ (lang sexp) (values sexp 'sexp)))))
     'x)
    (test
     (generate-term/decisions
      L any 2 preferred-production-threshold
      (decisions #:pref (list (λ (L) (make-immutable-hash `((e ,(car (pats L)))))))
                 #:nt (make-pick-nt (make-random 0 0 0))
                 #:any (list (λ (lang sexp) (values lang 'e)))))
     '(+ (+ 7 7) (+ 7 7)))
    (test
     (let ([generated null])
       (check-reduction-relation
        (reduction-relation L (--> e e))
        (λ (t) (set! generated (cons t generated)))
        #:decisions (decisions #:nt (make-pick-nt (make-random)
                                                  (λ (att rand) #t))
                               #:pref (list (λ (_) 'dontcare)
                                            (λ (_) 'dontcare)
                                            (λ (_) 'dontcare)
                                            (λ (L) (make-immutable-hash `((e ,(car (pats L))))))
                                            (λ (L) (make-immutable-hash `((e ,(cadr (pats L))))))))
        #:attempts 5)
       generated)
     '((* 7 7) (+ 7 7) 7 7 7))))

;; output : (-> (-> void) string)
(define (output thunk)
  (let ([p (open-output-string)])
    (parameterize ([current-output-port p])
      (unless (void? (thunk))
        (error 'output "expected void result")))
    (begin0
      (get-output-string p)
      (close-output-port p))))

;; redex-check
(let ()
  (define-language lang
    (d 5)
    (e e 4)
    (n number))
  (test (output (λ () (redex-check lang d #f))) 
        "counterexample found after 1 attempts:\n5\n")
  (test (output (λ () (redex-check lang d #t))) "")
  (test (output (λ () (redex-check lang (d e) (and (eq? (term d) 5) (eq? (term e) 4)) #:attempts 2)))
        "")
  (test (output (λ () (redex-check lang (d ...) (zero? (modulo (foldl + 0 (term (d ...))) 5)) #:attempts 2)))
        "")
  (test (output (λ () (redex-check lang (d e) #f)))
        "counterexample found after 1 attempts:\n(5 4)\n")
  (let* ([p (open-output-string)]
         [m (parameterize ([current-error-port p])
              (with-handlers ([exn:fail? exn-message])
                (redex-check lang d (error 'pred-raised))
                'no-exn-raised))])
    (test m "error: pred-raised")
    (test (get-output-string p) #rx"checking 5 raises.*\n$")
    (close-output-port p))
  (test (parameterize ([check-randomness (make-random 0 0)])
          (output
           (λ ()
             (redex-check lang n (eq? 42 (term n)) 
                          #:attempts 1
                          #:source (reduction-relation lang (--> 42 x))))))
        "")
  (test (output
         (λ ()
           (parameterize ([check-randomness (make-random 0 0)])
             (redex-check lang n (eq? 42 (term n)) 
                          #:attempts 1
                          #:source (reduction-relation lang (--> 0 x z))))))
        "counterexample found (z) after 1 attempts:\n0\n")
  (test (output
         (λ ()
           (parameterize ([check-randomness (make-random 1)])
             (redex-check lang d (eq? 42 (term n)) 
                          #:attempts 1
                          #:source (reduction-relation lang (--> 0 x z))))))
        "counterexample found after 1 attempts:\n5\n")
  (test (let ([r (reduction-relation lang (--> 0 x z))])
          (output 
           (λ ()
             (redex-check lang n (number? (term n)) 
                          #:attempts 10
                          #:source r))))
        "")
  (let ()
    (define-metafunction lang
      [(mf 0) 0]
      [(mf 42) 0])
    (test (parameterize ([check-randomness (make-random 0 1)])
            (output 
             (λ ()
               (redex-check lang (n) (eq? 42 (term n)) 
                            #:attempts 1
                            #:source mf))))
          ""))
  (let ()
    (define-language L)
    (test (with-handlers ([exn:fail? exn-message])
            (redex-check lang any #t #:source (reduction-relation L (--> 1 1))))
          #rx"language for secondary source"))
  (let ()
    (test (with-handlers ([exn:fail? exn-message])
            (redex-check lang n #t #:source (reduction-relation lang (--> x 1))))
          #rx"x does not match n"))
  
  (let ([stx-err (λ (stx)
                   (with-handlers ([exn:fail:syntax? exn-message])
                     (expand stx)
                     'no-syntax-error))])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval '(require "../reduction-semantics.ss"
                      "rg.ss"))
      (eval '(define-language empty))
      (test (stx-err '(redex-check empty any #t #:typo 3))
            #rx"redex-check: bad keyword syntax")
      (test (stx-err '(redex-check empty any #t #:attempts 3 #:attempts 4))
            #rx"bad keyword syntax")
      (test (stx-err '(redex-check empty any #t #:attempts))
            #rx"bad keyword syntax")
      (test (stx-err '(redex-check empty any #t #:attempts 3 4))
            #rx"bad keyword syntax")
      (test (stx-err '(redex-check empty any #t #:source #:attempts))
            #rx"bad keyword syntax"))))

;; check-metafunction-contract
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
  (test (output 
         (λ () 
           (parameterize ([generation-decisions 
                           (decisions #:num (list (λ _ 2) (λ _ 5)))])
             (check-metafunction-contract f))))
        "counterexample found after 1 attempts:\n(5)\n")
  ;; Rng(f) > Codom(f)
  (test (output
         (λ () 
           (parameterize ([generation-decisions
                           (decisions #:num (list (λ _ 3)))])
             (check-metafunction-contract f))))
        "counterexample found after 1 attempts:\n(3)\n")
  ;; LHS matches multiple ways
  (test (output
         (λ () 
           (parameterize ([generation-decisions
                           (decisions #:num (list (λ _ 1) (λ _ 1))
                                      #:seq (list (λ _ 2)))])
             (check-metafunction-contract g))))
        "counterexample found after 1 attempts:\n(1 1)\n")
  ;; OK -- generated from Dom(h)
  (test (output (λ () (check-metafunction-contract h))) "")
  ;; OK -- generated from pattern (any ...)
  (test (output (λ () (check-metafunction-contract i #:attempts 5))) ""))

;; check-reduction-relation
(let ()
  (define-language L
    (e (+ e ...) number)
    (E (+ number ... E* e ...))
    (E* hole E*)
    (n 4))
  
  (let ([generated null]
        [R (reduction-relation
            L
            (==> (+ number ...) whatever)
            (--> (side-condition number (even? (term number))) whatever)
            with
            [(--> (in-hole E a) whatever)
             (==> a b)])])
    (test (begin
            (check-reduction-relation 
             R (λ (term) (set! generated (cons term generated)))
             #:decisions (decisions #:seq (list (λ _ 0) (λ _ 0) (λ _ 0))
                                    #:num (list (λ _ 1) (λ _ 1) (λ _ 0)))
             #:attempts 1)
            generated)
          (reverse '((+ (+)) 0))))
  
  (let ([S (reduction-relation L (--> 1 2 name) (--> 3 4))])
    (test (output (λ () (check-reduction-relation S (λ (x) #t) #:attempts 1))) "")
    (test (output 
           (λ () (check-reduction-relation S (λ (x) #f))))
          "counterexample found after 1 attempts with name:\n1\n")
    (test (output 
           (λ () (check-reduction-relation S (curry eq? 1))))
          "counterexample found after 1 attempts with unnamed:\n3\n"))
  
  (let ([T (reduction-relation
            L
            (==> number number
                 (where num number)
                 (side-condition (eq? (term num) 4))
                 (where numb num)
                 (side-condition (eq? (term numb) 4)))
            with
            [(--> (9 a) b)
             (==> a b)])])
    (test (output
           (λ ()
             (check-reduction-relation 
              T (curry equal? '(9 4)) 
              #:attempts 1
              #:decisions (decisions #:num (build-list 5 (λ (x) (λ _ x)))))))
          "")))

; check-metafunction
(let ()
  (define-language empty)
  (define-metafunction empty
    [(m 1) whatever]
    [(m 2) whatever])
  (let ([generated null])
    (test (begin
            (check-metafunction m (λ (t) (set! generated (cons t generated))) #:attempts 1)
            generated) 
          (reverse '((1) (2)))))
  (test (output (λ () (check-metafunction m (λ (_) #t)))) "")
  (test (output (λ () (check-metafunction m (curry eq? 1))))
        #rx"counterexample found after 1 attempts with clause #1")
  (test (with-handlers ([exn:fail:contract? exn-message])
          (check-metafunction m #t #:attempts 'NaN))
        #rx"check-metafunction: expected"))

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
