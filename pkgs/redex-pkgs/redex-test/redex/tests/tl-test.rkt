#lang racket
  (require "test-util.rkt"
           redex/reduction-semantics
           (only-in redex/private/matcher make-bindings make-bind)
           racket/match
           racket/trace
           redex/private/struct)
  
  (reset-count)

  (define-namespace-anchor this-namespace)
  (parameterize ([current-namespace syn-err-test-namespace])
    (eval (quote-syntax
           (define-language syn-err-lang
             (M (M M)
                number)
             (E hole
                (E M)
                (number E))
             (X (number any)
                (any number))
             (Q (Q ...)
                variable)
             (UN (add1 UN)
                 zero)))))
  
;                                                          
;                                                          
;    ;;                                                    
;     ;                                                    
;     ;     ;;;  ;; ;;    ;; ;;;;  ;;   ;;;    ;; ;;  ;;;  
;     ;    ;   ;  ;;  ;  ;  ;;  ;   ;  ;   ;  ;  ;;  ;   ; 
;     ;     ;;;;  ;   ;  ;   ;  ;   ;   ;;;;  ;   ;  ;;;;; 
;     ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;     
;     ;    ;   ;  ;   ;  ;   ;  ;  ;;  ;   ;  ;   ;  ;     
;   ;;;;;   ;;;;;;;; ;;;  ;;;;   ;; ;;  ;;;;;  ;;;;   ;;;; 
;                            ;                    ;        
;                         ;;;                  ;;;         
;                                                          
;                                                          

  
  (define-language empty-language)
  
  (define-language grammar
    (M (M M)
       number)
    (E hole
       (E M)
       (number E))
    (X (number any)
       (any number))
    (Q (Q ...)
       variable)
    (UN (add1 UN)
        zero))
  
  (test (let ([m (redex-match 
                  empty-language
                  (side-condition (any_1 ...) #t)
                  '())])
          (and m
               (= 1 (length m))
               (match-bindings (car m))))
        (list (make-bind 'any_1 '())))
  
  (test (pair? (redex-match grammar M '(1 1))) #t)
  (test (pair? (redex-match grammar M '(1 1 1))) #f)
  (test (pair? (redex-match grammar
                            (side-condition (M_1 M_2) (equal? (term M_1) (term M_2)))
                            '(1 1)))
        #t)
  (test (pair? (redex-match grammar
                           (side-condition (M_1 M_2) (equal? (term M_1) (term M_2))) 
                           '(1 2)))
        #f)
  
  (test (pair? ((redex-match grammar M) '(1 1)))
        #t)
  
  (test (pair? (redex-match grammar (name not-an-nt_subscript 1) 1)) #t)

  ;; next 3: test naming of subscript-less non-terminals
  (test (pair? (redex-match grammar (M M) (term (1 1)))) #t)
  (test (pair? (redex-match grammar (M M) (term (1 2)))) #f)
  (test (pair? (redex-match grammar (M_1 M_2) (term (1 2)))) #t)
  
  (define-language base-grammar
    (q 1)
    (e (+ e e) number)
    (x (variable-except +)))
  
  (define-extended-language extended-grammar
    base-grammar 
    (e .... (* e e))
    (x (variable-except + *))
    (r 2))
  
  (test (pair? (redex-match extended-grammar e '(+ 1 1))) #t)
  (test (pair? (redex-match extended-grammar e '(* 2 2))) #t)
  (test (pair? (redex-match extended-grammar r '2)) #t)
  (test (pair? (redex-match extended-grammar q '1)) #t)
  (test (pair? (redex-match extended-grammar x '*)) #f)
  (test (pair? (redex-match extended-grammar x '+)) #f)
  (test (pair? (redex-match extended-grammar e '....)) #f)
  
  ;; make sure that `language' with a four period ellipses signals an error
  (test (regexp-match #rx"[.][.][.][.]" (with-handlers ([exn? exn-message]) 
                                          (let ()
                                            (define-language x (e ....))
                                            12)))
        '("...."))

  (let ()
    ; non-terminals added by extension can have underscores
    (define-extended-language L base-grammar
      (z () (1 z_1 z_1)))
    (test (redex-match L z (term (1 () (1 () ())))) #f))
  
  ;; test multiple variable non-terminals
  (let ()
    (define-language lang
      ((l m) (l m) x)
      (x variable-not-otherwise-mentioned))
    (test (pair? (redex-match lang m (term x)))
          #t))
  
  ;; test multiple variable non-terminals
  (let ()
    (define-language lang
      ((l m) (l m) x)
      (x variable-not-otherwise-mentioned))
    (test (pair? (redex-match lang l (term x)))
          #t))
  
  (let ()
    (define-language L
      [Cv (name n variable-not-otherwise-mentioned)])
    (test (redex-match L Cv (term ())) #f)
    (test (pair? (redex-match L Cv (term x))) #t))
  
  (let ()
    (define-language lang
      ((x y) 1 2 3))
    (define-extended-language lang2 lang
      (x .... 4))
    (test (pair? (redex-match lang2 x 4)) #t)
    (test (pair? (redex-match lang2 y 4)) #t)
    (test (pair? (redex-match lang2 x 1)) #t)
    (test (pair? (redex-match lang2 y 2)) #t))
  
  ;; test that the variable "e" is not bound in the right-hand side of a side-condition
  ;; this one signaled an error at some point
  (let ()
    (define-language bad
      (e 2 (side-condition (e) #t)))
    (test (pair? (redex-match bad e '(2)))
          #t))

  ;; test that the variable "e" is not bound in the right-hand side of a side-condition
  ;; this one tests to make sure it really isn't bound
  (let ([x #f])
    (define-language bad
      (e 2 (side-condition (e) (set! x (term e)))))
    (redex-match bad e '(2))
    (test x 'e))
  
  ;; test multiple variable non-terminals being extended
  (let ()
    (define-language lang
      ((x y) 1 2 3))
    (define-extended-language lang2 lang
      (x .... 4))
    (test (pair? (redex-match lang2 x 4)) #t)
    (test (pair? (redex-match lang2 y 4)) #t)
    (test (pair? (redex-match lang2 x 1)) #t)
    (test (pair? (redex-match lang2 y 2)) #t))
  
  ;; test multiple variable non-terminals in an extended language
  (let ()
    (define-language lang
      ((x y) 1 2 3))
    (define-extended-language lang2 lang
      ((z w) 5 6 7))
    (test (pair? (redex-match lang2 z 5)) #t)
    (test (pair? (redex-match lang2 w 6)) #t))
  
  ;; test cases that ensure that extending any one of a
  ;; multiply defined non-terminal gets extended properly
  (let ()
    (define-language iswim
      ((V U W) AA))

    (define-extended-language iswim-cont
      iswim
      (W .... QQ))

    (test (pair? (redex-match iswim-cont U (term QQ)))
          #t))
  
  (let ()
    (define-language iswim
      ((V U W) AA))

    (define-extended-language iswim-cont
      iswim
      (W .... QQ))

    (test (pair? (redex-match iswim-cont V (term QQ)))
          #t)
    (test (pair? (redex-match iswim-cont U (term QQ)))
          #t)
    (test (pair? (redex-match iswim-cont W (term QQ)))
          #t))
  
  (let ()
    (define-language iswim
      ((V U W) AA))
    
    (define-extended-language iswim-cont
      iswim
      (V .... QQ))
    
    (test (pair? (redex-match iswim-cont V (term QQ)))
          #t)
    (test (pair? (redex-match iswim-cont U (term QQ)))
          #t)
    (test (pair? (redex-match iswim-cont W (term QQ)))
          #t))
  
  (let ()
    (define-language okay
      [(X Y) z])
    
    (define-extended-language replace-both
      okay
      [(X Y) q])
    
    ;; this test ran into an infinite loop in an earlier version of redex.
    (test (redex-match replace-both X (term explode)) #f))
  
  (test (with-handlers ([exn? exn-message])
          (let () 
            (define-language main
              [(X Y) z])
            (define-extended-language new
              main
              [(X Y Z) q])
            (void)))
        "define-extended-language: new language extends old non-terminal X and also adds new shortcut Z")
  
  (test (with-handlers ([exn? exn-message])
          (let () 
            (define-language main
              [(X Y) z]
              [(P Q) w])
            (define-extended-language new
              main
              [(X P) q])
            (void)))
        "define-extended-language: new language does not have the same non-terminal aliases as the old, non-terminal P was not in the same group as X in the old language")
  
  ;; underscores in literals
  (let ()
    (define-language L
      (x (variable-except a_b))
      (y (variable-prefix a_b)))
    (test (pair? (redex-match L x (term a_c))) #t)
    (test (pair? (redex-match L y (term a_bc))) #t))
  
  ; underscores allowed on built-in non-terminals and names bound
  (let ([m (redex-match 
            grammar 
            (any_1 number_1 natural_1 integer_1
                   real_1 string_1 variable_1
                   variable-not-otherwise-mentioned_1)
            '(1 2 3 4 5 "s" s t))])
    (test (if m
              (map bind-exp
                   (sort (match-bindings (car m))
                         string<=?
                         #:key (compose symbol->string bind-name)))
              '())
          '(1 4 3 2 5 "s" t s)))

  (let ()
    (define-language L
      (e (e e) number))
    ;; not a syntax error since first e is not a binder
    (test (pair? (redex-match L ((cross e) e ...) (term ((hole 2) 1)))) #t))
  
  ;; match structures do not report ..._x bindings
  (test (map match-bindings (redex-match grammar (a ..._1) (term (a a a))))
        '(()))
  
  (define-syntax (test-match stx)
    (syntax-case stx ()
      [(_ actual (((var val) ...) ...))
       (syntax/loc stx
         (test (apply
                set
                (for/list ([match actual])
                  (for/list ([bind (match-bindings match)])
                    (list (bind-name bind) (bind-exp bind)))))
               (apply set (list (list (list 'var (term val)) ...) ...))))]))
  
  ;; cross
  (let ()
    (define-language L
      (e (e e)
         (cont (hide-hole E))
         number
         x)
      (E hole
         (e ... E e ...))
      (x variable-not-otherwise-mentioned))
    (test-match 
     (redex-match 
      L 
      (in-hole (cross e) e)
      (term (cont (1 hole))))
     (((e (cont (1 hole))))
      ((e 1)))))
  (let ()
    (define-language L
      (e (e e ...)
         x
         v)
      (v (λ (x ...) e)
         cont-val
         number)
      (cont-val (cont (hide-hole E)))
      (E hole
         (in-hole L E))
      (L (v ... hole e ...))
      (x variable-not-otherwise-mentioned))
    
    ;; no "found two holes" error
    (test (redex-match L (cross e) (term (cont ((λ (x) x) hole)))) #f)
    
    (test-match 
     (redex-match 
      L 
      (in-hole (cross e) e)
      (term ((cont ((λ (x) x) hole)) (λ (y) y))))
     (((e x))
      ((e ((cont ((λ (x) x) hole)) (λ (y) y))))
      ((e y))
      ((e (λ (y) y)))
      ((e (cont ((λ (x) x) hole)))))))
  
  ;; test caching
  (let ()
    (define match? #t)
    
    (define-language lang
      (x (side-condition any match?)))
    
    (test (pair? (redex-match lang x 1)) #t)
    (set! match? #f)
    (test (pair? (redex-match lang x 1)) #t)
    (parameterize ([caching-enabled? #f])
      (test (pair? (redex-match lang x 1)) #f)))
  
  
  (let ()
    (define sc-eval-count 0)
    (define-language lang
      (x (side-condition any (begin (set! sc-eval-count (+ sc-eval-count 1))
                                    #t))))
    
    (redex-match lang x 1)
    (redex-match lang x 1)
    (parameterize ([caching-enabled? #f])
      (redex-match lang x 1))
    (test sc-eval-count 2))
  
  (let ()
    (define rhs-eval-count 0)
    (define-metafunction empty-language
      [(f any) ,(begin (set! rhs-eval-count (+ rhs-eval-count 1))
                       1)])
    
    (term (f 1))
    (term (f 1))
    (parameterize ([caching-enabled? #f])
      (term (f 1)))
    (test rhs-eval-count 2))
  
  (let ()
    (define-language L)
    (define-extended-language E L
      (v ((bar X_1) X_1))
      ((X Y) any))
    (test (and (redex-match E v (term ((bar 1) 1))) #t) #t)
    (test (redex-match E v (term ((bar 1) 2))) #f))
  
  (let ()
    (define-language L
      (M N ::= (M N) (λ (x) M) x)
      (x ::= variable-not-otherwise-mentioned))
    (test (and (redex-match L M '(λ (x) (x x))) #t) #t)
    (test (and (redex-match L N '(λ (x) (x x))) #t) #t)
    (define-extended-language L+ L
      (M ::= .... n)
      (n m ::= number))
    (test (and (redex-match L+ M '(λ (x) 7)) #t) #t)
    (test (and (redex-match L+ m 7) #t) #t)
    (let ([::= void])
      (define-language L
        (::= () (number ::=)))
      (test (and (redex-match L ::= '(1 ())) #t) #t)))
  
  (let ()
    (define-language L1
      ((q x) 1 2 3)
      ((y w) 4 5 6 x)
      (z 7 8 9))
    
    (define-language L2
      ((x y) 100 101 102)
      (b 103 x))
    
    (define-union-language L L1 (- L2))
    
    (test (and (redex-match L x 3) #t) #t)
    (test (and (redex-match L y 2) #t) #t)
    (test (redex-match L x 100) #f)
    (test (and (redex-match L -x 100) #t) #t)
    (test (and (redex-match L -b 100) #t) #t)
    (test (redex-match L -b 3) #f))

  ;; The following two tests make sure that `define-union-language`
  ;; works with extended languages
  (let ()
    (define-language LBase
      (e (+ e e)
         number))

    (define-extended-language L1 LBase
      (e ....
         (- e e)))

    (define-extended-language L2 LBase
      (e ....
         (* e e)))

    (define-union-language LMerge (one. L1) (two. L2))

    #|
    The error that used to be raised:
    define-union-language: two sublanguages both contribute the non-terminal: one.e in:
      (one. L1)
      (one. L1)
    |#


    (test (and (redex-match LMerge one.e (term (- 0 0))) #t) #t)
    (test (and (redex-match LMerge two.e (term (* 0 0))) #t) #t)

    (define-union-language LMergeUntagged L1 L2)

    (for ([t (list (term 1) (term (* 1 1)) (term (+ 1 1)) (term (- 1 1)))])
       (test (redex-match? LMergeUntagged e t) #t)))
  
  ;; test that define-union-language properly merges non-terminals
  (let () 
    (define-language LBase
      (e (+ e e) number))
    
    (define-extended-language L1 LBase
      (e ....  (- e e)))
    
    (define-extended-language L2 LBase
      (e ....  (* e e)))
    
    ;; Untagged union of two languages that define the same nonterminal
    (define-union-language LMergeUntagged L1 L2)
    
    ;; Tagged merge of two extended languages that define the same
    ;; nonterminal
    (define-union-language LMergeTagged (f. L1) (d. L2))
    
    (test (redex-match? LMergeUntagged e (term 1)) #t)
    (test (redex-match? LMergeUntagged e (term (* 1 1))) #t)
    (test (redex-match? LMergeUntagged e (term (+ 1 1))) #t)
    (test (redex-match? LMergeUntagged e (term (- 1 1))) #t)
    
    (test (redex-match? LMergeTagged f.e 1) #t)
    (test (redex-match? LMergeTagged d.e 1) #t)
    
    (test (redex-match? LMergeTagged f.e (term (+ 1 1))) #t)
    (test (redex-match? LMergeTagged f.e (term (- 1 1))) #t)
    (test (redex-match? LMergeTagged f.e (term (* 1 1))) #f)
    
    (test (redex-match? LMergeTagged d.e (term (+ 1 1))) #t)
    (test (redex-match? LMergeTagged d.e (term (* 1 1))) #t)
    (test (redex-match? LMergeTagged d.e (term (- 1 1))) #f))
  
  (let ()
    (define-language L1 (e f ::= 1))
    (define-language L2 (e g ::= 2))
    (define-union-language Lc L1 L2)
    (test (redex-match? Lc e 1) #t)
    (test (redex-match? Lc e 2) #t)
    (test (redex-match? Lc f 1) #t)
    (test (redex-match? Lc f 2) #t)
    (test (redex-match? Lc g 1) #t)
    (test (redex-match? Lc g 2) #t))
  
  (let ()
    (define-language UT
      (e (e e)
         (λ (x) e)
         x))
    
    (define-language WT
      (e (e e)
         (λ (x t) e)
         x)
      (t (→ t t)
         num))
    
    (define-extended-language UT+ UT
      (e ....
         (foo e e)))
    
    (define-union-language B (ut. UT+) (wt. WT))
    
    (test (and (redex-match B ut.e (term (foo x x))) #t) #t)
    (test (redex-match B wt.e (term (foo x x))) #f))

  (let ()
    (test (redex-match empty-language number 'a) #f)
    (test (redex-match empty-language (in-hole hole number) 'a) #f))

  (parameterize ([current-namespace (make-base-namespace)])
    (eval '(require redex/reduction-semantics redex/pict))
    (eval '(define-language L
             (s a b c)))
    (exec-runtime-error-tests "run-err-tests/define-union-language.rktd"))
  
  (exec-syntax-error-tests "syn-err-tests/language-definition.rktd")
  
  ;; term with #:lang tests
  (exec-syntax-error-tests "syn-err-tests/term-lang.rktd")
  
  (let ()
    (define-language L
      (a number)
      (b (a a))
      (c (b b)))
    (test (term 1 #:lang L) 1)
    (test (term ((1 2) (3 4)) #:lang L) '((1 2) (3 4)))
    (test (term (1 2 3 4) #:lang L) '(1 2 3 4))
    (test (redex-let L ([a_1 5])
                     (term (a_1 6) #:lang L))
          '(5 6))
    (test (redex-let L ([number_1 5])
                     (term (number_1 6) #:lang L))
          '(5 6)))
    
;                                                                                             
;                                                                                             
;                                 ;;;                                ;                        
;                  ;             ;                           ;                                
;  ;;; ;    ;;;   ;;;;;   ;;;   ;;;;; ;;  ;; ;; ;;    ;;;;  ;;;;;  ;;;     ;;;  ;; ;;    ;;;; 
;   ; ; ;  ;   ;   ;     ;   ;   ;     ;   ;  ;;  ;  ;   ;   ;       ;    ;   ;  ;;  ;  ;   ; 
;   ; ; ;  ;;;;;   ;      ;;;;   ;     ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;   ;;;  
;   ; ; ;  ;       ;     ;   ;   ;     ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;      ; 
;   ; ; ;  ;       ;   ; ;   ;   ;     ;  ;;  ;   ;  ;   ;   ;   ;   ;    ;   ;  ;   ;  ;   ; 
;  ;;;;;;;  ;;;;    ;;;   ;;;;; ;;;;;   ;; ;;;;; ;;;  ;;;     ;;;  ;;;;;   ;;;  ;;; ;;; ;;;;  
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             

  
  (define-metafunction grammar
    [(f (side-condition (number_1 number_2)
                        (< (term number_1)
                           (term number_2))))
     x]
    [(f (number 1)) y]
    [(f (number_1 2)) ,(+ (term number_1) 2)]
    [(f (4 4)) q]
    [(f (4 4)) r])

  (define-metafunction grammar
    [(g X) x])
  
  (test (term (f (1 17))) 'x)
  (test (term (f (11 1))) 'y)
  (test (term (f (11 2))) 13)
  
  
  ;; match two clauess => take first one
  (test (term (f (4 4))) 'q)
  
  ;; match one clause two ways => error
  (let ()
    (define-metafunction empty-language
      [(ll (number_1 ... number_2 ...)) (number_1 ...)])
    (test (with-handlers ((exn? (λ (x) 'exn-raised))) 
            (term (ll ()))
            'no-exn)
          'no-exn)
    (test (with-handlers ((exn? (λ (x) 'exn-raised))) 
            (term (ll (4 4)))
            'no-exn)
          'exn-raised))
  
  ;; match no ways => error
  (test (with-handlers ((exn? (λ (x) 'exn-raised))) (term (f mis-match)) 'no-exn)
        'exn-raised)

  ;; test redex-match in RHS and side-condition
  (let ()
    (define-metafunction empty-language
      [(f)
       ,(and (redex-match empty-language number 7) #t)
       (side-condition (redex-match empty-language number 7))])
    (test (term (f)) #t))
  
  (define-metafunction grammar
    [(h (M_1 M_2)) ((h M_2) (h M_1))]
    [(h number_1) ,(+ (term number_1) 1)])
  
  (test (term (h ((1 2) 3)))
        (term (4 (3 2))))
  
  (define-metafunction grammar
    [(h2 (Q_1 ...)) ((h2 Q_1) ...)]
    [(h2 variable) z])
  
  (test (term (h2 ((x y) a b c)))
        (term ((z z) z z z)))
  
  (let ()
    (define-metafunction empty-language
      [(f (1)) 1]
      [(f (2)) 2]
      [(f 3) 3])
    (test (in-domain? (f 1)) #f)
    (test (in-domain? (f (1))) #t)
    (test (in-domain? (f ((1)))) #f)
    (test (in-domain? (f 3)) #t)
    (test (in-domain? (f 4)) #f))
  
  (let ()
    (define-metafunction empty-language
      f : number -> number
      [(f 1) 1])
    (test (in-domain? (f 1)) #t)
    (test (in-domain? (f 2)) #t)
    (test (in-domain? (f x)) #f))
  
  (let ()
    (define-metafunction empty-language
      [(f x) x])
    (test 
     (term-let ((y 'x))
               (in-domain? (f y)))
     #t)
    (test 
     (term-let ((y 'z))
               (in-domain? (f y)))
     #f))
  
  (let ()
    (define-language foo)

    (test (term-let ([bar 23])
                    (term 5 #:lang foo))
          5)

    (test (term-let ([foo 23])
                    (term 6 #:lang foo))
          6)

    (test (term-let ([foo 12])
                    (term-let ([foo 23])
                              (term 7 #:lang foo)))
          7)
    )
  
  ; Extension reinterprets the base meta-function's contract
  ; according to the new language.
  (let ()
    (define-language L (x 1))
    (define-extended-language M L (x 2))
    (define-metafunction L
      f : x -> x
      [(f x) x])
    (define-metafunction/extension f M
      [(g q) q])
    
    (with-handlers ([(λ (x) 
                       (and (exn:fail? x)
                            (regexp-match? #rx"no clauses matched"
                                           (exn-message x))))
                     (λ (_) #f)])
      (test (begin (term (g 2)) #t) #t))
    
    (test (in-domain? (g 2)) #t))
  
  ; in-domain? interprets base meta-function LHSs according to
  ; the new language.
  (let ()
    (define-language L (x 1))
    (define-extended-language M L (x 2))
    (define-metafunction L
      [(f x) x])
    (define-metafunction/extension f M
      [(g q) q])
    (test (in-domain? (g 2)) #t))
  
  ;; mutually recursive metafunctions
  (define-metafunction grammar
    [(odd zero) #f]
    [(odd (add1 UN_1)) (even UN_1)])
  
  (define-metafunction grammar
    [(even zero) #t]
    [(even (add1 UN_1)) (odd UN_1)])
  
  (test (term (odd (add1 (add1 (add1 (add1 zero))))))
        (term #f))
    
  (let ()
    (define-metafunction empty-language
      [(pRe xxx) 1])
    
    (define-metafunction empty-language
      [(Merge-Exns any_1) any_1])
    
    (test (term (pRe (Merge-Exns xxx)))
          1))
  
  (let ()
    (define-metafunction empty-language
      [(f (x)) ,(term-let ([var-should-be-lookedup 'y]) (term (f var-should-be-lookedup)))]
      [(f y) y]
      [(f var-should-be-lookedup) var-should-be-lookedup]) ;; taking this case is bad!
    
    (test (term (f (x))) (term y)))
  
  (let ()
    (define-metafunction empty-language
      [(f (x)) (x ,@(term-let ([var-should-be-lookedup 'y]) (term (f var-should-be-lookedup))) x)]
      [(f y) (y)]
      [(f var-should-be-lookedup) (var-should-be-lookedup)]) ;; taking this case is bad!
    
    (test (term (f (x))) (term (x y x))))
  
  (let ()
    (define-metafunction empty-language
      [(f (any_1 any_2))
       case1
       (side-condition (not (equal? (term any_1) (term any_2))))
       (side-condition (not (equal? (term any_1) 'x)))]
      [(f (any_1 any_2))
       case2
       (side-condition (not (equal? (term any_1) (term any_2))))]
      [(f (any_1 any_2))
       case3])
    (test (term (f (q r))) (term case1))
    (test (term (f (x y))) (term case2))
    (test (term (f (x x))) (term case3)))

  (let ()
    (define-metafunction empty-language
      [(f (n number)) (n number)]
      [(f (a any)) (a any)]
      [(f (v variable)) (v variable)]
      [(f (s string)) (s string)])
    (test (term (f (n 1))) (term (n 1)))
    (test (term (f (a (#f "x" whatever)))) (term (a (#f "x" whatever))))
    (test (term (f (v x))) (term (v x)))
    (test (term (f (s "x"))) (term (s "x"))))
  
  ;; test ..._1 patterns
  (let ()
    (define-metafunction empty-language
      [(zip ((variable_id ..._1) (number_val ..._1)))
       ((variable_id number_val) ...)])
    
    (test (term (zip ((a b) (1 2)))) (term ((a 1) (b 2)))))
  
  (let ()
    (define-metafunction empty-language
      [(f any_1 any_2 any_3) (any_3 any_2 any_1)])
    (test (term (f 1 2 3)) 
          (term (3 2 1))))
  
  (let ()
    (define-metafunction empty-language
      [(f (any_1 any_2 any_3)) 3])
    (define-metafunction/extension f empty-language
      [(g (any_1 any_2)) 2])
    (define-metafunction/extension g empty-language
      [(h (any_1)) 1])
    (test (term (h (1))) 1)
    (test (term (h (1 2))) 2)
    (test (term (h (1 2 3))) 3))
  
  (let ()
    (define-metafunction empty-language
      [(f any_1 any_2 any_3) 3])
    (define-metafunction/extension f empty-language
      [(g any_1 any_2) 2])
    (define-metafunction/extension g empty-language
      [(h any_1) 1])
    (test (term (h 1)) 1)
    (test (term (h 1 2)) 2)
    (test (term (h 1 2 3)) 3))
  
  (let ()
    (define-metafunction empty-language
      [(f number_1 number_2) (f number_1)])
    (define-metafunction/extension f empty-language
      [(g number_1) number_1])
    (define-metafunction empty-language
      [(h number_1 number_2) (h number_1)]
      [(h number_1) number_1])
    (test (term (g 11 17)) 11)
    (test (term (h 11 17)) 11))

  (let ()
    (define-language L 
      (v 1))
    (define-extended-language M
      L
      (v .... 2))
    (define-metafunction L
      [(f v) v])
    (define-metafunction/extension f M
      [(g 17) 17])
    (test (term (g 2)) 2))
  
  (let ()
    (define-metafunction empty-language
      [(f any) 1])
    (define-metafunction/extension f empty-language
      [(g any) 2])
    (test (term (g 0)) 2))
  
  (let ()
    (define-language L 
      (v 1 (v)))
    (define-metafunction L
      f : v -> v
      [(f (v)) 
       any_1
       (where any_1 (f v))])
    
    (define-extended-language M
      L
      (v .... 2))
    (define-metafunction/extension f M
      g : v -> v
      [(g 2) 2])

    (test (term (g (2))) 2))
  
  (let ()
    (define-language L (x 1))
    (define-extended-language M L (x 2))
    (define-metafunction L 
      [(f)
       yes
       (where x 2)]
      [(f)
       no])
    (define-metafunction/extension f M
      g : -> any)
    (test (term (g)) 'yes))
  
  (let ()
    (define-metafunction empty-language
      [(f (number_1 number_2))
       number_3
       (where number_3 ,(+ (term number_1) (term number_2)))])
    (test (term (f (11 17))) 28))
  
  (let ()
    (define-metafunction empty-language
      [(f variable) 
       (any_x any_x)
       (where any_x (variable variable))])
    (test (term (f z)) 
          (term ((z z) (z z)))))
  
  (let ()
    (define-metafunction empty-language
      [(f number_1)
       number_1
       (where number_2 ,(add1 (term number_1)))
       (where number_3 ,(add1 (term number_2)))
       (side-condition (and (number? (term number_3))
                            (= (term number_3) 4)))]
      [(f any) 0])
    (test (term (f 2)) 2))
  
  (let ()
    (define-language x-lang
      (x variable))
    (define-metafunction x-lang
      f : x x -> x
      [(f x_1 x_2) x_1])
    (test (term (f p q)) (term p))
    (test (in-domain? (f p q)) #t))
  
  (let ()
    (define-metafunction empty-language
      [(err number_1 ... number_2 ...) (number_1 ...)])
    (test (term (err)) (term ()))
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err 1 2))
            'no-exn)
          'right-exn))
  
  (let ()
    (define-metafunction empty-language
      err : number ... -> number
      [(err number ...) 1])
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err #f #t))
            'no-exn)
          'right-exn))
  
  (let ()
    (define-metafunction empty-language
      err : number ... -> number
      [(err number ...) #f])
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err 1 2))
            'no-exn)
          'right-exn))
  
  (let ()
    (define-metafunction empty-language
      err : number ... -> (number number)
      [(err number ...) (number ...)])
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err 1 2))
            'no-exn)
          'no-exn)
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err 1 1))
            'no-exn)
          'no-exn))
    
  (let ()
    ;; test that 'where' clauses can contain recursive calls.
    (define-metafunction empty-language
      [(f (any)) 
       x
       (where x (f any))]
      [(f any) any])
    (test (term (f ((((x))))))
          (term x)))
  
  (let ()
    (define-language lamv
      (z variable hole))

    (define-metafunction lamv
      foo : z  -> any
      [(foo hole) dontcare]
      [(foo variable) docare])

    (test (term (foo hole))
          (term dontcare))
    (test (term (foo y))
          (term docare)))
  
  (let ()
    (define f-called? #f)
    (define-metafunction empty-language
      f : (side-condition any_1 (begin (set! f-called? #t) #t)) -> any
      [(f any_1) any_1])
    (test (term (f 1)) 1)
    (test f-called? #t))
    
  (let ()
    (define g-called? #f)
    (define-metafunction empty-language
      g : any -> (side-condition any_1 (begin (set! g-called? #t) #t))
      [(g any_1) any_1])
    (test (term (g 1)) 1)
    (test g-called? #t))
  
  ;; test that tracing works properly
  ;; note that caching comes into play here (which is why we don't see the recursive calls)
  (let ()
    (define-metafunction empty-language
      [(f 0) 0]
      [(f number) (f ,(- (term number) 1))])
    
    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp])
        (term (f 1)))
      (test (get-output-string sp) ""))
    
    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp]
                     [current-traced-metafunctions 'all]
                     [print-as-expression #f])
        (term (f 1)))
      (test (get-output-string sp) "c>(f 1)\n <0\n"))
    
    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp]
                     [current-traced-metafunctions 'all]
                     [print-as-expression #f]
                     [caching-enabled? #f])
        (term (f 1)))
      (test (get-output-string sp) " >(f 1)\n > (f 0)\n < 0\n <0\n"))
    
    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp]
                     [current-traced-metafunctions '(f)]
                     [print-as-expression #f])
        (term (f 1)))
      (test (get-output-string sp) "c>(f 1)\n <0\n"))
    
    
    (define-metafunction empty-language
      [(g (any)) ((g any) (g any))]
      [(g 1) 1])
    
    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp]
                     [current-traced-metafunctions '(g)]
                     [print-as-expression #f])
        (term (g (1))))
      (test (get-output-string sp) " >(g (1))\n > (g 1)\n < 1\nc> (g 1)\n < 1\n <(1 1)\n"))
    
    )
  
  (let ()
    (define-language var-lang [(x y z w) variable])
    
    ;; this should produce the second case, 
    ;; since the where clause (should) fail to match 
    ;; in the first case.
    (define-metafunction var-lang
      [(f x)
       first-case
       (where (AnotherAtom y) (Atom x))]
      [(f x)
       second-case])
    
    (test (term (f a)) (term second-case)))
  
  (let ()
    
    ;; this is an ambiguous function 
    ;; and should signal an error if it is ever called
    ;; with multiple different arguments, but if the
    ;; arguments are all the same, it will return
    ;; the same result for any parse, and thus should be allowed.
    (define-metafunction empty-language
      [(f any_x ... any_y any_z ...)
       any_y])
    
    (test (term (f 1 1 1 1 1)) (term 1)))
  
  (let ()
    (define-metafunction empty-language
      [(ex variable_x) 
       variable_x
       (where quote variable_x)])
    
    (test (term (ex quote)) (term quote)))
  
  (let ()
    (define-metafunction empty-language
      [(f any ...)
       (any ...)
       (where variable_1 x)
       (side-condition #f)
       (where (number ...) y)]
      [(f any ...)
       12345])
    
    (test (term (f 8)) 12345))
  
  (let ()
    (define-metafunction empty-language
      [(f number_1 number_2 ... (number_s ...) ...)
       yes
       (where number_1 1)
       (where (number_3 ...) ,(cdr (term (number_2 ...))))
       (where (number_3 ...) (3 4 5))
       (where (number_1 (number_s ...) ...)
              ,(if (null? (term ((number_s ...) ...)))
                   (term (number_1))
                   (term (number_1 () (6) (7 8) (9 10 11)))))]
      [(f any ...)
       no])
    (test (term (f 1 2 3 4 5)) 'yes)
    (test (term (f 1 2 3 4)) 'no)
    (test (term (f 0 2 3 4 5)) 'no)
    (test (term (f 1 2 3 4 5 () (6) (7 8) (9 10 11))) 'yes))
  
  (let ()
    (define-language L 
      [bool #t #f])
    (define-metafunction L
      f : any -> bool or number
      [(f any) any])
    (test (term (f 1)) (term 1))
    (test (term (f #f)) (term #f)))
  
  (let ()
    (define-language L 
      [bool #t #f])
    (define-metafunction L
      f : any -> bool ∪ number
      [(f any) any])
    (test (term (f 1)) (term 1))
    (test (term (f #f)) (term #f)))
  
  (let ()
    (define-language L 
      [bool #t #f]
      [abc a b c]
      [def d e f])
    (define-metafunction L
      f : any -> bool ∨ number ∪ abc or def
      [(f any) any])
    (test (term (f 1)) (term 1))
    (test (term (f #f)) (term #f))
    (test (term (f c)) (term c))
    (test (term (f e)) (term e)))
  
  ;; test that the contracts are called in order (or else 'car' fails)
  (let ()
    (define x '())
    (define-language L
      [seq (any any ...)])
    (define-metafunction L
      g : any -> 
      (side-condition any_1 (begin (set! x (cons 1 x)) #f))
      or (side-condition any_1 (begin (set! x (cons 2 x)) #f))
      or any
      [(g any) any])
    (test (begin (term (g whatever))
                 x)
          '(2 1)))
  
  (let ()
    (define-metafunction empty-language
      [(same any_1 any_1) #t]
      [(same any_1 any_2) #f])
    
    (define-metafunction empty-language
      m : any_1 any_2 -> any_3
      #:pre (same any_1 any_2)
      [(m any_x any_y) any_x])
    
    (test (term (m 1 1)) 1)
    (test (with-handlers ((exn:fail:redex? exn-message))
            (term (m 1 2)))
          #rx"is not in my domain"))
  
  (let ()
    (define-language L
      (n z (s n)))
    
    (define-metafunction L
      [(f n)
       n_1
       (judgment-holds (p n n_1))])
    
    (define-judgment-form L
      #:mode (p I O)
      #:contract (p n n)
      [(p z z)]
      [(p (s n) n)]
      [(p (s n) z)])
    
    (test (term (f (s z)))
          (term z))
    (test (with-handlers ([exn:fail:redex? exn-message])
            (term (f (s (s z))))
            "")
          #rx"returned different results"))
  
  (parameterize ([current-namespace (make-base-namespace)])
    (eval '(require redex/reduction-semantics))
    (exec-runtime-error-tests "run-err-tests/judgment-form-undefined.rktd"))
  
  ;; errors for not-yet-defined metafunctions
  (test (parameterize ([current-namespace (make-empty-namespace)])
          (namespace-attach-module (namespace-anchor->namespace this-namespace) 'redex/reduction-semantics)
          (namespace-require 'racket)
          (eval '(module m racket
                   (require redex/reduction-semantics)
                   (term (q))
                   (define-language L)
                   (define-metafunction L [(q) ()])))
          (with-handlers ([exn:fail:redex? exn-message])
            (eval '(require 'm))
            #f))
        "reference to metafunction q before its definition")
  (test (with-handlers ([exn:fail:redex? exn-message])
          (let ()
            (term (q))
            (define-language L)
            (define-metafunction L [(q) ()])
            #f))
        "reference to metafunction q before its definition")
  
  (exec-syntax-error-tests "syn-err-tests/metafunction-definition.rktd")
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;       ;;           ;;; ;;                                   ;;           ;;  ;;                 
;       ;;          ;;;  ;;                                   ;;          ;;;  ;;                 
;    ;;;;;   ;;;;  ;;;;; ;;  ;; ;;;   ;;;;       ;;;;  ;;;;   ;;   ;;;;  ;;;;; ;;   ;;;;   ;; ;;; 
;   ;;;;;;  ;;  ;; ;;;;  ;;  ;;;;;;  ;;  ;;      ;;;; ;;  ;;  ;;  ;;  ;;  ;;;; ;;  ;;;;;;  ;;;;;; 
;  ;;;  ;; ;;;;;;;; ;;   ;;  ;;  ;; ;;;;;;;;;;;; ;;  ;;;;;;;; ;;    ;;;;  ;;;  ;; ;;;  ;;; ;;  ;; 
;  ;;;  ;; ;;;      ;;   ;;  ;;  ;; ;;;     ;;;; ;;  ;;;      ;;  ;;; ;;  ;;;  ;; ;;;  ;;; ;;  ;; 
;   ;;;;;;  ;;; ;;  ;;   ;;  ;;  ;;  ;;; ;;      ;;   ;;; ;;  ;; ;;;  ;;  ;;;; ;;  ;;;;;;  ;;  ;; 
;    ;;;;;   ;;;;   ;;   ;;  ;;  ;;   ;;;;       ;;    ;;;;   ;;  ;;;;;;   ;;; ;;   ;;;;   ;;  ;; 
;                                                                                                 
;                                                                                                 
;                                                                                                 

  
  (let ()
    (define-relation empty-language
      [(<: any any) #t])
    
    (test (term (<: 1 1)) #t)
    (test (term (<: 1 2)) #f))
  
  (let ()
    (define-relation empty-language
      [(<: number_1 number_2) ,(< (term number_1) (term number_2))]
      [(<: number_1 number_1) #t])
    
    (test (term (<: 1 2)) #t)
    (test (term (<: 1 1)) #t)
    (test (term (<: 2 1)) #f))
  
  (let ()
    (define-relation empty-language
      [(<: number_1 ... number_2 number_3 ... number_2 number_4 ...) #t])
    
    (test (term (<: 1 2 3 4)) #f)
    (test (term (<: 1 1 2 3 4)) #t)
    (test (term (<: 1 2 1 3 4)) #t)
    (test (term (<: 1 2 3 1 4)) #t)
    (test (term (<: 1 2 3 4 1)) #t))
  
  (let ()
    (define-relation empty-language
      [(<: number_1 number_1)])
    (test (term (<: 1 1)) #t)
    (test (term (<: 1 2)) #f))
  
  (let ()
    (define-relation empty-language
      [(<: number_1 number_2 number_3)
       ,(= (term number_1) (term number_2))
       ,(= (term number_2) (term number_3))])
    (test (term (<: 1 2 3)) #f)
    (test (term (<: 1 1 2)) #f)
    (test (term (<: 1 2 2)) #f)
    (test (term (<: 1 1 1)) #t))
  
  (let ()
    (define-relation empty-language
      d ⊆ any × any
      [(d (any) (any)) (d any any)]
      [(d () ())])
    
    (test (term (d ((())) ((())))) #t)
    (test (term (d ((())) ())) #f))
  
  (let ()
    (define-relation empty-language
      d ⊂ any x any
      [(d (any) (any)) (d any any)]
      [(d () ())])
    
    (test (term (d ((())) ((())))) #t)
    (test (term (d ((())) ())) #f))
  
  (let ()
    (define-relation empty-language
      d ⊂ (any)
      [(d (1))])
    
    (test (term (d (1))) #t)
    (test (term (d (2))) #f)
    (test (with-handlers ((exn:fail? (λ (x) 'passed)))
            (term (d 1))
            'failed)
          'passed))
  
  (let ()
    (define-language types
      ((τ σ) int
             num
             (τ ... → τ)))
    
    (define-relation types
      subtype ⊆ τ × τ
      [(subtype int num)]
      [(subtype (τ_1 ..._1 → τ_2) (σ_1 ..._1 → σ_2))
       (subtype σ_1 τ_1) ...
       (subtype τ_2 σ_2)]
      [(subtype τ τ)])
    
    (test (term (subtype int int)) #t)
    (test (term (subtype int num)) #t)
    (test (term (subtype (int int int → int) (int int → int))) #f)
    (test (term (subtype (int int → int) (int num → int))) #f)
    (test (term (subtype (int num → int) (int int → int))) #t)
    (test (term (subtype (int int → int) (int int → num))) #t))
  
  (let ()
    (define-relation empty-language
      [(R () ())]
      [(R (any_a) (any_b)) 
       (R any_c any_d) 
       (where any_c any_a)
       (where any_d any_b)])
    
    (test (term (R () ())) #t)
    (test (term (R (()) (()))) #t)
    (test (term (R (()) ())) #f))
  
  (let ()
    (define-relation empty-language
      [(R () ())]
      [(R (any_a) (any_b)) 
       (R any_c any_d) 
       (where/hidden any_c any_a)
       (where/hidden any_d any_b)])
    
    (test (term (R () ())) #t)
    (test (term (R (()) (()))) #t)
    (test (term (R (()) ())) #f))
  
  (let ()
    (define-relation empty-language
      [(R any_a any_b)
       (side-condition (equal? (term any_a)
                               (term any_b)))])
    
    (test (term (R (xx) (xx))) #t)
    (test (term (R (()) ())) #f))
  
  (let ()
    (define-relation empty-language
      [(R any_a any_b)
       (side-condition/hidden
        (equal? (term any_a)
                (term any_b)))])
    
    (test (term (R (xx) (xx))) #t)
    (test (term (R (()) ())) #f))
  
  (let ()
    
    (define-relation empty-language
      [(a number_1)
       (b number_1)]
      [(a 2)])
    
    (define-relation empty-language
      [(b 1)]
      [(b 2)])
    
    (test (term (a 1)) #t)
    (test (term (a 2)) #t)
    (test (term (a 3)) #f)
    (test (term (b 1)) #t)
    (test (term (b 2)) #t)
    (test (term (b 3)) #f))
  
  (let ()
    (define-relation empty-language
      [(a any)])
    (define-relation empty-language
      [(b any)])
    (define-relation empty-language
      [(c any) (a (b any))])
    
    (define-metafunction empty-language
      [(f any)
       (c any)])
    
    (define-judgment-form empty-language
      #:mode (J I O)
      [(J any_1 (a any_1))])
    
    (test (term (a 1)) #t)
    (test (term (b 2)) #t)
    (test (term (c 3)) #t)
    (test (term (c (b (a x)))) #t)
    (test (term (f q)) #t)
    (test (judgment-holds (J Z #t)) #t)
    (test (judgment-holds (J Z Z)) #f)
    )

  
  (exec-syntax-error-tests "syn-err-tests/relation-definition.rktd")
  
;                    ;;                         ;                                        ;;                    ;                 
;                     ;                 ;                                                 ;            ;                         
;   ;; ;;   ;;;    ;; ; ;;  ;;   ;;;;  ;;;;;  ;;;     ;;;  ;; ;;          ;; ;;   ;;;     ;     ;;;   ;;;;;  ;;;     ;;;  ;; ;;  
;    ;;    ;   ;  ;  ;;  ;   ;  ;   ;   ;       ;    ;   ;  ;;  ;          ;;    ;   ;    ;    ;   ;   ;       ;    ;   ;  ;;  ; 
;    ;     ;;;;;  ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;  ;;;;;   ;     ;;;;;    ;     ;;;;   ;       ;    ;   ;  ;   ; 
;    ;     ;      ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;          ;     ;        ;    ;   ;   ;       ;    ;   ;  ;   ; 
;    ;     ;      ;   ;  ;  ;;  ;   ;   ;   ;   ;    ;   ;  ;   ;          ;     ;        ;    ;   ;   ;   ;   ;    ;   ;  ;   ; 
;   ;;;;;   ;;;;   ;;;;;  ;; ;;  ;;;     ;;;  ;;;;;   ;;;  ;;; ;;;        ;;;;;   ;;;;  ;;;;;   ;;;;;   ;;;  ;;;;;   ;;;  ;;; ;;;
;                                                                                                                                
;                                                                                                                                
;                                                                                                                                
;                                                                                                                                

  
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (--> (in-hole E_1 (number_1 number_2))
               (in-hole E_1 ,(* (term number_1) (term number_2)))))
         '((2 3) (4 5)))
        (list '(6 (4 5))))

  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (~~> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          with
          [(--> (in-hole E_1 a) (in-hole E_1 b)) (~~> a b)])
         '((2 3) (4 5)))
        (list '(6 (4 5))))
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (==> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          with
          [(--> (M_1 a) (M_1 b)) (~~> a b)]
          [(~~> (M_1 a) (M_1 b)) (==> a b)])
         '((1 2) ((2 3) (4 5))))
        (list '((1 2) ((2 3) 20))))
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (~~> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          (==> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          with
          [(--> (M_1 a) (M_1 b)) (~~> a b)]
          [(--> (a M_1) (b M_1)) (==> a b)])
         '((2 3) (4 5)))
        (list '(6 (4 5))
              '((2 3) 20)))
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (--> (M_1 (number_1 number_2))
               (M_1 ,(* (term number_1) (term number_2))))
          (==> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          with
          [(--> (a M_1) (b M_1)) (==> a b)])
         '((2 3) (4 5)))
        (list '((2 3) 20)
              '(6 (4 5))))
  
  ; The scope of a `where' clause includes the left-hand sides
  ; of subsequent `where' clauses.
  (test (apply-reduction-relation
         (reduction-relation
          grammar
          (--> any
               1
               (where number_1 2)
               (where (side-condition any (number? (term number_1))) dontcare)))
         'dontcare)
        '(1))
  
  ; shortcuts like this fail if compilation fails to preserve
  ; lexical context for side-conditions expressions.
  (test (let ([x #t])
          (apply-reduction-relation
           (reduction-relation
            grammar
            (==> variable variable)
            with
            [(--> (a (side-condition number x)) b)
             (==> a b)])
           '(x 4)))
        '(x))
  
  ; test multiply matching `where' with failing `where' inside
  (test (apply-reduction-relation
         (reduction-relation
          empty-language
          (--> ()
               ()
               (where (number_1 ... number_i number_i+1 ...)
                      (1 2 3))
               (where number_i 2)))
         '())
        '(()))
  
  (test (apply-reduction-relation
         (reduction-relation
          empty-language
          (--> (in-hole (name E
                              (in-hole ((hide-hole hole) hole)
                                       hole))
                        number)
               (in-hole E ,(add1 (term number)))))
         (term (hole 2)))
        (list (term (hole 3))))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation 
          grammar
          (--> (number_1 number_2) 
               ,(* (term number_1) (term number_2))
               mul))
         '(4 5))
        (list (list "mul" 20)))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation 
          grammar
          (--> (number_1 number_2) 
               ,(* (term number_1) (term number_2))
               "mul"))
         '(4 5))
        (list (list "mul" 20)))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation 
          grammar
          (--> (number_1 number_2) 
               ,(* (term number_1) (term number_2))))
         '(4 5))
        (list (list #f 20)))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation 
          grammar
          (==> (number_1 number_2) 
               ,(* (term number_1) (term number_2))
               mult)
          with
          [(--> (M_1 a) (M_1 b)) (==> a b)])
         '((2 3) (4 5)))
        (list (list "mult" '((2 3) 20))))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation
          grammar
          (--> any
               (number_i number_i*)
               (where (number_0 ... number_i number_i+1 ...) any)
               (where (number_0* ... number_i* number_i+1* ...) any)
               pick-two
               (computed-name
                (format "(~s, ~s)"
                        (length (term (number_0 ...)))
                        (length (term (number_0* ...)))))))
         '(9 7))
        '(("(1, 1)" (7 7)) ("(1, 0)" (7 9)) ("(0, 1)" (9 7)) ("(0, 0)" (9 9))))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation grammar (--> 1 2 (computed-name 3))) 1)
        '(("3" 2)))
  
  (test (apply-reduction-relation
         (union-reduction-relations
          (reduction-relation empty-language
                              (--> x a)
                              (--> x b))
          (reduction-relation empty-language
                              (--> x c)
                              (--> x d)))
         'x)
        (list 'a 'b 'c 'd))
  
  (test (apply-reduction-relation
         (union-reduction-relations
          (reduction-relation empty-language (--> x a))
          (reduction-relation empty-language (--> x b))
          (reduction-relation empty-language (--> x c))
          (reduction-relation empty-language (--> x d)))
         'x)
        (list 'a 'b 'c 'd))
  
  
  (let ([R (reduction-relation empty-language #:domain number (--> 1 a "first"))]
        [S (reduction-relation empty-language (--> 2 a "second"))])
    (test (apply-reduction-relation (union-reduction-relations R S) 2)
          (list 'a))
    (test (apply-reduction-relation (union-reduction-relations S R) 2)
          (list 'a)))
  
  (test (apply-reduction-relation
         (reduction-relation 
          empty-language
          (--> (number_1 number_2) 
               number_2
               (side-condition (< (term number_1) (term number_2))))
          (--> (number_1 number_2) 
               number_1
               (side-condition (< (term number_2) (term number_1)))))
         '(1 2))
        (list 2))
  
  (test (apply-reduction-relation
         (reduction-relation 
          empty-language
          (--> x #f))
         (term x))
        (list #f))
  
  (define-language x-language
    (x variable))
  
  (test (apply-reduction-relation
         (reduction-relation 
          x-language
          (--> x (x x)))
         'y)
        (list '(y y)))
  
  (test (apply-reduction-relation
         (reduction-relation 
          x-language
          (--> (x ...) ((x ...))))
         '(p q r))
        (list '((p q r))))

  #;
  (test (apply-reduction-relation
         (reduction-relation 
          empty-language
          #:main-arrow :->
          (:-> 1 2))
         1)
        '(2))
  
  (test (apply-reduction-relation
         (reduction-relation 
          empty-language
          #:domain number
          (--> 1 2))
         1)
        '(2))
  
  
  (test (let ([red
               (reduction-relation 
                empty-language
                #:domain number
                (--> 1 2))])
          (with-handlers ((exn? exn-message))
            (apply-reduction-relation red 'x)
            'no-exception-raised))
        "reduction-relation: relation not defined for x")

  (test (let ([red
               (reduction-relation 
                empty-language
                #:domain number
                (--> 1 x))])
          (with-handlers ((exn? exn-message))
            (apply-reduction-relation red 1)
            'no-exception-raised))
        "reduction-relation: relation reduced to x via an unnamed rule, which is outside its domain")

  (let* ([red1
          (reduction-relation 
           empty-language
           #:domain (side-condition number_1 (even? (term number_1)))
           (--> number number))]
         [red2
          (reduction-relation 
           empty-language
           #:domain (side-condition number_1 (odd? (term number_1)))
           (--> number number))]
         [red-c
          (union-reduction-relations red1 red2)])
    
    ;; ensure first branch of 'union' is checked  
    (test (with-handlers ((exn? exn-message))
            (apply-reduction-relation red-c 1)
            'no-exception-raised)
          "reduction-relation: relation not defined for 1")

    ;; ensure second branch of 'union' is checked
    (test (with-handlers ((exn? exn-message))
            (apply-reduction-relation red-c 2)
            'no-exception-raised)
          "reduction-relation: relation not defined for 2"))

  (let ()
    (define-language l1
      (D 0 1 2))
    (define r1
      (reduction-relation 
       l1
       #:domain D
       (--> D D)))
    (define-language l2
      (D 0 1 2 3))
    (define r2
      (extend-reduction-relation r1 l2))
    
    ;; test that the domain is re-interpreted wrt the new language
    (test (apply-reduction-relation r2 3)
          '(3)))
  
  (let ()
    (define-language L)
    (define R
      (reduction-relation L #:domain 1 (--> any any)))
    (define S
      (extend-reduction-relation R L #:domain 2))
    
    ;; test that the new domain applies to inherited rules
    (test (apply-reduction-relation S 2)
          '(2))
    (test (with-handlers ([exn:fail? exn-message])
            (apply-reduction-relation S 1))
          #rx"not defined"))
  
  (let ()
    (define-language L)
    (define R
      (reduction-relation L (--> 1 1 "a")))
    (define S
      (extend-reduction-relation R L (--> 2 2 "a")))
    
    ;; test that overridden rules do not appear (twice)
    (test (reduction-relation->rule-names S)
          '(a)))
  
  (let ()
    (define-language L)
    
    ;; test that symbol-named rules replace string-named rules
    (test (apply-reduction-relation
           (extend-reduction-relation
            (reduction-relation L (--> 1 1 "a"))
            L (--> 1 2 a))
           1)
          '(2))
    ;; and vice versa
    (test (apply-reduction-relation
           (extend-reduction-relation
            (reduction-relation L (--> 1 1 a))
            L (--> 1 2 "a"))
           1)
          '(2)))
  
  (let ()
    (define-language l1
      (D 0 1 2))
    (define r1
      (reduction-relation 
       l1
       #:domain (D D)
       (--> (D_1 D_2) (D_2 D_1))))
    
    ;; test that duplicated identifiers in the domain contract do not have to be equal
    (test (apply-reduction-relation r1 (term (1 2)))
          (list (term (2 1)))))
  
  ;;test that #:arrow keyword works
  (test (apply-reduction-relation 
         (reduction-relation 
          empty-language
          #:arrow :->
          (:-> 1 2))
         1)
        '(2))

  (let ()
    (define-language n-lang
      [n number])
    (test (apply-reduction-relation
           (reduction-relation n-lang [--> any ,(length (redex-match n-lang n 1))])
           11)
          '(1)))
  
  (let ([R (reduction-relation
            grammar
            (--> (number_1 number_2 ... (number_s ...) ...)
                 yes
                 (where number_1 1)
                 (where (number_3 ...) ,(cdr (term (number_2 ...))))
                 (where (number_3 ...) (3 4 5))
                 (where (number_1 (number_s ...) ...)
                        ,(if (null? (term ((number_s ...) ...)))
                             (term (number_1))
                             (term (number_1 () (6) (7 8) (9 10 11)))))))])
    (test (apply-reduction-relation R (term (1 2 3 4 5))) '(yes))
    (test (apply-reduction-relation R (term (1 2 3 4))) '())
    (test (apply-reduction-relation R (term (0 2 3 4 5))) '())
    (test (apply-reduction-relation R (term (1 2 3 4 5 () (6) (7 8) (9 10 11)))) '(yes)))
  
  (exec-syntax-error-tests "syn-err-tests/reduction-relation-definition.rktd")
  
  ;; expect union with duplicate names to fail
  (test (with-handlers ((exn? (λ (x) 'passed)))
          (union-reduction-relations
           (reduction-relation 
            grammar
            (--> (number_1 number_2) 
                 ,(* (term number_1) (term number_2))
                 mult))
           (reduction-relation 
            grammar
            (--> (number_1 number_2) 
                 ,(* (term number_1) (term number_2))
                 mult)))
          'failed)
        'passed)
  
  (test (with-handlers ((exn? (λ (x) 'passed)))
          (union-reduction-relations
           (union-reduction-relations
            (reduction-relation 
             grammar
             (--> (number_1 number_2) 
                  ,(* (term number_1) (term number_2))
                  mult))
            (reduction-relation 
             grammar
             (--> (number_1 number_2) 
                  ,(* (term number_1) (term number_2))
                  mult3)))
           
           (union-reduction-relations
            (reduction-relation 
             grammar
             (--> (number_1 number_2) 
                  ,(* (term number_1) (term number_2))
                  mult))
            (reduction-relation 
             grammar
             (--> (number_1 number_2) 
                  ,(* (term number_1) (term number_2))
                  mult2))))
          'passed)
        'passed)
  
  ;; sorting in this test case is so that the results come out in a predictable manner.
  (test (sort
         (apply-reduction-relation
          (compatible-closure 
           (reduction-relation 
            grammar
            (--> (number_1 number_2) 
                 ,(* (term number_1) (term number_2))
                 mult))
           grammar
           M)
          '((2 3) (4 5)))
         (λ (x y) (string<=? (format "~s" x) (format "~s" y))))
        (list '((2 3) 20)
              '(6 (4 5))))
  
  (test (apply-reduction-relation
         (compatible-closure 
          (reduction-relation 
           grammar
           (--> (number_1 number_2) 
                ,(* (term number_1) (term number_2))
                mult))
          grammar
          M)
         '(4 2))
        (list '8))
  
  (test (with-handlers ((exn:fail? exn-message))
          (apply-reduction-relation
           (context-closure 
            (reduction-relation
             empty-language #:domain #f
             (--> #f #f))
            empty-language hole)
           #t)
          "exn not raised")
        #rx"^reduction-relation:")
  
  (test (apply-reduction-relation
         (context-closure 
          (context-closure 
           (reduction-relation grammar (--> 1 2))
           grammar
           (y hole))
          grammar
          (x hole))
         '(x (y 1)))
        (list '(x (y 2))))

  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (--> (variable_1 variable_2) 
               (variable_1 variable_2 x)
               mul
               (fresh x)))
         '(x x1))
        (list '(x x1 x2)))
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (~~> number 
               x
               (fresh x))
          with 
          [(--> (variable_1 variable_2 a) (variable_1 variable_2 b)) (~~> a b)])
         '(x x1 2))
        (list '(x x1 x2)))
  
  (test (apply-reduction-relation
         (reduction-relation 
          x-language
          (--> (x_1 ...)
               (x ...)
               (fresh ((x ...) (x_1 ...)))))
         '(x y x1))
        (list '(x2 x3 x4)))

  (test (apply-reduction-relation
         (reduction-relation
          empty-language
          (--> (variable_1 ...)
               (x ... variable_1 ...)
               (fresh ((x ...) (variable_1 ...) (variable_1 ...)))))
         '(x y z))
        (list '(x1 y1 z1 x y z)))

  (test (apply-reduction-relation 
         (reduction-relation 
          empty-language
          (--> any (any_y x)
               (where any_y x)
               (fresh x)))
         (term junk))
        (list '(x x1)))
  
  (test (apply-reduction-relation 
         (reduction-relation 
          empty-language
          (--> (variable ...) (variable_0 ... variable_1 ...)
               (fresh ((variable_0 ...) (variable ...)))
               (fresh ((variable_1 ...) (variable ...)))))
         (term (x y)))
        (list '(variable_0 variable_1 variable_2 variable_3)))

  
  ;; test that redex match can be used in a side-condition 
  ;; with the same language that is used to define the 
  ;; reduction relation.
  (test (apply-reduction-relation
         (reduction-relation
          empty-language
          (--> any_1 3
               (side-condition (redex-match empty-language (any_1 any_2) (term any_1)))))
         '(a b))
        '(3))
  
  (test (apply-reduction-relation
         (reduction-relation
          empty-language
          (--> variable_1
               (x variable_1)
               (fresh (x variable_1))))
         'q)
        (list '(q1 q)))
  
  (test (apply-reduction-relation
         (extend-reduction-relation (reduction-relation empty-language (--> 1 2))
                                    empty-language
                                    (--> 1 3))
         1)
        '(3 2))
  
  (test (apply-reduction-relation
         (extend-reduction-relation
          (reduction-relation empty-language (--> 1 2 (computed-name 1)))
          empty-language
          (--> 1 3 (computed-name 1)))
         1)
        '(3 2))
  
  (test (apply-reduction-relation
         (extend-reduction-relation
          (reduction-relation empty-language (--> 1 2 (computed-name 1) x))
          empty-language
          (--> 1 3 (computed-name 1) x))
         1)
        '(3))
  
  (let ()
    (define-language e1
      (e 1))
    (define-language e2
      (e 2))
    (define red1 (reduction-relation e1 (--> e (e e))))
    (define red2 (extend-reduction-relation red1 e2 (--> ignoreme ignoreme)))
    (test (apply-reduction-relation red1 1) '((1 1)))
    (test (apply-reduction-relation red1 2) '())
    (test (apply-reduction-relation red2 1) '())
    (test (apply-reduction-relation red2 2) '((2 2))))
  
  (let ()
    (define red1 (reduction-relation empty-language
                                     (--> a (a a) 
                                          a)
                                     (--> b (b b) 
                                          b)
                                     (--> q x)))
    (define red2 (extend-reduction-relation red1
                                            empty-language
                                            (--> a (c c)
                                                 a)
                                            (--> q z)))
    (test (apply-reduction-relation red1 (term a)) (list (term (a a))))
    (test (apply-reduction-relation red1 (term b)) (list (term (b b))))
    (test (apply-reduction-relation red1 (term q)) (list (term x)))
    (test (apply-reduction-relation red2 (term a)) (list (term (c c))))
    (test (apply-reduction-relation red2 (term b)) (list (term (b b))))
    (test (apply-reduction-relation red2 (term q)) (list (term z) (term x))))
  
  (let ()
    (define red1 
      (reduction-relation
       empty-language
       (==> a (a a) 
            a)
       (==> b (b b) 
            b)
       (==> q w)
       with
       [(--> (X a) (X b)) (==> a b)]))
    
    (define red2 
      (extend-reduction-relation
       red1
       empty-language
       (==> a (c c)
            a)
       (==> q z)
       with
       [(--> (X a) (X b)) (==> a b)]))
    
    (test (apply-reduction-relation red1 (term (X a))) (list (term (X (a a)))))
    (test (apply-reduction-relation red1 (term (X b))) (list (term (X (b b)))))
    (test (apply-reduction-relation red1 (term (X q))) (list (term (X w))))
    (test (apply-reduction-relation red2 (term (X a))) (list (term (X (c c)))))
    (test (apply-reduction-relation red2 (term (X b))) (list (term (X (b b)))))
    (test (apply-reduction-relation red2 (term (X q))) (list (term (X z)) 
                                                             (term (X w)))))
  
  (test (reduction-relation->rule-names
         (reduction-relation
          empty-language
          (--> x y a)))
        '(a))
  
  (test (reduction-relation->rule-names
         (reduction-relation
          empty-language
          (--> x y a)
          (--> y z b)
          (--> z w c)))
        '(a b c))
  
  (test (reduction-relation->rule-names
         (reduction-relation
          empty-language
          (--> x y a)
          (--> y z b)
          (--> z w c)
          (--> p q z)
          (--> q r y)
          (--> r p x)))
        '(a b c z y x))
  
  (test (reduction-relation->rule-names
         (reduction-relation
          empty-language
          (--> x y a (computed-name "x to y"))
          (--> y z (computed-name "y to z"))))
        '(a))
  
  (test (reduction-relation->rule-names
         (extend-reduction-relation
          (reduction-relation
           empty-language
           (--> x y a)
           (--> y z b)
           (--> z w c))
          empty-language
          (--> p q z)
          (--> q r y)
          (--> r p x)))
        '(a b c z y x))
  
    (test (reduction-relation->rule-names
           (union-reduction-relations
            (reduction-relation
             empty-language
             (--> x y a)
             (--> y z b)
             (--> z w c))
            (reduction-relation
             empty-language
             (--> p q z)
             (--> q r y)
             (--> r p x))))
        '(a b c z y x))
    
    (let ()
      (define-judgment-form empty-language
        #:mode (R I O)
        [(R a a)]
        [(R a b)])
      (test (apply-reduction-relation
             (reduction-relation
              empty-language
              (--> a any
                   (judgment-holds (R a any))))
             'a)
            '(a b)))
    
    ; a call to a metafunction that looks like a pattern variable
    (let ()
      (define result 'result)
      (define-language L
        (f any))
      (define-judgment-form L
        #:mode (J O)
        [(J (f_2))])
      (define-metafunction L
        [(f_2) ,result])
      (test (judgment-holds (J any) any)
            (list result)))
  
    ;                                                                 
    ;                                                                 
    ;                    ;;                        ;;                 
    ;                     ;                         ;            ;    
    ;   ;; ;;   ;;;    ;; ;   ;;;  ;;  ;;           ;     ;;;   ;;;;; 
    ;    ;;    ;   ;  ;  ;;  ;   ;  ;  ;            ;    ;   ;   ;    
    ;    ;     ;;;;;  ;   ;  ;;;;;   ;;    ;;;;;    ;    ;;;;;   ;    
    ;    ;     ;      ;   ;  ;       ;;             ;    ;       ;    
    ;    ;     ;      ;   ;  ;      ;  ;            ;    ;       ;   ;
    ;   ;;;;;   ;;;;   ;;;;;  ;;;; ;;  ;;         ;;;;;   ;;;;    ;;; 
    ;                                                                 
    ;                                                                 
    ;                                                                 
    ;                                                                 
  
    (let ()
      (define-language L
        (n number)
        (x variable))
      
      (test (redex-let L ([(n_1 n_2) '(1 2)])
                       (term (n_2 n_1)))
            (term (2 1)))
      (test (redex-let L ([(x_i ([x_0 n_0] ... [x_i n_i] [x_i+1 n_i+1] ...))
                           '(b ([a 1] [b 2] [c 3]))])
                       (term n_i))
            2)
      (test (with-handlers ([exn:fail:redex? exn-message])
              (redex-let L ([(n) 1]) 'no-exn))
            "redex-let: term 1 does not match pattern (n)")
      (test (with-handlers ([exn:fail:redex? exn-message])
              (redex-let L ([(n_1 ... n_i n_i+1 ...) '(1 2 3)]) 'no-exn))
            "redex-let: pattern (n_1 ... n_i n_i+1 ...) matched term (1 2 3) multiple ways")
      (test (redex-let L ([n_1 1])
                       (redex-let L ([n_1 2] [n_2 (term n_1)])
                                  (term (n_1 n_2))))
            (term (2 1)))
      (test (redex-let L ([n_1 1])
                       (redex-let* L ([n_1 2] [n_2 (term n_1)])
                                   (term (n_1 n_2))))
            (term (2 2)))
      
      (test (redex-let L ([(n_1 n_1) '(1 1)]) (term n_1))
            1)
      (test
       (redex-let* L ([(n_1) '(1)] [n_1 1]) (term n_1))
       1)
      (exec-syntax-error-tests "syn-err-tests/redex-let.rktd"))
  

;                                                                                                                                              
;                                                                                                                                              
;                                                                                                                                              
;       ;            ;;    ;                           ;               ;                                               ;;                      
;       ;           ;      ;                           ;               ;                               ;              ;                        
;       ;           ;                                                  ;                               ;              ;                        
;    ;;;;   ;;;    ;;;     ;    ;;;;    ;;;            ;    ;   ;   ;;;;   ;;;;  ;;;;;   ;;;   ;;;;   ;;;            ;;;    ;;;   ; ;;   ;;;;; 
;   ;   ;  ;   ;    ;      ;    ;   ;  ;   ;  ;;;;;    ;    ;   ;  ;   ;  ;   ;  ; ; ;  ;   ;  ;   ;   ;     ;;;;;    ;    ;   ;  ;;  ;  ; ; ; 
;   ;   ;  ;;;;;    ;      ;    ;   ;  ;;;;;           ;    ;   ;  ;   ;  ;   ;  ; ; ;  ;;;;;  ;   ;   ;              ;    ;   ;  ;   ;  ; ; ; 
;   ;   ;  ;        ;      ;    ;   ;  ;               ;    ;   ;  ;   ;  ;   ;  ; ; ;  ;      ;   ;   ;              ;    ;   ;  ;      ; ; ; 
;   ;   ;  ;   ;    ;      ;    ;   ;  ;   ;           ;    ;  ;;  ;   ;  ;   ;  ; ; ;  ;   ;  ;   ;   ;              ;    ;   ;  ;      ; ; ; 
;    ;;;;   ;;;     ;      ;    ;   ;   ;;;            ;     ;; ;   ;;;;   ;;;;  ; ; ;   ;;;   ;   ;    ;;            ;     ;;;   ;      ; ; ; 
;                                                      ;                      ;                                                                
;                                                      ;                  ;   ;                                                                
;                                                    ;;                    ;;;                                                                 

    (exec-syntax-error-tests "syn-err-tests/judgment-form-definition.rktd")
    (exec-syntax-error-tests "syn-err-tests/judgment-holds.rktd")
    
    (let ()
      (define-language nats
        (n z (s n)))
      
      (define-judgment-form nats
        #:mode (sumi I I O)
        #:contract (sumi n n n)
        [(sumi z n n)]
        [(sumi (s n_1) n_2 (s n_3))
         (sumi n_1 n_2 n_3)])
      (test (judgment-holds (sumi z (s z) n) n)
            (list (term (s z))))
      (test (judgment-holds (sumi (s (s z)) (s z) n) n)
            (list (term (s (s (s z))))))
      (test (judgment-holds (sumi ,'z (s z) (s z))) #t)
      
      (define-judgment-form nats
        #:mode (sumi2 I I O)
        #:contract (sumi2 n n n)
        [------------- sumz ;; symbol name
         (sumi2 z n n)]
        [(sumi2 n_1 n_2 n_3)
         ---------------------------  "sumn" ;; string name
         (sumi2 (s n_1) n_2 (s n_3))])
      (test (judgment-holds (sumi2 z (s z) n) n)
            (list (term (s z))))
      (test (judgment-holds (sumi2 (s (s z)) (s z) n) n)
            (list (term (s (s (s z))))))

      (define-judgment-form nats
        #:mode (sumo O O I)
        #:contract (sumo n n n)
        [(sumo z n n)]
        [(sumo (s n_1) n_2 (s n_3))
         (sumo n_1 n_2 n_3)])
      (test (judgment-holds (sumo n_1 n_2 z) ([,'n_1 n_1] [,'n_2 n_2]))
            (list (term ([n_1 z] [n_2 z]))))
      (test (judgment-holds (sumo n_1 n_2 (s z)) ([,'n_1 n_1] [,'n_2 n_2]))
            (list (term ([n_1 (s z)] [n_2 z]))
                  (term ([n_1 z] [n_2 (s z)]))))
      
      (define-judgment-form nats
        #:mode (sumo-ls O O I)
        [(sumo-ls (s n_1) n_2 n_3)
         (sumo (s n_1) n_2 n_3)])
      (test (judgment-holds (sumo-ls n_1 n_2 (s z)) ([,'n_1 n_1] [,'n_2 n_2]))
            (list (term ([n_1 (s z)] [n_2 z]))))
      (test (judgment-holds (sumo-ls (s n_1) n_2 (s z))) #t)
      (test (judgment-holds (sumo-ls z n_2 (s z))) #f)
      (test (judgment-holds (sumo-ls z n_2 (s z)) whatever) (list))
      
      (define-judgment-form nats
        #:mode (sumo-lz O O I)
        [(sumo-lz z n_2 n_3)
         (sumo z n_2 n_3)])
      (test (judgment-holds (sumo-lz n_1 n_2 (s z)) ([,'n_1 n_1] [,'n_2 n_2]))
            (list (term ([n_1 z] [n_2 (s z)]))))
      
      (define-judgment-form nats
        #:mode (member O I)
        [(member n_i (n_0 ... n_i n_i+1 ...))])
      
      (test (judgment-holds (member n (z (s z) z (s (s z)))) n)
            (list (term (s (s z))) (term (s z)) (term z)))
      
      (define-judgment-form nats
        #:mode (has-zero I)
        [(has-zero (n ...))
         (member z (n ...))])
      
      (test (judgment-holds (has-zero ((s z) z (s (s z))))) #t)
      
      (define-judgment-form nats
        #:mode (le2 I)
        [(le2 n)
         (le (add2 n) (s (s (s (s z)))))])
      
      (define-judgment-form nats
        #:mode (le I I)
        [(le z n)]
        [(le (s n_1) (s n_2))
         (le n_1 n_2)])
      
      (define-metafunction nats
        add2 : n -> n
        [(add2 n) (s (s n))])
      
      (test (judgment-holds (le2 (s (s z)))) #t)
      (test (judgment-holds (le2 (s (s (s z))))) #f)
      
      (define-judgment-form nats
        #:mode (uses-add2 I O)
        #:contract (uses-add2 n n)
        [(uses-add2 n_1 n_2)
         (sumo n_2 n_3 n_1)
         (where n_2 (add2 n_3))])
      
      (test (judgment-holds (uses-add2 (s (s (s (s z)))) n) n)
            (list (term (s (s (s z))))))
      
      (define-judgment-form nats
        #:mode (add1 I O)
        #:contract (add1 n n)
        [(add1 n (s n))])
      
      (define-judgment-form nats
        #:mode (map-add1 I O)
        #:contract (map-add1 (n ...) (n ...))
        [(map-add1 (n ...) (n_+ ...))
         (add1 n n_+) ...])
      
      (test (judgment-holds (map-add1 () (n ...))
                            (n ...))
            (list (term ())))
      
      (test (judgment-holds (map-add1 (z (s z) (s (s z))) (n ...))
                            (n ...))
            (list (term ((s z) (s (s z)) (s (s (s z)))))))

      (define-judgment-form nats
        #:mode (map-add1-check I O)
        #:contract (map-add1-check (n ...) (n ...))
        [(map-add1-check (n ...) ((s n) ...))
         (add1 n (s n)) ...])
      
      (test (judgment-holds (map-add1-check (z (s z) (s (s z))) (n ...))
                            (n ...))
            (list (term ((s z) (s (s z)) (s (s (s z)))))))
      
      (define-judgment-form nats
        #:mode (add-some-noz I O)
        #:contract (add-some-noz n n)
        [(add-some-noz z z)]
        [(add-some-noz (s n) (s n))]
        [(add-some-noz (s n) (s (s n)))])
      
      (define-judgment-form nats
        #:mode (map-add-some-noz I O)
        #:contract (map-add-some-noz (n ...) (n ...))
        [(map-add-some-noz (n ...) (n_+ ...))
         (add-some-noz n n_+) ...])
      
      (test (sort (judgment-holds (map-add-some-noz (z (s z) (s (s z))) (n ...))
                                  (n ...))
                  string<=?
                  #:key (λ (x) (format "~s" x)))
            (list (term (z (s (s z)) (s (s (s z)))))
                  (term (z (s (s z)) (s (s z))))
                  (term (z (s z) (s (s (s z)))))
                  (term (z (s z) (s (s z))))))
      
      (define-judgment-form nats
        #:mode (add-some I O)
        #:contract (add-some n n)
        [(add-some n n)]
        [(add-some n (s n))])
      
      (define-judgment-form nats
        #:mode (map-add-some-one I O)
        #:contract (map-add-some-one (n ...) (n ...))
        [(map-add-some-one (n ...) ((s n) ...))
         (add-some n (s n)) ...])
      
      (test (judgment-holds (map-add-some-one (z (s z) (s (s z))) (n ...))
                            (n ...))
            (list (term ((s z) (s (s z)) (s (s (s z)))))))
      
      (define-judgment-form nats
        #:mode (hyphens I)
        [(hyphens b)
         -----------
         (hyphens a)]
        [(hyphens c)
         -
         (hyphens b)]
        [(hyphens c)])
      (test (judgment-holds (hyphens a)) #t)
      
      (let-syntax ([test-trace 
                    (syntax-rules ()
                      [(_ expr trace-spec expected)
                       (test (let ([trace (open-output-string)])
                               (parameterize ([current-output-port trace]
                                              [current-traced-metafunctions trace-spec])
                                 expr)
                               (get-output-string trace))
                             expected)])])
        (test-trace (judgment-holds (sumi (s z) (s (s z)) n) n)
                    '(sumi)
                    #reader scribble/reader
                    @string-append{>(sumi '(s z) '(s (s z)) '_)
                                   > (sumi 'z '(s (s z)) '_)
                                   < '((sumi z (s (s z)) (s (s z))))
                                   <'((sumi (s z) (s (s z)) (s (s (s z)))))
                            
                                  })
        (test-trace (judgment-holds (sumo n_1 n_2 (s z)))
                    'all
                    #reader scribble/reader
                    @string-append{>(sumo '_ '_ '(s z))
                                   > (sumo '_ '_ 'z)
                                   < '((sumo z z z))
                                   <'((sumo (s z) z (s z)) (sumo z (s z) (s z)))
                            
                                  })
        (test-trace (letrec ([f (match-lambda
                                  ['z #t]
                                  [`(s ,n) (f n)])])
                      (define-judgment-form nats
                        #:mode (ext-trace I I)
                        [(ext-trace z (side-condition n (f (term n))))]
                        [(ext-trace (s n_1) n_2)
                         (ext-trace n_1 n_2)])
                      (trace f)
                      (judgment-holds (ext-trace (s z) (s z))))
                    'all
                    #reader scribble/reader
                    @string-append{>(ext-trace '(s z) '(s z))
                                   > (ext-trace 'z '(s z))
                                   > >(f '(s z))
                                   > >(f 'z)
                                   < <#t
                                   < '((ext-trace z (s z)))
                                   <'((ext-trace (s z) (s z)))
                                  
                                  })))
    
    
    
    (let ()
      (define-judgment-form empty-language
        #:mode (R I I)
        [(side-condition (different any_a any_b))
         -----
         (R any_a any_b)])
      (define-metafunction empty-language
        [(different any_a any_a) #f]
        [(different any_a any_b) #t])
      (test (judgment-holds (R 1 2)) #t)
      (test (judgment-holds (R 1 1)) #f)
      (test (term (R 1 2)) #t)
      (test (term (R 1 1)) #f))
    
    (let ()
      (define-judgment-form empty-language
        #:mode (J I O)
        [(J any_2 any_3)
         -----------------
         (J (any_2) any_3)]
        [---------------------
         (J variable variable)])
      
      
      (define-extended-judgment-form empty-language J 
        #:mode (J2 I O)
        
        [------------------
         (J2 number number)]
        
        [(J2 any_1 any_3)
         ------------------------
         (J2 (any_1 any_2) any_3)])
      
      (test (judgment-holds (J (x) any) any) '(x))
      (test (judgment-holds (J2 (1 y) any) any) '(1))
      (test (judgment-holds (J2 (x y) any) any) '(x))
      (test (judgment-holds (J2 ((((x) z) y)) any) any) '(x))
      (test (judgment-holds (J2 ((((11) z) y)) any) any) '(11)))
    
    (let ()
      
      (define-language L1
        (n 1 2 3))
      
      (define-extended-language L2 L1
        (n .... 4 5 6))
      
      (define-judgment-form L1
        #:mode (J1 I O)
        [-----------
         (J1 n_1 n_1)])
      
      (define-extended-judgment-form L2 J1 
        #:mode (J2 I O))
      
      (test (judgment-holds (J1 1 any) any) '(1))
      (test (judgment-holds (J2 1 any) any) '(1))
      (test (judgment-holds (J2 4 any) any) '(4)))
    
    (let ()
      (define-language L (N ::= z (s N) (t N)))
      
      (define-judgment-form L
        #:mode (J2 I O)
        [--------  "one"
         (J2 1 1)]
        [--------  two
         (J2 1 2)])
      
      (test (build-derivations (J2 1 any))
            (list (derivation '(J2 1 1) "one" '())
                  (derivation '(J2 1 2) "two" '())))
      
      
      
      (define-judgment-form L
        #:contract (K any any)
        #:mode (K I O)
        [-----------
         (K () z)]
        [(K any_1 N) ...
         ---------------------------
         (K (any_1 ...) (N ...))])
      
      
      
      (test (build-derivations (K (()) any))
            (list (derivation '(K (()) (z))
			      #f
                              (list (derivation '(K () z) #f '())))))
      
      (test
       (build-derivations (K (() ()) any))
       (list (derivation 
              '(K (() ()) (z z))
	      #f
              (list
               (derivation '(K () z) #f '())
               (derivation '(K () z) #f '())))))
      
      (define-judgment-form L
        #:contract (J any any)
        #:mode (J I O)
        [--------
         (J () z)]
        [(J any_1 N)  (J any_2 N)
         ----------------------------
         (J (any_1 any_2) (s N))]
        [(J any N)
         ---------------
         (J (any) (s N))])
      
      (test (build-derivations 
             (J ((()) (())) N))
            (list (derivation
                   '(J ((()) (())) (s (s z)))
		   #f
                   (list (derivation 
                          '(J (()) (s z))
			  #f
                          (list
                           (derivation
                            '(J () z)
			    #F
                            '())))
                         (derivation 
                          '(J (()) (s z))
			  #f
                          (list
                           (derivation
                            '(J () z)
			    #f
                            '())))))))
      
      (define-judgment-form L
        #:mode (J3 I O)
        [(J any_1 any_2)
         ------------
         (J3 any_1 any_2)])
      
      (test (build-derivations (J3 (()) N))
            (list (derivation
                   '(J3 (()) (s z))
		   #f
                   (list
                    (derivation
                     '(J (()) (s z))
		     #f
                     (list 
                      (derivation 
                       '(J () z)
		       #f
                       '()))))))))
    (let ()
      (define-language U
        (n Z (S n)))
      
      (define-judgment-form U
        #:mode (jsum I I I)
        [(jsum n Z n)]
        [(jsum n_1 (S n_2) (S n_3))
         (jsum n_1 n_2 n_3)])
      
      (define-relation U
        sum ⊆ n x n x n
        [(sum n Z n)]
        [(sum n_1 (S n_2) (S n_3))
         (sum n_1 n_2 n_3)])
      
      (define-relation U
        [(rjsum n_1 n_2 n_3)
         (jjsum n_1 n_2 n_3)])
      
      (define-judgment-form U
        #:mode (jjsum I I I)
        [(jjsum n_1 n_2 n_3)
         (rrsum n_1 n_2 n_3)])
      
      (define-relation U
        [(rrsum n_1 n_2 n_3)
         (jsum n_1 n_2 n_3)])
      
      (test (term (sum Z Z Z)) #t)
      (test (term (sum Z Z (S Z))) #f)
      (test (term (sum (S Z) (S Z) (S (S Z)))) #t)
      (test (term (sum (S Z) (S (S Z)) (S (S Z)))) #f)
      (test (term (sum (S (S Z)) (S (S Z)) (S (S (S (S Z)))))) #t)
      (test (term (sum (S (S Z)) (S (S Z)) (S (S (S Z))))) #f)
      (test (term (jsum Z Z Z)) #t)
      (test (term (jsum Z Z (S Z))) #f)
      (test (term (jsum (S Z) (S Z) (S (S Z)))) #t)
      (test (term (jsum (S Z) (S (S Z)) (S (S Z)))) #f)
      (test (term (jsum (S (S Z)) (S (S Z)) (S (S (S (S Z)))))) #t)
      (test (term (jsum (S (S Z)) (S (S Z)) (S (S (S Z))))) #f)
      (test (term (rjsum Z Z Z)) #t)
      (test (term (rjsum Z Z (S Z))) #f)
      (test (term (rjsum (S Z) (S Z) (S (S Z)))) #t)
      (test (term (rjsum (S Z) (S (S Z)) (S (S Z)))) #f)
      (test (term (rjsum (S (S Z)) (S (S Z)) (S (S (S (S Z)))))) #t)
      (test (term (rjsum (S (S Z)) (S (S Z)) (S (S (S Z))))) #f))
    
    (let ()
      (define-judgment-form empty-language
        #:mode (Q I O)
        [(Q number_1 ,(+ (term number_1) (term number_1)))]
        [(Q (number_1 number_2) number_3)
         (Q ,(+ (term number_1) (term number_2)) number_3)])
      
      (test (judgment-holds (Q 1 2))
                  #t)
      (test (judgment-holds (Q 1 3))
                  #f)
      (test (judgment-holds (Q 1 number_1) number_1)
                  '(2))
      (test (judgment-holds (Q 7 14))
                  #t)
      (test (judgment-holds (Q 7 symbol))
                  #f)
      (test (judgment-holds (Q 7 number_1) number_1)
                  '(14))
      (test (judgment-holds (Q (1 2) 6))
                  #t)
      (test (judgment-holds (Q (1 3) 6))
                  #f)
      (test (judgment-holds (Q (3 4) number_1) number_1)
                  '(14)))

    
    (parameterize ([current-namespace (make-base-namespace)])
      (eval '(require errortrace))
      (eval '(require redex/reduction-semantics))
      (eval '(define-language L))
      (eval '(define-judgment-form L
               #:mode (J I)
               [(J a)
                (J b)]
               [(J b)]))
      (test (eval '(judgment-holds (J a))) #t))
    
    (parameterize ([current-namespace (make-base-namespace)])
      (eval '(require redex/reduction-semantics))
      (eval '(define-language L
               (s a b c)))
      (eval '(define-judgment-form L
               #:mode (ctc-fail I O)
               #:contract (ctc-fail s s)
               [(ctc-fail a q)]
               [(ctc-fail b s)
                (ctc-fail q s)]
               [(ctc-fail c s)
                (ctc-fail a s)]))
      (exec-runtime-error-tests "run-err-tests/judgment-form-contracts.rktd")
      (exec-runtime-error-tests "run-err-tests/judgment-form-undefined.rktd")
      (exec-runtime-error-tests "run-err-tests/judgment-form-ellipses.rktd"))
    
  
;                                                                               
;                                                                               
;                                                                               
;       ;            ;;    ;                                                    
;       ;           ;      ;                          ;                         
;       ;           ;                                 ;                         
;    ;;;;   ;;;    ;;;     ;    ;;;;    ;;;          ;;;     ;;;   ; ;;   ;;;;; 
;   ;   ;  ;   ;    ;      ;    ;   ;  ;   ;  ;;;;;   ;     ;   ;  ;;  ;  ; ; ; 
;   ;   ;  ;;;;;    ;      ;    ;   ;  ;;;;;          ;     ;;;;;  ;   ;  ; ; ; 
;   ;   ;  ;        ;      ;    ;   ;  ;              ;     ;      ;      ; ; ; 
;   ;   ;  ;   ;    ;      ;    ;   ;  ;   ;          ;     ;   ;  ;      ; ; ; 
;    ;;;;   ;;;     ;      ;    ;   ;   ;;;            ;;    ;;;   ;      ; ; ; 
;                                                                               
;                                                                               
;                                                                               
  
  (test (let ()
          (define-term x 1)
          (term (x x)))
        (term (1 1)))
  (test (let ()
          (define-term x 1)
          (let ([x 'whatever])
            (term (x x))))
        (term (x x)))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; examples from doc.txt
  ;;
  
  (define-language lc-lang
    (e (e e ...)
       x
       v)
    (c (v ... c e ...)
       hole)
    (v (lambda (x ...) e))
    (x variable-not-otherwise-mentioned))
  
  (test (let ([m (redex-match lc-lang e (term (lambda (x) x)))])
          (and m (length m)))
        1)
  
  (define-extended-language qabc-lang lc-lang (q a b c))
  
  (test (redex-match qabc-lang
                     e
                     (term (lambda (a) a)))
        #f)
  
  (test (let ([m (redex-match qabc-lang
                              e
                              (term (lambda (z) z)))])
          (and m (length m)))
        1)
  
  (require mzlib/list)
  (let ()
    (define-metafunction lc-lang
      free-vars : e -> (x ...)
      [(free-vars (e_1 e_2 ...))
       (∪ (free-vars e_1) (free-vars e_2) ...)]
      [(free-vars x) (x)]
      [(free-vars (lambda (x ...) e))
       (- (free-vars e) (x ...))])
    
    (define-metafunction lc-lang
      ∪ : (x ...) ... -> (x ...)
      [(∪ (x_1 ...) (x_2 ...) (x_3 ...) ...)
       (∪ (x_1 ... x_2 ...) (x_3 ...) ...)]
      [(∪ (x_1 ...))
       (x_1 ...)]
      [(∪) ()])
    
    (define-metafunction lc-lang
      - : (x ...) (x ...) -> (x ...)
      [(- (x ...) ()) (x ...)]
      [(- (x_1 ... x_2 x_3 ...) (x_2 x_4 ...))
       (- (x_1 ... x_3 ...) (x_2 x_4 ...))
       (side-condition (not (memq (term x_2) (term (x_3 ...)))))]
      [(- (x_1 ...) (x_2 x_3 ...))
       (- (x_1 ...) (x_3 ...))])
    
    (test (term (∪)) (term ()))
    (test (term (∪ (x y) (z a) (b c))) (term (x y z a b c)))
    
    (test (term (- (x y) ())) (term (x y)))
    (test (term (- (x y) (x))) (term (y)))
    (test (term (- (y x) (x))) (term (y)))
    (test (term (- (x x x x x) (x))) (term ()))
    
    (test (term (free-vars (lambda (x) (x y)))) (list 'y))
    (test (term (free-vars (a (b (c (d e)))))) (term (a b c d e))))

  (test (variable-not-in (term (x y z)) 'x)
        (term x1))
  
  (test (variable-not-in (term (y z)) 'x)
        (term x))
  (test (variable-not-in (term (x x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)) 'x)
        (term x11))
  (test (variable-not-in (term (x x11)) 'x)
        (term x1))
  (test (variable-not-in (term (x x1 x2 x3)) 'x1)
        (term x4))
  (test (variable-not-in (term (x x1 x1 x2 x2)) 'x)
        (term x3))
  
  (test (variables-not-in (term (x y z)) '(x))
        '(x1))
  (test (variables-not-in (term (x2 y z)) '(x x x))
        '(x x1 x3))
  
  (test ((term-match/single empty-language
                            [(variable_x variable_y)
                             (cons (term variable_x)
                                   (term variable_y))])
         '(x y))
        '(x . y))
  
  (test ((term-match/single empty-language
                            [(side-condition (variable_x variable_y)
                                             (eq? (term variable_x) 'x))
                             (cons (term variable_x)
                                   (term variable_y))])
         '(x y))
        '(x . y))
  
  (test ((term-match/single empty-language [() 'a] [() 'b])
         '())
        'a)

  (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                        ((λ (x) #t) (λ (x) 'wrong-exn)))
          ((term-match/single empty-language
                              [(number_1 ... number_2 ...) 1])
           '(1 2 3))
          'no-exn)
        'right-exn)
  
  (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                        ((λ (x) #t) (λ (x) 'wrong-exn)))
          ((term-match/single empty-language
                              [(number_1 ... number_2 ...) 1])
           'x)
          'no-exn)
        'right-exn)
  
  (test ((term-match empty-language
                     [(number_1 ... number_2 ...) 1])
         'x)
        '())
                             
  (define-language x-is-1-language
    [x 1])
  
  (test ((term-match/single x-is-1-language
                            [(x x)
                             1])
         '(1 1))
        1)
  
  (test (call-with-values
         (λ () 
           ((term-match/single empty-language
                               [() (values 1 2)])
            '()))
         list)
        '(1 2))
  
  (test (let ([x 0])
          (cons ((term-match empty-language
                             [(any_a ... number_1 any_b ...)
                              (begin (set! x (+ x 1))
                                     (term number_1))])
                 '(1 2 3))
                x))
        '((1 2 3) . 3))
  
  (test ((term-match empty-language
                     [number_1
                      (term number_1)]
                     [number_1
                      (term number_1)])
         '1)
        '(1 1))
  
  (test (apply-reduction-relation
         (reduction-relation
          x-language
          (--> (x_one x_!_one x_!_one x_!_one)
               (x_one x_!_one)))
         (term (a a b c)))
        (list (term (a x_!_one))))
  
  ;; tests `where' clauses in reduction relation
  (test (apply-reduction-relation
         (reduction-relation empty-language
                             (--> number_1 
                                  any_y
                                  (where any_y ,(+ 1 (term number_1)))))
         3)
        '(4))
  
  ;; tests `where' clauses scoping
  (test (let ([x 5])
          (apply-reduction-relation
           (reduction-relation empty-language
                               (--> any 
                                    any_z
                                    (where any_y ,x)
                                    (where any_x 2)
                                    (where any_z ,(+ (term any_y) (term any_x)))))
           'whatever))
        '(7))
  
  ;; tests `where' clauses bind in side-conditions
  (test (let ([x 'unk])
          (apply-reduction-relation
           (reduction-relation empty-language
                               (--> any 
                                    the-result
                                    (where any_y any)
                                    (side-condition (eq? (term any_y) 'whatever))))
           'whatever))
        '(the-result))
  
  ;; test that where clauses bind in side-conditions that follow
  (let ([save1 #f]
        [save2 #f])
    (term-let ([any_y (term outer-y)])
              (test (begin (apply-reduction-relation
                            (reduction-relation empty-language
                                                (--> number_1 
                                                     any_y
                                                     (side-condition (set! save1 (term any_y)))
                                                     (where any_y inner-y)
                                                     (side-condition (set! save2 (term any_y)))))
                            3)
                           (list save1 save2))
                    (list 'outer-y 'inner-y))))
  
  (test (apply-reduction-relation
         (reduction-relation empty-language
                             (--> any 
                                  any_y
                                  (fresh x)
                                  (where any_y x)))
         'x)
        '(x1))
  
  (test (let ([not-and
               (λ () #f)])
          (redex-match empty-language
                       (side-condition any_1 (not-and))
                       1))
        #f)

  (let ()
    ;; tests where's ability to have redex patterns, not just syntax-case patterns
    (define-language var-lang [(x y z w) variable])
    
    (define red
      (reduction-relation
       var-lang
       (--> (x ...)
            (y ... z ...)
            (where (y ... w z ...) (x ...)))))
    
    (test (apply-reduction-relation red (term (a b c)))
          (list (term (a b)) (term (a c)) (term (b c)))))
  
  
  (let ([r (reduction-relation
            grammar
            (->1 1 2)
            (->2 3 4)
            (->4 5 6)
            with
            [(--> (side-condition (a number) (even? (term number))) b)
             (->1 a b)]
            [(--> (X 
                   (number number)
                   (X_1 X_1)
                   (M_!_1 M_!_1)
                   (1 ..._1 1 ..._1)
                   (1 ..._!_1 1 ..._!_1))
                  b)
             (->2 X b)]
            [(--> (a 1) b)
             (->3 a b)]
            [(->3 (a 2) b)
             (->4 a b)])])
    
    ; test that names are properly bound for side-conditions in shortcuts
    (let* ([lhs ((rewrite-proc-lhs (first (reduction-relation-make-procs r))) grammar)]
           [proc (third lhs)]
           [name (cadar (cddadr lhs))]
           [bind (λ (n) (make-bindings (list (make-bind name n))))])
      (test (and (proc (bind 4)) (not (proc (bind 3)))) #t))
    
    ; test binder renaming
    (let ([sym-mtch? (λ (rx) (λ (s) (and (symbol? s) (regexp-match? rx (symbol->string s)))))])
      (match (rewrite-proc-lhs (second (reduction-relation-make-procs r)))
        [`(3
           (,(and n1 (? (sym-mtch? #px"^number_\\d+$"))) ,n1)
           (,(and n2 (? (sym-mtch? #px"^X_1\\d+$"))) ,n2)
           (,(and m1 (? (sym-mtch? #px"^M_!_1\\d+$"))) ,m1)
           (1 ,(and ...1 (? (sym-mtch? #px"^\\.\\.\\._1\\d+$"))) 1 ,...1)
           (1 ,(and ...!1 (? (sym-mtch? #px"^\\.\\.\\._!_1\\d+$"))) 1 ,...!1))
         #t]
        [else #f]))
    
    ; test shortcut in terms of shortcut
    (test (match ((rewrite-proc-lhs (third (reduction-relation-make-procs r))) grammar)
            [`(list (list (side-condition 5 ,(? procedure?) ,_) 2) 1) #t]
            [else #f])
          #t))
  
  (let ([< (λ (c d) (string<? (car c) (car d)))])
    
    (let* ([R (reduction-relation
               empty-language
               (--> number (q ,(add1 (term number)))
                    (side-condition (odd? (term number)))
                    side-condition)
               (--> 1 4 plain)
               (==> 2 t
                    shortcut)
               with
               [(--> (q a) b)
                (==> a b)])]
           [c (make-coverage R)])
      (parameterize ([relation-coverage (list c)])
        (apply-reduction-relation R 4)
        (test (sort (covered-cases c) <)
              '(("plain" . 0) ("shortcut" . 0) ("side-condition" . 0)))
        
        (apply-reduction-relation R 3)
        (test (sort (covered-cases c) <)
              '(("plain" . 0) ("shortcut" . 0) ("side-condition" . 1)))
        
        (apply-reduction-relation* R 1)
        (test (sort (covered-cases c) <)
              '(("plain" . 1) ("shortcut" . 1) ("side-condition" . 2)))))
    
    (let ()
      (define-language L
        (e (e e)
           (delay e)
           +inf.0
           I)
        (v (delay e)
           +inf.0
           I))
      
      (define red
        (compatible-closure
         (reduction-relation
          L
          (--> (+inf.0 +inf.0) (+inf.0 +inf.0))
          (--> (I e) e))
         L
         e))
      
      (test (apply-reduction-relation* 
             red
             (term (I (delay (+inf.0 +inf.0))))
             #:stop-when (redex-match L v))
            (list (term (delay (+inf.0 +inf.0)))))
      
      (test (apply-reduction-relation* 
             red
             (term (I (delay (+inf.0 +inf.0)))))
            '()))
    
    (let* ([S (reduction-relation
               empty-language
               (--> 1 1 uno))]
           [S+ (extend-reduction-relation
                S empty-language
                (--> 2 2 dos))])
      (let ([c (make-coverage S+)])
        (parameterize ([relation-coverage (list c)])
          (apply-reduction-relation S (term 1))
          (test (sort (covered-cases c) <)
                '(("dos" . 0) ("uno" . 1)))))
      (let ([c (make-coverage S)])
        (parameterize ([relation-coverage (list c)])
          (apply-reduction-relation S+ (term 1))
          (test (sort (covered-cases c) <)
                '(("uno" . 1))))))
    
    (let* ([T (reduction-relation empty-language (--> any any))]
           [c (make-coverage T)])
      (parameterize ([relation-coverage (list c)])
        (apply-reduction-relation T (term q))
        (test (and (regexp-match #px"tl-test.(?:.+):\\d+:\\d+" (caar (covered-cases c))) #t)
              #t))))
  
  (let* ([R (reduction-relation
             empty-language
             (--> any any id))]
         [c (make-coverage R)]
         [c* (make-coverage R)])
    (parameterize ([relation-coverage (list c c*)])
      (apply-reduction-relation R 4)
      (test (covered-cases c) '(("id" . 1)))
      (test (covered-cases c*) '(("id" . 1)))))
  
  (let* ([< (λ (c d) 
              (let ([line-no (compose
                              string->number
                              second
                              (curry regexp-match #px".*:(\\d+):\\d+"))])
                (< (line-no (car c)) (line-no (car d)))))]
         [src-ok? (curry regexp-match? #px"tl-test.(?:.+):\\d+:\\d+")]
         [sorted-counts (λ (cc) (map cdr (sort (covered-cases cc) <)))])
    (define-metafunction empty-language
      [(f 1) 1]
      [(f 2) 2])
    (define-metafunction/extension f empty-language
      [(g 3) 3])
    (define-relation empty-language
      [(R number)
       ,(even? (term number))]
      [(R number)
       ,(= 3 (term number))])
    
    (let ([fc (make-coverage f)]
          [rc (make-coverage (reduction-relation empty-language (--> any any)))])
      (parameterize ([relation-coverage (list rc fc)])
        (term (f 2))
        (test (andmap (compose src-ok? car) (covered-cases fc))
              #t)
        (test (sorted-counts fc) '(0 1))
        
        (term (f 1))
        (term (f 1))
        (test (sorted-counts fc) '(2 1))))
    
    (let ([c (make-coverage f)])
      (parameterize ([relation-coverage (list c)])
        (term (g 1))
        (test (sorted-counts c) '(1 0))))
    (let ([c (make-coverage g)])
      (parameterize ([relation-coverage (list c)])
        (term (f 1))
        (test (sorted-counts c) '(1 0 0))))
    
    ;; coverage for define-relation not working since
    ;; it was changed to compile to judgment-form
    #;
    (let ([c (make-coverage R)])
      (parameterize ([relation-coverage (list c)])
        (term (R 2))
        (term (R 3))
        (term (R 5))
        (test (sorted-counts c) '(1 1))))
    
    (let ([c (make-coverage f)]
          [c* (make-coverage f)])
      (parameterize ([relation-coverage (list c* c)])
        (term (f 1))
        (test (sorted-counts c) '(1 0))
        (test (sorted-counts c*) '(1 0)))))
  
;                                                                       
;                                                                       
;                                                                       
;                                                                       
;                                                 ;;;                   
;   ;;                ;;           ;             ;; ;;                  
;   ;;                ;;            ;            ;; ;;                  
;  ;;;;;  ;;;    ;;; ;;;;;           ;;           ;;;      ;;;;   ;;;;  
;   ;;   ;; ;;  ;; ;  ;;  ;;;;;;;;     ;;        ;;;      ;; ;   ;;  ;; 
;   ;;   ;;;;;  ;;;   ;;              ;;;       ;;  ;  ;; ;;     ;;  ;; 
;   ;;   ;;       ;;  ;;            ;;;         ;;  ;;;;  ;;     ;;  ;; 
;   ;;   ;;  ;  ; ;;  ;;           ;;           ;;   ;;   ;; ;   ;;  ;; 
;   ;;;;  ;;;   ;;;   ;;;;         ;             ;;;;; ;;  ;;;;   ;;;;  
;                                                                       
;                                                                       
;                                                                       

  
  (define-syntax-rule 
    (capture-output arg1 args ...)
    (let ([p (open-output-string)])
      (parameterize ([current-output-port p]
                     [current-error-port p])
        arg1 args ...)
      (get-output-string p)))
  
  (let ()
    (define red (reduction-relation empty-language (--> 1 2)))
    (test (capture-output (test-->> red 1 2) (test-results))
          "One test passed.\n")
    (test (capture-output (test-->> red 2 3) (test-results))
          #rx"FAILED .*tl-test.(?:.+):[0-9.]+\nexpected: 3\n  actual: 2\n1 test failed \\(out of 1 total\\).\n"))
    
  (let ()
    (define red-share (reduction-relation 
                       empty-language
                       (--> a b)
                       (--> a c)
                       (--> c d)
                       (--> b d)))
    (test (capture-output (test-->> red-share (term a) (term d)) (test-results))
          "One test passed.\n"))
  
  (let ()
    (define red-cycle (reduction-relation 
                       empty-language
                       (--> a a)))
    (test (capture-output (test-->> red-cycle #:cycles-ok (term a)) (test-results))
          "One test passed.\n")
    (test (capture-output (test-->> red-cycle (term a)) (test-results))
          #rx"FAILED .*tl-test.(?:.+):[0-9.]+\nfound a cycle in the reduction graph\n1 test failed \\(out of 1 total\\).\n"))
  
  (let ()
    (define subred (reduction-relation empty-language (--> natural ,(- (term natural) 1))))
    (test (capture-output (test-->> subred #:pred (λ (x) #t) 1 -1) (test-results))
          "One test passed.\n")
    (test (capture-output (test-->> subred #:pred number? 1 -1) (test-results))
          "One test passed.\n")
    (test (capture-output (test-->> subred #:pred odd? 1 -1) (test-results))
         #rx"FAILED .*tl-test.rkt:[0-9.]+\nfound a term that failed #:pred: 0\n1 test failed \\(out of 1 total\\).\n"))
  
  (let ()
    (define-metafunction empty-language [(f any) ((any))])
    (test (capture-output (test-equal (term (f 1)) (term ((1))))
                          (test-results))
          "One test passed.\n"))
  
  (let ()
    (test (capture-output (test-predicate odd? 1)
                          (test-results))
          "One test passed.\n"))
  
  (let ()
    (define red (reduction-relation empty-language (--> any (any))))
    (test (capture-output (test--> red (term (1 2 3)) (term ((1 2 3)))) (test-results))
          "One test passed.\n"))
  
  (let ()
    (define red (reduction-relation empty-language 
                                    (--> any (any))
                                    (--> (any) any)))
    (test (capture-output (test--> red (term (x)) (term ((x))) (term x)) (test-results))
          "One test passed.\n")
    (test (capture-output (test--> red (term (x)) (term x) (term ((x)))) (test-results))
          "One test passed.\n"))
  
  (let ()
    (define-language L
      (i integer))
    
    (define R
      (reduction-relation
       L
       (--> i i)
       (--> i ,(add1 (term i)))))
    
    (define (mod2=? i j)
      (= (modulo i 2) (modulo j 2)))
    
    (test (capture-output (test--> R #:equiv mod2=? 7 1 0) (test-results))
          "One test passed.\n")
    (test (capture-output (test--> R #:equiv mod2=? 7 1) (test-results))
          #rx"FAILED .*tl-test\\.(?:.+):[0-9.]+\nexpected: 1\n  actual: 8\n  actual: 7\n1 test failed \\(out of 1 total\\).\n"))
  
  (let-syntax ([test-bad-equiv-arg
                (λ (stx)
                  (syntax-case stx ()
                    [(_ test-form)
                     (syntax/loc stx
                       (test-contract-violation
                        (test-form (reduction-relation empty-language (--> any any))
                                   #:equiv 1 2)
                        #:blaming "tl-test"))]))])
    (test-bad-equiv-arg test-->)
    (test-bad-equiv-arg test-->>))

  (let ()
    (capture-output (test-results))
    (define-language L)
    
    (define 1+
      (reduction-relation 
       L
       (--> number ,(add1 (term number)))))
    
    (define (equal-to-7 x) (= x 7))
    (test (capture-output (test-->>∃ #:steps 5 1+ 0 equal-to-7))
          #rx"^FAILED .*\nno term satisfying #<procedure:equal-to-7> reachable from 0 \\(within 5 steps\\)\n$")
    
    (test (capture-output (test-->>∃ 1+ 0 7)) "")
    (test (capture-output (test-->>E 1+ 0 7)) "")
    (test (capture-output (test-->>∃ #:steps +inf.0 1+ 0 7)) "")
    (test (capture-output (test-->>∃ 1+ 0 equal-to-7)) "")
    
    (define identity
      (reduction-relation
       L
       (--> any any)))
    
    (test (capture-output (test-->>∃ identity 0 1))
          #rx"^FAILED .*\nterm 1 not reachable from 0\n$")
    
    (test (capture-output (test-results)) "2 tests failed (out of 6 total).\n")
    
    (test-contract-violation
     (test-->>∃ 1+ 0 (λ (x y) x))
     #:blaming "tl-test"
     #:message "goal expression")
    (test-contract-violation
     (test-->>∃ 1 0 1)
     #:blaming "tl-test"
     #:message "reduction relation expression")
    (test-contract-violation
     (test-->>∃ #:steps 1.1 1+ 0 1)
     #:blaming "tl-test"
     #:message "steps expression"))
  
  (print-tests-passed 'tl-test.rkt)
