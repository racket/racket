#lang racket/base
(require
  "test-utils.rkt"
  rackunit
  racket/list
  (for-syntax racket/base syntax/parse)
  syntax/location syntax/srcloc
  (rep type-rep)
  (r:infer infer promote-demote)

  (types union substitute numeric-tower utils abbrev))

(provide tests)
(gen-test-main)

(define-syntax-rule (fv-t ty elems ...)
  (let ([ty* ty])
    (test-check (format "~a" 'ty)
                equal?
                (fv ty*)
                (list (quote elems) ...))))

(begin-for-syntax
  (define-splicing-syntax-class result
    (pattern (~seq) #:with v #'#f #:with exp #'#f)
    (pattern (~seq #:result [v:expr exp:expr])))
  (define-splicing-syntax-class vars
    (pattern (~seq) #:with vars #'empty)
    (pattern (~seq #:vars vars:expr)))
  (define-splicing-syntax-class indices
    (pattern (~seq) #:with indices #'empty)
    (pattern (~seq #:indices indices:expr)))
  (define-splicing-syntax-class pass
    (pattern (~seq) #:with pass #'#t)
    (pattern #:pass #:with pass #'#t)
    (pattern #:fail #:with pass #'#f)))

(define-syntax (infer-t stx)
  (syntax-parse stx
    ([_ S:expr T:expr . rest]
     (syntax/loc stx
       (infer-l (list S) (list T) . rest)))))

(define-syntax (infer-l stx)
  (syntax-parse stx
    ([_ S:expr T:expr :vars :indices R:result :pass]
     (quasisyntax/loc stx
       (test-case (format "~a ~a~a" S T (if pass "" " should fail"))
         (with-check-info (['location (build-source-location-list (quote-srcloc #,stx))])
           (define substitution (infer vars indices S T R.v))
           (define result (and substitution R.v (subst-all substitution R.v)))
           (cond
             [pass
               (unless substitution
                 (fail-check "Could not infer a substitution"))
               (when result
                 (with-check-info (['actual result] ['expected R.exp])
                   (unless (equal? result R.exp)
                     (fail-check "Did not infer the expected result."))))]
             [fail
               (when substitution
                 (fail-check "Inferred an unexpected substitution."))])))))))


(define-syntax-rule (i2-t t1 t2 (a b) ...)
  (test-equal? (format "~a ~a" t1 t2)
               (infer (fv t1) null (list t2) (list t1) (-lst* (make-F a) ...) #f)
               (make-immutable-hash (list (cons a (t-subst b)) ...))))

(define-syntax-rule (i2-l t1 t2 fv (a b) ...)
  (test-equal? (format "~a ~a" t2 t1)
               (infer fv null t2 t1 (-lst* (make-F a) ...) #f)
               (make-immutable-hash (list (cons a (t-subst b)) ...))))

(define (f t1 t2) (infer (fv t1) null (list t1) (list t2) #f))

(define-syntax-rule (i2-f t1 t2)
  (infer-t t2 t1 #:vars (fv t2) #:fail))

(define N -Number)
(define B -Boolean)

(define-syntax (pd-t stx)
  (syntax-parse stx
    ([_ S:expr (vars:id ...) D:expr P:expr]
     (quasisyntax/loc stx
       (test-case (format "~a => ~a < ~a < ~a" '(vars ...) 'D 'S 'P)
         (define S-v S)
         (define promoted (var-promote S-v '(vars ...)))
         (define demoted (var-demote S-v '(vars ...)))
         #,(syntax/loc stx
             (check-equal? promoted P "Promoted value doesn't match expected."))
         #,(syntax/loc stx
             (check-equal? demoted D "Demoted value doesn't match expected.")))))))


(define fv-tests
  (test-suite "Tests for fv"
              (fv-t -Number)
              [fv-t (-v a) a]
              [fv-t (-poly (a) a)]
              [fv-t (-poly (a b c d e) a)]
              [fv-t (-poly (b) (-v a)) a]
              [fv-t (-poly (b c d e) (-v a)) a]
              [fv-t (-mu a (-lst a))]
              [fv-t (-mu a (-lst (-pair a (-v b)))) b]

              [fv-t (->* null (-v a) -Number) a] ;; check that a is CONTRAVARIANT
              ))

(define pd-tests
  (test-suite "Tests for var-promote/var-demote"
    (pd-t Univ () Univ Univ)
    (pd-t (-v a) () (-v a) (-v a))
    (pd-t (-v a) (a) -Bottom Univ)
    (pd-t (-v a) (b) (-v a) (-v a))

    (pd-t (-vec (-v a)) (a) (-vec -Bottom) (-vec Univ))
    (pd-t (-vec (-lst (-v a))) (a) (-vec -Bottom) (-vec Univ))
    (pd-t (-vec (-v a)) (b) (-vec (-v a)) (-vec (-v a)))

    (pd-t (-box (-v a)) (a) (-box -Bottom) (-box Univ))
    (pd-t (-box (-lst (-v a))) (a) (-box -Bottom) (-box Univ))
    (pd-t (-box (-v a)) (b) (-box (-v a)) (-box (-v a)))

    (pd-t (-channel (-v a)) (a) (-channel -Bottom) (-channel Univ))
    (pd-t (-channel (-lst (-v a))) (a) (-channel -Bottom) (-channel Univ))
    (pd-t (-channel (-v a)) (b) (-channel (-v a)) (-channel (-v a)))

    (pd-t (-thread-cell (-v a)) (a) (-thread-cell -Bottom) (-thread-cell Univ))
    (pd-t (-thread-cell (-lst (-v a))) (a) (-thread-cell -Bottom) (-thread-cell Univ))
    (pd-t (-thread-cell (-v a)) (b) (-thread-cell (-v a)) (-thread-cell (-v a)))

    (pd-t (-HT (-v a) (-v a)) (a) (-HT -Bottom -Bottom) (-HT Univ Univ))
    (pd-t (-HT (-lst (-v a)) (-lst (-v a))) (a) (-HT -Bottom -Bottom) (-HT Univ Univ))
    (pd-t (-HT (-v a) (-v a)) (b) (-HT (-v a) (-v a)) (-HT (-v a) (-v a)))

    (pd-t (-Param (-v a) (-v b)) (a b) (-Param Univ -Bottom) (-Param -Bottom Univ))
    (pd-t (-Param (-lst (-v a)) (-lst (-v b))) (a b)
          (-Param (-lst Univ) (-lst -Bottom))
          (-Param (-lst -Bottom) (-lst Univ)))

    (pd-t (->* (list (-lst (-v a))) (-lst (-v a)) (-lst (-v a))) (a)
          (->* (list (-lst Univ)) (-lst Univ) (-lst -Bottom))
          (->* (list (-lst -Bottom)) (-lst -Bottom) (-lst Univ)))

    (pd-t (->key #:a (-lst (-v a)) #t #:b (-lst (-v a)) #f -Symbol) (a)
          (->key #:a (-lst Univ) #t #:b (-lst Univ) #f -Symbol)
          (->key #:a (-lst -Bottom) #t #:b (-lst -Bottom) #f -Symbol))

    (pd-t (->... (list) ((-lst (-v a)) b) -Symbol) (a)
          (->... (list) ((-lst Univ) b) -Symbol)
          (->... (list) ((-lst -Bottom) b) -Symbol))


    ))

(define infer-tests
  (test-suite "Tests for infer"
    (infer-t Univ Univ)
    (infer-t (-v a) Univ)
    (infer-t (-v a) (-v a) #:result [(-v a) (-v a)])
    (infer-t Univ (-v a) #:fail)
    (infer-t Univ (-v a) #:vars '(a))
    (infer-t (-v a) Univ #:vars '(a))
    (infer-t (-v a) -Bottom #:vars '(a))
    (infer-t (-v a) (-v b) #:fail)
    (infer-t (-v a) (-v b) #:vars '(a))
    (infer-t (-v a) (-v b) #:vars '(b))

    (infer-t (make-ListDots -Symbol 'b) (-lst -Symbol) #:indices '(b)
             #:result [(make-ListDots (-v b) 'b) -Null])
    (infer-t (make-ListDots (-v a) 'b) (-lst -Symbol) #:vars '(a) #:indices '(b)
             #:result [(-lst* (make-ListDots (-v b) 'b) (-v a))
                       (-lst* (-lst -Bottom) -Bottom)])
    (infer-t (make-ListDots (-v b) 'b) (-lst -Symbol) #:indices '(b)
             #:result [(make-ListDots (-v b) 'b) (-lst -Bottom)])

    (infer-t (-lst -Symbol) (make-ListDots -Symbol 'b) #:indices '(b)
             #:result [(make-ListDots (-v b) 'b) (-lst -Bottom)])
    (infer-t (-lst -Symbol) (make-ListDots (-v b) 'b) #:indices '(b)
             #:result [(make-ListDots (-v b) 'b) (-lst -Symbol)])
    (infer-t (make-ListDots (-v b) 'b) (-lst Univ) #:indices '(b))
    (infer-t (make-ListDots (-v a) 'a) (-lst Univ))
    (infer-t (make-ListDots (-lst (-v a)) 'a) (-lst (-lst Univ)))
    (infer-t (make-ListDots (-vec (-v a)) 'a) (-lst (-vec Univ)) #:fail)

    (infer-t (make-ListDots (-v a) 'b) (make-ListDots -Symbol 'b) #:vars '(a))
    (infer-t (make-ListDots (-v b) 'b) (make-ListDots -Symbol 'b) #:indices '(b))
    (infer-t (make-ListDots -Symbol 'b) (make-ListDots (-v b) 'b) #:indices '(b))
    (infer-t (make-ListDots -Symbol 'b) (make-ListDots Univ 'b) #:indices '(b))
    (infer-t (make-ListDots (-v b) 'b) (make-ListDots (-v b) 'b) #:indices '(b))
    (infer-t (make-ListDots (-v b) 'b) (make-ListDots Univ 'b) #:indices '(b))
    (infer-t (-pair (-v a) (make-ListDots (-v b) 'b))
             (-pair (-v a) (make-ListDots (-v b) 'b))
             #:result [(-v a) (-v a)])

    [infer-t (->... null ((-v a) a) (-v b)) (-> -Symbol -String) #:vars '(b) #:indices '(a)]
    [infer-t (->... null ((-v a) a) (make-ListDots (-v a) 'a)) (-> -String -Symbol  (-lst* -String -Symbol)) #:indices '(a)]
    [infer-t (->... (list (-v b)) ((-v a) a) (-v b)) (-> -String -Symbol -String) #:vars '(b) #:indices '(a)]
    [infer-t (->... (list (-v b)) ((-v a) a) (-v b))
             (->... (list -Symbol) (-String a) (-v b))
             #:vars '(b) #:indices '(a)
             #:result [(-lst* (make-ListDots (-v a) 'a) (-v b))
                       (-lst* (-lst -String) -Symbol)]]
    [infer-t (->* (list -Symbol) -String -Void)
             (->... (list) ((-v a) a) -Void)
             #:indices '(a)
             #:result [(-lst* (make-ListDots (-v a) 'a))
                       (-lst* (-lst* -Bottom #:tail (-lst -Bottom)))]]
    [infer-t (->* (list) -String -Void) (->... (list) (-String a) -Void)]

    [infer-l (list (->... null ((-v b) b) (-v a))  (-> (-v a) -Boolean))
             (list (-> -String            -Symbol) (-> -Symbol   -Boolean))
             #:vars '(a)
             #:indices '(b)]
    [infer-l (list (->... null ((-v a) a) (-v b)) (make-ListDots (-v a) 'a))
             (list (-> -Symbol -Symbol -String) (-lst* -Symbol -Symbol))
             #:vars '(b)
             #:indices '(a)]

    [infer-t (-> (-values (list -String))) (-> (-values-dots (list) -Symbol 'b)) #:indices '(b) #:fail]
    [infer-t (make-ListDots -String 'a) (make-ListDots -Symbol 'b) #:indices '(b) #:fail]
    [infer-t (make-ListDots -String 'a) (make-ListDots -Symbol 'b) #:indices '(a) #:fail]
    [infer-t (-lst* -String) (make-ListDots -Symbol 'b) #:indices '(b) #:fail]
    [infer-t (->* (list -Symbol) -Symbol -Void) (->* (list) (-v a) -Void) #:vars '(a) #:fail]

    [infer-t (-> (-values (list -Bottom))) (-> (-values (list (-v b) (-v b)))) #:vars '(a)]
    [infer-t (-> (-values (list (-v a)))) (-> (-values (list (-v b) (-v b)))) #:vars '(a)]

    [infer-t
     (-pair (->... (list) ((-v b) b) Univ) (make-ListDots (-lst (-v b)) 'b))
     (-lst* (-> Univ Univ))
     #:indices '(b) #:fail]
    [infer-t
     (-lst* (-> Univ Univ))
     (-pair (->... (list) ((-v b) b) Univ) (make-ListDots (-lst (-v b)) 'b))
     #:indices '(b) #:fail]
    [infer-t
     (-pair (->... (list) ((-v b) b) Univ) (make-ListDots (-v b) 'b))
     (-pair (-> -Symbol Univ) (-lst -String))
     #:indices '(b) #:fail]
    [infer-t
     (-pair (-> -Symbol Univ) (-lst -String))
     (-pair (->... (list) ((-v b) b) Univ) (make-ListDots (-v b) 'b))
     #:indices '(b) #:fail]

    [infer-t
      (-lst (-mu A (Un (-v b) (-lst A))))
      (-mu C (Un (-v b2) (-lst C)))
      #:vars '(b2)
      #:result [(-vec (-v b2)) (-vec (-lst (-mu A (Un (-v b) (-lst A)))))]]

    [infer-t
      (-mlst (-val 'b))
      (-mlst (-v a))
      #:vars '(a)
      #:result [(-seq (-v a)) (-seq (-val 'b))]]

    ;; Currently Broken
    ;(infer-t (make-ListDots -Symbol 'b) (-pair -Symbol (-lst -Symbol)) #:indices '(b))
    [i2-t (-v a) N ('a N)]
    [i2-t (-pair (-v a) (-v a)) (-pair N (Un N B)) ('a (Un N B))]
    [i2-t (-lst (-v a)) (-lst* N N) ('a N)]
    [i2-t (-lst (-v a)) (-lst* N B) ('a (Un N B))]
    [i2-t Univ (Un N B)]
    [i2-t ((-v a) . -> . (-v b)) (-> N N) ('b N) ('a (Un))]
    [i2-t (-> (-v a) (-v a)) (->* null B B) ('a B)]


    [i2-l (list (-v a) (-v a) (-v b))
          (list (Un (-val 1) (-val 2)) N N)
          '(a b) ('b N) ('a N)]
    [i2-l (list (-> (-v a) Univ) (-lst (-v a)))
          (list (-> N (Un N B)) (-lst N))
          '(a) ('a N)]
    [i2-l (list (-> (-v a) (-v b)) (-lst (-v a)))
          (list (-> N N) (-lst (Un (-val 1) (-val 2))))
          '(a b) ('b N) ('a (Un (-val 1) (-val 2)))]
    [i2-l (list  (-lst (-v a)))
          (list  (-lst (Un B N)))
          '(a) ('a (Un N B))]
    ;; error tests
    [i2-f (-lst (-v a)) Univ]
    [i2-f (->* null B B) (-> (-v a) (-v b))]
    ))


(define tests
  (test-suite "All inference tests"
    pd-tests
    fv-tests
    infer-tests))
