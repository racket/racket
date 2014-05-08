#lang racket/base
(require
  "test-utils.rkt"
  rackunit
  racket/list
  (for-syntax racket/base syntax/parse)
  (rep type-rep)
  (r:infer infer)

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
  (define-splicing-syntax-class vars
    (pattern (~seq) #:with vars #'empty)
    (pattern (~seq #:vars vars:expr) ))
  (define-splicing-syntax-class indices
    (pattern (~seq) #:with indices #'empty)
    (pattern (~seq #:indices indices:expr) ))
  (define-splicing-syntax-class pass
    (pattern (~seq) #:with pass #'#t)
    (pattern #:pass #:with pass #'#t)
    (pattern #:fail #:with pass #'#f)))

(define-syntax (infer-t stx)
  (syntax-parse stx
    ([_ S:expr T:expr :vars :indices :pass]
     (syntax/loc stx
       (test-case (format "~a ~a~a" S T (if pass "" " should fail"))
         (define result (infer vars indices (list S) (list T) #f))
         (unless (equal? result pass)
           (fail-check "Could not infer a substitution")))))))

(define-syntax (infer-l stx)
  (syntax-parse stx
    ([_ S:expr T:expr :vars :indices :pass]
     (syntax/loc stx
       (test-case (format "~a ~a~a" S T (if pass "" " should fail"))
         (define result (infer vars indices S T #f))
         (unless (equal? result pass)
           (fail-check "Could not infer a substitution")))))))


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

(define infer-tests
  (test-suite "Tests for infer"
    (infer-t Univ Univ)
    (infer-t (-v a) Univ)
    (infer-t Univ (-v a) #:fail)
    (infer-t Univ (-v a) #:vars '(a))
    (infer-t (-v a) Univ #:vars '(a))
    (infer-t (-v a) -Bottom #:vars '(a))
    (infer-t (-v a) (-v b) #:fail)
    (infer-t (-v a) (-v b) #:vars '(a))
    (infer-t (-v a) (-v b) #:vars '(b))

    (infer-t (make-ListDots -Symbol 'b) (-lst -Symbol) #:indices '(b))
    (infer-t (make-ListDots (-v b) 'b) (-lst Univ) #:indices '(b))
    (infer-t (make-ListDots (-v a) 'b) (make-ListDots -Symbol 'b) #:vars '(a))
    (infer-t (make-ListDots -Symbol 'b) (make-ListDots Univ 'b) #:indices '(b))
    (infer-t (make-ListDots (-v b) 'b) (make-ListDots (-v b) 'b) #:indices '(b))
    (infer-t (make-ListDots (-v b) 'b) (make-ListDots Univ 'b) #:indices '(b))

    [infer-t (->... null ((-v a) a) (-v b)) (-> -Symbol -String) #:vars '(b) #:indices '(a)]
    [infer-t (->... null ((-v a) a) (make-ListDots (-v a) 'a)) (-> -String -Symbol  (-lst* -String -Symbol)) #:indices '(a)]
    [infer-t (->... (list (-v b)) ((-v a) a) (-v b)) (-> -String -Symbol -String) #:vars '(b) #:indices '(a)]

    [infer-l (list (->... null ((-v b) b) (-v a))  (-> (-v a) -Boolean))
             (list (-> -String            -Symbol) (-> -Symbol   -Boolean))
             #:vars '(a)
             #:indices '(b)]
    [infer-l (list (->... null ((-v a) a) (-v b)) (make-ListDots (-v a) 'a))
             (list (-> -Symbol -Symbol -String) (-lst* -Symbol -Symbol))
             #:vars '(b)
             #:indices '(a)]
    ;; Currently Broken
    ;(infer-t (make-ListDots (-v b) 'b) (-lst -Symbol) #:indices '(b))
    ;(infer-t (-lst -Symbol) (make-ListDots -Symbol 'b) #:indices '(b))
    ;(infer-t (make-ListDots (-v b) 'b) (make-ListDots -Symbol 'b) #:indices '(b))
    ;(infer-t (make-ListDots -Symbol 'b) (make-ListDots (-v b) 'b) #:indices '(b))
    ;(infer-t (make-ListDots -Symbol 'b) (-pair -Symbol (-lst -Symbol)) #:indices '(b))
    [i2-t (-v a) N ('a N)]
    [i2-t (-pair (-v a) (-v a)) (-pair N (Un N B)) ('a (Un N B))]
    [i2-t (-lst (-v a)) (-lst* N N) ('a N)]
    [i2-t (-lst (-v a)) (-lst* N B) ('a (Un N B))]
    [i2-t Univ (Un N B)]
    [i2-t ((-v a) . -> . (-v b)) (-> N N) ('b N) ('a (Un))]


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
    ))


(define tests
  (test-suite "All inference tests"
    fv-tests
    infer-tests))
