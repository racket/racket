#lang racket

(require "SL-syntax.rkt"
         "SL-semantics.rkt"
         "common.rkt"
         "test-util.rkt"
         redex)

(test-SL-result
 ∅ 
 ((λ (x) ("S" x)) ("Z"))
 ("S" ("Z")))

(test-SL-stuck ∅ ((λ (x) ("S" x)) ("Z") ("Z")))

(test-SL-result
 ∅
 (match ("a" ("1"))
   [("a" x) x]
   [("b" y) y])
 ("1"))

(test-SL-result
 ∅
 (match ("b" ("1"))
   [("a" x) x]
   [("b" y) y])
 ("1"))

(test-SL-stuck
 ∅
 (match ("a" ("1"))
   [("a" x) x]
   [("a" y) y]))

(test-SL-result
 ∅
 (letrec ([(ref build-list)
           (λ (n f)
             (match n
               [("Z") ("nil")]
               [("S" m) 
                ((λ (x)
                   ((λ (xs) ("cons" x xs))
                    ((ref build-list) m f)))
                 (f m))]))])
   ((ref build-list) ("S" ("S" ("S" ("Z")))) (λ (i) ("S" i))))
 ("cons" ("S" ("S" ("S" ("Z"))))
         ("cons" ("S" ("S" ("Z")))
               ("cons" ("S" ("Z"))
                     ("nil")))))

(test-SL-result
 ∅
 ((λ (clobber)
    ((λ (a)
       ((λ (b) (a))
        (clobber ("b"))))
     (clobber ("a"))))
  (λ (x)
    (letrec ([(ref y) (λ () x)])
      (ref y))))
 ("b"))

(test-SL-result
 ∅
 (letrec ([(ref x) ("S" ("Z"))])
   (match (ref x)
     [("Z") ("a")]
     [("S" _) ("b")]))
 ("b"))

(test-SL-result
 ∅
 (w-c-m ("a") ("1")
        ((λ (x) x)
         (w-c-m ("a") ("2")
                (c-c-m [("a")]))))
 ("cons"
  ("cons" ("cons" ("a") ("1")) ("nil"))
  ("cons" ("cons" ("cons" ("a") ("2")) ("nil"))
          ("nil"))))

(test-SL-result
 ∅
 (w-c-m ("a") ("1")
        (w-c-m ("b") ("2")
               (c-c-m [("a") ("b")])))
 ("cons" ("cons" ("cons" ("b") ("2"))
                 ("cons" ("cons" ("a") ("1")) ("nil")))
         ("nil")))

(test-SL-result
 ∅
 (w-c-m ("a") ("1")
        (w-c-m ("b") ("2")
               (c-c-m [("b") ("a")])))
 ("cons" ("cons" ("cons" ("b") ("2"))
                 ("cons" ("cons" ("a") ("1")) ("nil")))
         ("nil")))

(test-SL-result
 ∅
 (w-c-m ("a") ("1")
        (c-c-m [("b") ("a")]))
 ("cons" ("cons" ("cons" ("a") ("1")) ("nil"))
         ("nil")))

(test-SL-result
 ∅
 (w-c-m ("a") ("1") 
        ((λ (x) x)
         ((λ (x) x)
          ((λ (x) x)
           (w-c-m ("a") ("2")
                  (w-c-m ("b") ("1")
                         (c-c-m [("a") ("b")])))))))
 ("cons"
  ("cons" ("cons" ("a") ("1")) ("nil"))
  ("cons" ("nil")
          ("cons" ("nil")
                  ("cons"
                   ("cons"
                    ("cons" ("b") ("1"))
                    ("cons" ("cons" ("a") ("2"))
                            ("nil")))
                   ("nil"))))))

(test-SL-result
 ∅
 (w-c-m ("a") ("1") 
        ((λ (x) x)
         (c-c-m [("a")])))
 ("cons"
  ("cons" ("cons" ("a") ("1")) ("nil"))
  ("cons" ("nil") ("nil"))))

(test-SL-result
 ∅
 ((λ (_)
    ((λ (x) (x x))
     (λ (x) (x x))))
  (abort ("Z")))
 ("Z"))

(test-SL-result
 ∅
 ((λ (x)
    (match x
      [("Z") ("a")]
      [("S" _) ("b")]))
  (call/cc
   (λ (k)
     ((λ (_)
        ((λ (x) (x x))
         (λ (x) (x x))))
      (k ("Z"))))))
 ("a"))

(test-SL-result
 ∅
 ((λ (x) ("S" ("S" x)))
  (letrec ([(ref k) (κ ((λ (x) ("S" x)) hole))])
    ((ref k) ("Z"))))
 ("S" ("Z")))

(test-SL-result
 ∅
 ((λ (x)
    (match ("b" x)
      [("b" x) x]))
  ("a"))
 ("a"))

(test-->>
 -->SL
 #:cycles-ok
 (term
  (∅
   /
   ((λ (t) (t t))
    (call/cc (λ (x) (call/cc x)))))))

;; fact
(define fact-impl
   `(λ (n)
      ,(:if `((ref =) n ,(num 0))
            (:let 'marks '(c-c-m [("fact")])
                  '(abort marks))
            `(w-c-m ("fact") n
                  ,(:let 'sub1-fact
                         (:let 'sub1 `((ref -) n ,(num 1))
                               `((ref fact) sub1))
                         `((ref *) n sub1-fact))))))
 (define fact-tr-impl
   `(λ (n a)
      ,(:if `((ref =) n ,(num 0))
            (:let 'marks '(c-c-m [("fact")])
                  '(abort marks))
            `(w-c-m ("fact") n
                    ,(:let 'sub1 `((ref -) n ,(num 1))
                           (:let 'multa `((ref *) n a)
                                 `((ref fact-tr) sub1 multa)))))))
 (define (test-fact n)
   (test-SL-result
    ∅
    ,(with-arith
      `(letrec ([(ref fact) ,fact-impl])
         ((ref fact) ,(num n))))
    ,(lst (append (build-list n (λ (i) (term ("cons" ("cons" ("fact") ,(num (- n i))) ("nil")))))
                  (list (term ("nil")) ; frame computing 1 * fact(0)
                        (term ("nil"))))))) ; frame that names c-c-m result
 (define (test-fact-tr n)
   (test-SL-result
    ∅
    ,(with-arith
      `(letrec ([(ref fact-tr) ,fact-tr-impl])
         ((ref fact-tr) ,(num n) ,(num 1))))
    ,(lst (list (term ("cons" ("cons" ("fact") ,(num 1)) ("nil")))
                (term ("nil")))))) ; frame that names c-c-m result
 (for ([i (in-range 1 4)]) (test-fact i))
 (for ([i (in-range 1 4)]) (test-fact-tr i))

;;; Values
(test-->> -->SL
          '(∅ / (λ (x) x))
          '(∅ / (λ (x) x)))
(test-->> -->SL
          '(∅ / ("nil"))
          '(∅ / ("nil")))
(test-->> -->SL
          '(∅ / ("S" ("0")))
          '(∅ / ("S" ("0"))))
(test-->> -->SL
          '(∅ / (ref x))
          '(∅ / (ref x)))

;;; Applications
(test-->> -->SL 
          '(∅ / ((λ (x) x) ("nil")))
          '(∅ / ("nil")))

;;; Store applications
(test-->> -->SL
          '((∅ [(ref x) ↦ (λ (x) ("nil"))])
            /
            ((ref x) ("0")))
          '((∅ [(ref x) ↦ (λ (x) ("nil"))])
            /
            ("nil")))

;;; Letrec
(test-->> -->SL
          '(∅ / (letrec ([(ref x) (λ (x) ("nil"))])
                  ("foo")))
          '((∅ [(ref x) ↦ (λ (x) ("nil"))])
            /
            ("foo")))
(test-->> -->SL
          '(∅ / (letrec ([(ref x) (λ (x) ("nil"))])
                  ((ref x) ("0"))))
          '((∅ [(ref x) ↦ (λ (x) ("nil"))])
            /
            ("nil")))

;;; match
(test-->> -->SL
          '(∅ / (match ("S" ("0"))
                  [("S" n) n]
                  [("0") ("0")]))
          '(∅ / ("0")))
(test-->> -->SL
          '(∅ / (match ("S" ("0"))
                  [("0") ("0")]
                  [("S" n) n]))
          '(∅ / ("0")))

; Store match
(test-->> -->SL
          '(∅ / (letrec ([(ref x) ("S" ("0"))])
                  (match (ref x)
                    [("S" n) n]
                    [("0") ("0")])))
          '((∅ [(ref x) ↦ ("S" ("0"))])
            /
            ("0")))

;; w-c-m
(test-->> -->SL
          `(∅ / (w-c-m ("k") ,(num 1) ,(num 2)))
          `(∅ / ,(num 2)))
(test-->> -->SL
          `(∅ / (w-c-m ("k") ,(num 1) (w-c-m ("k") ,(num 3) ,(num 2))))
          `(∅ / ,(num 2)))
(test-->> -->SL
          `(∅ / (w-c-m ("k") ,(num 1) ((λ (x) x) ,(num 2))))
          `(∅ / ,(num 2)))

;; c-c-m
(test-->> -->SL
          `(∅ / (c-c-m [("k")]))
          `(∅ / ("cons" ("nil") ("nil"))))
(test-->> -->SL
          `(∅ / (w-c-m ("k") ,(num 1) (c-c-m [("k")])))
          `(∅ / ("cons" ("cons" ("cons" ("k") ,(num 1)) ("nil")) ("nil"))))
(test-->> -->SL
          `(∅ / (w-c-m ("k") ,(num 1) (w-c-m ("k") ,(num 2) (c-c-m [("k")]))))
          `(∅ / ("cons" ("cons" ("cons" ("k") ,(num 2)) ("nil")) ("nil"))))
(test-->> -->SL
          `(∅ / (w-c-m ("k") ,(num 1) ((λ (x) x) (w-c-m ("k") ,(num 2) (c-c-m [("k")])))))
          `(∅ / ("cons" ("cons" ("cons" ("k") ,(num 1)) ("nil"))
                        ("cons" ("cons" ("cons" ("k") ,(num 2)) ("nil"))
                                ("nil")))))

(test-->> -->SL
          `(∅ / (w-c-m ("k1") ,(num 1) (c-c-m [("k1") ("k2")])))
          `(∅ / ("cons" ("cons" ("cons" ("k1") ,(num 1)) ("nil")) ("nil"))))
(test-->> -->SL
          `(∅ / (w-c-m ("k1") ,(num 1) (w-c-m ("k2") ,(num 2) (c-c-m [("k1") ("k2")]))))
          `(∅ / ("cons" ("cons" ("cons" ("k2") ,(num 2))
                                ("cons" ("cons" ("k1") ,(num 1))
                                        ("nil")))
                        ("nil"))))

;; abort
(test-->> -->SL
          `(∅ / (abort ,(num 2)))
          `(∅ / ,(num 2)))
(test-->> -->SL
          `(∅ / ((λ (x) x) (abort ,(num 2))))
          `(∅ / ,(num 2)))

;; arith
(test-->> -->SL
          `(∅ / ,(:let 'x (num 1) 'x))
          `(∅ / ,(num 1)))
(test-SL-result ∅ ,(with-arith (num 1)) ,(num 1))
(test-SL-result ∅ ,(with-arith `((ref +) ,(num 1) ,(num 1))) ,(num 2))
(test-SL-result ∅ ,(with-arith `((ref *) ,(num 2) ,(num 2))) ,(num 4))
(test-SL-result ∅ ,(with-arith `((ref =) ,(num 2) ,(num 2))) ("#t"))
(test-SL-result ∅ ,(with-arith `((ref =) ,(num 2) ,(num 3))) ("#f"))
(test-SL-result ∅ ,(with-arith `((ref -) ,(num 3) ,(num 2))) ,(num 1))
(test-SL-result ∅ ,(with-arith (:if '("#t") (num 1) (num 2))) ,(num 1))
(test-SL-result ∅ ,(with-arith (:if '("#f") (num 1) (num 2))) ,(num 2))

;; call/cc
(test-->> -->SL
          `(∅ / (call/cc (λ (k) (k ("v")))))
          `(∅ / ("v")))
(test-->> -->SL
          `(∅ / (call/cc (λ (k) 
                           ((λ (x) ("x"))
                            (k ("v"))))))
          `(∅ / ("v")))

;; call/cc + w-c-m
(test-->> -->SL
          `(∅ / (w-c-m ("k") ("v1")
                       ((λ (f) (f ("unit")))
                        (call/cc (λ (k)
                                   (w-c-m ("k") ("v2")
                                          (k (λ (x) (c-c-m [("k")])))))))))
          `(∅ / ("cons" ("cons" ("cons" ("k") ("v1")) ("nil")) ("nil"))))

(test-->> -->SL
          `(∅ / (w-c-m ("k") ("v1")
                     ((λ (f) (f ("unit")))
                      (call/cc (λ (k)
                                 (w-c-m ("k") ("v2")
                                      ((λ (cms)
                                         (k (λ (x) cms)))
                                       (c-c-m [("k")]))))))))
          `(∅ / ("cons" ("cons" ("cons" ("k") ("v1")) ("nil"))
                        ("cons" ("cons" ("cons" ("k") ("v2")) ("nil"))
                                ("cons" ("nil")
                                        ("nil"))))))
