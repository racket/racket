#lang racket

(require redex/private/pat-unify
         rackunit
         (for-syntax redex/private/rewrite-side-conditions 
                     racket/list))


(define-syntax check-dq
  (syntax-rules ()
    [(check-dq args ... #f)
     (let/ec fail
       (let ([check/fail (λ (c) (if (or (not c)
                                        (unif-fail? c))
                                    (fail (void))
                                    c))])
         (check-dq-hlp check/fail args ... #f)))]
    [(check-dq args ...)
     (let/ec fail
       (let ([check/fail (λ (c) (if (or (not c)
                                        (unif-fail? c))
                                    (fail (error 'check-dq "unexpected failure: ~s" '(args ...)))
                                    c))])
         (check-dq-hlp check/fail args ...)))]))

(define-syntax (check-dq-hlp stx)
  (syntax-case stx (=== =/= --> return env:)
    [(_ check/fail eigens --> return env: e)
       #'e]
    [(_ check/fail eigens --> #f env: e)
       #'(check-false e)]
    [(_ check/fail eigens --> #t env: e)
     #'(check-not-false e)]
    [(_ check/fail eigens --> l2 =/= r2 || l3 =/= r3 env: e)
     (with-syntax ([(l2-p r2-p l3-p r3-p) (to-pats (list #'l2 #'r2 #'l3 #'r3))])
       #'(let ([res-dq (dq-dq (first (env-dqs (check/fail e))))])
           (check-not-false
            (or (equal? (nts->any res-dq)
                       (list l2-p r2-p))
                (equal? (nts->any res-dq)
                       (list l3-p r3-p))))))]
    [(_ check/fail eigens --> l2 =/= r2 env: e)
     (with-syntax ([(l2-p r2-p) (to-pats (list #'l2 #'r2))])
       #'(let ([res-dq (dq-dq (first (env-dqs (check/fail e))))])
           (check-equal? (nts->any res-dq)
                         (list l2-p r2-p))))]
    [(_ check/fail eigens l === r rest ... env: e)
     (with-syntax ([(l-p r-p) (to-pats (list #'l #'r))])
       #'(check-dq-hlp check/fail eigens rest ... env: (p*e-e (check/fail (unify l-p r-p (check/fail e) #f)))))]
    [(_ check/fail eigens l =/= r rest ... env: e)
     (with-syntax ([(l-p r-p) (to-pats (list #'l #'r))])
       #'(check-dq-hlp check/fail eigens rest ... env: (disunify 'eigens l-p r-p (check/fail e) #f)))]
    [(_ no-env ...)
     (not (memq 'env: (flatten (syntax->datum #'(no-env ...)))))
     #'(check-dq-hlp no-env ... env: empty-env)]))

(define (nts->any s-exp)
  (match s-exp
    [`(nt ,_) 'any]
    [(bound) 'any]
    [(list es ...)
     (map nts->any es)]
    [_ s-exp]))

(define-for-syntax (to-pats ps-in)
  (define syms (filter symbol? (flatten (map syntax->datum ps-in))))
  (define (make-pat s) 
    (with-syntax ([(_ p _ _) 
                   (rewrite-side-conditions/check-errs syms 'check-dq #t s)])
      #'(nts->any 'p)))
  (map make-pat ps-in))


(check-dq
   (a b)
   (a b) =/= x
   x === (1 2)
   -->
   #f)

(check-dq
   (a b)
   x === (1 2)
   (a b) =/= x
   -->
   #f)

(check-dq
   (a b)
   (a a) =/= x
   x === (1 2)
   -->
   #t)

(check-dq
   (a b)
   x === (1 2)
   (a a) =/= x
   -->
   #t)

(check-dq
   (c d)
   x === (1 2)
   x === (a b)
   x =/= (c d)
   -->
   #f)

(check-dq
   (c d)
   x === (1 2)
   x =/= (c d)
   x === (a b)
   -->
   #f)
(check-dq
   (c d)
   x =/= (c d)
   x === (1 2)
   x === (a b)
   -->
   #f)

(check-dq
   (c d)
   x === (1 2)
   x === (a b)
   x =/= (c c)
   -->
   #t)

(check-dq
   (c d)
   x === (1 2)
   x =/= (c c)
   x === (a b)
   -->
   #t)
(check-dq
   (c d)
   x =/= (c c)
   x === (1 2)
   x === (a b)
   -->
   #t)

;; The following is translated from the kanren
;; test suite. Only those tests that actually
;; apply in this context were transcribed.
;; Essentially, those of the form:
;; exists x ... eqs ... forall y ... single-dq
;; i.e. some conjuction of equations under 
;; existential quantifiers and a single dq 
;; under an additional universal quantifier.

;; Eigen test suite
;; Jason Hemann, Will Byrd, Dan Friedman
;; Pre-alpha


#|
(test-check "eigen test 10"
  (run 1 (q) (eigen (x) (=/= x q)))
  '())
|#

(check-false
 (disunify '(x) '(name x any) '(name q any) empty-env #f))

#|
(test-check "eigen test 12"
  (run 1 (q) (eigen (e1 e2) (=/= 5 e2)))
  '())
|#

(check-false
 (disunify '(e1 e2) 5 '(name e2 any) empty-env #f))

#|
(test-check "eigen test 13"
  (run 1 (q) (eigen (e1 e2) (=/= e1 e2)))
  '())
|#

(check-false
 (disunify '(e1 e2) '(name e1 any) '(name e2 any) empty-env #f))

#|
(test-check "eigen test 14.5"
  (run 1 (q)
    (eigen (a b)
      (=/= `(,a . ,b) q)))
  '((_.0 (=/= ((_.0 (e.0 . e.1)))))))
|#

(check-dq
   (a b)
   (a b) =/= q 
   -->
   (q) =/= ((a b)))


#|
(test-check "pair w/eigen younger than var"
  (run 1 (q) (eigen (a) (=/= `(1 . ,q) a)))
  '())
|#

(check-dq
 (a)
 (1 q) =/= a
 -->
 #f)

#|


(test-check "eigen test 16"
  (run 1 (q) 
    (fresh (x) 
      (eigen (a)
        (=/= `(6 ,x) `(5 ,a)))))
  '(_.0))
|#

(check-dq
 (a)
 (6 x) =/= (5 a)
 -->
 #t)

#|

(test-check "D & J 1"
  (run 1 (q) (eigen (a) (=/= `(1 2 3 ,q 4) a)))
  '())
|#

(check-dq
 (a)
 (1 2 3 q 4) =/= a
 -->
 #f)


#|

(test-check "Will's 4"
  (run 1 (q) (fresh (x) (eigen (a) (=/= `(1 2 3 ,a 4) x))))
  '(_.0))
|#

(check-dq
 (a)
 (1 2 3 a 4) =/= x
 -->
 #t)

#|

(test-check "Will's 5"
  (run 1 (q) (fresh (x) (eigen (a) (=/= `(1 2 3 ,x 4) a))))
  '())
|#

(check-dq
 (a)
 (1 2 3 x 4) =/= a
 -->
 #f)

#|

(test-check "eigen test"
  ;; Exists X, Forall A . A =/= `(,X)
  ;; Fails. Let A be `(,X)
  (run 1 (x) (eigen (a) (=/= a `(,x))))
  '())
|#

(check-dq
 (a)
 a =/= (x)
 -->
 #f)

#|

(test-check "eigen disequalities, working"
  ;; Exists X, Forall A. `(1 2 3 ,a 4) =/= X
  ;; Let X be any symbol
  (run 1 (q) (fresh (x) (eigen (a) (=/= `(1 2 3 ,a 4) x))))
  '(_.0))
|#

(check-dq
 (a)
 (1 2 3 a 4) =/= x
 --> 
 #t)

#|

(test-check "Eigen on pair"
  (run 1 (q) (eigen (e) (=/= e `(,e))))
  '(_.0))
|#

(check-dq
 (e)
 e =/= (e)
 -->
 #t)

#|

(test-check "eigen-pairs 12"
  (run 1 (q)
    (fresh (y) 
      (eigen (e1 e2)
        (=/= `(,e1 . ,e2) y))))
  '(_.0))
|#

(check-dq
 (e1 e2)
 (e1 e2) =/= y
 -->
 #t)

#|
(test-check "eigen-pairs 13"
  (run 1 (q)
    (fresh (y)
      (== y 7)
      (eigen (e1 e2)
        (=/= `(,e1 . ,e2) y))))
  '(_.0))
|#

(check-dq
 (e1 e2)
 y === 7
 (e1 e2) =/= y
 -->
 #t)

#|

(test-check "eigen-pairs 14"
  (run 1 (q)
    (fresh (y)
      (eigen (e1 e2)
        (=/= `(,e1 . ,e2) y)
        (== y 7))))
  '(_.0))
|#

(check-dq
 (e1 e2)
 y === 7
 (e1 e2) =/= y
 -->
 #t)

#|

(test-check "Eigen violation, again"
  (run 1 (q) (eigen (x) (=/= x q)))
  '())
|#

(check-dq
 (q)
 x =/= q
 --> 
 #f)

#|

(test-check "Eigen violation, again 2"
  (run 1 (q) (eigen (x) (=/= q x)))
  '())
|#

(check-dq
 (x)
 q =/= x
 --> 
 #f)


#|

(test-check "same eigen issues, small"
  (run 1 (q) 
    (eigen (e1)
      (=/= `(1 . 0) `(,e1 . ,e1))))
  '(_.0))
|#

(check-dq
 (e1)
 (1 0) =/= (e1 e1)
 -->
 #t)

#|
(test-check "inviolable contraint should be tossed"
  (run 1 (q) 
    (eigen (e e2) 
      (=/= `(1 1 . ()) `(,e ,e2 . ,e))))
  '(_.0))
|#

(check-dq
 (e e2)
 (1 1 0) =/= (e e2 e)
 --> 
 #t)

#|

(test-check "same eigens can't unify to distinct values"
  (run 1 (q) 
    (== q 1)
    (eigen (e e2) 
      (=/= `(,q ,q . ()) `(,e ,e2 . ,e)) (prt)))
  '(1))
|#

(check-dq
 (e e2)
 q === 1
 (q q 0) =/= (e e2 2)
 -->
 #t)

#|

(test-check "shapes are issues, should fail"
  (run 1 (q) 
    (eigen (e e2) 
      (=/= `(,q ,q . ()) `(,e ,e2 . ,e)) (prt))
    (== q '()))
  '())
|#

(check-dq
 (e e2)
 q === 0
 (q q 0) =/= (e e2 e)
 --> 
 #f)

#|


(printf "These are off for the moment~n")

#!eof
|#
#|
(test-check "eigens need to create disequality constraints; we smuggle'em in"
  (run 1 (q)
    (eigen (e)
      (=/= `(,q . ()) `(,e . ,e))))
  '((_.0) (=/= ((_.0 ())))))
|#

(check-dq
 (e)
 (q 0) =/= (e e)
 -->
 (q) =/= (0))

#|

(test-check "eigens need to create disequality constraints; we smuggle'em in b"
  (run 1 (q)
    (eigen (e)
      (=/= `(() . ,q) `(,e . ,e))))
  '((_.0 (=/= ((_.0 ()))))))
|#

(check-dq
 (e)
 (0 q) =/= (e e)
 -->
 (q) =/= (0))

#|

(test-check "eigens need to find their way to causing disequalities, through subst"
  (run 1 (q)
    (fresh (a b c)
      (== `(,a ,b ,c) q)
      (eigen (e)
        (=/= `(,e ,b ,e) `(,a ,c ,c))
        (== b c))))
  '((_.0 _.1 _.2) (=/= ((_.0 _.2)))))
|#

;; equivalent, since b = c?
;!!!
(check-dq
 (e)
 b === c
 (e b e) =/= (a c c)
 -->
 (b a) =/= (e e)
 ||
 (a b) =/= (e e))

#|

(test-check "shapes are issues"
  (run 1 (q) 
    (eigen (e e2) 
      (=/= `(,q ,q . ()) `(,e ,e2 . ,e)) (prt)))
  '(_.0))
|#

(check-dq
 (e1 e2)
 (q q 0) =/= (e e2 e)
 -->
 #t)

#|

(test-check "one of these things is not like the other"
  (run 1 (q) 
    (fresh (a b c)
      (== `(,a ,b ,c) q)
      (eigen (e)
        (=/= `(,a ,b ,c) `(,e ,e ,e)))))
  '((_.0 _.1 _.2) (=/= ((_.0 _.1) (_.0 _.2) (_.1 _.2)))))
|#

;; a =/= b \/ a =/= c \/ b =/= c <-> a =/= c \/ b =/= c <-> (b a) =/= (c c)
(check-dq
 (e)
 (a b c) =/= (e e e)
 -->
 (b a) =/= (c c)
 ||
 (a b) =/= (c c))

#|

(test-check "should succeed b/c same eigen, diff vars"
  (run 1 (q) 
    (fresh (a b) 
      (== `(,a . ,b) q)
      (eigen (e1)
        (=/= `(,a . ,b) `(,e1 . ,e1)))))
  '((_.0 _.1) (=/= ((_.0 _.1)))))
|#

(check-dq
 (e1)
 (a b) =/= (e1 e1)
 -->
 (a) =/= (b))


#|

(test-check "occurs-checky-thing-w-same-var"
  (run 1 (q)
    (fresh (a)
      (eigen (e)
        (=/= `(,a . (,a)) `(,e . ,e)))))
  '(_.0))
|#

(check-dq
 (e)
 (a (a)) =/= (e e)
 -->
 #t)

#|

(test-check "occurs-checky-thing-w-same-var b"
  (run 1 (q)
    (fresh (a)
      (eigen (e)
        (=/= `((,a) . ,a) `(,e . ,e)))))
  '(_.0))
|#

(check-dq
 (e)
 ((a) a) =/= (e e)
 --> 
 #t)