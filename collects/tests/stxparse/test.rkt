#lang scheme
(require syntax/parse
         syntax/private/stxparse/rep-attrs
         syntax/private/stxparse/runtime)
(require schemeunit)

;; tok = test pattern ok
(define-syntax tok
  (syntax-rules ()
    [(tok s p expr #:pre [pre-p ...] #:post [post-p ...])
     (test-case (format "line ~s: ~s match ~s"
                        (syntax-line (quote-syntax s))
                        's 'p)
       (syntax-parse (quote-syntax s)
         [pre-p (error 'wrong-pattern "~s" 'pre-p)] ...
         [p expr]
         [post-p (error 'wrong-pattern "~s" 'post-p)] ...)
       (void))]
    [(tok s p expr)
     (tok s p expr #:pre () #:post ())]
    [(tok s p)
     (tok s p 'ok)]))

(define-syntax-rule (bound b ...)
  (begin (bound1 b) ...))

(define-syntax bound1
  (syntax-rules ()
    [(bound1 (name depth))
     (let ([a (attribute-binding name)])
       (check-pred attr? a)
       (when (attr? a)
         (check-equal? (attr-depth a) 'depth)))]
    [(bound1 (name depth syntax?))
     (let ([a (attribute-binding name)])
       (check-pred attr? a)
       (when (attr? a)
         (check-equal? (attr-depth a) 'depth)
         (check-equal? (attr-syntax? a) 'syntax?)))]))

(define-syntax-rule (s= t v)
  (check-equal? (syntax->datum #'t) v))

(define-syntax-rule (a= a v)
  (check-equal? (attribute a) v))

(define-syntax-rule (terx s p rx ...)
  (terx* s [p] rx ...))

(define-syntax terx*
  (syntax-rules ()
    [(terx s [p ...] rx ...)
     (test-case (format "line ~s: ~a match ~s for error"
                        (syntax-line (quote-syntax s))
                        's '(p ...))
       (check-exn (lambda (exn)
                    (erx rx (exn-message exn)) ... #t)
                  (lambda ()
                    (syntax-parse (quote-syntax s)
                      [p 'ok] ...)))
       (void))]))

(define-syntax erx
  (syntax-rules (not)
    [(erx (not rx) msg)
     (check (compose not regexp-match?) rx msg)]
    [(erx rx msg)
     (check regexp-match? rx msg)]))


;; ========

(define-syntax-class one
  (pattern (a)))
(define-syntax-class two
  (pattern (a b)))

;; ========


;; == Parsing tests

;; -- S patterns
;; name patterns
(tok 1 a
     (and (bound (a 0)) (s= a 1)))
(tok (a b c) a
     (and (bound (a 0)) (s= a '(a b c))))
(tok 1 a
     'ok
     #:pre [] #:post [1])

;; wildcard patterns
(tok 1 _)
(tok (a b c) _)
(tok (a b) (_ _)) ;; multiple _'s allowed

;; sc tests -> lib tests
(tok (1) x:one
     (and (bound (x 0) (x.a 0)) (s= x '(1)) (s= x.a 1)))
(tok (1 2) x:two
     (and (bound (x 0) (x.a 0) (x.b 0)) (s= x '(1 2)) (s= x.a 1) (s= x.b 2)))
(tok (1 2) x:two
     'ok
     #:pre [x:one] #:post [])
(tok (1) x:one
     'ok
     #:pre [()] #:post [x:two])
;; check if wildcard, no attr bound

(terx (1) _:two "expected two")
;(terx (1 2) _:one "expected one")
(terx (1 (2 3)) (_:one _:two) "expected one")
(terx ((1) 2) (_:one _:two) "expected two")

;; datum patterns
(tok 1 1
     'ok)
(tok 1 _
     #t
     #:pre [2] #:post [])
(tok "here" "here"
     'ok
     #:pre ["there"] #:post [])
(tok #f #f
     'ok
     #:pre [#t 0] #:post [_])

(terx 1 2 "literal 2")
(terx (1 2) 1 "literal 1")
(terx (1 2) (1 1) "literal 1")

;; literal patterns
(syntax-parse #'+ #:literals (+ -)
  [+ (void)])
(syntax-parse #'+ #:literals (+ -)
  [- (error 'wrong)]
  [+ (void)])
(syntax-parse #'+ #:literals (+ -)
  [+ (void)]
  [_ (error 'wrong)])

;; compound patterns
(tok (a b c) (x y z)
     (and (bound (x 0) (y 0) (z 0)) (s= x 'a) (s= y 'b))
     #:pre [(x y)] #:post [])
(tok (a . b) (x . y)
     (and (bound (x 0) (y 0)) (s= x 'a) (s= y 'b))
     #:pre [(x y)] #:post [])
(tok #(a b c) #(x y z)
     (and (bound (x 0) (y 0) (z 0)) (s= x 'a) (s= y 'b)))
(tok #(a b c) #(x y z)
     (and (bound (x 0) (y 0) (z 0)) (s= x 'a) (s= y 'b)))
(tok #&1 #&x
     (and (bound (x 0)) (s= x 1)))

;; head patterns
;; See H-patterns

;; dots patterns
;; See EH-patterns

;; and patterns
(tok 1 (~and a 1)
     (and (bound (a 0)) (s= a 1)))
(tok 1 (~and 1 1)
     'ok
     #:pre [(~and 1 2)] #:post [(~and 2 2)])
(tok (1 2 3) (~and w (x y z))
     (and (bound (w 0) (x 0) (y 0) (z 0))
          (s= w '(1 2 3)) (s= x 1)))
(tok (1 2 3) (~and (1 _ _) (_ 2 _) (_ _ 3))
     'ok)
(tok (1 2 3) (~and (x _ _) (_ y _) (_ _ z))
     (and (bound (x 0) (y 0) (z 0))))

;; or patterns
(tok 1 (~or 1 2 3)
     'ok)
(tok 3 (~or 1 2 3)
     'ok)
(tok (1) (~or (a) (a b) (a b c))
     (and (bound (a 0 #t) (b 0 #f) (c 0 #f)) (s= a 1) (a= b #f) (a= c #f)))
(tok (1 2 3) (~or (a) (a b) (a b c))
     (and (bound (a 0 #t) (b 0 #f) (c 0 #f)) (s= a 1) (s= b 2) (s= c 3)))
(tok 1 (~or 5 _)
     'ok)
(tok #t (~or #t #f)
     'ok)
(tok #t (~or (~and #t x) (~and #f x))
     (and (bound (x 0 #t))))

;; epsilon-name patterns
(tok (1) :one
     (and (bound (a 0)) (s= a 1)))
(tok (1 2) :two
     (and (bound (a 0) (b 0)) (s= a 1) (s= b 2)))
(tok (1 2) (~and x:two :two)
     (and (bound (x 0) (x.a 0) (a 0)) (s= x '(1 2)) (s= x.a 1) (s= a 1)))

;; -- H patterns

;; seq
(tok (1 2 3) ((~seq 1 2) 3))
(tok (1 2 3) (1 (~seq 2) 3))
(tok (1 2 3) ((~seq) 1 2 3))

;; or
(tok (1 2 3) ((~or (~seq 1 2) 1) 3))
(tok (1 2 3) ((~or 1 (~seq 1 2)) 3))
(tok (1 2 3) ((~or (~seq 1) (~seq 1 2)) 3))
(tok (1 2 3) ((~or (~seq 1) (~seq)) 1 2 3))
(tok (1 2 3) ((~or (~seq 1) (~seq)) 1 2 3 (~or (~seq 4) (~seq))))

;; describe
(tok (1 2 3) ((~describe "one-two" (~seq 1 2)) 3))
(terx (1 3 3) ((~describe "one-two" (~seq 1 2)) 3)
      "one-two")

;; -- A patterns

;; cut patterns
(terx* (1 2 3) [(1 ~! 4) (1 _:nat 3)]
       "4" (not "exact nonnegative integer"))

;; cut-in-and
(terx* 1 [(~and a:nat ~! 2) b:nat]
       "2")

;; cut&describe interaction
(tok (1 (2 3)) (1 (~or (~describe "foo" (2 ~! 4)) (2 3))))
(tok (1 2 3) (1 2 3)
     'ok
     #:pre [(~describe "foo" (1 2 ~! 4))] #:post [])

;; bind patterns
(tok 1 (~and x (~bind [y #'x]))
     (s= y '1))
(tok 1 (~or x:id (~bind [x #'default]))
     (s= x 'default))

;; fail patterns
(tok (1 2 3) _
     'ok
     #:pre [(~fail "pass") (error 'wrong)] #:post [])
(terx 1 (~fail "wanted 2")
      #rx"wanted 2")
(terx 1 (~and n:nat (~fail #:unless (even? (syntax-e #'n)) "wanted even number"))
      #rx"wanted even number")

;; fail as S-pattern
(terx 1 (~fail "grr")
      #rx"grr")

(tok (1 2 3) (x:nat y:nat (~parse (~or 2 3) (+ (syntax-e #'x) (syntax-e #'y))) z:nat))
(terx (1 2 3) (x:nat y:nat (~parse 4 (+ (syntax-e #'x) (syntax-e #'y))) z:nat)
      "expected the literal 4")
(terx (1 2 3) (x:nat y:nat (~parse (2 4) #'(x y)))
      "expected the literal 2")

;; == Lib tests

;; == Error tests
