#lang racket
(require rackunit
         syntax/parse
         syntax/parse/debug
         "setup.rkt"
         (for-syntax syntax/parse))

;; Main syntax class and pattern tests

;; ========

(define-syntax-class one
  (pattern (a)))
(define-syntax-class two
  (pattern (a b)))

;; ========

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

(terx (1) _:two "expected more terms")
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
(test-case "literals: +"
           (syntax-parse #'+ #:literals (+ -)
             [+ (void)]))
(test-case "literals: - +"
           (syntax-parse #'+ #:literals (+ -)
             [- (error 'wrong)]
             [+ (void)]))
(test-case "literals: + _"
           (syntax-parse #'+ #:literals (+ -)
             [+ (void)]
             [_ (error 'wrong)]))

(test-case "datum literals"
  (syntax-parse #'one #:datum-literals (one)
    [one (void)]))
(test-case "datum literals (not id=?)"
  (let ([one 1])
    (syntax-parse (let ([one 2]) #'one) #:datum-literals (one)
      [one (void)])))

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
(tok #(1 2 3 4 5) #(a b ~rest c)
     (s= c '(3 4 5)))
(tok #&1 #&x
     (and (bound (x 0)) (s= x 1)))

(tok #s(foo 1 2) #s(foo a b)
     (and (s= a 1) (s= b 2)))
(tok #s(foo 1 2 3 4 5) #s(foo a b ~rest c)
     (s= c '(3 4 5)))

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

;; and scoping
(tok 1 (~and a (~fail #:unless (equal? (syntax->datum #'a) 1))))

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

;; describe
(tok ((1 2) 3) ((~describe "one-two" (1 2)) 3))
(terx ((1 3) 3) ((~describe #:opaque "one-two" (1 2)) 3)
      "one-two")
(terx ((1 3) 3) ((~describe "one-two" (1 2)) 3)
      "2")
(terx (1 3) ((~describe "one-two" (1 2)) 3)
      "one-two")

;; epsilon-name patterns
(tok (1) :one
     (and (bound (a 0)) (s= a 1)))
(tok (1 2) :two
     (and (bound (a 0) (b 0)) (s= a 1) (s= b 2)))
(tok (1 2) (~and x:two :two)
     (and (bound (x 0) (x.a 0) (a 0)) (s= x '(1 2)) (s= x.a 1) (s= a 1)))

;; delimit-cut
(tok (1 (2 3)) (1 (~or (~delimit-cut (2 ~! 4)) (2 3))))
(tok (1 2 3) (1 2 3)
     'ok
     #:pre [(~delimit-cut (1 2 ~! 4))] #:post [])

(define-syntax-class def
  #:no-delimit-cut
  #:literals (define-values)
  (pattern (define-values ~! (x:id ...) e:expr)))

(tok (define-values (a b c) 1) d:def
     'ok)
(terx (define-values (a 2) 3) (~or d:def e:expr)
      #rx"expected identifier")
(terx* (define-values (a 2) 3) [d:def e:expr]
       #rx"expected identifier")

;; commit
(define-syntax-class xyseq
  #:commit
  (pattern ((~or x y) ...)))

(tok (1 2 3 4 5 6 7 8)
     (~and ((~or s.x s.y) ...)
           (~fail #:unless (= (apply + (syntax->datum #'(s.x ...)))
                              (apply + (syntax->datum #'(s.y ...))))
                  "nope"))
     (equal? (syntax->datum #'(s.x ...)) '(1 2 3 4 8)))
(terx (1 2 3 4 5 6 7 8)
      (~and s:xyseq
            (~fail #:unless (= (apply + (syntax->datum #'(s.x ...)))
                               (apply + (syntax->datum #'(s.y ...))))
                   "nope"))
      #rx"nope")
(terx (1 2 3 4 5 6 7 8)
      (~and (~commit ((~or s.x s.y) ...))
            (~fail #:unless (= (apply + (syntax->datum #'(s.x ...)))
                               (apply + (syntax->datum #'(s.y ...))))
                   "nope"))
      #rx"nope")

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
(terx (1 3 3) ((~describe #:opaque "one-two" (~seq 1 2)) 3)
      "one-two")

;; Regression (2/2/2010)
(define-splicing-syntax-class twoseq
  (pattern (~seq a b)))
(tok (1 2 3 4) (x:twoseq ...))

;; -- A patterns

;; cut patterns
(terx* (1 2 3) [(1 ~! 4) (1 _:nat 3)]
       "4" (not "exact nonnegative integer"))

;; cut-in-and
(terx* 1 [(~and a:nat ~! 2) b:nat]
       "2")

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

;; == syntax-parse: other feature tests

(test-case "syntax-parse: #:context"
           (check-exn
            (lambda (exn)
              (regexp-match #rx"me: expected exact-nonnegative-integer" (exn-message exn)))
            (lambda ()
              (syntax-parse #'(m x) #:context #'me
                [(_ n:nat) 'ok])))
           (void))

(test-case "syntax-parse: #:literals"
           (syntax-parse #'(0 + 1 * 2)
             #:literals (+ [times *])
             [(a + b * c) (void)]))


;; == syntax classes: other feature tests

;; #:auto-nested-attributes

(define-syntax-class square0
  (pattern (x:two y:two)))

(define-syntax-class square
  #:auto-nested-attributes
  (pattern (x:two y:two)))

(test-case "nested attributes omitted by default"
  (check-equal? (syntax-class-attributes square0)
                '((x 0) (y 0)))
  (void))

(test-case "nested attributes work okay"
  (check-equal? (syntax-class-attributes square)
                '((x 0) (x.a 0) (x.b 0) (y 0) (y.a 0) (y.b 0)))
  (void))

;; conventions

(define-syntax-class (nat> bound)
  #:description (format "natural number greater than ~s" bound)
  (pattern n:nat #:when (> (syntax-e #'n) bound)))

(define-conventions nat-convs
  [N (nat> 0)])

(test-case "syntax-parse: #:conventions"
           (syntax-parse #'(5 4)
             #:conventions (nat-convs)
             [(N ...) (void)]))

(test-case "syntax-parse: #:conventions fail"
           (check-exn
            (lambda (exn)
              (check regexp-match? #rx"expected natural number greater than 0"
                     (exn-message exn)))
            (lambda ()
              (syntax-parse #'(4 0)
                #:conventions (nat-convs)
                [(N ...) (void)])))
           (void))

;; local conventions

(define-syntax-class (nats> bound)
  #:local-conventions ([N (nat> bound)])
  (pattern (N ...)))

(test-case "local conventions 1"
           (syntax-parse #'(1 2 3)
             #:local-conventions ([ns (nats> 0)])
             [ns (void)]))
(test-case "local conventions 2"
           (check-exn
            (lambda (exn)
              (check regexp-match? #rx"expected natural number greater than 2"
                     (exn-message exn)))
            (lambda ()
              (syntax-parse #'(1 2 3)
                #:local-conventions ([ns (nats> 2)])
                [ns (void)])))
           (void))

;; lazy attributes

(test-case "lazy syntax-valued attributes"
  (let ([counter 0])
    (define-syntax-class foo
      (pattern n:nat
               #:attr 2n
                      (delay
                        (set! counter (add1 counter))
                        (datum->syntax #'n (* 2 (syntax-e #'n))))))
    (syntax-parse #'45
      [x:foo
       (check-equal? counter 0) ;; hasn't run yet
       (attribute x.2n)
       (check-pred promise? (attribute x.2n))
       (check-equal? counter 0) ;; still hasn't run yet
       #'(lambda (q) x.2n)
       (check-equal? counter 1) ;; run
       #'(lambda (q) x.2n)
       (force (attribute x.2n))
       (check-equal? counter 1) ;; still only run once
       (void)])))

(test-case "lazy syntax-valued attributes, lists"
  ;; check both (promiseof (listof syntax)) and (listof (promiseof syntax)) work
  (let ([counter 0])
    (define-syntax-class foo
      (pattern (x:id ...)
               #:attr [alpha 1]
                      (delay (set! counter (add1 counter))
                             (filter (lambda (x)
                                       (regexp-match #rx"^[a-z]+$" (symbol->string (syntax-e x))))
                                     (syntax->list #'(x ...))))
               #:attr [alpha-part 1]
                      (map (lambda (x)
                             (delay
                               (set! counter (add1 counter))
                               (datum->syntax #f
                                 (car (regexp-match #rx"^[a-z]+" (symbol->string (syntax-e x)))))))
                           (syntax->list #'(x ...)))))
    (syntax-parse #'(abc g64 xyz c%)
      [f:foo
       (check-equal? counter 0)
       (check-pred syntax? #'(f.alpha ...))
       (check-equal? (syntax->datum #'(f.alpha ...)) '(abc xyz))
       (check-equal? counter 1)
       (check-pred syntax? #'(f.alpha-part ...))
       (check-equal? (syntax->datum #'(f.alpha-part ...)) '("abc" "g" "xyz" "c"))
       (check-equal? counter 5)
       (void)])))

;; == Lib tests

;; static

(tcerr "static: correct error"
       (let ()
         (define-syntax zero 0)
         (define-syntax (m stx)
           (syntax-parse stx
             [(_ x)
              #:declare x (static number? "identifier bound to number")
              #`(quote #,(attribute x.value))]))
         (m twelve))
       #rx"identifier bound to number")

(test-case "static: works"
  (check-equal? 
   (convert-syntax-error
    (let ()
      (define-syntax zero 0)
      (define-syntax (m stx)
        (syntax-parse stx
          [(_ x)
           #:declare x (static number? "identifier bound to number")
           #`(quote #,(attribute x.value))]))
      (m zero)))
   0)
  (void))


;; -- test #:declare scoping

(test-case "#:declare magical scoping"
  (syntax-parse #'(1 2)
    [(a b)
     #:declare a nat
     #:declare b (nat> (syntax-e #'a))
     (void)]))

(tcerr "#:declare magical scoping 2"
  (syntax-parse #'(1 1)
    [(a b)
     #:declare a nat
     #:declare b (nat> (syntax-e #'a))
     (void)]))

;; ---- Regression tests

(test-case "pvar is syntax"
  ;; from clklein 9/21/2011
  (check-true (syntax-parse #'(m 1 1 2 1 2 3)
                [(_ 1 ... . after-ones:expr)
                 (syntax? #'after-ones)]))
  (void))

(begin
  ;; from samth 2/4/2012
  ;; opaque head patterns used to propagate progress *with opaque marker* to tail
  (test-case "opaque H, ok"
    (check-equal? (syntax-parse #'(a b)
                    [((~describe #:opaque "x" (~seq x)) y:id) 'ok])
                  'ok))
  (test-case "opaque splicing stxclass, ok"
    (check-equal? (let ()
                    (define-splicing-syntax-class foo
                      #:opaque
                      #:description "foo"
                      (pattern (~seq x)))
                    (syntax-parse #'(a b)
                      [(f:foo y:id) 'ok]))
                  'ok))

  (test-case "opaque empty H, ok"
    (check-equal? (syntax-parse #'(b)
                    [((~describe #:opaque "x" (~seq)) y:id) 'ok])
                  'ok))
  (test-case "opaque empty splicing stxclass, ok"
    (check-equal? (let ()
                    (define-splicing-syntax-class foo
                      #:opaque
                      #:description "foo"
                      (pattern (~seq)))
                    (syntax-parse #'(b)
                      [(f:foo y:id) 'ok]))
                  'ok))

  (tcerr "extent of opaque in H pattern"
    (syntax-parse #'(a b)
      [((~describe #:opaque "x" (~seq x)) y:nat) (void)])
    (not #rx"expected x") ;; y:nat was incorrectly considered part of opaque region
    #rx"expected exact-nonnegative-integer")
  (tcerr "extent of opaque in splicing stxclass"
    (let ()
      (define-splicing-syntax-class foo
        #:description "foo"
        #:opaque
        (pattern (~seq x)))
      (syntax-parse #'(a b)
        [(f:foo n:nat) (void)]))
    (not #rx"expected foo") ;; y:nat was incorrectly considered part of opaque region
    #rx"expected exact-nonnegative-integer")

  (tcerr "extent of opaque in empty H pattern"
    (syntax-parse #'(b)
      [((~describe #:opaque "x" (~seq)) y:nat) (void)])
    (not #rx"expected x") ;; y:nat was incorrectly considered part of opaque region
    #rx"expected exact-nonnegative-integer")
  (tcerr "extent of opaque in empty splicing stxclass"
    (let ()
      (define-splicing-syntax-class foo
        #:description "foo"
        #:opaque
        (pattern (~seq)))
      (syntax-parse #'(b)
        [(f:foo n:nat) (void)]))
    (not #rx"expected foo") ;; y:nat was incorrectly considered part of opaque region
    #rx"expected exact-nonnegative-integer")
  )

;; from Neil Van Dyke (7/28/2012)
(test-case "specialized predicate-ellipsis-parser"
  ;; test that it works on improper lists
  ;; ... when input is syntax
  (check-eq? (syntax-parse #'(a b c . d) [(x:id ...) #t] [_ #f]) #f)
  ;; ... and when input is stx pair (but not syntax)
  (check-eq? (syntax-parse #'(a b c . d) [(_ x:id ...) #t] [_ #f]) #f)
  ;; test that it works on proper lists w/ embedded stxpairs
  (check-eq? (syntax-parse #'(a b . (c  d)) [(x:id ...) #t] [_ #f]) #t)
  (check-eq? (syntax-parse #'(a b . (c  d)) [(_ x:id ...) #t] [_ #f]) #t))

;; from Eric Dobson (11/30/2012)
(terx (x y) ((~describe #:opaque "an X" x:id) n:number)
      #rx"expected number"
      (not #rx"expected an X"))

;; from Eric Dobson (10/5/2013)
(test-case "optional/defaults checks delayed in stxclass def"
  (let ()
    (define-syntax-class blah
      #:attributes (a)
      [pattern ((~seq (~optional :one #:defaults [(a 'bar)])))])
    (void)))

;; from http://lists.racket-lang.org/users/archive/2014-June/063095.html
(test-case "pattern-expanders"
  (let ()
      (define-splicing-syntax-class binding #:literals (=)
        [pattern (~seq name:id = expr:expr)])
      
      (define-syntax ~separated
        (pattern-expander
         (lambda (stx)
           (syntax-case stx ()
             [(separated sep pat)
              (with-syntax ([ooo '...])
                #'((~seq pat (~or (~peek-not _)
                                  (~seq sep (~peek _))))
                   ooo))]))))
      
      (define-splicing-syntax-class bindings
        [pattern (~separated (~datum /) b:binding)
                 #:with (name ...) #'(b.name ...)
                 #:with (expr ...) #'(b.expr ...)])
    
    (define (parse-my-let stx)
      (syntax-parse stx
        [(_ bs:bindings body)
         #'(let ([bs.name bs.expr] ...)
             body)]))
    
    (check-equal? (syntax->datum
                   (parse-my-let #'(my-let (x = 1 / y = 2 / z = 3)
                                     (+ x y z))))
                  (syntax->datum #'(let ([x 1] [y 2] [z 3])
                                     (+ x y z))))
    ))
