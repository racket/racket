#lang scheme
(require rackunit
         syntax/parse)
(require (for-syntax syntax/parse))
(provide (all-defined-out))

;; Error selection tests

(error-print-source-location #f)

(define-syntax-rule (terx s p stuff ...)
  (terx* s [p] stuff ...))

(define-syntax-rule (terx* s [p ...] stuff ...)
  (terx** s [[p] ...] stuff ...))

(define-syntax terx**
  (syntax-parser
    [(terx s [[p c ...] ...] (~optional (~seq #:term term) #:defaults ([term #'#f])) rx ...)
     #`(test-case (format "line ~s: ~a match ~s for error"
                          '#,(syntax-line #'s)
                          's '(p ...))
         (let ([exn (let/ec escape
                      (check-exn (lambda (exn)
                                   (escape exn))
                                 (lambda ()
                                   (syntax-parse (quote-syntax s)
                                     [p c ... (void)] ...))))])
           (let ([msg (exn-message exn)]
                 [stxs (and (exn:fail:syntax? exn)
                            (exn:fail:syntax-exprs exn))])
             (when 'term
               (check-equal? (and (pair? stxs) (syntax->datum (car stxs))) 'term))
             (erx rx (exn-message exn)) ... #t))
         (void))]))

(define-syntax erx
  (syntax-rules (not)
    [(erx (not rx) msg)
     (check (compose not regexp-match?) rx msg)]
    [(erx rx msg)
     (check regexp-match? rx msg)]))

;; ----

(terx (a b c 7) (x:id ...)
      #:term 7
      #rx"expected identifier")

;; ----

(terx* (1 2) [x:nat (y:id z:id)]
       #:term 1
       #rx"expected identifier")

;; --

(define-syntax-class bindings
  (pattern ((var:id rhs:expr) ...)))

(terx* ((x 1 2)) [x:id bs:bindings]
       #:term 2
       #rx"unexpected term")

;; --

(terx ((a 1) (a 2))
      ((~or (~once ((~datum a) x) #:name "A clause")
            (~optional ((~datum b) y) #:name "B clause"))
       ...)
      ;; #:term (a 2)
      #rx"too many occurrences of A clause")

;; --

(define-syntax-class A
  (pattern ((~datum a) x)))
(define-syntax-class B
  (pattern ((~datum b) y)))

(terx ((a 1) (a 2))
      ((~or (~once a:A #:name "A clause")
            (~optional b:B #:name "B clause"))
       ...)
      #rx"too many occurrences of A clause")

(terx ((a 1 2) _)
      ((~or (~once a:A #:name "A clause")
            (~optional b:B #:name "B clause"))
       ...)
      #rx"unexpected term")

(terx ((b 1 2) _)
      ((~or (~once a:A #:name "A clause")
            (~optional b:B #:name "B clause"))
       ...)
      #rx"unexpected term")

;; Ellipses

(terx (a b c 4)
      (x:id ...)
      #rx"expected identifier")

;; Repetition constraints

(terx (1 2)
      ((~or (~once x:id #:name "identifier") n:nat) ...)
      #rx"missing required occurrence of identifier")

(terx (1 a 2 b)
      ((~or (~once x:id #:name "identifier") n:nat) ...)
      #rx"too many occurrences of identifier")

;; Roles

(terx 1
      (~var x id #:role "var")
      #rx"expected identifier for var")
(terx 1
      (~describe #:opaque #:role "R" "D" (_))
      #rx"expected D for R")
(terx 1
      (~describe #:role "R" "D" (_))
      #rx"expected D for R")

(test-case "#:describe #:role"
  (check-exn #rx"expected identifier for var"
             (lambda ()
               (syntax-parse #'1
                 [x
                  #:declare x id #:role "var"
                  'ok]))))

(test-case "role coalescing"
  (check-exn #rx"^m: expected identifier for thing$" ;; not repeated
             (lambda ()
               (syntax-parse #'(m 0 b)
                 [(_ x y:nat)
                  #:declare x id #:role "thing"
                  'a]
                 [(_ x y:id)
                  #:declare x id #:role "thing"
                  'b]))))

;; Expected more terms

(terx (1)
      (a b)
      #rx"expected more terms starting with any term$")

(terx (1)
      (a b:id)
      #rx"expected more terms starting with identifier$")

(terx (1)
      (a (~describe "thing" b))
      #rx"expected more terms starting with thing$")

(let ()
  (define-syntax-class B1 #:description "B1" (pattern _:id))
  (define-syntax-class B2 (pattern _:id))
  (terx (1)
        (a b:B1)
        #rx"expected more terms starting with B1")
  (terx (1)
        (a b:B2)
        #rx"expected more terms starting with B2"))

;; Post:

(terx "hello"
      (~or a:nat (~post a:id))
      #rx"expected identifier"
      (not #rx"exact-nonnegative-integer"))

(terx "hello"
      (~or a:nat (~and (~post (~fail "xyz")) _))
      #rx"xyz"
      (not #rx"exact-nonnegative-integer"))

(terx ("x")
      (~or (a:nat) (~post (a:id)))
      #rx"expected identifier"
      (not #rx"exact-nonnegative-integer"))

;; sequential ~and

(terx 1
      (~and (~or x:nat x:id) (~fail "never happy"))
      #rx"never happy"
      (not #rx"expected identifier"))

(terx** 1
        ([(~post (~or x:nat x:id)) #:fail-when #t "never happy"])
        #rx"never happy"
        (not #rx"expected identifier"))

;; indexes only compared within same ~and pattern
(terx** 1
        ([(~and (~fail "banana") _)]
         [(~and x:nat (~fail "apple"))]
         [(~and x:nat y:nat (~fail "orange"))])
        #rx"apple"
        #rx"orange"
        #rx"banana")

;; default for min rep constraint

(terx ()
      (x:id ...+)
      #rx"expected more terms starting with identifier")

(let ()
  (define-syntax-class thing (pattern _))
  (terx ()
        (x:thing ...+)
        #rx"expected more terms starting with thing"))

;; ----------------------------------------
;; See "Simplification" from syntax/parse/private/runtime-report

(define-syntax-class X #:opaque (pattern 1))
(define-syntax-class Y #:opaque (pattern 2))

(let ()
  ;; Case 1: [A B X], [A B Y]
  (define-syntax-class A (pattern (b:B _)))
  (define-syntax-class B (pattern (x:X _)) (pattern (y:Y _)))
  (terx ((3 _) _)
        a:A
        #:term 3
        #rx"expected X or expected Y"
        #rx"while parsing B.*while parsing A"))

(let ()
  ;; Case 2: [A X], [A]
  (terx 1
        (~describe "A" (x:id ...))
        #rx"expected A"))

(let ()
  ;; Case 3:  [t1:A t2:B t3:X], [t1:A t2:C t3:Y]
  (define-syntax-class A (pattern (b:B _)) (pattern (c:C _)))
  (define-syntax-class B (pattern (x:X _)))
  (define-syntax-class C (pattern (y:Y _)))
  (terx ((3 _) _)
        a:A
        #:term 3
        #rx"expected X or expected Y"
        (not #rx"while parsing [BC]")
        #rx"while parsing A"))

(let ()
  ;; Case 4: [t1:A t2:B t4:X], [t1:A t3:C t4:Y]
  (define-syntax-class A (pattern (b:B _)) (pattern (c:outerC _)))
  (define-syntax-class B (pattern (b:innerB _)))
  (define-syntax-class innerB #:description #f (pattern (x:X _)))
  (define-syntax-class outerC #:description #f (pattern (c:C _)))
  (define-syntax-class C (pattern (y:Y _)))
  (terx (((3 _) _) _)
        a:A
        #:term 3
        #rx"expected X or expected Y"
        (not #rx"while parsing (B|C|innerB|outerC|X|Y)")
        #rx"while parsing A"))

(let ()
  ;; Case 5: [t1:A t2:B t3:X], [t1:A t4:C t5:Y]
  ;; Need to use ~parse to get t3 != t5
  (define-syntax-class A (pattern (b:B)) (pattern (c:outerC)))
  (define-syntax-class B (pattern (b:innerB)))
  (define-syntax-class innerB #:description #f (pattern _ #:with x:X #'4))
  (define-syntax-class outerC #:description #f (pattern (c:C)))
  (define-syntax-class C (pattern _ #:with y:Y #'5))
  (terx (((3)))
        a:A
        #:term (((3)))
        #rx"expected A"
        (not #rx"while parsing (A|B|C|innerB|outerC|X|Y)")))


(let ()
  ;; Case 7: [_ t2:B t3:C _], [_ t3:C t2:B _]
  ;; Need to use ~parse; not sure if there's a realistic way for this to happen.
  ;; We will find the common frame, either B or C
  (define stxB #'4)
  (define stxC #'5)
  (define-syntax-class A
    (pattern (~and _ (~parse (~describe "B" (~and _ (~parse (~describe "C" 1) stxC))) stxB)))
    (pattern (~and _ (~parse (~describe "C" (~and _ (~parse (~describe "B" 2) stxB))) stxC))))
  (terx 3
        a:A
        ;; #:term {4 or 5}
        #rx"expected (B|C)"
        #rx"while parsing A"
        (not #rx"while parsing (B|C)")))

;; ------------------------------------------------------------
;; Regression tests

;; 4/16/2016, distilled from report by stchang
;; Want error message in second side clause to take precedence over
;; ellipsis-matching failures in first side clause.

(test-case "side-clauses order 1"
  (check-exn #rx"unhappy about last number"
             (lambda ()
               (syntax-parse #'(1 2 3 4)
                 [(x:nat ...)
                  #:with (y ... z) #'(x ...)
                  #:fail-unless (>= (syntax->datum #'z) 10)
                                "unhappy about last number"
                  'ok]))))

(test-case "side-clauses order 2"
  (check-exn (lambda (exn)
               (and (regexp-match? #rx"unhappy about last number" (exn-message exn))
                    (exn:fail:syntax? exn)
                    (let* ([terms (exn:fail:syntax-exprs exn)]
                           [term (and (pair? terms) (syntax->datum (car terms)))])
                      (check-equal? term '4))))
             (lambda ()
               (syntax-parse #'(1 2 3 4)
                 [(x:nat ...)
                  #:with (y ... z) #'(x ...)
                  #:fail-when (and (< (syntax->datum #'z) 10) #'z)
                              "unhappy about last number"
                  'ok]))))

(test-case "side-clauses in different stxclasses don't compare"
  (check-exn #rx"message1 or message2"
             (lambda ()
               (syntax-parse #'(1 2 3 4)
                 [(x:nat ...)
                  #:with (y ... z) #'(x ...)
                  #:fail-unless #f "message1" ;; (post 'g1 2)
                  'ok]
                 [(x:nat ...)
                  #:with (y ... z) #'(x ...)
                  #:with w #'whatever
                  #:fail-unless #f "message2" ;; (post 'g2 3), incomp w/ above
                  'ok]))))
