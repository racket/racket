#lang racket/base
(require (for-syntax racket/base)
         rackunit
         (only-in "setup.rkt" convert-syntax-error tcerr)
         racket/promise
         racket/syntax
         syntax/parse)

;; Additional tests for syntax w/ ~?, ~@, etc

(define-syntax (tc stx)
  (syntax-case stx ()
    [(tc expr expected)
     #`(test-equal? (format "line ~s" #,(syntax-line stx))
                    (syntax->datum (convert-syntax-error expr))
                    expected)]))

(define-syntax (terx stx)
  (syntax-case stx ()
    [(terx expr err-rx ...)
     #`(tcerr (format "line ~s" #,(syntax-line stx)) expr err-rx ...)]))

;; ----------------------------------------

;; Common pattern variable definitions
;; (avoids having to have 'with-syntax' in every test case)

(define/with-syntax uu #'abc)
(define/with-syntax (aa ...) #'(a b c))
(define/with-syntax (xx ...) #'(x y z))
(define/with-syntax (nn ...) #'(1 2 3))
(define/with-syntax ((yy ...) ...) #'((1 2 3) (4 5 6) (7 8 9)))

(define/syntax-parse (~or* oo:nat _:id) #'x)
(define/syntax-parse ((~or* pp:nat _:id) ...) #'(a 1 b 2 3))

;; ----------------------------------------

(tc (syntax uu) 'abc)

;; FIXME: add other atoms when supported
;; FIXME: add other compound stx when supported
(tc (syntax abz) 'abz)
(tc (syntax ()) '())
(tc (syntax 5) '5)
(tc (syntax (1 2 #f #t "hey")) '(1 2 #f #t "hey"))
(tc (syntax (1 . b)) '(1 . b))
(tc (syntax (1 . uu)) '(1 . abc))

(tc (syntax #(aa ... done))
    '#(a b c done))
(tc (syntax #s(blah xx ...))
    '#s(blah x y z))

(tc (syntax (aa ...))
    '(a b c))
(tc (syntax ((uu aa) ...))
    '((abc a) (abc b) (abc c)))
(tc (syntax ((aa aa) ...))
    '((a a) (b b) (c c)))
(tc (syntax (start (aa ok) ... done))
    '(start (a ok) (b ok) (c ok) done))
(tc (syntax ((aa nn xx) ...))
    '((a 1 x) (b 2 y) (c 3 z)))
(tc (syntax (aa ... ((nn xx) ...)))
    '(a b c ((1 x) (2 y) (3 z))))
(tc (syntax (aa ... (nn xx) ...))
    '(a b c (1 x) (2 y) (3 z)))

(tc (syntax (aa ... ((yy ok) ...) ...))
    '(a b c ((1 ok) (2 ok) (3 ok)) ((4 ok) (5 ok) (6 ok)) ((7 ok) (8 ok) (9 ok))))

(tc (syntax ((~@ 1 2) 3))
    '(1 2 3))
(tc (with-syntax ([w '(1 2 3)])
      (syntax ((~@ 0 . w) 4)))
    '(0 1 2 3 4))
(tc (syntax ((~@ aa ok) ...))
    '(a ok b ok c ok))
(tc (syntax ((~@ aa nn) ...))
    '(a 1 b 2 c 3))
(tc (syntax (aa ... (~@ nn xx) ...))
    '(a b c 1 x 2 y 3 z))

;; escape
(tc (syntax (abc (xx (... (q ...))) ...))
    '(abc (x (q ...)) (y (q ...)) (z (q ...))))
(tc (syntax (abc (xx (... (q ... nn))) ...))
    '(abc (x (q ... 1)) (y (q ... 2)) (z (q ... 3))))

;; consecutive ellipses
(tc (syntax (yy ... ...))
    '(1 2 3 4 5 6 7 8 9))

;; ~?
(tc (syntax (~? (ok oo go) nah))
    'nah)
(tc (syntax ((~? (ready oo)) done))
    '(done))

;; liberal depth rules

(tc (syntax (((uu aa yy) ...) ...))
    '(((abc a 1) (abc b 2) (abc c 3))
      ((abc a 4) (abc b 5) (abc c 6))
      ((abc a 7) (abc b 8) (abc c 9))))
(tc (syntax (((uu aa yy) ...) ...))
    ;; compatible with syntax
    (syntax->datum #'(((uu aa yy) ...) ...)))

(tc (syntax ((aa ... xx) ...))
    '((a b c x) (a b c y) (a b c z)))

;; liberal depth rules with consecutive ellipses

(tc (syntax ((aa yy) ... ...))
    '((a 1) (b 2) (c 3) (a 4) (b 5) (c 6) (a 7) (b 8) (c 9)))
(tc (syntax ((aa yy) ... ...))
    (syntax->datum #'((aa yy) ... ...)))

;; head ~?

(tc (syntax ((~? (~@ #:yes uu) (~@ #:no)) done))
    '(#:yes abc done))
(tc (syntax ((~? (~@ #:yes oo) (~@ #:no)) done))
    '(#:no done))

(tc (syntax ((~? (~@ #:yes pp) (~@ #:no)) ...))
    '(#:no #:yes 1 #:no #:yes 2 #:yes 3))

;; ----------------------------------------

;; combined ~? ~@
(tc (syntax-parse #'(a b c 1 2 3)
      [(a:id ... (~optional s:str) n:nat ...)
       (syntax (a ... n ... (~@ . (~? (string: s) ()))))])
    '(a b c 1 2 3))
(tc (syntax-parse #'(a b c "hello!" 1 2 3)
      [(a:id ... (~optional s:str) n:nat ...)
       (syntax (a ... n ... (~@ . (~? (string: s) ()))))])
    '(a b c 1 2 3 string: "hello!"))

;; ----------------------------------------

(tc (quasisyntax (a #,'b))
    '(a b))
(tc (quasisyntax ((aa #,'0) ...))
    '((a 0) (b 0) (c 0)))

;; quasiquote-style nesting
(tc (quasisyntax (#,1 (quasisyntax #,(+ 1 2))))
    '(1 (quasisyntax (unsyntax (+ 1 2)))))
(tc (quasisyntax (#,1 (quasisyntax #,#,(+ 1 2))))
    '(1 (quasisyntax (unsyntax 3))))

;; quasi-inside-escape
(tc (quasisyntax (... (1 2 #,@(list #'3) 4)))
    '(1 2 3 4))

;; ============================================================

;; Error tests

(terx (syntax (1 ...))
      #rx"no pattern variables before ellipsis in template")

(terx (syntax (uu ...))
      #rx"too many ellipses in template")

(terx (syntax ((aa ... uu) ...))
      #rx"too many ellipses in template")

(terx (syntax aa)
      #rx"missing ellipsis with pattern variable in template")

(terx (syntax (~@))
      #rx"illegal use")

(terx (syntax ((~@ . uu)))
      #rx"splicing template did not produce a syntax list")

(terx (with-syntax ([(bb ...) #'(y z)]) (syntax ((aa bb) ...)))
      #rx"incompatible ellipsis match counts")

;; ============================================================

(define loc (datum->syntax #'here 'loc (list "I have a location!" #f #f 42 17)))

(define-syntax-rule (tloc tform tmpl loc?)
  (test-case (format "~s" '(loc tmpl))
    (let ([result (convert-syntax-error (tform loc tmpl))])
      (cond [loc?
             (check-equal? (syntax-source result) (syntax-source loc))
             (check-equal? (syntax-position result) (syntax-position loc))]
            [else
             (check-equal? (syntax-source result) (syntax-source (quote-syntax here)))]))))

(tloc syntax/loc uu #f)
(tloc syntax/loc lambda #t)
(tloc syntax/loc (lambda (x) x) #t)
(tloc syntax/loc (aa ... 1) #t)
(tloc syntax/loc (aa ... . 1) #t)
(with-syntax ([(z ...) '()])
  (tloc syntax/loc (z ... . 2) #f)) ;; zero iters + syntax tail => no relocation
(tloc syntax/loc ((~@ aa ...) 2) #t)
(tloc syntax/loc ((~@ aa ...) . 2) #t)
(with-syntax ([lst #'(a b c)] [nil #'()])
  (tloc syntax/loc ((~@ . lst) 2) #t)
  (tloc syntax/loc ((~@ . lst) . 2) #t)
  (tloc syntax/loc ((~@ . nil) 2) #t)
  (tloc syntax/loc ((~@ . nil) . 2) #f)) ;; empty + syntax tail => no relocation
(tloc syntax/loc (~? 1 2) #t)

(tloc quasisyntax/loc uu #f)
(tloc quasisyntax/loc lambda #t)
(tloc quasisyntax/loc (lambda (x) x) #t)
(tloc quasisyntax/loc (aa ... 1) #t)
(tloc quasisyntax/loc (aa ... . 1) #t)
(with-syntax ([(z ...) '()])
  (tloc quasisyntax/loc (z ... . 2) #f)) ;; zero iters + syntax tail => no relocation
(tloc quasisyntax/loc (#,'a) #t)

;; Lazy attribute tests from test.rkt

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
       (syntax (lambda (q) x.2n))
       (check-equal? counter 1) ;; run
       (syntax (lambda (q) x.2n))
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
       (check-pred syntax? (syntax (f.alpha ...)))
       (check-equal? (syntax->datum (syntax (f.alpha ...))) '(abc xyz))
       (check-equal? counter 1)
       (check-pred syntax? (syntax (f.alpha-part ...)))
       (check-equal? (syntax->datum (syntax (f.alpha-part ...))) '("abc" "g" "xyz" "c"))
       (check-equal? counter 5)
       (void)])))

(test-case "lazy syntax-valued attributes, ~?, ~@"
  (let ()
    (define-syntax-class foo
      (pattern n:nat
               #:attr [factor 1]
                      (delay
                        (let ([n (syntax-e #'n)])
                          (for/list ([f (in-range 2 n)]
                                     #:when (zero? (remainder n f)))
                            (datum->syntax #f f))))
               #:attr half
                      (let ([n (syntax-e #'n)])
                        (if (zero? (remainder n 2))
                            (delay (datum->syntax #f (quotient n 2)))
                            #f))))
    (syntax-parse #'(1 2 3 4 5 6 7)
      [(n:foo ...)
       (let ([factors (syntax ((n.factor ...) ...))])
         (check-pred syntax? factors)
         (check-equal? (syntax->datum factors)
                       '(() () () (2) () (2 3) ())))
       (check-exn #rx"attribute contains non-syntax value"
                  (lambda () (syntax (n.half ...))))
       (let ([halves (syntax ((~? n.half) ...))])
         (check-pred syntax? halves)
         (check-equal? (syntax->datum halves)
                       '(1 2 3)))
       (void)])))

;; ----------------------------------------
;; Testing raise/handlers-based ~? (used to be based on drivers check)

(tc (syntax-parse #'()
      [((~optional abs))
       (syntax (~? (~? abs inner) outer))])
    'inner)

;; test from ianj, 11/18/2013
(tc (syntax-parse #'(a)
      [(a:expr (~optional b:expr))
       (syntax (~? '(a (~? b 0)) 0))])
    ''(a 0))

(define/syntax-parse ((~or* i:id n:nat) ...) '(a b 1 2 3 4))

;; note: i,n both 6 elts long
(tc (syntax ((~? i X) ...))
    '(a b X X X X))
(tc (syntax ((~? i n) ...))
    '(a b 1 2 3 4))

(tc (syntax ((~? i) ...)) '(a b))
(tc (syntax ((~? n) ...)) '(1 2 3 4))
(tc (syntax (~? (i ...) no)) 'no)
(tc (syntax (~? (n ...) no)) 'no)

;; test from ianj, 5/14/2014
(tc (syntax-parse #'(A)
      [(x:id (~optional (~seq #:a [a b] ...)))
       (syntax (~? (hash (~@ a b) ...) x))])
    'A)
