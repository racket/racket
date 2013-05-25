#lang racket/base
(require (for-syntax racket/base)
         rackunit
         (only-in "setup.rkt" convert-syntax-error tcerr)
         racket/promise
         racket/syntax
         syntax/parse
         syntax/parse/experimental/template)

;; FIXME: need to test errors, too

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

(define/syntax-parse (~or oo:nat _:id) #'x)
(define/syntax-parse ((~describe "x" (~or pp:nat _:id)) ...) #'(a 1 b 2 3))

;; ----------------------------------------

(tc (template uu) 'abc)

;; FIXME: add other atoms when supported
;; FIXME: add other compound stx when supported
(tc (template abz) 'abz)
(tc (template ()) '())
(tc (template 5) '5)
(tc (template (1 2 #f #t "hey")) '(1 2 #f #t "hey"))
(tc (template (1 . b)) '(1 . b))
(tc (template (1 . uu)) '(1 . abc))

(tc (template #(aa ... done))
    '#(a b c done))
(tc (template #s(blah xx ...))
    '#s(blah x y z))

(tc (template (aa ...))
    '(a b c))
(tc (template ((uu aa) ...))
    '((abc a) (abc b) (abc c)))
(tc (template ((aa aa) ...))
    '((a a) (b b) (c c)))
(tc (template (start (aa ok) ... done))
    '(start (a ok) (b ok) (c ok) done))
(tc (template ((aa nn xx) ...))
    '((a 1 x) (b 2 y) (c 3 z)))
(tc (template (aa ... ((nn xx) ...)))
    '(a b c ((1 x) (2 y) (3 z))))
(tc (template (aa ... (nn xx) ...))
    '(a b c (1 x) (2 y) (3 z)))

(tc (template (aa ... ((yy ok) ...) ...))
    '(a b c ((1 ok) (2 ok) (3 ok)) ((4 ok) (5 ok) (6 ok)) ((7 ok) (8 ok) (9 ok))))

(tc (template ((?@ 1 2) 3))
    '(1 2 3))
(tc (with-syntax ([w '(1 2 3)])
      (template ((?@ 0 . w) 4)))
    '(0 1 2 3 4))
(tc (template ((?@ aa ok) ...))
    '(a ok b ok c ok))
(tc (template ((?@ aa nn) ...))
    '(a 1 b 2 c 3))
(tc (template (aa ... (?@ nn xx) ...))
    '(a b c 1 x 2 y 3 z))

;; escape
(tc (template (abc (xx (... (q ...))) ...))
    '(abc (x (q ...)) (y (q ...)) (z (q ...))))
(tc (template (abc (xx (... (q ... nn))) ...))
    '(abc (x (q ... 1)) (y (q ... 2)) (z (q ... 3))))

;; consecutive ellipses
(tc (template (yy ... ...))
    '(1 2 3 4 5 6 7 8 9))

;; ??
(tc (template (?? (ok oo go) nah))
    'nah)
(tc (template ((?? (ready oo)) done))
    '(done))

;; liberal depth rules

(tc (template (((uu aa yy) ...) ...))
    '(((abc a 1) (abc b 2) (abc c 3))
      ((abc a 4) (abc b 5) (abc c 6))
      ((abc a 7) (abc b 8) (abc c 9))))
(tc (template (((uu aa yy) ...) ...))
    ;; compatible with syntax
    (syntax->datum #'(((uu aa yy) ...) ...)))

(tc (template ((aa ... xx) ...))
    '((a b c x) (a b c y) (a b c z)))

;; liberal depth rules with consecutive ellipses

(tc (template ((aa yy) ... ...))
    '((a 1) (b 2) (c 3) (a 4) (b 5) (c 6) (a 7) (b 8) (c 9)))
(tc (template ((aa yy) ... ...))
    (syntax->datum #'((aa yy) ... ...)))

;; head ??

(tc (template ((?? (?@ #:yes uu) (?@ #:no)) done))
    '(#:yes abc done))
(tc (template ((?? (?@ #:yes oo) (?@ #:no)) done))
    '(#:no done))

(tc (template ((?? (?@ #:yes pp) (?@ #:no)) ...))
    '(#:no #:yes 1 #:no #:yes 2 #:yes 3))

;; ----------------------------------------

;; combined ?? ?@
(tc (syntax-parse #'(a b c 1 2 3)
      [(a:id ... (~optional s:str) n:nat ...)
       (template (a ... n ... (?@ . (?? (string: s) ()))))])
    '(a b c 1 2 3))
(tc (syntax-parse #'(a b c "hello!" 1 2 3)
      [(a:id ... (~optional s:str) n:nat ...)
       (template (a ... n ... (?@ . (?? (string: s) ()))))])
    '(a b c 1 2 3 string: "hello!"))

;; ----------------------------------------

(define-template-metafunction (join stx)
  (syntax-parse stx
    [(join a:id b:id ...)
     (datum->syntax #'a
                    (string->symbol
                     (apply string-append
                            (map symbol->string
                                 (syntax->datum #'(a b ...)))))
                    stx)]))

(tc (template (join a b c))
    'abc)
(tc (template ((xx (join tmp- xx)) ...))
    '((x tmp-x) (y tmp-y) (z tmp-z)))
(tc (template ((xx (join uu - xx)) ...))
    '((x abc-x) (y abc-y) (z abc-z)))
(tc (template ((xx (join aa xx)) ...))
    '((x ax) (y by) (z cz)))

;; ----------------------------------------

(tc (quasitemplate (a #,'b))
    '(a b))
(tc (quasitemplate ((aa #,'0) ...))
    '((a 0) (b 0) (c 0)))

;; quasiquote-style nesting
(tc (quasitemplate (#,1 (quasitemplate #,(+ 1 2))))
    '(1 (quasitemplate (unsyntax (+ 1 2)))))
(tc (quasitemplate (#,1 (quasitemplate #,#,(+ 1 2))))
    '(1 (quasitemplate (unsyntax 3))))

;; ============================================================

;; Error tests

(terx (template (1 ...))
      #rx"no pattern variables before ellipsis in template")

(terx (template (uu ...))
      #rx"too many ellipses in template")

(terx (template ((aa ... uu) ...))
      #rx"too many ellipses in template")

(terx (template aa)
      #rx"missing ellipses with pattern variable in template")

(terx (template (?@))
      #rx"illegal use")

(terx (template ((?@ . uu)))
      #rx"splicing template did not produce a syntax list")

(define-template-metafunction (bad-mf stx) 123)

(terx (template (bad-mf))
      #rx"result of template metafunction was not syntax")

(terx (with-syntax ([(bb ...) #'(y z)]) (template ((aa bb) ...)))
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

(tloc template/loc uu #f)
(tloc template/loc lambda #t)
(tloc template/loc (lambda (x) x) #t)
(tloc template/loc (aa ... 1) #f)
(terx (template/loc loc ((?@ aa ...) 2))
      #rx"cannot apply syntax location to template")
(terx (template/loc loc (?? 1 2))
      #rx"cannot apply syntax location to template")

(tloc quasitemplate/loc uu #f)
(tloc quasitemplate/loc lambda #t)
(tloc quasitemplate/loc (lambda (x) x) #t)
(tloc quasitemplate/loc (aa ... 1) #f)
(tloc quasitemplate/loc (#,'a) #t)
(tloc quasitemplate/loc #,'a #f)
(tloc quasitemplate/loc (#,@(list 1 2 3)) #f)
(terx (quasitemplate/loc loc ((?@ aa ...) 2))
      #rx"cannot apply syntax location to template")
(terx (quasitemplate/loc loc (?? 1 2))
      #rx"cannot apply syntax location to template")

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
       (template (lambda (q) x.2n))
       (check-equal? counter 1) ;; run
       (template (lambda (q) x.2n))
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
       (check-pred syntax? (template (f.alpha ...)))
       (check-equal? (syntax->datum (template (f.alpha ...))) '(abc xyz))
       (check-equal? counter 1)
       (check-pred syntax? (template (f.alpha-part ...)))
       (check-equal? (syntax->datum (template (f.alpha-part ...))) '("abc" "g" "xyz" "c"))
       (check-equal? counter 5)
       (void)])))

(test-case "lazy syntax-valued attributes, ??, ?@"
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
       (let ([factors (template ((n.factor ...) ...))])
         (check-pred syntax? factors)
         (check-equal? (syntax->datum factors)
                       '(() () () (2) () (2 3) ())))
       (check-exn #rx"attribute is bound to non-syntax value"
                  (lambda () (template (n.half ...))))
       (let ([halves (template ((?? n.half) ...))])
         (check-pred syntax? halves)
         (check-equal? (syntax->datum halves)
                       '(1 2 3)))
       (void)])))
