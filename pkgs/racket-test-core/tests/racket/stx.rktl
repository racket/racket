
(load-relative "loadtest.rktl")

(Section 'stx)

(test #t syntax? (datum->syntax #f 'hello #f))

(test #f syntax-line (datum->syntax #f 10 '(aha #f #f 19 #f)))
(test #f syntax-column (datum->syntax #f 10 '(aha #f #f 19 #f)))
(test 19 syntax-position (datum->syntax #f 10 '(aha #f #f 19 #f)))
(test 'aha syntax-source (datum->syntax #f 10 '(aha #f #f 19 #f)))
(test #f syntax-span (datum->syntax #f 10 '(aha #f #f 19 #f)))
(test 88 syntax-span (datum->syntax #f 10 '(aha #f #f 19 88)))

(test 7 syntax-line (datum->syntax #f 10 '(aha 7 88 999 #f)))
(test 88 syntax-column (datum->syntax #f 10 '(aha 7 88 999 #f)))
(test 999 syntax-position (datum->syntax #f 10 '(aha 7 88 999 #f)))
(test 'aha syntax-source (datum->syntax #f 10 '(aha 7 88 999 #f)))
(test #f syntax-span (datum->syntax #f 10 '(aha 7 88 999 #f)))
(test 22 syntax-span (datum->syntax #f 10 '(aha 7 88 999 22)))
(test 0 syntax-span (datum->syntax #f 10 '(aha 1 1 1 0)))
(test 0 syntax-column (datum->syntax #f 10 '(aha 1 0 1 0)))

(err/rt-test (datum->syntax #f 10 10))
(err/rt-test (datum->syntax #f 10 '(10)))
(err/rt-test (datum->syntax #f 10 '(10 11)))
(err/rt-test (datum->syntax #f 10 '(10 11 12)))
(err/rt-test (datum->syntax #f 10 '(10 11 12 13)))
(err/rt-test (datum->syntax #f 10 '(10 11 12 13 14 15)))
(err/rt-test (datum->syntax #f 10 '(a 11.0 12 13 14)))
(err/rt-test (datum->syntax #f 10 '(a 11 12 -13 14)))
(err/rt-test (datum->syntax #f 10 '(a 11 12 -13 14)))
(err/rt-test (datum->syntax #f 10 '(a 11 12 13 -1)))
(err/rt-test (datum->syntax #f 10 '(a 0 12 13 0)))
(err/rt-test (datum->syntax #f 10 '(a 11 -1 13 0)))
(err/rt-test (datum->syntax #f 10 '(a 11 12 0 0)))

(syntax-test #'quote-syntax)
(syntax-test #'(quote-syntax))
(syntax-test #'(quote-syntax . 7))

;; Property is attached only to immediate syntax object:
(test #f
      syntax-property
      (car (syntax-e (datum->syntax #f '(a) #f (syntax-property #'x 'ok 'value))))
      'ok)
(test 'value
      syntax-property
      (datum->syntax #f '(a) #f (syntax-property #'x 'ok 'value))
      'ok)

(let ([s (syntax-property #'s 'key 'val)])
  (test 'val syntax-property s 'key)
  (test #f syntax-property (syntax-property-remove s 'key) 'key))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some syntax-case patterns
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test 17 'syntax-case (syntax-case '(1 1 1) () [(1 ...) 17]))

(define-syntax sd (syntax-rules () [(_ v) (syntax->datum (syntax v))]))

(test '(3 1 2) 'syntax-case (syntax-case '(1 2 3) () [(a ... b) (sd (b a ...))]))
(test '(3 1 2) 'syntax-case (syntax-case '(1 2 3) () [(a ... b . c) (sd (b a ...))]))
(test '(3 1 2) 'syntax-case (syntax-case '(1 2 3) () [(a ... 3 . c) (sd (3 a ...))]))
(test 5 'syntax-case (syntax-case '(1 2 3 4) () [(a ... 3 . c) (sd (3 a ... c))][_else 5]))
(test '(3 1 2 4) 'syntax-case (syntax-case '(1 2 3 . 4) () [(a ... b . c) (sd (b a ... c))][_else 5]))
(test '(3 1 2 4) 'syntax-case (syntax-case '(1 2 (3 . 4)) () [(a ... (b . c)) (sd (b a ... c))][_else 5]))
(test '((3) 1 2 4) 'syntax-case (syntax-case '(1 2 (3 . 4)) () [(a ... (b ... . c)) (sd ((b ...) a ... c))][_else 5]))
(test '(3 1 2 4) 'syntax-case (syntax-case '(1 2 (3 . 4)) () [(a ... (b ... . c)) (sd (b ... a ... c))][_else 5]))
(test '((3) 1 2 4) 'syntax-case (syntax-case '(1 2 ((3) . 4)) () [(a ... ((b ...) ... . c)) (sd ((b ...) ... a ... c))][_else 5]))
(test '(3 1 2 4) 'syntax-case (syntax-case '(1 2 ((3) . 4)) () [(a ... ((b ...) ... . c)) (sd (b ... ... a ... c))][_else 5]))

(syntax-test (quote-syntax (syntax-case 0 () [(a ... b c ...) 1][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ... b . (c ...)) 1][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ... ...) 1][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ...) #'a][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ...) #'((a ...) ...)][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [(a ...) #'(a ... ...)][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [((a ...) ...) #'a][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [((a ...) ...) #'(a ...)][_else 5])))
(syntax-test (quote-syntax (syntax-case 0 () [((a ...) ...) #'(a ... ... ...)][_else 5])))

(test 'no 'dot-literal (syntax-case #'(1 2) () [(_ . #t) 'yes] [_ 'no]))
(test 'yes 'dot-literal (syntax-case #'(1 . #t) () [(_ . #t) 'yes] [_ 'no]))

(test '(((x 3) (y 3) (z 3)) ;; each line should be x y z, not x x x...
        ((x 4) (y 4) (z 4))
        ((x 5) (y 5) (z 5)))
      'ellipses
      (syntax->datum (syntax-case '(_ 1 (x y z) ((3 3 3) (4 4 4) (5 5 5))) ()
                       [(_ x (a ...) ((b ...) ...)) #'(((a b) ...) ...)])))

(test '(((x y z 3) (x y z 3) (x y z 3))
        ((x y z 4) (x y z 4) (x y z 4))
        ((x y z 5) (x y z 5) (x y z 5)))
      'ellipses
      (syntax->datum (syntax-case '(_ 1 (x y z) ((3 3 3) (4 4 4) (5 5 5))) ()
                       [(_ x (a ...) ((b ...) ...)) #'(((a ... b) ...) ...)])))


(test '((1 z) (2 w) (x z) (y w))
      'ellipses
      (syntax->datum (syntax-case '(((1 2) (x y)) (z w)) ()
                       [(((a ...) ...) (b ...)) #'((a b) ... ...)])))

(test '(#(1) #(2 3))
      'ellipses+vector
      (syntax->datum
       (syntax-case '((1) (2 3)) ()
         [((a ...) ...) #'(#(a ...) ...)])))

(test '(1 2 3 6 8 9 0 1 2 3)
      syntax->datum
      (syntax-case '(((1) (2 3)) ((6)) ((8 9 0) (1 2 3))) ()
        [(((a ...) ...) ...) #'(a ... ... ...)]))
(test '((1 2 3) (6) (8 9 0 1 2 3))
      syntax->datum
      (syntax-case '(((1) (2 3)) ((6)) ((8 9 0) (1 2 3))) ()
        [(((a ...) ...) ...) #'((a ... ...) ...)]))
(test '((1) (2 3) (6) (8 9 0) (1 2 3))
      syntax->datum
      (syntax-case '(((1) (2 3)) ((6)) ((8 9 0) (1 2 3))) ()
        [(((a ...) ...) ...) #'((a ...) ... ...)]))

(test (syntax-case #'((([n 1] [m 2]) ([p 10] [q 20]))
                      (([nn -1] [mm -2]) ([pp -10] [qq -20]))) ()
        [((([x y] ...) ...) ...)
         (syntax->datum #'(ell ((ull (+ x ...)
                                     ((- x ... y ...) ...))
                                ...)
                               ...))])
      'fancy-ellipses
      '(ell ((ull (+ n m) ((- n m 1 2) (- p q 10 20)))
             (ull (+ p q) ((- nn mm -1 -2) (- pp qq -10 -20))))
            ((ull (+ nn mm) ((- n m 1 2) (- p q 10 20)))
             (ull (+ pp qq) ((- nn mm -1 -2) (- pp qq -10 -20))))))

(test (syntax-case #'((([n 1] [m 2]) ([p 10] [q 20]))
                      (([nn -1] [mm -2]) ([pp -10] [qq -20]))) ()
        [((([x y] ...) ...) ...)
         (syntax->datum #'(ell ((ull (+ x ...)
                                     ((- x ...) ...))
                                ...)
                               ...))])
      'fancy-ellipses
      '(ell ((ull (+ n m) ((- n m) (- p q)))
             (ull (+ p q) ((- nn mm) (- pp qq))))
            ((ull (+ nn mm) ((- n m) (- p q)))
             (ull (+ pp qq) ((- nn mm) (- pp qq))))))

(test 5 syntax-e (syntax-case #'#&5 ()
                   [#&x #'x]))
(test '(0 1 2 3 4) syntax->datum
      (syntax-case #'#&(1 2 3) ()
                   [#&(x ...) #'(0 x ... 4)]))

(syntax-test #'(syntax-case (syntax x) [x (define x 1)]))
(syntax-test #'(syntax-case (syntax x) () [x (define x 1)])
             #rx"stx.rktl:.*no expression after a sequence of internal definitions")
(syntax-test #'(syntax-case* (syntax x) () eq? [x (define x 1)])
             #rx"stx.rktl:.*no expression after a sequence of internal definitions")


;; ----------------------------------------

(test #t syntax-original? #'here)
(test #f syntax-original? ((make-syntax-introducer) #'here))
(test #t syntax-original? ((make-syntax-introducer #t) #'here))

(let* ([a (datum->syntax #f 'a)]
       [a1 ((make-syntax-introducer) a)]
       [a2 ((make-syntax-introducer) a)])
  (test #f bound-identifier=? a1 a2)
  (test #t bound-identifier=? a1 ((make-syntax-delta-introducer a1 a2) a))
  (test #t bound-identifier=? a2 ((make-syntax-delta-introducer a2 a1) a))
  (test #t bound-identifier=? a2 ((make-syntax-delta-introducer a2 #f) a))
  (test #t bound-identifier=?
        ((make-syntax-delta-introducer a1 a2) a2)
        ((make-syntax-delta-introducer a2 a1) a1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test basic expansion and property propagation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-map f)
  (lambda (l)
    (if (pair? l)
        (cons ((tree-map f) (car l))
              ((tree-map f) (cdr l)))
        (if (null? l)
            null
            (f l)))))

(define-syntax mcr
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) (syntax (begin x))])))

(define s (quote-syntax (mcr 5)))
(define se (expand-once s))

(syntax-case se ()
  [(bg five)
   (let ([bg (syntax bg)]
         [five (syntax five)])
     (test 'begin syntax-e bg)
     (test 5 syntax-e five)

     (test #t syntax-original? five)
     (test #f syntax-original? bg)

     'ok)])

(test #f syntax-property s 'testing)


(define-syntax mcr0
  (lambda (stx)
    (syntax-case stx ()
      [(_) (syntax (begin 0))])))

(define s (quote-syntax (mcr0)))
(define se (expand-once s))

(test #t syntax-original? s)
(test #f syntax-original? se)

;; Check that a property in a template is preserved by #'

(define-syntax (define-define-stx stx)
  (syntax-case stx ()
    [(_ stx)
     (with-syntax ([template (syntax-property #'(x)
                                              'x
                                              'y)])
       #'(define stx
           (with-syntax ([x #'hi])
             #'template)))]))

(define-define-stx stx-with-property)
(test 'y syntax-property stx-with-property 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that protected references are annotated with a 'protected property

(module exports-macros-that-expand-to-protected-references racket/base
  (provide emtetpr-m1 emtetpr-m2 (protect-out x1))
  (define x1 1)
  (define x2 2)
  (define-syntax-rule (emtetpr-m1) x1)
  (define-syntax-rule (emtetpr-m2) x2))

(require 'exports-macros-that-expand-to-protected-references)

(test #t syntax-property (expand #'(emtetpr-m1)) 'protected)
(test #t syntax-property (expand #'(emtetpr-m2)) 'protected)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check immutability

(test #t immutable? (syntax-e #'#(1 2)))
(test #t immutable? (syntax-e #'#&1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plain s, se derived from part of s

(define s (syntax-property (quote-syntax (mcr 5)) 'testing 10))
(define se (expand-once s))

(test 10 syntax-property s 'testing)
(test 10 syntax-property se 'testing)
(test '(mcr) (tree-map syntax-e) (syntax-property se 'origin))

(test 10 syntax-property (datum->syntax #f 0 #f s) 'testing)

(test #t syntax-original? s)
(test #f syntax-original? se)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plain s, se is part of s

(define-syntax mcr2
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) (syntax x)])))

(define s (syntax-property (quote-syntax (mcr2 5)) 'testing 10))
(define se (expand-once s))

(test (syntax-e (cadr (syntax-e s))) syntax-e se)

(test 10 syntax-property s 'testing)
(test 10 syntax-property se 'testing)
(test '(mcr2) (tree-map syntax-e) (syntax-property se 'origin))

(test #t syntax-original? s)
(test #t syntax-original? se)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructed s, se is part of s, part of s tagged

(define s
  (syntax-property
   (with-syntax ([five (syntax-property (quote-syntax 5) 'testing 12)])
     (syntax (mcr2 five)))
   'testing 10))
(define se (expand-once s))

(test (syntax-e (cadr (syntax-e s))) syntax-e se)

(test 10 syntax-property s 'testing)
(test '(12 . 10) syntax-property se 'testing)
(test '(mcr2) (tree-map syntax-e) (syntax-property se 'origin))

(test #t syntax-original? s)
(test #t syntax-original? se)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paren-shape:

(let ([s (with-syntax ([a (quote-syntax [x y])])
           #'[a 10])])
  (test #f syntax-property #'(x) 'paren-shape)
  (test #\[ syntax-property #'[x] 'paren-shape)
  (test #\[ syntax-property s 'paren-shape)
  (test #\[ syntax-property (syntax-case s () [(b _) #'b]) 'paren-shape))

(let ([s (with-syntax ([(a ...) '(1 2 3)])
           #'[a ...])])
  (test #\[ syntax-property s 'paren-shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two-step macro chain

(define-syntax mcr5
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) (syntax x)])))

(define s (quote-syntax (mcr5 (mcr2 5))))
(define se (expand-once (expand-once s)))

(test (syntax-e (cadr (syntax-e (cadr (syntax-e s))))) syntax-e se)

(test '(mcr2 mcr5)
      (tree-map syntax-e)
      (syntax-property se 'origin))

(test #t syntax-original? s)
(test #t syntax-original? se)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two-step macro chain with expansion

(define-syntax mcr7
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) (local-expand (syntax x) '(internal-define) (list (quote-syntax #%datum)))])))

(define s (quote-syntax (mcr7 (mcr2 5))))
(define se (expand-once s))

(test (syntax-e (cadr (syntax-e (cadr (syntax-e s))))) syntax-e se)

(test '((mcr2) mcr7)
      (tree-map syntax-e)
      (syntax-property se 'origin))

(test #t syntax-original? s)
(test #t syntax-original? se)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Three-step macro chain, with one expansion

(define s (quote-syntax (mcr5 (mcr7 (mcr2 5)))))
(define se (expand-once (expand-once s)))

(test '((mcr2) mcr7 mcr5)
      (tree-map syntax-e)
      (syntax-property se 'origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Three-step macro chain, with other expansion

(define s (quote-syntax (mcr7 (mcr5 (mcr2 5)))))
(define se (expand-once s))

(test '((mcr2 mcr5) mcr7)
      (tree-map syntax-e)
      (syntax-property se 'origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure `let-syntax` (which involves a rename transformer)
;; attaches the right 'origin

(test #t
      syntax-original?
      (let ([stx (expand #'(let-syntax ([m (lambda (stx) #''m)])
                             m))])
        (syntax-case stx ()
          [(_ () (_ () e)) (car (syntax-property #'e 'origin))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check 'origin tracing for a set!-transformer

(test #t
      'has-do-not-forget-me-origin?
      (let ([stx (expand #'(let-syntax ([do-not-forget-me (make-set!-transformer
                                                           (lambda (stx)
                                                             #'10))])
                             (set! do-not-forget-me 5)))])
        (let loop ([v stx])
          (cond
            [(syntax? v)
             (or (loop (syntax-property v 'origin))
                 (loop (syntax-e v)))]
            [(pair? v) (or (loop (car v))
                           (loop (cdr v)))]
            [(eq? v 'do-not-forget-me) #t]
            [else #f]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that 'origin has the right source location,
;; and that it doesn't have excessive properties

(let ()
  (define m #`(module m racket/base
                (require (for-syntax racket/base))
                (let ()
                  #,(syntax-property #`(define-values (x y) (values 1 2))
                                     'on-form
                                     'dv)
                  x)))
  (define e (expand m))
  (define dv-src
    (let loop ([m m])
      (cond
        [(syntax? m)
         (or (and (eq? (syntax-e m) 'define-values)
                  m)
             (loop (syntax-e m)))]
        [(pair? m) (or (loop (car m)) (loop (cdr m)))]
        [else #f])))
  (define dv-origin
    (let loop ([e e])
      (cond
        [(syntax? e)
         (define p (syntax-property e 'origin))
         (or (let loop ([p p])
               (cond
                 [(and (identifier? p)
                       (eq? (syntax-e p) 'define-values))
                  p]
                 [(pair? p) (or (loop (car p)) (loop (cdr p)))]
                 [else #f]))
             (loop (syntax-e e)))]
        [(pair? e) (or (loop (car e)) (loop (cdr e)))]
        [else #f])))
  (test (list (syntax-line dv-src)
              (syntax-column dv-src)
              (syntax-span dv-src))
        list
        (syntax-line dv-origin)
        (syntax-column dv-origin)
        (syntax-span dv-origin))
  (test #f syntax-property dv-origin 'on-form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check property tracking on `let[rec]-values` binding clauses

(let ([mk-e (lambda (bind)
              #`(let-syntax ([m (lambda (stx)
                                  (syntax-case stx ()
                                    [(_ e) (local-expand #'e 'expression '())]))])
                  (m (#,bind (#,(syntax-property #'[(x) 0] 'keep-me #t)) 1))))])
  (define (find-keep-me? s)
    (cond
      [(syntax? s) (or (syntax-property s 'keep-me)
                       (find-keep-me? (syntax-e s)))]
      [(pair? s) (or (find-keep-me? (car s))
                     (find-keep-me? (cdr s)))]
      [else #f]))
  (test #t find-keep-me? (expand (mk-e #'let-values)))
  (test #t find-keep-me? (expand (mk-e #'letrec-values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure a language name via `#lang` is original

(parameterize ([read-accept-reader #t])
  (syntax-case (read-syntax 'hi (open-input-string "#lang racket/base 10")) ()
    [(_ _ lang . _)
     (test #t syntax-original? #'lang)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #%app, etc.

(define s (syntax-property (quote-syntax (add1 5)) 'testing 10))
(test 10 syntax-property (expand s) 'testing)

(define s (syntax-property (quote-syntax 5) 'testing 10))
(test 10 syntax-property (expand s) 'testing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check tracking of (formerly) primitive expanders

(test '(let) (tree-map syntax-e) (syntax-property (expand #'(let ([x 10]) x)) 'origin))
(test '((let*) let*-values let*) (tree-map syntax-e) (syntax-property (expand #'(let* ([x 10]) x)) 'origin))
(test '(let) (tree-map syntax-e) (syntax-property (expand #'(let loop ([x 10]) x)) 'origin))
(test '(letrec) (tree-map syntax-e) (syntax-property (expand #'(letrec ([x 10]) x)) 'origin))
(test '(let*-values) (tree-map syntax-e) (syntax-property (expand #'(let*-values ([(x) 10]) x)) 'origin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol Keys
(test null syntax-property-symbol-keys #'a)
(let ([ssort (lambda (l)
               (if (equal? l '(yep aha))
                   '(aha yep)
                   l))])
  (test '(aha) syntax-property-symbol-keys (syntax-property #'a 'aha 1))
  (test '(aha yep) ssort (syntax-property-symbol-keys (syntax-property (syntax-property #'a 'aha 1) 'yep 2)))
  (test '(aha yep) ssort (syntax-property-symbol-keys
                          (syntax-property
                           (syntax-property
                            (syntax-property #'a 'aha 1)
                            'yep 2)
                           'aha 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test free-identifier=? on different phases via syntax-case*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module mta racket/base
  (define mtax 10)
  (provide mtax))

(module mtb racket/base
  (define mtby 10)
  (provide mtby))

(module mt1 racket/base
  (require (prefix-in a: 'mta))
  (require (for-syntax (prefix-in b: 'mtb)
                       racket/base))
  (require (prefix-in mz: racket/base))

  (define-syntax ck
    (lambda (stx)
      (syntax-case stx ()
        [(_ id et?)
         (with-syntax ([cmp (if (syntax-e (syntax et?))
                                (syntax free-transformer-identifier=?)
                                (syntax free-identifier=?))])
           (syntax
            (lambda (x)
              (syntax-case* x (id) cmp
                [(_ id) #t]
                [else #f]))))])))

  (define has-lam? (ck case-lambda #f))
  (define has-mz:lam? (ck mz:case-lambda #f))
  (define has-mtax? (ck a:mtax #f))
  (define has-mtby? (ck b:mtby #f))

  (define has-et-lam? (ck case-lambda #t))
  (define has-et-mz:lam? (ck mz:case-lambda #t))
  (define has-et-mtax? (ck a:mtax #t))
  (define has-et-mtby? (ck b:mtby #t))

  (provide has-lam? has-mz:lam? has-mtax? has-mtby?
           has-et-lam? has-et-mz:lam? has-et-mtax? has-et-mtby?))

(require 'mt1)
(require (for-syntax 'mtb))

(test #t has-lam? #'(any case-lambda))
(test #f has-lam? #'(any case-lambada))

(test #t has-et-lam? #'(any case-lambda))
(test #f has-et-lam? #'(any case-lambada))

;; mz: prefix is there in normal environment:
(test #t has-mz:lam? #'(any case-lambda))
(test #f has-et-mz:lam? #'(any case-lambda))
(test #f has-mz:lam? #'(any mz:case-lambda))
(test #t has-et-mz:lam? #'(any mz:case-lambda))

;; No mtax anywhere:
(test #f has-mtax? #'(any mtax))
(test #f has-mtax? #'(any a:mtax))
(test #f has-et-mtax? #'(any mtax))
(test #t has-et-mtax? #'(any a:mtax))

;; mtby (without prefix) in trans env
(test #f has-mtby? #'(any mtby))
(test #t has-mtby? #'(any b:mtby))
(test #t has-et-mtby? #'(any mtby))
(test #f has-et-mtby? #'(any b:mtby))

(module mt2 '#%kernel
  (#%require (for-syntax '#%kernel))
  (#%require 'mt1)
  (#%require 'mta)

  ;; For #':
  (define-syntaxes (syntax)
    (lambda (stx)
      (datum->syntax
       stx
       (cons
        (quote-syntax quote-syntax)
        (cdr (syntax-e stx)))
       stx)))

  (define-values (run-mt2-test)
    (lambda (test)

      (test #t has-lam? #'(any case-lambda))
      (test #f has-lam? #'(any case-lambada))

      (test #t has-et-lam? #'(any case-lambda))
      (test #f has-et-lam? #'(any case-lambada))

      ;; mz: prefix is there in normal environment:
      (test #t has-mz:lam? #'(any case-lambda))
      (test #f has-et-mz:lam? #'(any case-lambda))
      (test #f has-mz:lam? #'(any mz:case-lambda))
      (test #t has-et-mz:lam? #'(any mz:case-lambda))

      ;; mtax in both places normal env:
      (test #t has-mtax? #'(any mtax))
      (test #f has-mtax? #'(any a:mtax))
      (test #f has-et-mtax? #'(any mtax))
      (test #t has-et-mtax? #'(any a:mtax))

      ;; no mtby here
      (test #f has-mtby? #'(any mtby))
      (test #t has-mtby? #'(any b:mtby))
      (test #f has-et-mtby? #'(any mtby))
      (test #f has-et-mtby? #'(any b:mtby))))

  (#%provide run-mt2-test))

(require 'mt2)
(run-mt2-test test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test '(1 2 3) syntax->datum (syntax (1 2 3)))
(test '(1 ... 2 3) syntax->datum (syntax (... (1 ... 2 3))))

(syntax-test #'(syntax (a (... ...))))
(syntax-test #'(syntax (... ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; identifier-binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identifier-binding* s)
  (let ([b (identifier-binding s)])
    (if (list? b)
        (list* (let-values ([(name base) (module-path-index-split (car b))])
                 ;(fprintf (current-error-port) ">>>>base = ~s\n" base)
                 name)
               (cadr b)
               (let-values ([(name base) (module-path-index-split (caddr b))])
                 name)
               (cdddr b))
        b)))

(define base-lib (caddr (identifier-binding* #'lambda)))

(test `('#%core case-lambda ,base-lib case-lambda 0 0 0)
      identifier-binding* #'case-lambda)
(test `("private/promise.rkt" delay* ,base-lib delay 0 0 0)
      identifier-binding* #'delay)
(test `('#%core #%module-begin ,base-lib #%plain-module-begin 0 0 0)
      identifier-binding* #'#%plain-module-begin)
(require (only-in racket/base [#%plain-module-begin #%pmb]))
(test '('#%core #%module-begin racket/base #%plain-module-begin 0 0 0)
      identifier-binding* #'#%pmb)

(let ([b (identifier-binding
          (syntax-case (expand #'(module m racket/base
                                   (require (only-in racket/base [make-base-namespace s-mbn]))
                                   s-mbn)) ()
            [(mod m mz (#%mod-beg run-conf req (app call-with-values (lambda () make-base-namespace) print)))
             (let ([s (syntax make-base-namespace)])
               (test 's-mbn syntax-e s)
               s)]))])
  (let-values ([(real real-base) (module-path-index-split (car b))]
               [(nominal nominal-base) (module-path-index-split (caddr b))])
    (test '"namespace.rkt" values real)
    (test 'make-base-namespace cadr b)
    (test 'racket/base values nominal)
    (test 'make-base-namespace cadddr b)))

(let ([b (identifier-binding
          (syntax-case (expand #'(module m racket/base
                                   make-base-namespace)) ()
            [(mod m beg (#%mod-beg run-conf (app call-w-vals (lam () make-base-namespace) prnt)))
             (let ([s (syntax make-base-namespace)])
               (test 'make-base-namespace syntax-e s)
               s)]))])
  (let-values ([(real real-base) (module-path-index-split (car b))]
               [(nominal nominal-base) (module-path-index-split (caddr b))])
    (test '"namespace.rkt" values real)
    (test 'make-base-namespace cadr b)
    (test 'racket/base values nominal)
    (test 'make-base-namespace cadddr b)))

(let ()
  (define (check wrap)
    (test #f identifier-binding (wrap (datum->syntax #f 'lambda)))
    (test #f identifier-template-binding (wrap #'lambda))
    (test (identifier-binding #'lambda) identifier-template-binding (wrap (syntax-shift-phase-level #'lambda -1)))
    (test #f identifier-label-binding (wrap #'lambda))
    (test (identifier-binding #'lambda) identifier-label-binding (wrap (syntax-shift-phase-level #'lambda #f)))
    (test #f identifier-binding (wrap (syntax-shift-phase-level #'lambda #f)))
    (test #f identifier-template-binding (wrap (syntax-shift-phase-level #'lambda #f))))
  (check values)
  (check (lambda (s)
           (define-values (i o) (make-pipe))
           (write (compile-syntax #`(quote-syntax #,s)) o)
           (parameterize ([read-accept-compiled #t])
             (eval (read i))))))

(module x-with-identifier-binding-of-alt racket/base
  (define x 1)
  (define-syntax-rule (m id)
    (begin
      (define x 5)
      (define id #'x)))
  (m x-id)
  (provide x-id))
(let ([b (identifier-binding
          (dynamic-require ''x-with-identifier-binding-of-alt 'x-id))])
  (test #f eq? 'x (cadr b))
  (test 'x cadddr b)
  (test #t equal? (car b) (caddr b)))

;; Top-level bindings:
(test #f identifier-binding #'test 0)
(test #f identifier-binding #'test 0 #f)
(test '(test) identifier-binding #'test 0 #t)
(test '#f identifier-binding #'this-identifier-is-never-defined 0 #t)

(define-syntax-rule (introduce-a-definition-of-x bind-id)
  (begin
    (define x 10)
    (define bind-id (identifier-binding #'x 0 #t))))
(introduce-a-definition-of-x sym-list-for-x)
(test #t pair? sym-list-for-x)
(test #t symbol? (car sym-list-for-x))
(test #f eq? 'x (car sym-list-for-x)) ; since macro-introduced

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; identifier-binding and (nominal) phase reporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ib-mod-1 racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base)))
  (define extra #f)
  (provide extra)
  
  (define x-1-0 0)
  (provide x-1-0)
  
  (begin-for-syntax
    (define x-1-1 1)
    (provide x-1-1)
    
    (begin-for-syntax
      (define x-1-2 2)
      (provide x-1-2))))

(module ib-mod-2 racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base))
           'ib-mod-1)
  
  (define x-1-0-b (identifier-binding #'x-1-0))
  (define x-1-0-b+1 (identifier-transformer-binding (syntax-shift-phase-level #'x-1-0 1)))
  (define x-1-0-b+f (identifier-label-binding (syntax-shift-phase-level #'x-1-0 #f)))
  (define x-1-1-b (identifier-transformer-binding #'x-1-1))
  (define x-1-1-b+f (identifier-label-binding (syntax-shift-phase-level #'x-1-1 #f)))
  (define x-1-2-b (identifier-binding #'x-1-2 2))
  (provide x-1-0-b
           x-1-0-b+1
           x-1-0-b+f
           x-1-1-b
           x-1-1-b+f
           x-1-2-b))

(module ib-mod-2b racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base))
           (only-in 'ib-mod-1
                    x-1-1
                    x-1-0
                    x-1-2))
  
  (define x-1-0-b2 (identifier-binding #'x-1-0))
  (define x-1-0-b2+1 (identifier-transformer-binding (syntax-shift-phase-level #'x-1-0 1)))
  (define x-1-0-b2+f (identifier-label-binding (syntax-shift-phase-level #'x-1-0 #f)))
  (define x-1-1-b2 (identifier-transformer-binding #'x-1-1))
  (define x-1-2-b2 (identifier-binding #'x-1-2 2))
  (provide x-1-0-b2
           x-1-0-b2+1
           x-1-0-b2+f
           x-1-1-b2
           x-1-2-b2))

(module ib-mod-3 racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base))
           (for-template 'ib-mod-1))
  (provide (for-template x-1-0)
           x-1-1
           (for-syntax x-1-2)
           extra2)
  
  (define extra2 #f)
  
  (define x-1-0-b3 (identifier-template-binding #'x-1-0))
  (define x-1-1-b3 (identifier-binding #'x-1-1))
  (define x-1-2-b3 (identifier-transformer-binding #'x-1-2))
  (provide x-1-0-b3
           x-1-1-b3
           x-1-2-b3))

(module ib-mod-4 racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base))
           'ib-mod-3)
  
  (define x-1-0-b4 (identifier-template-binding #'x-1-0))
  (define x-1-1-b4 (identifier-binding #'x-1-1))
  (define x-1-2-b4 (identifier-transformer-binding #'x-1-2))
  (provide x-1-0-b4
           x-1-1-b4
           x-1-2-b4))

(module ib-mod-5 racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base))
           (for-syntax 'ib-mod-3))
  
  (define x-1-0-b5 (identifier-binding #'x-1-0))
  (define x-1-1-b5 (identifier-transformer-binding #'x-1-1))
  (define x-1-2-b5 (identifier-binding #'x-1-2 2))
  (provide x-1-0-b5
           x-1-1-b5
           x-1-2-b5))

(module ib-mod-5b racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base))
           (for-syntax (only-in 'ib-mod-3
                                x-1-1
                                x-1-0
                                x-1-2)))
  
  (define x-1-0-b6 (identifier-binding #'x-1-0))
  (define x-1-1-b6 (identifier-transformer-binding #'x-1-1))
  (define x-1-2-b6 (identifier-binding #'x-1-2 2))
  (provide x-1-0-b6
           x-1-1-b6
           x-1-2-b6))

(module ib-mod-7 racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base))
           (for-label 'ib-mod-1))
  
  (define x-1-0-b7 (identifier-label-binding #'x-1-0))
  (define x-1-1-b7 (identifier-label-binding #'x-1-1))
  (define x-1-2-b7 (identifier-label-binding #'x-1-2))
  (provide x-1-0-b7
           x-1-1-b7
           x-1-2-b7))

(require 'ib-mod-2
         'ib-mod-2b
         'ib-mod-3
         'ib-mod-4
         'ib-mod-5
         'ib-mod-5b
         'ib-mod-7)

(define (simplify l)
  (and l
       (for/list ([v (in-list l)])
         (if (module-path-index? v)
             (let-values ([(name base) (module-path-index-split v)])
               (cadr name))
             v))))

(test '(ib-mod-1 x-1-0 ib-mod-1 x-1-0 0 0 0) simplify x-1-0-b)
(test '(ib-mod-1 x-1-0 ib-mod-1 x-1-0 0 0 0) simplify x-1-0-b+1)
(test '(ib-mod-1 x-1-0 ib-mod-1 x-1-0 0 0 0) simplify x-1-0-b+f)
(test '(ib-mod-1 x-1-0 ib-mod-1 x-1-0 0 0 0) simplify x-1-0-b2)
(test '(ib-mod-1 x-1-0 ib-mod-1 x-1-0 0 0 0) simplify x-1-0-b2+1)
(test '(ib-mod-1 x-1-0 ib-mod-1 x-1-0 0 0 0) simplify x-1-0-b2+f)
(test '(ib-mod-1 x-1-0 ib-mod-1 x-1-0 0 -1 0) simplify x-1-0-b3)
(test '(ib-mod-1 x-1-0 ib-mod-3 x-1-0 0 0 -1) simplify x-1-0-b4)
(test '(ib-mod-1 x-1-0 ib-mod-3 x-1-0 0 1 -1) simplify x-1-0-b5)
(test '(ib-mod-1 x-1-0 ib-mod-3 x-1-0 0 1 -1) simplify x-1-0-b6)
(test '(ib-mod-1 x-1-0 ib-mod-1 x-1-0 0 #f 0) simplify x-1-0-b7)

(test '(ib-mod-1 x-1-1 ib-mod-1 x-1-1 1 0 1) simplify x-1-1-b)
(test '#f                                    simplify x-1-1-b+f)
(test '(ib-mod-1 x-1-1 ib-mod-1 x-1-1 1 0 1) simplify x-1-1-b2)
(test '(ib-mod-1 x-1-1 ib-mod-1 x-1-1 1 -1 1) simplify x-1-1-b3)
(test '(ib-mod-1 x-1-1 ib-mod-3 x-1-1 1 0 0) simplify x-1-1-b4)
(test '(ib-mod-1 x-1-1 ib-mod-3 x-1-1 1 1 0) simplify x-1-1-b5)
(test '(ib-mod-1 x-1-1 ib-mod-3 x-1-1 1 1 0) simplify x-1-1-b6)
(test '(ib-mod-1 x-1-1 ib-mod-1 x-1-1 1 #f 1) simplify x-1-1-b7)

(test '(ib-mod-1 x-1-2 ib-mod-1 x-1-2 2 0 2) simplify x-1-2-b)
(test '(ib-mod-1 x-1-2 ib-mod-1 x-1-2 2 0 2) simplify x-1-2-b2)
(test '(ib-mod-1 x-1-2 ib-mod-1 x-1-2 2 -1 2) simplify x-1-2-b3)
(test '(ib-mod-1 x-1-2 ib-mod-3 x-1-2 2 0 1) simplify x-1-2-b4)
(test '(ib-mod-1 x-1-2 ib-mod-3 x-1-2 2 1 1) simplify x-1-2-b5)
(test '(ib-mod-1 x-1-2 ib-mod-3 x-1-2 2 1 1) simplify x-1-2-b6)
(test '(ib-mod-1 x-1-2 ib-mod-1 x-1-2 2 #f 2) simplify x-1-2-b7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval versus eval-syntax, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless building-flat-tests?

  (test eval eval 'eval)
  (test eval eval eval)
  (test eval eval #'eval)
  (test eval eval (datum->syntax #f 'eval))

  (err/rt-test (eval-syntax 'eval))
  (err/rt-test (eval-syntax eval))
  (test eval eval-syntax #'eval)
  (test #t
        'eval-syntax
        (with-handlers ([exn:fail:syntax? (lambda (x) #t)])
          (eval-syntax (datum->syntax #f 'eval))))

  (test eval (current-eval) 'eval)
  (test eval (current-eval) eval)
  (test eval (current-eval) #'eval)
  (test #t
        'current-eval-syntax
        (with-handlers ([exn:fail:syntax? (lambda (x) #t)])
          ((current-eval) (datum->syntax #f 'eval))))

  (test eval 'compile (eval (compile 'eval)))
  (when (eq? 'racket (system-type 'vm))
    (test eval 'compile (eval (compile eval))))
  (test eval 'compile (eval (compile #'eval)))
  (test eval 'compile (eval (compile (datum->syntax #f 'eval))))

  (err/rt-test (compile-syntax 'eval))
  (err/rt-test (compile-syntax eval))
  (test eval 'compile (eval (compile-syntax #'eval)))
  (test #t
        'compile-syntax
        (with-handlers ([exn:fail:syntax? (lambda (x) #t)])
          (compile-syntax (datum->syntax #f 'eval))))

  (test eval 'expand (eval (expand 'eval)))
  (test eval 'expand (eval (expand eval)))
  (test eval 'expand (eval (expand #'eval)))
  (test eval 'expand (eval (expand (datum->syntax #f 'eval))))

  (err/rt-test (expand-syntax 'eval))
  (err/rt-test (expand-syntax eval))
  (test eval 'expand (eval (expand-syntax #'eval)))
  (test #t
        'expand-syntax
        (with-handlers ([exn:fail:syntax? (lambda (x) #t)])
          (expand-syntax (datum->syntax #f 'eval))))

  (test eval 'expand-once (eval (expand-once 'eval)))
  (test eval 'expand-once (eval (expand-once eval)))
  (test eval 'expand-once (eval (expand-once #'eval)))
  (test eval 'expand-once (eval (expand-once (datum->syntax #f 'eval))))

  (err/rt-test (expand-syntax-once 'eval))
  (err/rt-test (expand-syntax-once eval))
  (test eval 'expand-once (eval (expand-syntax-once #'eval)))
  (test #t
        'expand-syntax-once
        (with-handlers ([exn:fail:syntax? (lambda (x) #t)])
          (expand-syntax-once (datum->syntax #f 'eval))))

  (test eval 'expand-to-top-form (eval (expand-to-top-form 'eval)))
  (test eval 'expand-to-top-form (eval (expand-to-top-form eval)))
  (test eval 'expand-to-top-form (eval (expand-to-top-form #'eval)))
  (test eval 'expand-to-top-form (eval (expand-to-top-form (datum->syntax #f 'eval))))

  (err/rt-test (expand-syntax-to-top-form 'eval))
  (err/rt-test (expand-syntax-to-top-form eval))
  (test eval 'expand-to-top-form (eval (expand-syntax-to-top-form #'eval)))
  (test #t syntax? (expand-syntax-to-top-form (datum->syntax #f 'eval))))

(define-syntax @$@name 'dummy)
(define-syntax @$@alias (make-rename-transformer #'@$@name))
(test (identifier-binding-symbol #'@$@name)
      identifier-binding-symbol #'@$@alias)

(require (only-in racket/base [add1 increment-by-one]))
(test (identifier-binding-symbol #'add1)
      identifier-binding-symbol #'increment-by-one)

(define top-level-add1 add1)
(define-syntax top-level-increment-by-one (make-rename-transformer #'top-level-add1))
(test (identifier-binding-symbol #'top-level-add1)
      identifier-binding-symbol #'top-level-increment-by-one)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; origin tracking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Checks whether stx includes an mapping for
;;  a `where' form (indicated by a symbol) going back to
;;  a `what' form (another symbol)
;; If `where' is #f, look for the annotation on a let...-values
;;  binding clause
(define (has-stx-property? stx where what prop)
  (define (has-p? stx)
    (let ([p (syntax-property stx prop)])
      (and p
           (let loop ([p p])
             (cond
              [(pair? p) (or (loop (car p))
                             (loop (cdr p)))]
              [else (and (identifier? p)
                         (eq? what (syntax-e p)))])))))

  (let loop ([stx stx])
    (or (and (has-p? stx)
             (or (eq? #t where)
                 (eq? (syntax-e stx) where)
                 (and (pair? (syntax-e stx))
                      (eq? (syntax-e (car (syntax-e stx)))
                           where))))
        (syntax-case stx (#%plain-lambda case-lambda begin begin0
                                         set! with-continuation-mark
                                         if #%plain-app module #%plain-module-begin
                                         define-values)
          [(#%plain-lambda formals expr ...)
           (ormap loop (syntax->list #'(expr ...)))]
          [(case-lambda [formals expr ...] ...)
           (ormap (lambda (l)
                    (ormap loop (syntax->list l)))
                  (syntax->list #'((expr ...) ...)))]
          [(let ([(id ...) rhs] ...) expr ...)
           (or (free-identifier=? #'let #'let-values)
               (free-identifier=? #'let #'letrec-values))
           (or (and (boolean? where)
                    (syntax-case stx ()
                      [(let [clause ...] expr)
                       (ormap has-p? (syntax->list #'(clause ...)))]))
               (ormap loop (syntax->list #'(expr ...)))
               (ormap loop (syntax->list #'(rhs ...))))]
          [(begin expr ...)
           (ormap loop (syntax->list #'(expr ...)))]
          [(begin0 expr ...)
           (ormap loop (syntax->list #'(expr ...)))]
          [(set! id expr)
           (loop #'expr)]
          [(with-continuation-mark key val expr)
           (or (loop #'key) (loop #'val) (loop #'expr))]
          [(if test then else)
           (or (loop #'test) (loop #'then) (loop #'else))]
          [(#%plain-app expr ...)
           (ormap loop (syntax->list #'(expr ...)))]
          [(module name init body)
           (loop #'body)]
          [(#%plain-module-begin expr ...)
           (ormap loop (syntax->list #'(expr ...)))]
          [(define-values (id ...) expr)
           (loop #'expr)]
          [_ #f]))))

(test #t has-stx-property? (expand #'(let ([x 1]) 2)) 'let-values 'let 'origin)

;; The define-struct macro expands to begin,
(test #t has-stx-property? (expand #'(define-struct x (a))) 'begin 'define-struct 'origin)
(test #t has-stx-property? (expand #'(module m racket/base (define-struct x (a)))) 'define-values 'define-struct 'origin)
(test #t has-stx-property? (expand #'(module m racket/base (define-struct x (a)))) 'define-syntaxes 'define-struct 'origin)

;; The s macro also expands to begin:
(test #t has-stx-property? (expand #'(module m racket/base
                                       (require (for-syntax racket/base))
                                       (define-syntax (s stx)
                                         #'(begin
                                             (+ 1 10)
                                             14))
                                       s))
      '#%app 's 'origin)
(test #t has-stx-property? (expand #'(module m racket/base
                                       (require (for-syntax racket/base))
                                       (define-syntax (s stx)
                                         #'(begin
                                             (+ 1 10)
                                             14))
                                       (let ()
                                         s)))
      '#%app 's 'origin)

;; Check per-clause origin from internal-defn conversion
(test #t has-stx-property? (expand #'(let () (define x 1) x)) #f 'define 'origin)
(test #t has-stx-property? (expand #'(let () (define-struct x (a)) 12)) #f 'define-struct 'origin)

;; Disappearing syntax decls:
(test #t has-stx-property? (expand #'(let () (define-syntax x 1) (define y 12) 10)) 'let-values 'x 'disappeared-binding)
(test #t has-stx-property? (expand #'(let () (define-syntax x 1) (define y y) 10)) 'letrec-values 'x 'disappeared-binding)
(test #t has-stx-property? (expand #'(let () (define-struct s (x)) 10)) 'let-values 's 'disappeared-binding)
(test #t has-stx-property? (expand #'(let () (define-syntax x 1) 10)) 'let-values 'x 'disappeared-binding)
(test #f has-stx-property? (expand #'(fluid-let-syntax ([x 1]) 10)) 'let-values 'x 'disappeared-binding)

;; Disappearing use:
(test #t has-stx-property? (expand #'(let () (define-struct a (x)) (define-struct (b a) (z)) 10))
      #f 'a 'disappeared-use)

;; Check that origin is bound by disappeared binding:
(test #t has-stx-property? (expand #'(let () (define-syntax (x stx) #'(quote y)) x)) 'quote 'x 'origin)
(let ([check-expr
       (lambda (expr)
         (let ([e (expand expr)])
           (syntax-case e ()
             [(lv (bind ...) beg)
              (let ([db (syntax-property #'beg 'disappeared-binding)])
                (let-values ([(bg e)
                              (syntax-case #'beg (#%plain-app list)
                                [(bg () (#%plain-app list e))
                                 (values #'bg #'e)]
                                [(bg () e)
                                 (values #'bg #'e)])])
                  (let ([o (syntax-property e 'origin)])
                    (test #t (lambda (db o)
                               (and (list? db)
                                    (list? o)
                                    (<= 1 (length db) 2)
                                    (= 1 (length o))
                                    (andmap identifier? db)
                                    (identifier? (car o))
                                    (ormap (lambda (db) (free-identifier=? db (car o))) db)))
                          db o))))])))])
  (check-expr #'(let () (letrec-syntaxes+values ([(x) (lambda (stx) #'(quote y))]) () x)))
  (check-expr #'(let () (letrec-syntaxes+values ([(x) (lambda (stx) #'(quote y))]) () (list x))))
  (check-expr #'(let-values () (define-syntax (x stx) #'(quote y)) x))
  (check-expr #'(let-values () (define-syntax (x stx) #'(quote y)) (list x)))
  (check-expr #'(let-values ([(y) 2]) (define-syntax (x stx) #'(quote y)) x))
  (check-expr #'(let-values ([(y) 2]) (define-syntax (x stx) #'(quote y)) (list x)))
  (check-expr #'(let () (define-syntax (x stx) #'(quote y)) x))
  (check-expr #'(let () (define-syntax (x stx) #'(quote y)) (list x)))
  (check-expr #'(let ([z 45]) (define-syntax (x stx) #'(quote y)) x))
  (check-expr #'(let ([z 45]) (define-syntax (x stx) #'(quote y)) (list x))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protected identifiers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ++p racket/base
  (require (for-syntax racket/base))
  (define ++c 12)
  (define-syntax (++goo stx) (syntax-protect #'++c))
  (provide ++goo))
(module ++q racket/base
  (require (for-syntax '++p
                       racket/base))
  (define ++d 11)
  (define-syntax (++o stx) (syntax-protect #'++d))
  (define-syntax (++s stx)
    (syntax-case stx ()
      [(_ id) (syntax-protect
               #'(define-syntax (id stx)
                   (datum->syntax #'here (++goo))))]))
  (define-syntax (++t stx) (syntax-case stx () [(_ id) (syntax-protect #'(define-values (id) ++d))]))
  (define-syntax (++t2 stx) (syntax-protect #'(begin ++d)))
  (define-syntax (++t3 stx) (syntax-protect (syntax-property #'(begin0 ++d) 'certify-mode 'transparent)))
  (define-syntax (++t4 stx) (syntax-case stx () [(_ id) (syntax-protect #'(define id ++d))]))
  (define-syntax (++v stx) (syntax-protect #'(begin0 ++d)))
  (define-syntax (++v2 stx) (syntax-protect #'(++d)))
  (define-syntax (++v3 stx) (syntax-protect (syntax-property #'(begin ++d) 'certify-mode 'opaque)))
  (define-syntax ++ds 17)
  (define-syntax (++check-val stx)
    (syntax-case stx ()
      [(_ id) (syntax-protect (datum->syntax #'here (add1 (syntax-local-value #'id))))]))
  (define-syntax (++o2 stx) (syntax-protect #'(++check-val ++ds)))
  (define-syntax (++apply-to-ds stx)
    (syntax-case stx ()
      [(_ id) (syntax-protect #'(id ++ds))]))
  (define-syntax (++apply-to-d stx)
    (syntax-case stx ()
      [(_ id) (syntax-protect #'(id ++d))]))
  (provide ++o ++o2 ++s ++t ++t2 ++t3 ++t4 ++v ++v2 ++v3
           ++apply-to-d ++apply-to-ds))

(require '++q)
(++s ++ack)
(test 12 values ++ack)
(test 11 values ++v)
(test 11 values ++o)
(test 18 values ++o2)
(test 13 values (let () (++t id) 13))

(let-syntax ([goo (lambda (stx)
                    (syntax-case stx ()
                      [(_ id) (datum->syntax #'here (sub1 (syntax-local-value #'id)))]))])
  (test 16 'goo (++apply-to-ds goo)))

(unless building-flat-tests?
  (test 11 eval-syntax (expand-syntax #'++o))

  (test 11 eval-syntax (syntax-case (expand-syntax #'++t2) ()
                         [(_ x) #'x]))
  (test 11 eval-syntax (syntax-case (expand #'(++t z)) ()
                         [(d-v (_) x) #'x]))
  (test 11 eval-syntax (syntax-case (expand-syntax #'++t3) ()
                         [(_ x) #'x]))
  (test 11 eval-syntax (syntax-case (expand #'(++t4 z)) ()
                         [(d-v (_) x) #'x]))

  (err/rt-test (teval (syntax-case (expand #'++v) ()
                        [(_ x) #'x]))
               exn:fail:syntax?)
  (err/rt-test (teval (syntax-case (expand #'++v2) ()
                        [(_ x) #'x]))
               exn:fail:syntax?)
  (err/rt-test (teval (syntax-case (expand #'++v3) ()
                        [(_ x) #'x]))
               exn:fail:syntax?))

(let ()
  (define (test-disarm disarm)
    (let ([expr (expand-syntax #'++v)])
      (test expr syntax-protect expr)
      (let ([new (syntax-protect #'no-marks)])
        (test #t syntax? new)
        (test 'no-marks syntax-e new))
      (test #t (lambda (v) (and (syntax? v) (syntax-tainted? v)))
            (syntax-case expr ()
              [(beg id) #'beg]))
      (test #t (lambda (v) (and (syntax? v) (not (syntax-tainted? v))))
            (syntax-case (disarm expr) ()
              [(beg id) #'beg]))
      (test #t (lambda (v) (and (syntax? v) (syntax-tainted? v)))
            (syntax-case (disarm (datum->syntax expr (syntax-e expr))) ()
              [(beg id) #'beg]))
      (test #t (lambda (v) (and (syntax? v) (not (syntax-tainted? v))))
            (syntax-case (let ([expr (disarm expr)]) (datum->syntax expr (syntax-e expr))) ()
              [(beg id) #'beg]))))
  (test-disarm (lambda (stx)
                 (syntax-disarm stx (current-code-inspector))))
  (test-disarm (lambda (stx)
                 (syntax-disarm stx #f))))

#;
(let ([expr (expand-syntax #'(++apply-to-d ack))])
  (test '(#%app (#%top . ack) ++d) syntax->datum expr)
  (let ([try (lambda (cvt? other)
               (syntax-recertify (datum->syntax
                                  expr
                                  (cons (car (syntax-e expr))
                                        ((if cvt?
                                             (lambda (x) (datum->syntax
                                                          (cdr (syntax-e expr))
                                                          x))
                                             values)
                                         (cons
                                          other
                                          (cdr (syntax-e (cdr (syntax-e expr))))))))
                                 expr
                                 (current-inspector)
                                 #f))])
    (test #t syntax? (try #f #'other!))
    (let ([new (try #t #'other!)])
      (test #t syntax? new)
      (test '(#%app other! ++d) syntax->datum new))
    ;; we'd prefer this to fail, but it's defined to succeed:
    (test #t syntax? (try #t (syntax-case expr ()
                               [(ap _ d) #'d])))))


;; ----------------------------------------

(module ++m racket/base
  (require (for-syntax racket/base))
  (define ++x 10)
  (define-syntax (++xm stx) (syntax-protect #'100))
  (provide (protect-out ++x ++xm)))
(module ++n racket/base
  (require (for-syntax racket/base)
           '++m)
  (define ++y ++x)
  (define-syntax (++y-macro stx) (syntax-protect #'++x))
  (define-syntax (++y-macro2 stx) (syntax-protect (datum->syntax stx '++x)))
  (define-syntax (++u-macro stx) (syntax-protect #'++u))
  (define-syntax (++v-macro stx) (syntax-protect #'++v))
  (define-syntax ++u2 (make-rename-transformer (syntax-protect #'++u)))
  (define ++u 8) ; would be unexported, but export of rename transformer exports it
  (define ++v 9) ; unexported
  (provide ++y ++y-macro ++y-macro2 ++u-macro ++u2 ++v-macro))
(require '++n)

(test 10 values ++y)
(test 10 values ++y-macro)
(test 8 values ++u-macro)
(test 8 values ++u2)
(test 9 values ++v-macro)

(require '++m)

(test 10 values ++x)
(test 100 values ++xm)
(test 10 values ++y-macro2)

(let ()
  (define i (make-inspector))
  (define n (current-namespace))
  (define n2 (parameterize ([current-code-inspector i])
               (make-base-empty-namespace)))

  (parameterize ([current-namespace n2])
    (namespace-attach-module n ''++n))

  (parameterize ([current-code-inspector i]
                 [current-namespace n2])
    (namespace-require 'racket/base)
    (teval '(require '++n))

    (test 10 teval '++y)
    (test 10 teval '++y-macro)
    (test 8 teval '++u-macro)
    (test 8 teval '++u2)

    (err/rt-test (teval '++y-macro2) exn:fail:contract:variable?)
    (err/rt-test (teval '++x) exn:fail:contract:variable?)
    (err/rt-test (teval '++xm) exn:fail:contract:variable?)

    (teval '(require '++m))
    (err/rt-test (teval '++x) exn:fail:syntax?)
    (err/rt-test (teval '++xm) exn:fail:syntax?)
    (err/rt-test (teval '++y-macro2) exn:fail:syntax?)

    (teval '(module zrt racket/base
              (require '++n)
              (define (vy) ++y)
              (define (vy2) ++y-macro)
              (define (vu) ++u-macro)
              (define (vu2) ++u2)
              (provide vy vy2 vu vu2)))
    (teval '(module zct racket/base
              (require (for-syntax racket/base
                                   '++n))
              (define-syntax (wy stx) (datum->syntax #'here ++y))
              (let-syntax ([goo ++y-macro]) 10)
              (define-syntax (wy2 stx) (datum->syntax #'here ++y-macro))
              (define-syntax (wu stx) (datum->syntax #'here ++u-macro))
              (provide wy wy2 wu)))

    (teval '(require 'zct))

    (test 10 teval 'wy)
    (test 10 teval 'wy2)
    (test 8 teval 'wu)

    (teval '(require 'zrt))

    (test 10 teval '(vy))
    (test 10 teval '(vy2))
    (test 8 teval '(vu))
    (test 8 teval '(vu2)))

  (let ([old-insp (current-code-inspector)])
    (parameterize ([current-code-inspector i]
                   [current-namespace n2])
      (namespace-unprotect-module old-insp ''++m)))

  (parameterize ([current-code-inspector i]
                 [current-namespace n2])
    (test 10 teval '++y-macro)
    (test 10 teval '++y-macro2)))


(module ++/n racket/base
  (require (for-syntax racket/base))
  (provide ++/get-foo)
  (define-syntax foo #'10)
  (define-syntax (++/get-foo stx)
    (syntax-local-value #'foo)))
(require '++/n)
(test 10 values ++/get-foo)

(module ++//n racket/base
  (require (for-syntax racket/base))
  (provide ++//def)
  (define-syntax foo #'17)
  (define-syntax ++//def
    (syntax-rules ()
      [(_ get-foo)
       (define-syntax (get-foo stx)
         (syntax-local-value #'foo))])))
(require '++//n)
(++//def ++//get-foo)
(test 17 values ++//get-foo)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lifting expressions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (@@foo stx)
  (syntax-case stx ()
    [(_ n)
     (if (zero? (syntax-e #'n))
         #'(list #f 0)
         (with-syntax ([m (sub1 (syntax-e #'n))])
           #`(list '#,(syntax-local-lift-context)
                   #,(syntax-local-lift-expression #'(add1 (cadr (@@foo m)))))))]))

(define lifted-output #f)

(define-syntax (@@goo stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([id (syntax-local-lift-expression #'(set! lifted-output "lifted!"))])
       #'(list lifted-output id))]))

(let ([res (@@foo 2)])
  (test res '@@foo (@@foo 2))
  (test res eval-syntax #'(@@foo 2))
  (test res eval (expand-once #'(@@foo 2)))
  (test res eval (expand-syntax-once #'(@@foo 2)))
  (test res eval (expand #'(@@foo 2)))
  (test res eval (expand-syntax #'(@@foo 2)))
  (test res eval (expand-to-top-form #'(@@foo 2)))
  (test res eval (expand-syntax-to-top-form #'(@@foo 2))))
(test (list "lifted!" (void)) '@@goo (@@goo))
(set! lifted-output #f)
(test (list "lifted!" (void)) eval (expand-once #'(@@goo)))
(test (list "lifted!" (void)) eval (expand #'(@@goo)))
(test (list "lifted!" (void)) eval (expand-to-top-form #'(@@goo)))

(module @@n racket/base
  (require (for-syntax racket/base))
  (define-syntax (@@foo stx)
    (syntax-case stx ()
      [(_ n)
       (if (zero? (syntax-e #'n))
           #'0
           (with-syntax ([m (sub1 (syntax-e #'n))])
             (syntax-local-lift-expression #'(add1 (@@foo m)))))]))
  (define-syntax (@@foox stx)
    (syntax-case stx ()
      [(_ n)
       (syntax-local-lift-expression #'n)]))
  (provide @@foo @@foox))

(require (for-syntax '@@n))

(test (void) eval (expand #'(define-syntax (@@x stx) #`(list #,(@@foo 1) #,(@@foo 2) #,(@@foo 3)))))
(test (list 1 2 3) '@@x @@x)
(test (void) eval (expand #'(define-syntax (@@x stx) #`(list #,(@@foox 1) #,(@@foox 2) #,(@@foox 3)))))
(test (list 1 2 3) '@@x @@x)
(define-syntax (@@x stx) #`(list #,(@@foox 1) #,(@@foox 2) #,(@@foox 3)))
(test (list 1 2 3) '@@x @@x)
(define-syntax (@@x stx) #`(list #,(@@foo 1) #,(@@foo 2) #,(@@foo 3)))
(test (list 1 2 3) '@@x @@x)
(define-syntax (@@x stx) #`#,(@@foo 2))
(test 2 '@@x @@x)

(test 3
      'ls-foo
      (let-syntax ([z (lambda (stx) #`#,(@@foo 3))])
        z))

(test (void) eval-syntax (expand #'(begin-for-syntax (define @@zoo (@@foo 2)))))
(define-syntax (@@x stx) #`#, @@zoo)
(test 2 '@@x/@@zoo @@x)
(begin-for-syntax (define @@zoo2 (@@foo 2)))
(define-syntax (@@x stx) #`#, @@zoo2)
(test 2 '@@x/@@zoo @@x)

(begin-for-syntax (@@foo 1))
(test (void) eval-syntax (expand #'(begin-for-syntax (@@foo 1))))

(module @@p racket/base
  (require (for-syntax racket/base
                       '@@n))
  (provide @@goo)
  (define-syntax (@@goo stx) #`#,(@@foo 10)))

(require '@@p)
(test 10 '@@goo (@@goo))

(module @@m racket/base
  (require (for-syntax racket/base))
  (define-for-syntax prev-ctx #f)
  (define-syntax (@@foo stx)
    (syntax-case stx ()
      [(_ n)
       (if (zero? (syntax-e #'n))
           #'(list #f 0)
           (with-syntax ([m (sub1 (syntax-e #'n))])
             (let ([prev prev-ctx])
               (if prev
                   (unless (eq? prev (syntax-local-lift-context))
                     (error 'context
                            "mismatch: ~s vs.: ~s"
                            prev
                            (syntax-local-lift-context)))
                   (set! prev-ctx (syntax-local-lift-context))))
             #`(list '#,(syntax-local-lift-context)
                     #,(syntax-local-lift-expression #'(add1 (cadr (@@foo m)))))))]))
  (define @@local #f)
  (define (set-local v)
    (set! @@local v))
  (set-local (@@foo 2))
  (provide @@local))

(require '@@m)
(test 2 '@@local (cadr @@local))
(test #t '@@local (symbol? (car @@local)))

(define-syntaxes (@@local-top @@local-top2 @@local-top3)
  (let ([mk
         (lambda (stops)
           (lambda (stx)
             (syntax-case stx ()
               [(_ expr)
                (let ([v (local-expand/capture-lifts #'expr
                                                     (list (gensym))
                                                     stops
                                                     #f
                                                     'the-key)])
                  ;; make sure that it's a `begin' form:
                  (syntax-case v (begin)
                    [(begin e ... e0) v]))])))])
    (values
     (mk (list #'begin #'#%top))
     (mk null)
     (mk #f))))

(let ([res (let ([x 5]) (@@foo 1))])
  (test res 'let-foo (let ([x 5]) (@@foo 1)))
  (test res eval (expand #'(let ([x 5]) (@@foo 1)))))
(test '(the-key 1) 'local-foo (let ([x 5]) (@@local-top (@@foo 1))))
(test '(the-key 1) eval (expand #'(let ([x 5]) (@@local-top (@@foo 1)))))
(test '(the-key 1) eval (expand #'(@@local-top (@@foo 1))))
(test '(the-key 1) eval (expand #'(@@local-top2 (@@foo 1))))
(test '(the-key 1) eval (expand #'(@@local-top3 (@@foo 1))))

;; Check for distinct top-level contexts for different namespaces:
(module example-that-uses-the-lift-context racket/base
  (require (for-syntax racket/base))
  (provide m)
  (define-for-syntax ht (make-hash))
  (define-syntax (m stx)
    (or (hash-ref ht (syntax-local-lift-context) #f)
        (let ([id (syntax-local-lift-expression #`(quote #,(gensym)))])
          (hash-set! ht (syntax-local-lift-context) id)
          id))))
(dynamic-require ''example-that-uses-the-lift-context 0)
(let ([go
       (lambda ()
         (define orig-ns (current-namespace))
         (parameterize ([current-namespace (make-base-namespace)])
           (namespace-attach-module orig-ns ''example-that-uses-the-lift-context)
           (namespace-require ''example-that-uses-the-lift-context)
           (test (eval 'm) eval 'm)
           (eval '(module extra-module racket/base
                    (require 'example-that-uses-the-lift-context)))
           (dynamic-require ''extra-module 0)
           (let ([ns (module->namespace ''extra-module)])
             (test (eval 'm ns) eval 'm ns)
             (test #f eq? (eval 'm) (eval 'm ns)))))])
  (go)
  (go)
  (go))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check interaction of macro-introduced/lifted names and
;;  module->namespace

(let ([go-once
       (lambda (eval)
         (parameterize ([current-namespace (make-base-namespace)])
           (eval '(module mm racket/base
                    (require (for-syntax racket/base))
                    (define-syntax (define$ stx)
                      (syntax-case stx ()
                        [(_ id val)
                         (with-syntax ([x (datum->syntax #f 'x)])
                           #'(begin
                               (define x val)
                               (define-syntax (id stx) #'x)))]))
                    (define$ a 1)
                    (define$ b 2)
                    (printf "~a ~a\n" a b)))
           (eval '(require 'mm))
           (eval '(current-namespace (module->namespace ''mm)))

           (test '(1 2) eval '(list a b))
           (eval '(define$ c 7))
           (test '(1 2 7) eval '(list a b c))
           (eval '(define$ d 8))
           (test '(1 2 7 8) eval '(list a b c d)))

         (parameterize ([current-namespace (make-base-namespace)])
           (eval '(module mm racket/base
                    (require (for-syntax racket/base))
                    (define-syntax (define$ stx)
                      (syntax-case stx ()
                        [(_ id val)
                         (with-syntax ([x (syntax-local-lift-expression #'val)])
                           #'(define-syntax (id stx) #'x))]))
                    (define$ a 1)
                    (define$ b 2)
                    (printf "~a ~a\n" a b)))
           (eval '(require 'mm))
           (eval '(current-namespace (module->namespace ''mm)))
           (eval '(define$ c 7))
           (test '(1 2 7) eval '(list a b c))
           (eval '(define$ d 8))
           (test '(1 2 7 8) eval '(list a b c d))))])
  (go-once eval)
  (go-once (lambda (e) (eval (expand e)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; layers of lexical binding

#|
 This test is supposed to fail, now: 

(test '(1 2) 'macro-nested-lexical
      (let ()
        (define-syntax (m stx)
          (with-syntax ([x1 (let ([x 0]) #'x)]
                        [x2 (let ([x 0]) #'x)])
            #'(begin
                (define x1 1)
                (define x2 2)
                (list x1 x2))))
        (m)))

(module @!$m racket/base
  (require (for-syntax racket/base))
  (define-syntax (d stx)
    (syntax-case stx ()
      [(_ id)
       (with-syntax ([x1 (let ([x 0]) #'x)]
                     [x2 (let ([x 0]) #'x)])
         #'(begin
             (define x1 10)
             (define x2 20)
             (define id (list x1 x2
                              (list? (identifier-binding (quote-syntax x1)))))))]))
  (d @!$get)
  (provide @!$get))
(require '@!$m)
(test '(10 20 #t) '@!$get @!$get)
|#

(test '(1) ; old expander produced 12
      eval
      (expand
       #'(let ([b 12])
           (let-syntax ([goo (lambda (stx)
                               #`(let ()
                                   (define #,(syntax-local-introduce #'b) 1)
                                   (define z (list b))
                                   z))])
             (goo)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test lazy unmarshaling of renamings and module-name resolution

(let ([load-ok? #t]
      [old (current-module-name-resolver)])
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-module-name-resolver
                  (case-lambda
                   [(name ns)
                    (if (equal? name "a")
                        (void)
                        (old name ns))]
                   [(name _ __) (make-resolved-module-path 'huh?)]
                   [(name base stx load?)
                    (if (equal? name "a")
                        (begin
                          (unless load-ok?
                            (test #f 'load-ok load?))
                          (make-resolved-module-path 'a))
                        (old name base stx load?))])])
    (let ([a-code '(module a racket/base
                     (provide x y)
                     (define x 1)
                     (define y #'x))])
      (eval a-code)
      (let ([b-code (let ([p (open-output-bytes)])
                      (write (compile
                              '(module b racket/base
                                 (require "a")
                                 (provide f)
                                 (define (f) #'x)))
                             p)
                      (lambda ()
                        (parameterize ([read-accept-compiled #t])
                          (read (open-input-bytes (get-output-bytes p))))))]
            [x-id (parameterize ([current-namespace (make-base-namespace)])
                    (eval a-code)
                    (eval '(require 'a))
                    (eval '#'x))])
        (eval (b-code))
        (eval '(require 'b))
        (set! load-ok? #f)
        (test #f eval '(free-identifier=? (f) #'x))
        (test #t eval `(free-identifier=? (f) (quote-syntax ,x-id)))
        (eval '(require 'a))
        (test #t eval '(free-identifier=? (f) #'x))
        ;; check namespace fallbacks:
        (test #t eval `(free-identifier=? (f) (quote-syntax ,x-id)))
        (test #t free-identifier=? (eval '(f)) x-id)
        (parameterize ([current-namespace (make-base-namespace)])
          (eval '(module a racket/base
                   (provide y)
                   (define y 3)))
          (set! load-ok? #t)
          (eval (b-code))
          (eval '(require 'b))
          (set! load-ok? #f)
          (test #t eval '(free-identifier=? (f) #'x))
          (test #f eval `(free-identifier=? (f) (quote-syntax ,x-id))))))))

(test #t free-identifier=? #'lambda #'lambda 0 1)
(test #f free-identifier=? #'lambda #'lambda 0 4)
(require (for-meta 4 racket/base))
(test #t free-identifier=? #'lambda #'lambda 0 4)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unbound and bound at toplevel are equivalent by `free-identifier=?`

(test #t free-identifier=? #'defined-at-the-top-level (datum->syntax #f 'defined-at-the-top-level))
(define defined-at-the-top-level 'yep)
(test #t free-identifier=? #'defined-at-the-top-level (datum->syntax #f 'defined-at-the-top-level))

(define-syntax-rule (define-defined-at-the-top-level)
  (begin
    (define defined-at-the-top-level 10)
    (test #f free-identifier=? #'defined-at-the-top-level (datum->syntax #f 'defined-at-the-top-level))))
(define-defined-at-the-top-level)
(test #t free-identifier=? #'defined-at-the-top-level (datum->syntax #f 'defined-at-the-top-level))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  certification example from the manual

(module @-m racket/base
  (require (for-syntax racket/base))
  (provide def-go)
  (define (unchecked-go n x)
    (+ n 17))
  (define-syntax (def-go stx)
   (syntax-case stx ()
     [(_ go)
      #'(define-syntax (go stx)
          (syntax-case stx ()
           [(_ x)
            #'(unchecked-go 8 x)]))])))

(module @-n racket/base
  (require '@-m)
  (def-go go)
  (go 10)) ; access to unchecked-go is allowed

(require '@-n)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propagating inactive certificates through a transparent macro-expansion
;; result:

(module @!m racket/base
  (require (for-syntax racket/base))
  (provide define-x)

  (define-syntax (define-x stx)
    (syntax-case stx ()
      [(_ x)
       #'(define-syntax (x stx)
           #'(begin
               (define-y y 10)))]))

  (define-syntax define-y
    (syntax-rules ()
      [(_ id v)
       (define id v)])))

(module @!n racket/base
  (require '@!m)
  (define-x def-y)
  (def-y))

;; If we get here, then macro expansion didn't fail.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that the free-identifier=? cache doesn't kick in too eagerly.

(module @w@ racket/base
  (define add '+)

  (provide (rename-out [add plus])))

(module @q@ racket/base
  (require (for-syntax racket/base))
  (provide result)

  (define-for-syntax a #'plus)
  (define-for-syntax b #'plus)

  (define-for-syntax accum null)

  (begin-for-syntax
   (set! accum (cons (free-identifier=? a #'plus)
                     accum)))

  (require '@w@)

  (begin-for-syntax
   (set! accum (list*
                (free-identifier=? a #'plus)
                (free-identifier=? b #'plus)
                accum)))

  (define-syntax (accumulated stx)
    (datum->syntax stx `',accum))

  (define result (accumulated)))

(require '@q@)
(test '(#t #t #t) values result)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Test namespace-attach with phase-levels -2 and 2


(module tn racket/base
  (require racket/file)
  (define tmp10 (make-temporary-file))
  (provide tmp10)
)

(module @!a racket/base
  (require 'tn)
  (provide x)
  (with-output-to-file tmp10
    #:exists 'append
    (lambda ()
      (printf "a\n")))
  (define x 5))

(module @!b racket/base
  (provide get-x)
  (require (for-meta -2 '@!a))
  (define (get-x) #'x))

(module @!c racket/base
  (require 'tn)
  (require (for-meta 2 '@!b)
           (for-syntax racket/base
                       (for-syntax racket/base)))
  (define-syntax (foo stx)
    (let-syntax ([ref-x (lambda (stx)
                          #`(quote-syntax #,(get-x)))])
      (ref-x)))

  (with-output-to-file tmp10
    #:exists 'append
    (lambda ()
      (printf "~s\n" (foo)))))

(require 'tn)

(define (check-tmp10 s)
  (test s with-input-from-file tmp10 (lambda () (read-string 1000))))

(require '@!c)
(check-tmp10 "a\n5\n")

(let ()
  (define n (make-base-namespace))
  (namespace-attach-module (current-namespace) ''@!c n)
  (test 5
        'use-a
        (parameterize ([current-namespace n])
          ;; Shouldn't instantiate new:
          (namespace-require ''@!a)
          ;; Should see `x' from @!a:
          (eval 'x)))
  (check-tmp10 "a\n5\n"))

(when (file-exists? tmp10)
  (delete-file tmp10))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure post-ex renames aren't simplied away too soon:

(module @simp@ racket/base

  (require (for-syntax racket/base))

  (define-syntax-rule (foo)
    (begin
      (define-for-syntax goo #'intro)
      (define intro 5)
      (define-syntax (extract stx)
        #`(quote #,(identifier-binding goo)))
      (define @simp@tst (extract))
      (provide @simp@tst)))

  (foo))
(require '@simp@)

(test #t list? @simp@tst)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check marshaling of compiled code to disallow
;;  unreadable values in a hash-table literal

;; cyclic hash table as a bad "constant":
(err/rt-test (let ([s (open-output-bytes)])
               (write (compile `(quote ,(let ([ht (make-hasheq)])
                                          (hash-set! ht #'bad ht)
                                          ht)))
                      s)
               (get-output-bytes s))
             exn:fail?)
;; non-cyclic variant:
(err/rt-test (let ([s (open-output-bytes)])
               (write (compile `(quote ,(let ([ht (make-hasheq)])
                                          (hash-set! ht #'bad 10)
                                          ht)))
                      s)
               (get-output-bytes s))
             exn:fail?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-syntax-rule

(define-syntax-rule (a-rule-pattern x [y z])
  (list 'x 'y 'z))

(test '(1 2 3) 'a-rule (a-rule-pattern 1 [2 3]))
(test '(1 2 3) 'a-rule (a-rule-pattern 1 . ([2 3])))
(test '(1 2 3) 'a-rule (a-rule-pattern 1 [2 . (3)]))
(syntax-test #'a-rule-pattern)
(syntax-test #'(a-rule-pattern 1 2 3))
(syntax-test #'(a-rule-pattern 1 . 2))
(syntax-test #'(a-rule-pattern . 1))
(syntax-test #'(a-rule-pattern 1 [2 3] 4))

(let ([no-match? (lambda (exn) 
                   (regexp-match? #"does not match pattern" (exn-message exn)))])
  (error-test #'a-rule-pattern no-match?)
  (error-test #'(a-rule-pattern) no-match?)
  (error-test #'(a-rule-pattern 1) no-match?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explicit binding sets

(let ([bs (syntax-binding-set-extend
           (syntax-binding-set-extend
            (syntax-binding-set-extend
             (syntax-binding-set)
             'car 0 (module-path-index-join ''#%runtime #f))
            'cdr 1 (module-path-index-join ''#%runtime #f)
            #:source-phase 0
            #:nominal-require-phase 1)
           'items 0 (module-path-index-join ''#%runtime #f)
           #:source-symbol 'list)])
  (test #t free-identifier=?
        (syntax-binding-set->syntax bs 'car)
        #'car)
  (test #f free-identifier=?
        (syntax-binding-set->syntax bs 'cdr)
        #'cdr)
  (test #t free-identifier=?
        (syntax-binding-set->syntax bs 'cdr)
        #'cdr
        1)
  (test #f free-identifier=?
        (syntax-binding-set->syntax bs 'list)
        #'list)
  (test #t free-identifier=?
        (syntax-binding-set->syntax bs 'items)
        #'list))

(module synthesizes-self-reference racket/base
  (require (for-syntax racket/base))
  (provide results)

  (define x 5)
  
  (define-syntax (f stx)
    (define the-x
      (syntax-binding-set->syntax
       (syntax-binding-set-extend
        (syntax-binding-set)
        'x 0 (variable-reference->module-path-index
              (#%variable-reference)))
       'x))
  
    #`(list #,the-x
            x
            (eval (quote-syntax #,the-x))
            (eval (quote-syntax x))))

  (define results (f)))

(dynamic-require ''synthesizes-self-reference 0)
(test '(5 5 5 5) dynamic-require ''synthesizes-self-reference 'results)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra taint tests

(define (syntax-touch s) (datum->syntax s (syntax-e s)))

(test #f syntax-tainted? #'x)
(test #f syntax-tainted? (syntax-touch #'x))
(test #f syntax-tainted? (syntax-arm #'x))
(test #t syntax-tainted? (syntax-touch (syntax-arm #'x)))
(test #t syntax-tainted? (car (syntax-e (syntax-arm #'(x y)))))

(test #f syntax-tainted? (car (syntax-e (syntax-arm (syntax-property #'(x y) 
                                                                     'taint-mode
                                                                     'transparent)
                                                    #f #t))))
(test #f syntax-tainted? (car (syntax-e (syntax-arm (syntax-property #'(x y) 
                                                                     'certify-mode
                                                                     'transparent)
                                                    #f #t))))
(test #f ormap syntax-tainted? (syntax-e (syntax-arm #'(begin x) #f #t)))
(test #t andmap syntax-tainted? (syntax-e (syntax-arm (syntax-property #'(begin x)
                                                                       'taint-mode
                                                                       'opaque)
                                                      #f #t)))

(test #f andmap syntax-tainted? (syntax-e (cadr (syntax-e (syntax-arm #'(define-values (x y z) (values 1 2 3))
                                                                      #f #t)))))
(test #f andmap syntax-tainted? (syntax-e 
                                 (cadr 
                                  (syntax-e 
                                   (cadr 
                                    (syntax-e 
                                     (syntax-arm #'(begin (define-values (x y z) (values 1 2 3)))
                                                 #f #t)))))))

(let ()
  (define i1 (make-inspector))
  (define i2 (make-inspector))
  
  (define x (syntax-arm #'(x) i1))
  
  (test #f syntax-tainted? (car (syntax-e (syntax-disarm x i1))))
  (test #t syntax-tainted? (car (syntax-e (syntax-disarm x i2))))
  
  (define y (syntax-rearm (syntax-arm #'(y) i2) x))
  
  (test #t syntax-tainted? (car (syntax-e (syntax-disarm y i1))))
  (test #t syntax-tainted? (car (syntax-e (syntax-disarm y i2))))
  (test #f syntax-tainted? (car (syntax-e (syntax-disarm (syntax-disarm y i1) i2)))))

(let ([round-trip
       (lambda (stx)
         (parameterize ([current-namespace (make-base-namespace)])
           (let ([s (open-output-bytes)])
             (write (compile `(quote-syntax ,stx)) s)
             (parameterize ([read-accept-compiled #t])
               (eval (read (open-input-bytes (get-output-bytes s))))))))])
  (test #f syntax-tainted? (round-trip (syntax-arm (quote-syntax foo))))
  (test #t syntax-tainted? (syntax-touch (round-trip (syntax-arm (quote-syntax foo)))))
  (test #t syntax-tainted? (round-trip (syntax-touch (syntax-arm (quote-syntax foo))))))

;; Make sure that a taint-transparent syntax object loses its lexical context:
(let ([b-stx #'(begin 1)])
  (test #t free-identifier=? #'begin (datum->syntax b-stx 'begin))
  (let ([a-b-stx (parameterize ([current-namespace (make-base-namespace)])
                   (eval '(define-syntax-rule (b e)
                            (begin e)))
                   (expand '(b 1)))])
    (test #f free-identifier=? #'begin (datum->syntax a-b-stx 'begin))
    (test #t free-identifier=? #'begin (syntax-case a-b-stx ()
                                         [(b . _) (datum->syntax #'b 'begin)]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax-debug-info

(let ([check (lambda (syntax-debug-info)
               (test 'x hash-ref (syntax-debug-info #'x) 'name)
               (test 'nope hash-ref (syntax-debug-info #'1) 'name 'nope)
               (test 'nope hash-ref (syntax-debug-info #'(x y)) 'name 'nope))])
  (check syntax-debug-info)
  (parameterize ([current-namespace (make-base-namespace)])
    (eval '(require (prefix-in foo: racket/base)))
    (check (lambda (stx) (syntax-debug-info (namespace-syntax-introduce stx))))))

(test #t
      'syntax-debug-info-all-binding
      (let ([y 10])
        (for/or ([e (in-list (hash-ref (syntax-debug-info (quote-syntax x #:local) 0 #t) 'bindings null))])
          (eq? 'y (hash-ref e 'name #f)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that attacks are thwarted via `syntax-local-get-shadower'
;; or `make-syntax-delta-introducer':

(module secret-value-42 racket
   (define secret 42)
   (define-syntax-rule (m) (even? secret))
   (provide m))
(require 'secret-value-42)

(define-syntax (evil-via-shadower stx)
  (syntax-case stx ()
    [(_ e)
     (let* ([ee (local-expand #'e 'expression null)]
            [id (with-syntax ([(app f x) ee]) #'f)]
            [okid (syntax-local-get-shadower id)])
       #`(let ([#,okid values])
           #,ee))]))

(define-syntax (evil-via-delta-introducer stx)
  (syntax-case stx ()
    [(_ e)
     (let* ([ee (local-expand #'e 'expression null)]
            [id (with-syntax ([(app f x) ee]) #'f)]
            [okid ((make-syntax-delta-introducer id #'e) #'even?)])
       #`(let ([#,okid values])
           #,ee))]))

(syntax-test #'(evil-via-shadower (m)))
(syntax-test #'(evil-via-delta-introducer (m)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `syntax-make-delta-introducer` transfers
;; shifts along with scopes [example by Alexis]

(let ([m '(module defines-introducer-to-submodule-binding racket/base
            (provide foo-val)

            (module foo racket/base
              (provide foo)
              (define foo 42))

            (module introducer racket/base
              (require (for-syntax racket/base
                                   racket/syntax)
                       syntax/parse/define)

              (provide begin-foo)

              (begin-for-syntax
                (define scopeless-stx (datum->syntax #f #f)))

              (define-syntax-parser define-cached-require-introducer
                [(_ x:id mod-path)
                 #:with scoped-stx (syntax-local-introduce #'mod-path)
                 #'(begin
                     (require scoped-stx)
                     (begin-for-syntax
                       (define x (make-syntax-delta-introducer (quote-syntax scoped-stx) scopeless-stx))))])

              (define-cached-require-introducer introduce-foo (submod ".." foo))

              (define-syntax-parser begin-foo
                [(_ form ...)
                 (introduce-foo
                  #'(begin form ...))]))

            (require 'introducer)
            (define foo-val (begin-foo foo)))])
  (eval (expand m)))

(test 42 dynamic-require ''defines-introducer-to-submodule-binding 'foo-val)

(module uses-introducer-to-submodule-binding racket/base
  (provide also-foo-val)
  
  (require (submod 'defines-introducer-to-submodule-binding introducer))

  (define also-foo-val (begin-foo foo)))

(test 42 dynamic-require ''uses-introducer-to-submodule-binding 'also-foo-val)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a for-syntax reference can precede a
;;  for-syntax definition

(module pre-definition-reference racket/base
  (require (for-syntax racket/base))
  (provide (for-syntax f g))
  (define-for-syntax (f x) (g (+ x 1)))
  (define-for-syntax (g y) (+ y 2)))

(require 'pre-definition-reference)
(test 3 'use (let-syntax ([m (lambda (stx) (datum->syntax stx (f 0)))])
               m))

(syntax-test #'(module unbound-reference racket/base
                 (require (for-syntax racket/base))
                 (define-for-syntax (f x) nonesuch)))
(syntax-test #'(module unbound-reference racket/base
                 (require (for-syntax racket/base))
                 (#%expression
                  (let-syntax ([g (lambda (stx) nonesuch)])
                    10))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `syntax-transforming?' and `syntax-transforming-module-expression?'

(test #f syntax-transforming?)
(test #f syntax-transforming-module-expression?)
(test #t 'trans (let-syntax ([m (lambda (stx)
                                  (datum->syntax stx (syntax-transforming?)))])
                  (m)))
(test #f 'trans-mod (let-syntax ([m (lambda (stx)
                                      (datum->syntax stx (syntax-transforming-module-expression?)))])
                      (m)))
(let ([o (open-output-string)])
  (parameterize ([current-output-port o])
    (eval `(module m racket/base
             (require (for-syntax racket/base))
             (define-syntax (m stx)
               (displayln (syntax-transforming-module-expression?))
               #'1)
             (m)))
    (eval `(module m racket/base
             (require (for-syntax racket/base))
             (begin-for-syntax
              (displayln (syntax-transforming-module-expression?))))))
  (test "#t\n#t\n" get-output-string o))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a common wraps encoding that is detected only
;; after simplification and encoding is shared propery. If
;; it's not shared properly in this example, a gensym for
;; the internal-definition context gets duplicated.

(parameterize ([current-namespace (make-base-namespace)])
  (define e
    (compile '(module producer racket/base
                (#%module-begin

                 (require (for-syntax racket/base))

                 (define-syntax (compare stx)
                   (syntax-case stx ()
                    [(_ formal body)
                     (let ()

                       (define (internal-definition-context-apply ctx s)
                         (syntax-case (local-expand #`(quote-syntax #,s)
                                                    'expression 
                                                    (list #'quote-syntax)
                                                    ctx) ()
                           [(qs e) #'e]))

                       (define ctx (syntax-local-make-definition-context))
                       (syntax-local-bind-syntaxes (list #'formal) #f ctx)
                       (internal-definition-context-seal ctx)

                       (with-syntax ([one
                                      (internal-definition-context-apply ctx #'formal)]
                                     [two
                                      (syntax-local-introduce
                                       (internal-definition-context-apply 
                                        ctx
                                        (syntax-local-introduce
                                         (internal-definition-context-apply ctx #'body))))])

                         (unless (free-identifier=? #'one #'two)
                           (error 'before
                                  "identifiers were never the same"))
                         
                         #'(begin-for-syntax
                            (unless (free-identifier=? #'one #'two)
                              (error 'after
                                     "identifiers used to be the same, but now are not")))))]))

                 (compare z z)))))
  (let ([o (open-output-bytes)])
    (write e o)
    (parameterize ([read-accept-compiled #t])
      (eval (read (open-input-bytes (get-output-bytes o))))))
  (namespace-require ''producer)
  (eval 10))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check handling of module context

(module mm-context-m1 racket/base
  (require (for-syntax racket/base))
  (provide m1)
  (define-syntax (m1 stx)
    #`(begin
        (define #,(syntax-local-introduce #'x) 1)
        #,(syntax-local-introduce #'x))))

(module mm-context-m2 racket/base
  (require (for-syntax racket/base))
  (provide m2)
  (define-syntax (m2 stx)
    #`(begin
        (define #,(syntax-local-introduce #'x) 2)
        #,(syntax-local-introduce #'x))))

(module mm-context-m3 racket/base
  (require 'mm-context-m1 'mm-context-m2)
  (m1)
  (m2))

(let ([o (open-output-bytes)])
  (parameterize ([current-output-port o])
    (dynamic-require ''mm-context-m3 #f))
  (test #"1\n2\n" get-output-bytes o))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `quasisyntax' finds `unsyntax'

(with-syntax ([a #'1] [(c ...) #'(3 4 5)])
  (let ([b #'2] [ds (list #'3 #'4 #'5)])
    (test '(1 2) syntax->datum (quasisyntax (a (unsyntax b))))
    (test '(2 1) syntax->datum (quasisyntax ((unsyntax b) a)))
    (test '(1 . 2) syntax->datum (quasisyntax (a unsyntax b)))
    (test '((1) (2)) syntax->datum (quasisyntax ((a) ((unsyntax b)))))
    (test '#(1 2) syntax->datum (quasisyntax #(a (unsyntax b))))
    (test '#(1 2 3 4 5) syntax->datum (quasisyntax #(a (unsyntax b) c ...)))
    (test '#s(PS 1 2) syntax->datum (quasisyntax #s(PS a (unsyntax b))))
    (test '#s(PS 1 2 3 4 5) syntax->datum (quasisyntax #s(PS a (unsyntax b) c ...)))
    (test '#(1 2 3 4 5) syntax->datum (quasisyntax #(a (unsyntax b) (unsyntax-splicing ds))))
    (test '#(3 4 5) syntax->datum (quasisyntax #((unsyntax-splicing ds))))
    #|
    (test '#s(PS 1 2 3 4 5) syntax->datum
          (quasisyntax #s(PS a (unsyntax b) (unsyntax-splicing ds))))
    |#))

(syntax-test #'(quasisyntax unsyntax))
(syntax-test #'(quasisyntax (unsyntax)))
(syntax-test #'(quasisyntax (unsyntax 1 2)))
(syntax-test #'(quasisyntax unsyntax-splicing))
(syntax-test #'(quasisyntax (unsyntax-splicing)))
(syntax-test #'(quasisyntax (unsyntax-splicing 1 2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check preservation of properties by `quasisyntax'

(test #\[ syntax-property #'[x] 'paren-shape)
(test #\[ syntax-property #`[x] 'paren-shape)
(test #\[ syntax-property #`[x #,#'y] 'paren-shape)
(test #\[ syntax-property #`[0 #,@(list #'1 #'2)] 'paren-shape)
(test #\[ syntax-property #`[0 #,@null] 'paren-shape)
(test #\[ syntax-property (quasisyntax [x (unsyntax (syntax y))]) 'paren-shape)
(test #\[ syntax-property (quasisyntax [x y]) 'paren-shape)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that quasisyntax in quasisyntax doesn't infinite loop

(test #t syntax? (quasisyntax (quote-syntax quasisyntax)))
(test #t syntax? (quasisyntax (quasisyntax . x)))
(test #t syntax? (quasisyntax (list quasisyntax)))
(test #t syntax? (quasisyntax (x . (quasisyntax . x))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check srcloc on result of `syntax-local-value/immediate':

(let ()
  (define-syntax (displayln-syntax-local-value/immediate stx)
    (syntax-case stx ()
      [(_ id)
       (let-values ([(x y)
                     (syntax-local-value/immediate (datum->syntax #'id
                                                                  (syntax-e #'id)))])
         #`#,(syntax-source y))]))
  (define-syntax ++ (make-rename-transformer (datum->syntax #'here '+)))
  (test #f values (displayln-syntax-local-value/immediate ++)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that syntax structure is preserved precisely with
;; #'(a . ()) as opposed to #'(a)

(let ()
  (define-values (i o) (make-pipe))
  (write (compile #'#'(a)) o)
  (close-output-port o)
  (define s (parameterize ([read-accept-compiled #t])
              (read i)))
  (test #t null? (cdr (syntax-e (eval s)))))
(let ()
  (define-values (i o) (make-pipe))
  (write (compile #'#'(a . ())) o)
  (close-output-port o)
  (define s (parameterize ([read-accept-compiled #t])
              (read i)))
  (test #t syntax? (cdr (syntax-e (eval s)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check interation of bindings across namespaces:

(let ()
  (define ns1 (make-base-namespace))
  (define ns2 (make-base-namespace))
  (eval '(require (only-in racket/base [add1 cons])) ns1)
  ;; In `ns1`, `cons` refers to `add1`
  ;; In `ns2`, `cons` refers to `cons`
  (define cons-id/ns1 (eval '(quote-syntax cons) ns1))
  (test add1 eval cons-id/ns1 ns1)
  (test add1 eval cons-id/ns1 ns2)
  (eval `(define ,cons-id/ns1 1) ns2)
  (test 1 eval cons-id/ns1 ns2)
  (test cons eval 'cons ns2)
  (test 1 eval (quasiquote (let () (define ,cons-id/ns1 1) ,cons-id/ns1)) ns2))

(module x-id-is-alias-for-plus racket/base
  (provide x-id)
  (require (only-in racket/base [+ x]))
  (define x-id #'x))
(let ([x-id (dynamic-require ''x-id-is-alias-for-plus 'x-id)])
  (define ns (make-base-namespace))
  (eval '(require (only-in racket/base [- x])) ns)
  (test - eval 'x ns)
  (test + eval x-id ns))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a phase shift also shifts fallback contexts

(let ()
  (define ns (make-base-namespace))
  (define (evalx e)
    (parameterize ([current-namespace ns])
      (eval-syntax (expand (datum->syntax #f e)))))
  (evalx '(module m racket/base (provide e) (define e #'1)))
  (evalx '(module n racket/base (require (for-syntax 'm)) (provide s) (define-syntax (s stx) e)))
  (evalx '(require 'n))
  (err/rt-test (evalx 's) (lambda (exn) (regexp-match? #rx"literal data is not allowed" (exn-message exn)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check source-location reporting by `raise-syntax-error`

(let ()
  (define (check a0 a1 . args)
    (err/rt-test (apply raise-syntax-error #f "oops" a0 a1 args)
                 (lambda (exn)
                   (and (exn:fail:syntax? exn)
                        (regexp-match? (format "^([a-zA-Z]:)?[^:\n]*:~a:~a:"
                                               (or (syntax-line a1)
                                                   (syntax-line a0))
                                               (or (syntax-column a1)
                                                   (syntax-column a0)))
                                       (exn-message exn))))))
  (define stx #'(a b))
  (define a-stx (car (syntax-e stx)))
  (check stx a-stx)
  (check stx #f)
  (check #f a-stx))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test prop:rename-transformer with procedure content

(begin-for-syntax
  (struct slv-struct-1 (id)
    #:property prop:rename-transformer
    (λ (o) (slv-struct-1-id o)))

  (struct slv-struct-2 (t1? t1 t2)
    #:property prop:rename-transformer
    (λ (o)
      (if (slv-struct-2-t1? o)
          (slv-struct-2-t1 o)
          (slv-struct-2-t2 o))))

  (struct slv-struct-bad ()
    #:property prop:rename-transformer
    (λ (o) 'not-an-identifier)))

(let ()
  (define-syntax target-1 't1)
  (define-syntax target-2 't2)

  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ id)
       #`(quote #,(syntax-local-value #'id) )]))

  (define-syntax (m2 stx)
    (syntax-case stx ()
      [(_ id)
       (let-values ([(x y) (syntax-local-value/immediate #'id)])
         #`(list (quote #,(if (rename-transformer? x) 'rename-transformer x))
                 (quote #,(and y (syntax-e y)))))]))

  (define-syntax s1 (slv-struct-1 #'target-1))
  (define-syntax s2 (slv-struct-1 #'target-2))
  (define-syntax s3 (make-rename-transformer #'target-2))
  (define-syntax s4 (slv-struct-1 #'s3))
  (define-syntax s5 (slv-struct-2 #t #'target-1 #'target-2))
  (define-syntax s6 (slv-struct-2 #f #'target-1 #'target-2))
  (define-syntax s7 (slv-struct-2 #t #'s3 #'target-2))
  (define-syntax s8 (slv-struct-2 #f #'s3 #'target-2))
  (define-syntax s9 (make-rename-transformer #'s8))

  (test 't1 values (m s1))
  (test '(rename-transformer target-1) values (m2 s1))
  (test 't2 values (m s2))
  (test '(rename-transformer target-2) values (m2 s2))
  (test 't2 values (m s4))
  (test '(rename-transformer s3) values (m2 s4))
  (test 't1 values (m s5))
  (test '(rename-transformer target-1) values (m2 s5))
  (test 't2 values (m s6))
  (test '(rename-transformer target-2) values (m2 s6))
  (test 't2 values (m s7))
  (test '(rename-transformer s3) values (m2 s7))
  (test 't2 values (m s8))
  (test '(rename-transformer target-2) values (m2 s8))
  (test 't2 values (m s9))
  (test '(rename-transformer s8) values (m2 s9))

  (define target-3 't3)
  (define target-4 't4)
  (define-syntax r1 (slv-struct-1 #'target-3))
  (define-syntax r2 (slv-struct-1 #'target-4))
  (define-syntax r3 (slv-struct-2 #t #'target-3 #'target-4))
  (define-syntax r4 (slv-struct-2 #f #'target-3 #'target-4))
  (test 't3 values r1)
  (test 't4 values r2)
  (test 't3 values r3)
  (test 't4 values r4)

  (err/rt-test
   (let ()
     (struct foo () #:property prop:rename-transformer (λ (x y) 3))
     (void))
   exn:fail:contract?)
  (err/rt-test
   (eval #'(let () (define-syntax s-bad (slv-struct-bad)) (m s-bad)))
   exn:fail:contract?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require (for-syntax racket/base)))
  (eval
   '(define-syntax (m stx)
     (define x (car (generate-temporaries '(1))))
     (syntax-case stx ()
       [(_ lib name)
        #`(begin (require (only-in lib [name #,x]))
                 (define-syntax name
                   (make-rename-transformer (quote-syntax #,x)))
                 name)])))
  (eval '(m racket/base values)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check marshaling and unmarshaling with relative paths

(let ()
  (define dir (find-system-path 'temp-dir))

  (define x (parameterize ([current-namespace (make-base-namespace)])
              (compile (datum->syntax #f '#'x (vector (build-path dir "sub" "x.rkt")
                                                      1
                                                      1
                                                      1
                                                      1)))))
  (define-values (i o) (make-pipe))
  (parameterize ([current-write-relative-directory
                  (cons (build-path dir "nested")
                        dir)])
    (write x o))
  (test (build-path dir "inner" 'up "sub" "x.rkt")
        syntax-source
        (eval (parameterize ([read-accept-compiled #t]
                             [current-load-relative-directory (build-path dir "inner")])
                (read i)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that shadowing doesn't create an ill-formed internal
;; representation of binding:

(let ()
  ;; Make introducers before namespace, so they have older scopes, which
  ;; means that bindings will be attached to the namespace's scope:
  (define i1 (make-syntax-introducer))
  (define i2 (make-syntax-introducer))
  (define ns (make-base-namespace))
  (eval `(define car 0) ns)
  (eval `(define ,(i1 (datum->syntax #f 'car)) 1) ns)
  (eval `(define ,(i2 (datum->syntax #f 'car)) 2) ns)
  (eval `(require racket/base) ns) ; replaces plain `car` mapping
  (write (compile-syntax
          #`(quote-syntax #,(parameterize ([current-namespace ns])
                              (namespace-syntax-introduce (datum->syntax #f 'car)))))
         (open-output-bytes)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that reading a compiled module doesn't mutate the
;; shared "self" modix for a submodule:

(parameterize ([current-namespace (make-base-namespace)])
  (define o (open-output-bytes))
  (write (compile `(module name-1 racket/base (module+ inside))) o)
  (define m
    (parameterize ([read-accept-compiled #t])
      (read (open-input-bytes (get-output-bytes o)))))
  (define s (expand `(module name-2 racket/base (module+ inside (define check-me 1)))))
  (test "(|expanded module| inside)"
        format
        "~s"
        (resolved-module-path-name
         (let loop ([s s])
           (cond
            [(identifier? s)
             (and (equal? 'check-me (syntax-e s))
                  (module-path-index-resolve (car (identifier-binding s))))]
            [(syntax? s) (loop (syntax-e s))]
            [(pair? s)
             (or (loop (car s)) (loop (cdr s)))]
            [else #f])))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([zo-bounce
       (lambda (stx)
         (define o (open-output-bytes))
         (write (compile #`(quote-syntax #,stx)) o)
         (eval
          (parameterize ([read-accept-compiled #t])
            (read (open-input-bytes (get-output-bytes o))))))])
  (test #\{ syntax-property (zo-bounce #'{0}) 'paren-shape)
  (test #\[ syntax-property (zo-bounce #'[0]) 'paren-shape)
  (test #f syntax-property (zo-bounce (syntax-property #'[0] 'something-else 1))
        'something-else)
  (test 1 syntax-property (zo-bounce (syntax-property #'[0] 'something-else 1 #t))
        'something-else)
  
  (define s0 (syntax-property
              (syntax-property
               (syntax-property #'[0]
                                'something-else 1 #t)
               'something-not-saved 2)
              'a-third-thing 3 #t))
  (define s (zo-bounce s0))
  ;; Like `s`, but without source locations or paren shape:
  (define sx (zo-bounce
              (syntax-property
               (syntax-property
                (syntax-property (datum->syntax #f '[0])
                                 'something-else 1 #t)
                'something-not-saved 2)
               'a-third-thing 3 #t)))

  (test #\[ syntax-property s 'paren-shape)
  (test #f syntax-property sx 'paren-shape)
  (test #\[ syntax-property s0 'paren-shape)
  (test #t syntax-property-preserved? s 'paren-shape)
  (test #f syntax-property-preserved? sx 'paren-shape)
  (test #t syntax-property-preserved? s0 'paren-shape)
  
  (test 1 syntax-property s 'something-else)
  (test 1 syntax-property sx 'something-else)
  (test 1 syntax-property s0 'something-else)
  (test #t syntax-property-preserved? s 'something-else)
  (test #t syntax-property-preserved? sx 'something-else)
  (test #t syntax-property-preserved? s0 'something-else)
  
  (test #f syntax-property s 'something-not-saved)
  (test 2 syntax-property s0 'something-not-saved)
  (test #f syntax-property-preserved? s 'something-not-saved)
  (test #f syntax-property-preserved? s0 'something-not-saved)

  (test 3 syntax-property s 'a-third-thing)
  (test 3 syntax-property sx 'a-third-thing)
  (test 3 syntax-property s0 'a-third-thing)
  (test #t syntax-property-preserved? s 'a-third-thing)
  (test #t syntax-property-preserved? sx 'a-third-thing)
  (test #t syntax-property-preserved? s0 'a-third-thing)
  
  ;; 'paren-shape has a special default:
  (test #t syntax-property-preserved? (syntax-property #'#f 'paren-shape #\() 'paren-shape)
  
  ;; Without 'paren-shape ------------------------------
  
  (define s2-0 (syntax-property
                (syntax-property
                 (syntax-property #'0 'something-else 1.0 #t)
                 'something-not-saved 2.0)
                'a-third-thing 3.0 #t))
  (define s2 (zo-bounce s2-0))
  
  (test #f syntax-property s2 'paren-shape)
  (test #f syntax-property s2-0 'paren-shape)
  
  (test 1.0 syntax-property s2 'something-else)
  (test 1.0 syntax-property s2-0 'something-else)
  (test #t syntax-property-preserved? s2 'something-else)
  (test #t syntax-property-preserved? s2-0 'something-else)
  
  (test #f syntax-property s2 'something-not-saved)
  (test 2.0 syntax-property s2-0 'something-not-saved)
  (test #f syntax-property-preserved? s2 'something-not-saved)
  (test #f syntax-property-preserved? s2-0 'something-not-saved)

  (test 3.0 syntax-property s2 'a-third-thing)
  (test 3.0 syntax-property s2-0 'a-third-thing)
  (test #t syntax-property-preserved? s2 'a-third-thing)
  (test #t syntax-property-preserved? s2-0 'a-third-thing)
  
  ;; Check value encoding and decoding:
  (define prop-val (list 1
                         1.0
                         (vector-immutable 'a "apple" #"apple")
                         (vector-immutable)
                         (vector 7 8 9)
                         (hash 'a 1 'b 2)
                         (hasheq 'a 1 'b 2)
                         (box-immutable 3/4)
                         (box 'b)
                         #rx"."))
  (define s3 (syntax-property #'3 'saved prop-val #t))
  (test prop-val syntax-property (zo-bounce s3) 'saved)
  
  (define s4 (syntax-property #'4 'saved-stx (vector-immutable s3 #'cons) #t))
  (define p-v (syntax-property (zo-bounce s4) 'saved-stx))
  (test #t vector? p-v)
  (test prop-val syntax-property (vector-ref p-v 0) 'saved)
  (test #t free-identifier=? #'cons (vector-ref p-v 1))
  
  (define (check-bad val)
    (err/rt-test (zo-bounce (syntax-property #'#f 'saved val #t))
                 (lambda (exn) (regexp-match #rx"write: disallowed" (exn-message exn)))))
  
  (check-bad (lambda (x) x))
  (check-bad (mcons 1 2))
  (check-bad (read (open-input-string "#0=(1 . #0#)")))
  (check-bad void))

(err/rt-test (syntax-property #'+ 1 #'+ #t)
             (lambda (exn)
               (regexp-match
                #rx"key for a preserved property must be an interned symbol"
                (exn-message exn))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that paths from the current installation are not
;; preserved in marshaled bytecode

(let ([m '(module m racket/base
            ;; Extending a primitive structure type tends to
            ;; capture an identifier whose source is "kernstruct.rkt"
            (define-struct (cookie-error exn:fail) ()))])
  (define o (open-output-bytes))
  (write (compile m) o)
  (test #t
        not
        (regexp-match? (regexp-quote
                        (path->bytes (collection-file-path "kernstruct.rkt" "racket/private")))
                       (get-output-bytes o))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the srcloc encoding doesn't do something strange
;; with a path in a root directory:

(parameterize ([current-namespace (make-base-namespace)])
  (define path (build-path (car (filesystem-root-list)) "x.rkt"))
  (parameterize ([current-module-declare-name (make-resolved-module-path path)]
                 [read-accept-reader #t]
                 [read-accept-compiled #t])
    (define p (open-input-string "#lang racket/base (provide f) (define (f) #'a)"))
    (port-count-lines! p)
    (define-values (in out) (make-pipe))
    (write (compile (read-syntax path p)) out)
    (eval (read in))
    (define src (syntax-source ((dynamic-require path 'f))))
    (test (path->string path) values src)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #t
      'rename-transformer-srcloc
      ;; make sure `cons` in the expansion gets the same source line as `1`
      (let ([stx (expand #'(letrec-syntax ([kons (make-rename-transformer #'cons)])
                             (kons 1 2)))])
        (syntax-case stx ()
          [(_ () (_app cons one . _))
           (equal? (syntax-line #'cons) (syntax-line #'one))])))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minimal tests for syntax and ~@, ~?
;; More tests in pkgs/racket-test/tests/stxparse/test-syntax.rkt

(test '(a 1 b 2 c 3)
      'syntax
      (with-syntax ([(x ...) #'(a b c)] [(y ...) #'(1 2 3)])
        (syntax->datum #'((~@ x y) ...))))

(test '(1 2 3 4)
      'syntax
      (with-syntax ([xs #'(2 3)])
        (syntax->datum #'(1 (~@ . xs) 4))))

(test '(1 2 3)
      'syntax
      (with-syntax ([x #'(1 2 3)])
        (syntax->datum #'(~? x "missing"))))

(test '(4 5 6)
      'syntax
      (with-syntax ([(x ...) #'(4 5 6)])
        (syntax->datum #'((~? x) ...))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for syntax/loc

(let ()
  (define (f stx) (list (syntax-source stx) (syntax-position stx)))
  (define (same-src? x y) (equal? (syntax-source x) (syntax-source y)))
  (define good1 (datum->syntax #f 'good '(source #f #f 1 4)))
  (define good3 (datum->syntax #f 'good '(source #f #f #f #f)))
  (define good4 (datum->syntax #f 'good '(#f #f #f 1 4)))
  (define bad1  (datum->syntax #f 'bad #f))
  (test '(source 1)  'syntax/loc (f (syntax/loc good1 (x))))
  (test '(source #f) 'syntax/loc (f (syntax/loc good3 (x))))
  (test '(#f 1)      'syntax/loc (f (syntax/loc good4 (x))))
  (test #t 'syntax/loc (same-src? (syntax/loc bad1 (x)) (syntax (x))))
  ;; syntax/loc only applies loc to *new* syntax
  (with-syntax ([x #'here])
    (test #t 'syntax/loc (same-src? (syntax/loc good1 x) (syntax x))))
  (with-syntax ([(x ...) #'()] [y #'(here)])
    (test #t 'syntax/loc (same-src? (syntax/loc good1 (x ... . y)) (syntax y)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
