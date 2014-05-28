
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

(test #f syntax-original? s)
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
;; #%app, etc.

(define s (syntax-property (quote-syntax (add1 5)) 'testing 10))
(test 10 syntax-property (expand s) 'testing)

(define s (syntax-property (quote-syntax 5) 'testing 10))
(test 10 syntax-property (expand s) 'testing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check tracking of (formerly) primitive expanders

(test '(let) (tree-map syntax-e) (syntax-property (expand #'(let ([x 10]) x)) 'origin))
(test '(let*-values let*) (tree-map syntax-e) (syntax-property (expand #'(let* ([x 10]) x)) 'origin))
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

(test `('#%kernel case-lambda ,base-lib case-lambda 0 0 0)
      identifier-binding* #'case-lambda)
(test `("private/promise.rkt" delay* ,base-lib delay 0 0 0)
      identifier-binding* #'delay)
(test `('#%kernel #%module-begin ,base-lib #%plain-module-begin 0 0 0)
      identifier-binding* #'#%plain-module-begin)
(require (only-in racket/base [#%plain-module-begin #%pmb]))
(test '('#%kernel #%module-begin racket/base #%plain-module-begin 0 0 0)
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
  (test eval 'compile (eval (compile eval)))
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

(let ()
  (define-syntax name 'dummy)
  (define-syntax alias (make-rename-transformer #'name))
  (test (identifier-binding-symbol #'name)
        identifier-binding-symbol #'alias))

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
                                    (ormap (lambda (db) (bound-identifier=? db (car o))) db)))
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

(let ([expr (expand-syntax #'++v)]
      [disarm (lambda (stx)
                (syntax-disarm stx (current-code-inspector)))])
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
          [(beg id) #'beg])))

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
  (define-syntax ++u2 (make-rename-transformer (syntax-protect #'++u)))
  (define ++u 8) ; unexported
  (provide ++y ++y-macro ++y-macro2 ++u-macro ++u2))
(require '++n)

(test 10 values ++y)
(test 10 values ++y-macro)
(test 8 values ++u-macro)
(test 8 values ++u2)

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

(test (void) eval (expand #'(begin-for-syntax (define @@zoo (@@foo 2)))))
(define-syntax (@@x stx) #`#, @@zoo)
(test 2 '@@x/@@zoo @@x)
(begin-for-syntax (define @@zoo2 (@@foo 2)))
(define-syntax (@@x stx) #`#, @@zoo2)
(test 2 '@@x/@@zoo @@x)

(begin-for-syntax (@@foo 1))
(test (void) eval (expand #'(begin-for-syntax (@@foo 1))))

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
        (let ([id (syntax-local-lift-expression #`(quote #,(current-inexact-milliseconds)))])
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
             (test #f = (eval 'm) (eval 'm ns)))))])
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

(unless building-flat-tests?
  (test '(12)
        eval
        (expand
         #'(let ([b 12])
             (let-syntax ([goo (lambda (stx)
                                 #`(let ()
                                     (define #,(syntax-local-introduce #'b) 1)
                                     (define z (list b))
                                     z))])
               (goo))))))

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
                    (printf "here\n")
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
        (test #t eval `(free-identifier=? (f) (quote-syntax ,x-id)))
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
                   (expand #'(b 1)))])
    (test #f free-identifier=? #'begin (datum->syntax a-b-stx 'begin))
    (test #t free-identifier=? #'begin (syntax-case a-b-stx ()
                                         [(b . _) (datum->syntax #'b 'begin)]))))

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
  (test "#t\n#f\n" get-output-string o))

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
    #|
    (test '#(1 2 3 4 5) syntax->datum (quasisyntax #(a (unsyntax b) (unsyntax-splicing ds))))
    (test '#s(PS 1 2 3 4 5) syntax->datum
          (quasisyntax #s(PS a (unsyntax b) (unsyntax-splicing ds))))
    |#))

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

(report-errs)
