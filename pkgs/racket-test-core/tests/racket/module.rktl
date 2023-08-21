
(load-relative "loadtest.rktl")

(Section 'module)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module n racket/base
  (define n 'n)
  (define-struct s (field1 field2) #:mutable)
  (provide n
	   (struct-out s)
	   (rename-out [n m])))

(require 'n)
(test 'n 'required-n n)
(test 'n 'required-n m)

(test 's-field1 object-name s-field1)
(test 's-field2 object-name s-field2)
(test 'set-s-field1! object-name set-s-field1!)
(test 'set-s-field2! object-name set-s-field2!)
(test 's? object-name s?)
(test 7 s-field1 (make-s 7 8))
(test 8 s-field2 (make-s 7 8))
(define an-s (make-s 7 8))
(test (void) set-s-field1! an-s 12)
(test (void) set-s-field2! an-s 13)
(test 12 s-field1 an-s)
(test 13 s-field2 an-s)

(syntax-test #'(set! n 10))
(syntax-test #'(set! m 10))
(syntax-test #'(set! make-s 10))

(syntax-test #'(module))
(syntax-test #'(module m))
(syntax-test #'(module 5 racket/base))

(syntax-test #'(module m 5))

(syntax-test #'(module m racket/base . 1))

(syntax-test #'(#%module-begin))
(syntax-test #'(+ (#%module-begin) 2))

(syntax-test #'(module n+ racket/base (#%module-begin (#%module-begin (define n+ 'n+) (provide n+)))))
(syntax-test #'(module n+ racket/base (define n+ 'n+) (#%module-begin (provide n+))))
(syntax-test #'(module n+ racket/base (define n+ 'n+) (#%module-begin) (provide n+)))
(syntax-test #'(module n+ racket/base (#%module-begin) (define n+ 'n+) (provide n+)))
(module n+ racket/base (#%module-begin (define n+ 'n+) (provide n+)))

(syntax-test #'(#%declare))
(syntax-test #'(module m racket/base (#%declare something)))
(syntax-test #'(module m racket/base (#%declare "something")))
(syntax-test #'(module m racket/base (#%declare #:something)))
(syntax-test #'(module m racket/base (#%declare #:realm)))
(syntax-test #'(module m racket/base (#%declare #:unsafe #:unsafe)))
(syntax-test #'(module m racket/base (#%declare #:unsafe) (#%declare #:unsafe)))
(syntax-test #'(module m racket/base (#%declare #:realm elsewhere #:realm elsewhere)))
(syntax-test #'(module m racket/base (#%declare #:realm elsewhere) (#%declare #:realm elsewhere)))

(syntax-test #'(#%provide))
(syntax-test #'(#%provide . x))
(syntax-test #'(#%provide y . x))
(syntax-test #'(module m racket/base (define x 10) (#%provide . x)))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide y . x)))
(syntax-test #'(module m racket/base (define x 10) (#%provide 1)))
(syntax-test #'(module m racket/base (define x 10) (#%provide "bad")))
(syntax-test #'(module m racket/base (define x 10) (#%provide not-here)))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide x (rename y x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide x z)))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide x y (rename x y))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename x y z))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename not-here x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename x 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (rename 1 x))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct . x))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (y) z))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (y) . z))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct 1 ()))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (1)))))
(syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (y . 1)))))
;; (syntax-test #'(module m racket/base (define-struct x (y)) (#%provide (struct x (y y)))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from . racket/base))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from xxxx))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from racket/base x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except . racket/base))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except racket/base + . -))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except racket/base 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except xxxx +))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except racket/base no))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-from-except racket/base + no))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined . x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined-except . x))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined-except 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined-except x 1))))
(syntax-test #'(module m racket/base (define x 10) (define y 11) (#%provide (all-defined-except no))))

(syntax-test #'(#%require . x))
(syntax-test #'(#%require m . x))
(syntax-test #'(module m racket/base (#%require n . x)))
(syntax-test #'(module m racket/base (#%require (prefix))))
(syntax-test #'(module m racket/base (#%require (prefix n))))
(syntax-test #'(module m racket/base (#%require (prefix . pre:))))
(syntax-test #'(module m racket/base (#%require (prefix pre: . n))))
(syntax-test #'(module m racket/base (#%require (prefix 1 n))))
(syntax-test #'(module m racket/base (#%require (prefix pre: n more))))
(syntax-test #'(module m racket/base (#%require (prefix pre: n . more))))
(syntax-test #'(module m racket/base (#%require (all-except))))
(syntax-test #'(module m racket/base (#%require (all-except . n))))
(syntax-test #'(module m racket/base (#%require (all-except n 1))))
(syntax-test #'(module m racket/base (#%require (all-except n . n))))
(syntax-test #'(module m racket/base (#%require (rename))))
(syntax-test #'(module m racket/base (#%require (rename . n))))
(syntax-test #'(module m racket/base (#%require (rename 'n))))
(syntax-test #'(module m racket/base (#%require (rename 'n . n))))
(syntax-test #'(module m racket/base (#%require (rename 'n n))))
(syntax-test #'(module m racket/base (#%require (rename 'n n . m))))
(syntax-test #'(module m racket/base (#%require (rename 'n 1 m))))
(syntax-test #'(module m racket/base (#%require (rename 'n n 1))))
(syntax-test #'(module m racket/base (#%require (rename 'n n not-there))))
(syntax-test #'(module m racket/base (#%require (rename 'n n m extra))))

(syntax-test #'(module m racket/base (define x 6) (define x 5)))
(syntax-test #'(module m racket/base (define x 10) (define-syntax x 10)))
(syntax-test #'(module m racket/base (define-syntax x 10) (define x 10)))

;; Cyclic re-def of n:
(syntax-test #'(module n 'n 10))

;; It's now ok to shadow the initial import:
(module _shadow_ racket/base
  (define car 5)
  (provide car))

;; ... even in `#:require=define` mode
(module _shadow_initial_always_ racket/base
  (#%declare #:require=define)
  (define car 5)
  (provide car))

(test 5 dynamic-require ''_shadow_ 'car)

;; Ok to redefine imported:
(module defines-car-that-overrides-import racket/base (#%require racket/base) (define car 5) (provide car))
(module defines-car-that-overrides-import/stx racket/base (#%require racket/base (for-syntax racket/base)) (define-syntax (car stx) #'6) (provide car))
(test 5 dynamic-require ''defines-car-that-overrides-import 'car)
(test 6 dynamic-require ''defines-car-that-overrides-import/stx 'car)
;; Can't redefine multiple times or import after definition:
(syntax-test #'(module m racket/base (#%require racket/base) (define car 5) (define car 5)))

;; Not in `#:require=define` mode
(syntax-test #'(module m racket/base (#%require racket/base) (#%declare #:require=define) (define car 5)))
(syntax-test #'(module m racket/base (#%require racket/base (for-syntax racket/base)) (#%declare #:require=define) (define-syntax car 5)))
(syntax-test #'(module m racket/base (#%declare #:require=define) (define car 5) (#%require racket/base)))
(syntax-test #'(module m racket/base (#%require (for-syntax racket/base)) (#%declare #:require=define) (define-syntax car 5) (require racket/base)))
(syntax-test #'(module m racket/base (#%require (for-syntax racket/base)) (#%declare #:require=define) (define-syntax car 5) (require (only-in racket/base car))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define (do-try export import ref val)
    (parameterize ([current-namespace (make-base-namespace)])
      (eval `(module a racket/base
              (define x 'x)
              (define y 'y)
              (provide ,export)))
      (eval `(module b racket/base
              (require ,import)
              (define result ,ref)
              (provide result)))
      (test val dynamic-require ''b 'result)))
  (define-syntax try
    (syntax-rules (=>)
      [(_ export import ref => val)
       (do-try 'export 'import 'ref val)]))

  (try x 'a x => 'x)
  (try y 'a y => 'y)
  (try (combine-out x y) 'a x => 'x)
  (try (combine-out x y) 'a y => 'y)

  (try (combine-out x y) (only-in 'a x) x => 'x)
  (try (combine-out x y) (only-in 'a [x y]) y => 'x)

  (try (rename-out [x y]) 'a y => 'x)

  (try x (prefix-in a: 'a) a:x => 'x)
  (try x (prefix-in |a :| 'a) |a :x| => 'x)
  (try x (prefix-in z. (prefix-in |a :| 'a)) |z.a :x| => 'x)
  (try (prefix-out o: x) 'a o:x => 'x)
  (try (prefix-out |o :| x) 'a |o :x| => 'x)

  (try (prefix-out o: x) (prefix-in i. 'a) i.o:x => 'x)
  (try (prefix-out |o :| x) (rename-in 'a [|o :x| ex]) ex => 'x))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check namespace-attach-module:

(let* ([n (make-empty-namespace)]
       [l null]
       [here (lambda (v)
	       (set! l (cons v l)))])
  (namespace-attach-module (current-namespace) 'racket/base n)
  (parameterize ([current-namespace n])
    (namespace-require 'racket/base)
    (eval `(module a racket/base
	     (define a 1)
	     (,here 'a)
	     (provide a)))
    (test null values l)
    (eval `(module b racket/base
	     (require (for-template 'a))
	     (define b 1)
	     (,here 'b)
	     (provide b)))
    (test null values l)
    (eval `(module c racket/base
	     (require (for-template 'b))
	     (define c 1)
	     (,here 'c)
	     (provide c)))
    (test null values l)
    (eval `(module d racket/base
	     (require (for-syntax 'c))
	     (define d 1)
	     (,here 'd)
	     (provide d)))
    (test '(c) values l)
    (eval `(module e racket/base
	     (require (for-syntax 'd))
	     (define e 1)
	     (,here 'e)
	     (provide e)))
    (test '(d b c) values l)
    (eval `(module f racket/base
	     (,here 'f)
	     (require 'e 'b)))
    (test '(d b d b c) values l)
    (eval `(require 'f))
    (let ([finished '(f b e  a  d b d b c)])
      (test finished values l)
      (eval '10) ; triggers `d` and `b`
      (let ([finished (append '(d b) finished)])
        (test finished values l)
        (namespace-attach-module n ''f)
        (test finished values l)
        (parameterize ([current-namespace (make-empty-namespace)])
          (namespace-attach-module n ''f)
          (test finished values l)
          (namespace-require 'racket/base)
          (eval `(require 'a))
          (eval `(require 'f))
          (test finished values l)
          (eval '10)
          (test (list* 'd 'b finished) values l))))))

(let* ([n (make-base-namespace)]
       [l null]
       [here (lambda (v)
	       (set! l (cons v l)))])
  (parameterize ([current-namespace n])
    (eval `(module a racket/base
             (require (for-syntax racket/base)
                      (for-meta 2 racket/base))
	     (define a 1)
             (define-syntax (a-macro stx) #'-1)
             (begin-for-syntax
              (,here 'pma))
             (begin-for-syntax
              (,here 'ma)
              (define a-meta 10)
              (define-syntax (a-meta-macro stx) #'-1)
              (begin-for-syntax
               (define a-meta-meta 100)
               (,here 'mma)))
	     (,here 'a)
	     (provide a a-macro (for-syntax a-meta-macro))))
    (test '(ma mma pma) values l)
    (set! l null)
    (dynamic-require ''a #f)
    (test '(a) values l)
    (eval `10)
    (test '(a) values l)
    (dynamic-require ''a 0) ; => 'a is available...
    (eval `10)
    (test '(ma pma a) values l)
    (eval '(begin-for-syntax)) ; triggers phase-1 visit => phase-2 instantiate
    (test '(mma ma pma a) values l)
    (void)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check redundant import and re-provide

(module m_cr racket/base
  (provide x_cr y_cr z_cr w_cr)
  (define x_cr 12)
  (define y_cr 14)
  (define z_cr 16)
  (define w_cr 18))

(syntax-test #'(module n_cr racket/base
		 (require 'm_cr)
		 (#%provide (all-from-except 'm_cr no-such-var))))
(syntax-test #'(module n_cr racket/base
		 (require 'm_cr)
		 (#%provide (all-from-except 'm_cr cons))))

(module n_cr racket/base
  (require 'm_cr)
  (#%provide (all-from-except 'm_cr x_cr)))

(module p_cr racket/base
  (require 'n_cr 'm_cr)
  (#%provide (all-from 'm_cr)))

(require 'p_cr)
(test 14 values y_cr)

(module p2_cr racket/base
  (require 'm_cr 'n_cr)
  (#%provide (all-from 'm_cr)))

(require 'p2_cr)
(test 16 values z_cr)

(module p3_cr racket/base
  (require 'm_cr 'n_cr)
  (#%provide (all-from 'n_cr)))

(require 'p3_cr)
(test 18 values w_cr)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test `require' scoping

(module fake-prefix-in racket/base
  (require (for-syntax racket/base)
           racket/require-syntax)
  (define-require-syntax (pseudo-+ stx)
    (syntax-case stx ()
      [(_ id)
       #'(only-in racket/base [+ id])]))
  (provide pseudo-+))

(require 'fake-prefix-in
         (pseudo-+ ++))
(test 12 values (++ 7 5))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test binding-space support

(let ()
  (define-syntax (in-space stx)
    (syntax-case stx ()
      [(_ space id) #`(quote-syntax
                       #,((make-interned-syntax-introducer (syntax-e #'space))
                          (syntax-local-introduce (datum->syntax #f (syntax-e #'id)))))]))
  
  (define (make-soup-module #:with-default? with-default?
                            #:all-defined-out? [all-defined-out? #f])
    `(module soup-kettle racket/base
       (require (for-syntax racket/base))
       (provide (for-space soup ,(if all-defined-out?
                                     '(all-defined-out)
                                     'kettle))
                ,@(if with-default?
                      '(kettle)
                      '()))
       (define-syntax (define-soup stx)
         (syntax-case stx ()
           [(_ id rhs)
            #`(define #,((make-interned-syntax-introducer 'soup)
                         #'id)
                rhs)]))
       (define-soup kettle 'soup)
       (define kettle 'default)))

  ;; check just providing in `soup` space
  (parameterize ([current-namespace (make-base-namespace)])
    (err/rt-test/once
     (eval '(module soup-kettle racket/base
              (#%provide (for-space soup kettle))
              (define kettle 'default)))
     exn:fail:syntax?
     #rx"defined only outside the space")

    (eval (make-soup-module #:with-default? #f))
    (eval '(require 'soup-kettle))

    (err/rt-test/once (eval 'kettle) exn:fail:contract:variable?)

    (test 'soup eval (namespace-syntax-introduce (in-space soup kettle)))

    (test #t namespace? (module->namespace ''soup-kettle))
    (test (void) namespace-require '(rename 'soup-kettle also-kettle kettle))
    (test (void) namespace-require '(for-space bisque 'soup-kettle)))

  ;; check providing in `soup` and default spaces
  (parameterize ([current-namespace (make-base-namespace)])
    (eval (make-soup-module #:with-default? #t))
    (eval '(require 'soup-kettle))
    (test 'soup eval (namespace-syntax-introduce (in-space soup kettle)))
    (test 'default eval 'kettle))

  ;; check all-from-out and spaces
  (for ([with-default? '(#f #t)])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval (make-soup-module #:with-default? with-default?))
      (eval '(module also-soup-kettle racket/base
               (require 'soup-kettle)
               (provide (all-from-out 'soup-kettle))))
      (eval '(require 'also-soup-kettle))
      (test 'soup eval (namespace-syntax-introduce (in-space soup kettle)))
      (if with-default?
          (test 'default eval 'kettle)
          (err/rt-test/once (eval 'kettle) exn:fail:contract:variable?))))

    ;; check all-defined-out and spaces
  (for ([with-default? '(#f #t)])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval (make-soup-module #:with-default? with-default? #:all-defined-out? #t))
      (eval '(require 'soup-kettle))
      (test 'soup eval (namespace-syntax-introduce (in-space soup kettle)))
      (if with-default?
          (test 'default eval 'kettle)
          (err/rt-test/once (eval 'kettle) exn:fail:contract:variable?))))

  ;; check shifting spaces when both `soup` and default are provided
  (parameterize ([current-namespace (make-base-namespace)])
    (eval (make-soup-module #:with-default? #t))
    (err/rt-test/once (eval '(require (for-space bisque 'soup-kettle)))
                      exn:fail:syntax?
                      #rx"identifier already required")
    (eval '(require (for-space bisque (only-space-in soup 'soup-kettle))))
    (test 'soup eval (namespace-syntax-introduce (in-space bisque kettle)))
    (err/rt-test/once (eval 'kettle) exn:fail:contract:variable?))

  ;; check shifting phase for `soup` and default spaces
  (parameterize ([current-namespace (make-base-namespace)])
    (eval (make-soup-module #:with-default? #t))
    (eval '(require (for-syntax 'soup-kettle racket/base)))
    (err/rt-test/once (eval 'kettle) exn:fail:contract:variable?)
    (err/rt-test/once (eval (namespace-syntax-introduce (in-space bisque kettle))) exn:fail:contract:variable?)
    (eval '(define-syntax (m stx) #`(quote #,(datum->syntax #'here kettle))))
    (test 'default eval '(m))
    (eval (namespace-syntax-introduce
           (datum->syntax #f `(define-syntax (soup-m stx) #`(quote #,(datum->syntax #'here ,(in-space soup kettle)))))))
    (test 'soup eval '(soup-m)))

  ;; check that definition in the default space makes the soup-space binding ambiguous
  (for ([with-default? '(#f #t)])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval (make-soup-module #:with-default? with-default?))
      (err/rt-test/once (eval `(module m racket/base
                                 (require 'soup-kettle)
                                 (define kettle 'fish)
                                 ,(in-space soup kettle)))
                        exn:fail:syntax?
                        #rx"ambiguous")
      (err/rt-test/once (eval `(module m racket/base
                                 (require (only-in 'soup-kettle kettle)) ; non-bulk
                                 (define kettle 'fish)
                                 ,(in-space soup kettle)))
                        exn:fail:syntax?
                        #rx"ambiguous")))

  ;; same, but define before `require`
  (for ([with-default? '(#f #t)])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval (make-soup-module #:with-default? with-default?))
      (err/rt-test/once (eval `(module m racket/base
                                 (define kettle 'fish)
                                 (require 'soup-kettle)
                                 ,(in-space soup kettle)))
                        exn:fail:syntax?
                        #rx"ambiguous")
      (err/rt-test/once (eval `(module m racket/base
                                 (define kettle 'fish)
                                 (require (only-in 'soup-kettle kettle)) ; non-bulk
                                 ,(in-space soup kettle)))
                        exn:fail:syntax?
                        #rx"ambiguous")))

  ;; same idea, but `require` shadowing initial import
  (for ([with-default? '(#f #t)])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval (make-soup-module #:with-default? with-default?))
      (eval `(module soup-base racket/base
               (require 'soup-kettle)
               (provide (all-from-out racket/base)
                        (for-space soup kettle))))
      (define-values (b-vals b-stxs) (module->exports 'racket/base))
      (define-values (sb-vals sb-stxs) (module->exports ''soup-base))
      (test #t = (length b-stxs) (length sb-stxs))
      (test (length b-vals) sub1 (length sb-vals))
      (eval `(module fish-kettle racket/base
               (provide kettle)
               (define kettle 'fish)))
      (err/rt-test/once (eval `(module m 'soup-base
                                 (require 'fish-kettle)
                                 ,(in-space soup kettle)))
                        exn:fail:syntax?
                        #rx"ambiguous")
      (void)))

  (void))

;; make sure `provide` isn't confused by a rename transformer

(module should-be-an-ok-provide-for-space racket/base
  (require (for-syntax racket/base))
  (define-syntax (go stx)
    #`(begin
        (provide (for-space example_space x))
        (define x 'ok)
        (define-syntax #,((make-interned-syntax-introducer 'example_space) #'x)
          (make-rename-transformer (quote-syntax x)))))
  (go))

;; make sure `for-space #f` works

(module should-be-an-ok-provide-for-default-space racket/base
  (provide (for-space #f x))
  (define x "ok"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test proper bindings for `#%module-begin'

(define expand-test-use-toplevel? #t)

(test (void) eval
      '(begin
	 (module mod_beg2 racket/base
           (require (for-syntax racket/base))
           (#%provide (all-from-except racket/base #%module-begin))
           (#%provide (rename mb #%module-begin))
           (define-syntax (mb stx)
             (syntax-case stx ()
               [(_ . forms)
                #`(#%plain-module-begin
                   #,(datum->syntax stx '(require (for-syntax racket/base)))
                   . forms)])))
	 (module m 'mod_beg2
           3)))

(test (void) eval
      '(begin
	 (module mod_beg2 racket/base
           (require (for-syntax racket/base))
           (#%provide (all-from-except racket/base #%module-begin))
           (#%provide (rename mb #%module-begin))
           (define-syntax (mb stx)
             (syntax-case stx ()
               [(_ . forms)
                #`(#%plain-module-begin
                   #,(datum->syntax stx '(require (for-syntax racket/base)))
                   . forms)])))
	 (module m 'mod_beg2
           3 4)))

(test (void) eval
      '(begin
	 (module mod_beg2 racket/base
           (require (for-syntax racket/base))
           (#%provide (all-from-except racket/base #%module-begin))
           (#%provide (rename mb #%module-begin))
           (define-syntax (mb stx)
             (syntax-case stx ()
               [(mb . forms)
                #`(#%plain-module-begin
                   #,(datum->syntax #'mb '(require (for-syntax racket/base)))
                   . forms)])))
	 (module m 'mod_beg2
           3)))

(define expand-test-use-toplevel? #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check line between macro definition and use:

(module local-binding-produces-identity racket/base
  (provide proc)

  (define proc
    (let ()
      (define-syntax identity
        (syntax-rules ()
          [(_ misc-id)
           (lambda (x)
             (let ([misc-id 'other])
               x))]))

      (identity x))))

(test 77 (dynamic-require ''local-binding-produces-identity 'proc) 77)

(module module-binding-produces-identity racket/base
  (define-syntax identity
    (syntax-rules ()
      [(_ misc-id)
       (lambda (x)
         (let ([misc-id 'other])
           x))]))
  (identity x))

(test 79
      (let ([proc #f])
        (parameterize ([current-print (lambda (v) (set! proc v))])
          (dynamic-require ''module-binding-produces-identity #f))
        proc)
      79)

(module macro-introduced-binding-produces-identity racket/base
  (define-syntax-rule (gen)
    (begin
      (define-syntax identity
        (syntax-rules ()
          [(_ misc-id)
           (lambda (x)
             (let ([misc-id 'other])
               x))]))
      (identity x)))
  (gen))

(test 78
      (let ([proc #f])
        (parameterize ([current-print (lambda (v) (set! proc v))])
          (dynamic-require ''macro-introduced-binding-produces-identity #f))
        proc)
      78)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([f1 (make-temporary-file)]
      [f2 (make-temporary-file)]
      [exn:fail-cycle? (lambda (exn)
                         (and (exn:fail? exn)
                              (regexp-match? #rx"cycle" (exn-message exn))))])
  (let-values ([(b1 tmp1 mbd1?) (split-path f1)]
               [(b2 tmp2 mbd2?) (split-path f2)])

  (with-output-to-file f1
    #:exists 'truncate/replace
    (lambda ()
      (write `(module ,(string->symbol (path->string tmp1)) racket/base (require (file ,(path->string f2)))))))
  (with-output-to-file f2
    #:exists 'truncate/replace
    (lambda ()
      (write `(module ,(string->symbol (path->string tmp2)) racket/base (require (file ,(path->string f1)))))))
  (err/rt-test (dynamic-require f1 #f) exn:fail-cycle?)
  (err/rt-test (dynamic-require f2 #f) exn:fail-cycle?)
  (delete-file f1)
  (delete-file f2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #t module-path? "hello")
(test #t module-path? "hello.rkt")
(test #f module-path? "hello*ss")
(test #t module-path? "hello%2ess")
(test #t module-path? "hello%00ss")
(test #f module-path? "hello%2Ess")
(test #f module-path? "hello%41ss")
(test #f module-path? "hello%4")
(test #f module-path? "hello%")
(test #f module-path? "hello%q0")
(test #f module-path? "hello%0q")
(test #f module-path? "foo.rkt/hello")
(test #f module-path? "foo/")
(test #f module-path? "a/foo/")
(test #f module-path? "/foo.rkt")
(test #f module-path? "/a/foo.rkt")
(test #f module-path? "a/foo.rkt/b")
(test #t module-path? "a/foo%2ess/b")
(test #t module-path? "a/_/b")
(test #t module-path? "a/0123456789+-_/b.---")
(test #t module-path? "a/0123456789+-_/b.-%2e")
(test #t module-path? "../foo.rkt")
(test #t module-path? "x/../foo.rkt")
(test #t module-path? "x/./foo.rkt")
(test #t module-path? "x/.")
(test #t module-path? "x/..")
(test #f module-path? "@")
(test #f module-path? "\0")
(test #f module-path? "x@")
(test #f module-path? "x\0")
(test #f module-path? "@x")
(test #f module-path? "\0x")


(test #t module-path? (collection-file-path "module.rktl" "tests" "racket"))
(test #t module-path? (string->path "x"))

(test #t module-path? 'hello)
(test #f module-path? 'hello/)
(test #f module-path? 'hello.rkt)
(test #t module-path? 'hello%2ess)
(test #f module-path? 'hello%2Ess)
(test #f module-path? 'hello/a.rkt)
(test #f module-path? '/hello/a.rkt)
(test #f module-path? '/hello)
(test #f module-path? '/a/hello)
(test #f module-path? 'a//hello)
(test #f module-path? '../hello)
(test #f module-path? './hello)
(test #f module-path? 'a/../hello)
(test #f module-path? 'b/./hello)
(test #f module-path? 'b/*/hello)

(test #t module-path? '(lib "hello"))
(test #f module-path? '(lib "hello/"))
(test #f module-path? '(lib "hello/../b"))
(test #t module-path? '(lib "hello/a"))
(test #t module-path? '(lib "hello/a.rkt"))
(test #f module-path? '(lib "hello.bb/a.rkt"))
(test #f module-path? '(lib "/hello/a.rkt"))
(test #t module-path? '(lib "hello/a.rkt" "ack"))
(test #t module-path? '(lib "hello/a.rkt" "ack" "bar"))
(test #t module-path? '(lib "hello/a.rkt" "ack/bar"))
(test #f module-path? '(lib "hello/a.rkt" "ack/"))
(test #f module-path? '(lib "hello/a.rkt" "ack" "/bar"))
(test #f module-path? '(lib "hello/a.rkt" "ack" ".."))
(test #f module-path? '(lib "hello/a.rkt" "ack" bar))
(test #f module-path? '(lib "hello/a.rkt"  . bar))
(test #f module-path? '(lib . "hello/a.rkt"))
(test #f module-path? '(lib))

(test #f module-path? '(planet))
(test #f module-path? '(planet robby))
(test #t module-path? '(planet robby/redex))
(test #t module-path? '(planet robby%2e/%2eredex))
(test #f module-path? '(planet robby%2/redex))
(test #f module-path? '(planet robby/redex%2))
(test #f module-path? '(planet robby/redex/))
(test #f module-path? '(planet robby/redex/foo/))
(test #f module-path? '(planet /robby/redex/foo))
(test #f module-path? '(planet robby/redex.plt/foo))
(test #f module-path? '(planet robby/redex/foo.rkt))
(test #f module-path? '(planet robby/redex/foo.rkt/bar))
(test #f module-path? '(planet robby/../foo))
(test #t module-path? '(planet robby/redex/foo))
(test #t module-path? '(planet robby/redex/foo/bar))
(test #t module-path? '(planet robby/redex:7/foo))
(test #t module-path? '(planet robby/redex:7))
(test #t module-path? '(planet robby/redex:7:8/foo))
(test #t module-path? '(planet robby/redex:7:<=8/foo))
(test #t module-path? '(planet robby/redex:7:>=8/foo))
(test #t module-path? '(planet robby/redex:7:8-9/foo))
(test #t module-path? '(planet robby/redex:7:8-9))
(test #t module-path? '(planet robby/redex:700:800-00900/foo))
(test #t module-path? '(planet robby/redex:700:800-00900/foo%2e))
(test #f module-path? '(planet robby/redex:=7/foo))
(test #f module-path? '(planet robby/redex::8/foo))
(test #f module-path? '(planet robby/redex:7:/foo))
(test #f module-path? '(planet robby/redex.plt:7:8/foo))
(test #f module-path? '(planet robby/redex:a/foo))
(test #f module-path? '(planet robby/redex:7:a/foo))
(test #f module-path? '(planet robby/redex:7:a-10/foo))
(test #f module-path? '(planet robby/redex:7:10-a/foo))

(test #f module-path? '(planet "foo.rkt"))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt")))
(test #f module-path? '(planet "../foo.rkt" ("robby" "redex.plt")))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt" 7 (7 8))))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt" 7 8)))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt" 7 (= 8))))
(test #t module-path? '(planet "foo.rkt" ("robby" "redex.plt") "sub" "deeper"))
(test #t module-path? '(planet "foo%2e.rkt" ("robby%2e" "redex%2e.plt") "sub%2e" "%2edeeper"))

(err/rt-test (make-resolved-module-path "not good"))
(err/rt-test (resolved-module-path-name "not good"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check `relative-in'

(let ([check
       (lambda (path)
         (parameterize ([current-namespace (make-base-namespace)])
           (eval
            `(module relative-in-test racket/base
               (require ,path)
               (provide x)
               (define x (string-join '("a" "b" "c") "."))))
           (test "a.b.c" dynamic-require ''relative-in-test 'x)))])
  (check 'racket/string)
  (check '(relative-in racket/delay "string.rkt"))
  (check '(relative-in racket "string.rkt"))
  (check '(relative-in (lib "racket/main.rkt") "string.rkt"))
  (check '(relative-in (lib "racket") "string.rkt"))
  (check '(relative-in (lib "main.rkt" "racket") "string.rkt"))
  (check `(relative-in ,(collection-file-path "promise.rkt" "racket") "string.rkt"))
  (check '(relative-in racket (relative-in "private/reqprov.rkt" "../string.rkt"))))


(module module-that-defines-submodule-named-a racket/base
  (module a racket/base
    (define a 'a)
    (provide a)))

(module module-that-uses-submodule-named-a racket/base
  (require (relative-in (submod 'module-that-defines-submodule-named-a a) (submod ".." a)))
  (define got-a a)
  (provide got-a))

(test 'a dynamic-require ''module-that-uses-submodule-named-a 'got-a)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check collection-path details

(test-values '(not there) (lambda ()
                            (collection-path "nonesuch"
                                             #:fail (lambda (s)
                                                      (test #t string? s)
                                                      (values 'not 'there)))))
(test-values '(1 2) (lambda ()
                      (collection-file-path "none.rkt" "nonesuch"
                                       #:fail (lambda (s)
                                                (test #t string? s)
                                                (values 1 2)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check 'module-language, `module-compiled-language-info', and `module->language-info'

(let ([mk (lambda (val)
            (compile (syntax-property #'(module m racket/base)
                                      'module-language
                                      val)))])
  (test #f 'info (module-compiled-language-info (mk 10)))
  (test '#(racket x "whatever") 'info (module-compiled-language-info (mk '#(racket x "whatever"))))
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (eval mk ns)
      (eval (mk '#(racket x "whatever")))
      (test '#(racket x "whatever") module->language-info ''m)
      (let ([path (build-path (collection-path "tests" "racket")
                              "langm.rkt")])
        (parameterize ([read-accept-reader #t]
                       [current-module-declare-name (module-path-index-resolve
                                                     (module-path-index-join path #f))])
          (eval
           (read-syntax path
                        (open-input-string "#lang tests/racket (provide x) (define x 1)"
                                           path)))
          ((current-module-name-resolver) (current-module-declare-name) #f)))
      (test '#(tests/racket/lang/getinfo get-info closure-data)
            module->language-info 'tests/racket/langm))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The default module name resolver invents an uninterned symbol as a
;; module name when resolving a submodule for a base path where the
;; *collection* can't even be found for making a potential path name.

(let ([m (module-path-index-resolve
          (module-path-index-join '(submod no-such-collection/x nested) #f))])
  (test #f symbol-interned? (car (resolved-module-path-name m)))
  (test '(nested) cdr (resolved-module-path-name m)))

(test #f module-declared? '(submod no-such-collection/x nested) #t)

;; don't call the resolver in this case:
(err/rt-test (module-path-index-resolve (module-path-index-join #f #f))
             exn:fail:contract?
             #rx"^module-path-index-resolve")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide a source-location syntax object to `module-path-index-resolve`

(let ([bad #'no-such-collection-based-module])
  (err/rt-test (module-path-index-resolve (module-path-index-join (syntax-e bad) #f)
                                          #t
                                          bad)
               exn:fail:syntax:missing-module?))

(let ([bad #'no-such-collection-based-module])
  (err/rt-test (module-path-index-resolve (module-path-index-join (syntax-e bad) #f)
                                          #t)
               exn:fail:filesystem:missing-module?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check shadowing of initial imports:

(let ([m-code '(module m racket/base (define-syntax-rule (lambda . _) 5) (provide lambda))]
      [n-code '(module n racket/base
                 (require 'm)
                 (define five (lambda (x) x))
                 (define five-stx #'lambda)
                 (provide five five-stx))]
      [p-code '(module p racket/base
                 (require 'n)
                 (define same? (free-identifier=? #'lambda five-stx))
                 (provide same?))])
  (let ([ns (make-base-namespace)])
    (eval m-code ns)
    (eval '(require 'm) ns)
    (test 5 eval '(lambda (x) x) ns)
    (let ([m-ns (eval '(module->namespace ''m) ns)])
      (test 5 eval '(lambda (x) x) m-ns))
    (eval n-code ns)
    (eval '(require 'n) ns)
    (test 5 eval 'five ns)
    (eval p-code ns)
    (eval '(require 'p) ns)
    (test #f eval 'same? ns)
    (let ([n-ns (eval '(module->namespace ''n) ns)])
      (test 5 eval '(lambda (x) x) n-ns)))
  (let ([ns (make-base-namespace)])
    (eval m-code ns)
    (let ([n-zo (let ([s (open-output-bytes)])
                  (parameterize ([current-namespace ns])
                    (write (compile n-code) s))
                  (parameterize ([read-accept-compiled #t])
                    (read (open-input-bytes (get-output-bytes s)))))])
      (eval n-zo ns)
      (eval '(require 'n) ns)
      (test 5 eval 'five ns)
      (eval p-code ns)
      (eval '(require 'p) ns)
      ; (test #f eval 'same? ns)
      (let ([n-ns (eval '(module->namespace ''n) ns)])
        (test 5 eval '(lambda (x) x) n-ns)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check shadowing when `define` precedes `require`

(module definition-shadows-later-require racket/base
  (provide result)
  (define first "last")
  (require racket/list)
  (define result first))

(test "last" dynamic-require ''definition-shadows-later-require 'result)

(module definition-shadows-later-require/rename racket/base
  (provide result)
  (define first "last")
  (require (rename-in racket/function
                      [curry first]))
  (define result first))

(test "last" dynamic-require ''definition-shadows-later-require/rename 'result)

(module definition-shadows-later-require/2 racket/base
  (provide result)
  (define first "last")
  (require racket/list)
  (require racket/list)
  (define result first))

(test "last" dynamic-require ''definition-shadows-later-require/2 'result)

(module definition-shadows-require-shadowing-initial-require racket/base
  (provide result)
  (define version 42)
  (module M racket/base
    (provide (all-defined-out))
    (define version 42))
  (require 'M)
  (define result version))

(test 42 dynamic-require ''definition-shadows-require-shadowing-initial-require 'result)

(err/rt-test
 (eval
  '(module m racket/base
     (define first "last")
     (require racket/list)
     ;; late `require` collision:
     (require (rename-in racket/function
                         [curry first]))))
 exn:fail:syntax?)

(err/rt-test
 (eval
  '(module m racket/base
     (require racket/list)
     (define first "last")
     ;; late `require` collision:
     (require (rename-in racket/function
                         [curry first]))))
 exn:fail:syntax?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check printing of resolved module paths

(let ([s (open-output-string)])
  (print (make-resolved-module-path (build-path (current-directory) "a.rkt")) s)
  (test #t regexp-match? #rx"<resolved-module-path:\"" (get-output-string s)))
(let ([s (open-output-string)])
  (print (make-resolved-module-path 'other) s)
  (test #t regexp-match? #rx"<resolved-module-path:'" (get-output-string s)))

(let ([s (open-output-string)])
  (print (module->namespace 'racket/base) s)
  (test #t regexp-match? #rx"<namespace:\"" (get-output-string s)))
(let ([s (open-output-string)])
  (print (module->namespace ''n) s)
  (test #t regexp-match? #rx"<namespace:'" (get-output-string s)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check "source" name of built-in module:

(parameterize ([current-namespace (module->namespace ''#%network)])
  (test '#%network
        variable-reference->module-source
        (eval (datum->syntax #'here '(#%variable-reference)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check handling of unbound names in local-expand:

(err/rt-test (expand '(module m racket
                        (require racket/require)
                        (require (filtered-in (lambda (n) foo) racket))))
             exn:fail:contract:variable?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `quote' can be renamed for use in
;; require specs

(parameterize ([current-namespace (make-base-namespace)])
  (map
   eval
   '((module service racket
       (#%module-begin
        (module s racket/base)))

     (module good-client racket
       (#%module-begin
        (require (quote service))))

     (module another-good-client racket
       (#%module-begin
        (require
         (rename-in racket/base
                    [quote dynamic-in]))
        (require
         (dynamic-in service))))

     (module also-good-client racket
       (#%module-begin
        (require
         (rename-in racket/base
                    [quote dynamic-in]))
        (require
         (rename-in (dynamic-in service)))))

     (module submodule-good-client racket
       (#%module-begin
        (require
         (rename-in racket/base
                    [quote dynamic-in]))
        (require
         (rename-in (submod (dynamic-in service) s)))))

     (module another-submodule-good-client racket
       (#%module-begin
        (require
         (rename-in racket/base
                    [quote dynamic-in]
                    [submod alt:submod]))
        (require
         (rename-in (alt:submod (dynamic-in service) s))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check phase-1 syntax used via for-template
;; and other indirections

(module there-and-back-x racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
   (provide s s?)
   (struct s (x y))))

(module there-and-back-y racket/base
  (require (for-template 'there-and-back-x))
  (s 1 2)
  (provide s s?))

(module there-and-back-z racket/base
  (require 'there-and-back-y)
  (provide f)
  (define (f) (s 1 2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check printing of an error message:

(err/rt-test (eval '(module bad-module '#%kernel
                      (#%require (for-meta -1 (only racket make-base-namespace) (only scheme make-base-namespace)))))
             (lambda (exn) (regexp-match? #rx"phase -1" (exn-message exn))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check renames and lifts:

(module post-ex-rename-example-1 racket/base
  (require (for-syntax racket/base))
  (provide go)
  (define-syntax (go stx)
    (syntax-local-lift-module-end-declaration
     #'(define-stuff))
    #'(define-syntax (define-stuff stx)
        #'(define x #f))))

(module post-ex-rename-example-2 racket/base
  (require 'post-ex-rename-example-1)
  (go))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check interaction of binding-context and mark:

(module binding-context-a racket
  (provide q)

  (define-syntax (q stx)
    (syntax-case stx ()
      [(_ f) (with-syntax ([x (syntax-local-introduce #'x)])
               #'(f x))])))

(module binding-context-b racket
  (require 'binding-context-a)

  (define-syntax-rule (go id)
    (begin
      (define id 5)
      (define-syntax-rule (prov)
        (provide id))
      (prov)))

  (q go))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check modidx for 'origin items

(syntax-case (parameterize ([current-namespace (make-base-namespace)])
               (expand
                '(module m racket/base
                   (define-syntax-rule (m x) 1)
                   (m x)))) ()
  [(_ name lang (mb rc ds (app cwv (lam () (qt one)) pnt)))
   (begin
     (test 1 syntax-e #'one)
     (test #t identifier? (car (syntax-property #'one 'origin)))
     (test #t symbol?
           (resolved-module-path-name
            (module-path-index-resolve
             (car (identifier-binding (car (syntax-property #'one 'origin))))))))])

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that set! of an unbound for-syntax variable is a syntax error

(err/rt-test (expand '(module m racket/base
                        (require (for-syntax racket/base))
                        (begin-for-syntax
                         (lambda () (set! x 6)))))
             exn:fail:syntax?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that an exception during a `provide' expansion
;; doesn't leave the thread in the during-expansion state:

(with-handlers ([exn? void])
  (eval '(module m racket
           (require (for-syntax racket/provide-transform))
           (define-syntax ex
             (make-provide-transformer
              (lambda args
                (/ 0))))
           (provide (ex)))))

(err/rt-test (eval '(define-syntax m (syntax-local-module-defined-identifiers))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that invocation order matches `require' order:

(module order-check-module-a racket/base 'a)
(module order-check-module-b racket/base 'b)
(module order-check-module racket/base (require 'order-check-module-a
                                                'order-check-module-b))
(let ([o (open-output-string)])
  (parameterize ([current-output-port o])
    (dynamic-require ''order-check-module #f))
  (test "'a\n'b\n" get-output-string o))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check phase-shifted, compile-time use of `variable-reference->namespace'

(module uses-variable-reference->namespace racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
   (variable-reference->namespace (#%variable-reference))))
(module uses-uses-variable-reference->namespace racket/base
  (require (for-template 'uses-variable-reference->namespace)))

(require 'uses-uses-variable-reference->namespace)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check reference to phase-2 definition:

(let ()
  (define m1-expr
    '(module m1 racket/base
       (require (for-syntax racket/base))
       (begin-for-syntax
        (require (for-syntax racket/base))
        (begin-for-syntax
         (define m1 2)
         (provide m1)))))

  (define m2-expr
    '(module m2 racket/base
       (require (for-meta -2 'm1))
       m1))

  (parameterize ([current-namespace (make-base-namespace)])
    (eval m1-expr)
    (eval m2-expr))

  (parameterize ([current-namespace (make-base-namespace)])
    (define (compile-eval e)
      (define-values (i o) (make-pipe))
      (write (compile e) o)
      (parameterize ([read-accept-compiled #t])
        (eval (read i))))
    (compile-eval m1-expr)
    (compile-eval m2-expr)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disabling module constants

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(module m racket/base (define x 1) (provide x)))
  (eval '(require 'm))
  (err/rt-test (eval '(module m racket/base (define x 2) (provide x)))
               exn:fail:contract:variable?))

(parameterize ([current-namespace (make-base-namespace)]
               [compile-enforce-module-constants #f])
  (eval '(module m racket/base (define x 1) (provide x)))
  (eval '(require 'm))
  (eval '(module m racket/base (define x 2) (provide x)))
  (test 2 eval 'x))

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(module m racket/base (define x 1) (provide x)))
  (eval '(require 'm))
  (parameterize ([compile-enforce-module-constants #f])
    (err/rt-test (eval '(module m racket/base (define x 2) (provide x)))
                 exn:fail:contract:variable?)))

(parameterize ([current-namespace (make-base-namespace)])
  (parameterize ([compile-enforce-module-constants #f])
    (eval '(module m racket/base (eq? y y) (define y 2))))
  (err/rt-test/once (eval '(dynamic-require ''m #f))
                    exn:fail:contract:variable?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check JIT treatement of seemingly constant imports

(let ()
  (define (a-expr mut?)
    `(module a racket/base
       (#%printing-module-begin
        ,(if mut?
             `(define a 5)
             `(define (a x)
                ;; long enough to not be inlined:
                (list x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)))
        (provide a))))
  (define b-expr
    `(module b racket/base
       (#%printing-module-begin
        (require 'a)
        (define (b q) (a q))
        (provide b))))

  (define (compile-m e strs)
    (parameterize ([current-namespace (make-base-namespace)])
      (for ([str (in-list strs)])
        (parameterize ([read-accept-compiled #t])
          (eval (read (open-input-bytes str)))))
      (define o (open-output-bytes))
      (write (compile e) o)
      (define s (get-output-bytes o))
      (define vlen (bytes-ref s 2))
      (define vmlen (bytes-ref s (+ 3 vlen)))
      ;; Add a hash, so that loading this module in two contexts tries to
      ;; use the same loaded bytecode and same JIT-generated code:
      (bytes-copy! s (+ 5 vlen vmlen)
                   (subbytes
                    (bytes-append (string->bytes/utf-8 (format "~s" (bytes-length s)))
                                  (make-bytes 20 0))
                    0
                    20))
      s))

  (define a-s (compile-m (a-expr #f) '()))
  (define am-s (compile-m (a-expr #t) '()))
  (define b-s (compile-m b-expr (list a-s)))

  (define temp-dir (make-temporary-file "comp~a" 'directory))
  (define dir (build-path temp-dir (car (use-compiled-file-paths))))
  (make-directory* dir)

  (define (go a-s)
    (parameterize ([current-namespace (make-base-namespace)]
                   [read-accept-compiled #t])
      (eval (read (open-input-bytes a-s)))
      (with-output-to-file (build-path dir "check-gen_rkt.zo")
        #:exists 'truncate
        (lambda () (write-bytes b-s)))
      ((dynamic-require (build-path temp-dir "check-gen.rkt") 'b) 10)))
  ;; Trigger JIT generation with constant function as `a':
  (go a-s)
  ;; Check that we don't crash when trying to use a different `a':
  (err/rt-test (go am-s) exn:fail?)
  ;; Cleanup
  (delete-directory/files temp-dir))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test 5
      'm->n
      (parameterize ([current-namespace (make-base-namespace)])
        (eval '(module m racket/base (define x 5) (provide (protect-out x))))
        (eval '(module n racket/base (require 'm)))
        (eval '(require 'n))
        (parameterize ([current-namespace (module->namespace ''n)])
          (eval 'x))))

(test #t
      'ffi/unsafe->n
      (parameterize ([current-namespace (make-base-namespace)])
        (eval '(module n racket/base (require ffi/unsafe)))
        (eval '(require 'n))
        (parameterize ([current-namespace (module->namespace ''n)])
          (eval '(procedure? ptr-set!)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check link checking and a constructor with auto fields:

(module a-with-auto-field racket/base
  (provide make-region)
  (define-values (struct:region make-region region? region-get region-set!)
    (make-struct-type 'region #f 6 6 #f)))

(module use-a-with-auto-field racket/base
  (require 'a-with-auto-field)
  (void (make-region 1 2 3 4 5 6)))

(require 'use-a-with-auto-field)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check that `require' inside `begin-for-syntax' sets up the right phase dependency

(let ([o (open-output-bytes)])
  (parameterize ([current-output-port o]
                 [current-namespace (make-base-namespace)])
    (eval
     '(module m racket/base
        (printf "~s\n" (variable-reference->phase (#%variable-reference)))))
    (eval
     '(module n racket/base
        (require (for-syntax racket/base))
        (begin-for-syntax
         (require 'm))))
    (eval '(require 'n))
    (eval '10))
  (test #"1\n1\n" get-output-bytes o))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check re-expansion of a module with a macro-introduced `only-in'
;; and a definition of the name that `only-in' switched away from:

(parameterize ([current-namespace (make-base-namespace)])
  (define src
    '(module m racket
       (define-syntax (req stx)
         (syntax-case stx ()
           [(_ spec)
            (let ()
              (with-syntax {[name (datum->syntax #'spec 'enqueue!)]}
                #'(begin
                    (require (rename-in spec [name temp]))
                    (define-syntax name 10))))]))

       (req (only-in data/queue enqueue!))))
  (expand-syntax (expand src)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; catch errors due to a module that is not available

(module avail-z racket/base
  (provide foo)
  (define-syntax-rule (foo x) x))

(module avail-y racket/base
  (require 'avail-z)
  (eval-syntax #'(foo 10)))

(err/rt-test/once (dynamic-require ''avail-y #f)
                  (lambda (exn) (and (exn? exn)
                                     (regexp-match? #rx"module that is not available"
                                                    (exn-message exn)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a `syntax-local-ift-require' into a top-level context
;; appropriately forces a visit of compile-time code:

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(module m racket/base
           (provide x)
           (define-syntax-rule (x) 5)))
  (eval '(require (for-syntax racket/base)))
  (eval '(define-syntax (m stx)
           (syntax-local-lift-require ''m (datum->syntax stx '(x)))))
  (eval '(m)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that local-expanding module body
;; doesn't pollute future expansion with
;; bindings in any phase (such as phase 2)

(module check-defn-le-lang racket
   (provide
    (except-out (all-from-out racket) #%module-begin)
    (rename-out [module-begin #%module-begin]))

   (define-syntax (module-begin stx)
     (syntax-case stx ()
       ((_ . bs)
        (local-expand
         #'(#%module-begin . bs)
         'module-begin null)))))

(module check-defn-le-module 'check-defn-le-lang
   (require (for-meta 2 racket/base))
   (define x 0)
   (begin-for-syntax
     (begin-for-syntax
       (define y 2))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `dynamic-require` re-export fast path

(for ([specs (list (list '(provide)
                         'x)
                   (list '(provide x)
                         'x)
                   (list '(provide (rename-out [x xx]))
                         'x)
                   (list '(provide (rename-out [y x]))
                         'x)
                   (list '(provide)
                         '(rename-out [y x])
                         "y")
                   (list '(provide)
                         '(rename-out [z x])
                         "x"
                         ;; slow:
                         "exp\nexp\nrun\nexp\n"))])
  (define ns (make-base-namespace))
  (define o (open-output-string))
  (parameterize ([current-output-port o])
    (eval `(module m racket/base
             (require (for-syntax racket/base))
             (begin-for-syntax (displayln "exp"))
             (define x "x")
             (define y "y")
             (define-syntax (z stx) #'x)
             ,(car specs)
             (module* sub #f
               (displayln "run")
               (provide ,(cadr specs))))
          ns))
  (define expected (if (null? (cddr specs)) "x" (caddr specs)))
  (define expected-out (if ((length specs) . < . 4)
                           "exp\nexp\nrun\n"
                           (list-ref specs 3)))
  (define (dynamic-require/o m x)
    (parameterize ([current-output-port o])
      (dynamic-require m x)))
  (parameterize ([current-namespace ns])
    (test expected dynamic-require/o '(submod 'm sub) 'x)
    (test expected dynamic-require/o '(submod 'm sub) 'x))
  (test expected-out get-output-string o))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check handling of module contexts that are kept
;; only for the context's identity (where sealing
;; could be mishandled)

(let ()
  (define m '(module m racket
               (provide (all-defined-out) def)
               (define-syntax def (make-rename-transformer #'define))))
  (define c #f)
  (sync (thread ; thread isolates `errortrace` parameter side effects
         (lambda ()
           (parameterize ([current-namespace (make-base-namespace)])
             (namespace-require 'errortrace)
             (set! c (compile m))))))
  (write c (open-output-bytes)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that the prompt around a module definitions works and allows
;; assignment to an otherwise mutable variable:

(module assigns-to-variable-through-a-continuation racket/base
  (provide result)
  (define x (let/cc k k))
  (set! x x)
  (x 5)
  (define result x))

(test 5 dynamic-require ''assigns-to-variable-through-a-continuation 'result)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that the prompt around a module definitions does not allow
;; assignment to an otherwise constant binding.

(module tries-to-assign-to-variable-through-a-continuation racket/base
  (define x (let/cc k k))
  (x 5))

(err/rt-test/once (dynamic-require ''tries-to-assign-to-variable-through-a-continuation #f)
                  exn:fail:contract:variable?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that skipping definitions (but continuing
;; with the rest of a module body) is disallowed.

(module disallowed-definition-avoider racket/base

  (define fail
    ((call-with-continuation-prompt
      (lambda ()
        (call/cc values)))))

  (error "no"))

(err/rt-test/once (dynamic-require ''disallowed-definition-avoider #f)
                  exn:fail:contract:variable?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `syntax-local-lift-require` works interactively
;; with a namespace from `module->namespace`:
(let ()
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (eval '(module m racket/base
             (require (for-syntax racket/base))
             (define-syntax (m stx)
               (syntax-case stx ()
                 [(_)
                  (syntax-local-introduce
                   (syntax-local-lift-require
                    'racket/list
                    (datum->syntax stx 'empty)))]))))
    (eval '(require 'm))
    (parameterize ([current-namespace (module->namespace ''m)])
      (eval '(m)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a submodule can be armed:

(test #t
      syntax?
      (expand
       (expand
        #'(module m racket/base
            (define-syntax-rule (s) (module x racket/base 10))
            (s)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check interaction of marks and a syntax-object side channel

;; Tests a special case that makes a reference to an identifier in
;; the enclosing module work, even though the identifier is missing
;; a module context.

#|

I think this was a bad idea. It's trying to make generated identifiers
"just work", but the hack to provide this behavior only covered the
case of module-leve bindings; it doesn't cover local bindings.

(let ()
  (define (mk mode wrap?)
    `(module m racket
       (require (for-syntax syntax/parse racket/syntax))
       (define-for-syntax funny #f)
       (define-syntax (make-funny-set! stx)
         (syntax-parse stx
           [(_ v)
            (define unmarked (generate-temporary))
            (set! funny (syntax-local-introduce unmarked))
            #`(define #,unmarked v)]))
       (define-syntax (funny-ref stx)
         (syntax-parse stx
           [(_)
            funny]))
       (define-syntax (funny-set! stx)
         (syntax-parse stx
           [(_ v)
            #`(set! #,funny v)]))
       (define-syntax (funny-varref stx)
         (syntax-parse stx
           [(_)
            #`(#%variable-reference #,funny)]))
       (make-funny-set! 2)
       ,((if wrap? (lambda (v) `(let () ,v)) values)
         (case mode
           [(ref) '(funny-ref)]
           [(set) '(funny-set! 3)]
           [(var) '(funny-varref)]))))
  (for* ([m '(ref set var)]
         [wrap? '(#t #f)])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval (mk m wrap?)))))

|#

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that module caching doesn't cause submodules
;; to be loaded/declared too early

(define (install-module-hashes! s start len c)
  (define vlen (bytes-ref s (+ start 2)))
  (define vslen (bytes-ref s (+ start 3 vlen)))
  (define mode (integer->char (bytes-ref s (+ start 4 vlen vslen))))
  (case mode
    [(#\B)
     (define h (make-bytes 20 (+ 42 c)))
     (bytes-copy! s (+ start 5 vlen vslen) h)]
    [(#\D)
     (define (read-num rel-pos)
       (define pos (+ start rel-pos))
       (integer-bytes->integer s #t #f pos (+ pos 4)))
     (define count (read-num (+ 5 vlen vslen)))
     (for/fold ([pos (+ 9 vlen vslen)]) ([i (in-range count)])
       (define pos-pos (+ pos 4 (read-num pos)))
       (define mod-start (read-num pos-pos))
       (define mod-len (read-num (+ pos-pos 4)))
       (install-module-hashes! s (+ start mod-start) mod-len i)
       (+ pos-pos 16))
     (void)]
    [else (error "unknown")]))

(let ()
  (define dir (make-temporary-file "tmx~a" 'directory))
  (define tmx (build-path dir "tmx.rkt"))
  (define e (compile '(module tmx racket/base
                        (module s racket/base
                          (provide x)
                          (define x 1)))))
  (make-directory* (build-path dir  (car (use-compiled-file-paths))))
  (define zo-path (build-path dir  (car (use-compiled-file-paths)) "tmx_rkt.zo"))

  (define bstr
    (let ([b (open-output-bytes)])
      (write e b)
      (let* ([s (get-output-bytes b)])
        (install-module-hashes! s 0 (bytes-length s) 0)
        s)))

  (call-with-output-file zo-path
    #:exists 'truncate
    (lambda (o)
      (write-bytes bstr o)))
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-module-declare-name (make-resolved-module-path tmx)]
                 [current-load-relative-directory dir])
    (eval (parameterize ([read-accept-compiled #t])
            (read (open-input-bytes bstr)))))
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require tmx #f)
    (test #f module-declared? `(submod ,tmx s) #f)
    (test 1 dynamic-require `(submod ,tmx s) 'x))
  (delete-directory/files dir))

;; Check that module-code caching works
(let ()
  (define dir (make-temporary-file "tmx~a" 'directory))
  (define tmx (build-path dir "tmx2.rkt"))
  (define e (compile '(module tmx2 racket/kernel
                        (#%provide x)
                        (define-values (x) 1))))
  (make-directory* (build-path dir (car (use-compiled-file-paths))))
  (define zo-path (build-path dir (car (use-compiled-file-paths)) "tmx2_rkt.zo"))

  (define bstr
    (let ([b (open-output-bytes)])
      (write e b)
      (let* ([s (get-output-bytes b)])
        (install-module-hashes! s 0 (bytes-length s) 100)
        s)))

  (call-with-output-file zo-path
    #:exists 'truncate
    (lambda (o)
      (write-bytes bstr o)))
  (define first-namespace (make-base-namespace))
  (parameterize ([current-namespace first-namespace]
                 [current-module-declare-name (make-resolved-module-path tmx)]
                 [current-load-relative-directory dir])
    (eval (parameterize ([read-accept-compiled #t])
            (read (open-input-bytes bstr)))))
  ;; Mangle the bytecode file; cached variant should be used:
  (call-with-output-file zo-path
    #:exists 'update
    (lambda (o)
      (file-position o (- (file-size zo-path) 100))
      (write-bytes (make-bytes 100 (char->integer #\!)) o)))

  (test 2 add1
        (parameterize ([current-namespace (make-base-namespace)]
                       [current-load-relative-directory dir])
          (dynamic-require tmx 'x)))
  (delete-file zo-path)

  ;; Need to retain the namespace until here
  (ephemeron-value (make-ephemeron first-namespace 7) first-namespace)

  (delete-directory/files dir))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `provide` doesn't run enclosed expanders until within a
;; module (as opposed to a `#%module-begin` expansion):

(module check-contract-out-by-itself racket (provide (contract-out)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `local-require` in a compile-time position:

(module provides-a-for-local-require racket/base
  (define a 1)
  (provide a))

(module uses-a-in-macro-rhs racket/base
  (require (for-syntax racket/base))
  (provide one)

  (define-syntax (m stx)
    (local-require 'provides-a-for-local-require)
    #`#,a)

  (define one (m)))

(test 1 dynamic-require ''uses-a-in-macro-rhs 'one)

(module uses-a-in-begin-for-syntax racket/base
  (require (for-syntax racket/base))
  (provide one)

  (begin-for-syntax
    (define one-ct
      (let ()
        (local-require 'provides-a-for-local-require)
        a)))

  (define-syntax (m stx)
    #`#,one-ct)

  (define one (m)))

(test 1 dynamic-require ''uses-a-in-begin-for-syntax 'one)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check lifted requires, submodules, and re-expansion:

(define lifted-require-of-submodule
  `(,#'module m racket/base
    (require (for-syntax racket/base))
    (module a racket/base
      (provide a)
      (define a 'a))

    (define-syntax (m stx)
      (syntax-local-lift-require '(submod "." a) (syntax-local-introduce #'a)))

    (m)))

(test #t syntax? (expand-syntax (expand lifted-require-of-submodule)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check module lifting

(module module-lift-example-1 racket/base
  (require (for-syntax racket/base))
  (define-syntax (m stx)
    (syntax-local-lift-module
     #'(module m racket/base
         (provide x)
         (define x 10)))
    #'(begin
        (require 'm)
        (define out x)
        (provide out)))
  (m))

(test 10 dynamic-require ''module-lift-example-1 'out)

(module module-lift-example-2 racket/base
  (require (for-syntax racket/base))
  (define-syntax (m stx)
    (syntax-local-lift-module #'(module* sub #f
                                  (provide s)
                                  (define s (add1 a))))
    #'(void))
  (m)
  (define a 1))

(test 2 dynamic-require '(submod 'module-lift-example-2 sub) 's)


(module module-lift-example-3 racket/base
  (require (for-syntax racket/base))
  (define-syntax (m stx)
    (syntax-local-lift-module #'(module m racket/base
                                  (provide x)
                                  (define x 11)))
    (syntax-local-lift-module-end-declaration
     #'(let ()
         (local-require (submod "." m))
         (set! out x)))
    #'(void))
  (define out -10)
  (m)
  (provide out))

(test 11 dynamic-require ''module-lift-example-3 'out)

(module module-lift-example-4 racket/base
  (require (for-syntax racket/base))

  (define-syntax (main stx)
    (syntax-case stx ()
      [(_ body ...)
       (syntax-local-lift-module #`(module* main #f (main-method)))
       #'(define (main-method)
           body ...)]))

  (provide out)
  (define out #f)

  (main (set! out 12)))

(test (void) dynamic-require '(submod 'module-lift-example-4 main) #f)
(test 12 dynamic-require ''module-lift-example-4 'out)

(module module-lift-example-5 racket/base
  (module a racket/base
    (require (for-syntax racket/base))

    (provide main)

    (define-syntax (main stx)
      (syntax-case stx ()
        [(_ body ...)
         (syntax-local-lift-module #`(module* main #f (main-method)))
         #'(define (main-method)
             body ...)])))

  (module b racket/base
    (require (submod ".." a))
    (provide out)
    (define out #f)
    (main (set! out 13))))

(test (void) dynamic-require '(submod 'module-lift-example-5 b main) #f)
(test 13 dynamic-require '(submod 'module-lift-example-5 b) 'out)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check addition of 'disappeared-use by `provide`

(require (rename-in racket/base [lib racket-base:lib]))

(let ()
  (define (find-disappeared stx pred)
    (let loop ([s stx])
      (cond
       [(syntax? s)
        (define p (cons (syntax-property s 'disappeared-use)
                        (syntax-property s 'origin)))
        (or (let loop ([p p])
              (cond
               [(identifier? p) (pred p)]
               [(pair? p) (or (loop (car p))
                              (loop (cdr p)))]
               [else #f]))
            (loop (syntax-e s)))]
       [(pair? s)
        (or (loop (car s))
            (loop (cdr s)))]
       [else #f])))
  (define ((id=? a) b)
    (and (free-identifier=? a b)
         (eq? (syntax-e a) (syntax-e b))))
  (let ([form (expand #'(module m racket/base
                          (provide a (struct-out abc))
                          (struct abc ())
                          (define a 1)))])
    (test #t find-disappeared form (id=? #'struct-out))
    (test #t find-disappeared form (λ (id) (eq? (syntax-e id) 'abc))))
  (let ([form (expand `(module m racket/base
                        (require (only-in racket/base car))))])
    (test #t find-disappeared form (id=? #'only-in)))
  (let ([form (expand `(module m racket/base
                        (require (rename-in racket/base [lib racket-base:lib])
                                 (racket-base:lib "racket/base"))))])
    (test #t find-disappeared form (id=? #'racket-base:lib)))
  ;; Check case where the provide transformer also sets disappeared-use
  (let ([form (expand `(module m racket/base
                         (require (for-syntax racket/base racket/provide-transform))
                           (define-syntax my-out
                             (make-provide-transformer
                               (lambda (stx phases) null)
                               (lambda (stx phases)
                                 (syntax-case stx ()
                                   [(head id)
                                    (syntax-property #'(rename-out)
                                                     'disappeared-use
                                                     (syntax-local-introduce #'id))]))))
                           (provide (my-out map))))])
    (test #t find-disappeared form (id=? #'map))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module force-local-expand-of-body racket/base
  (require (for-syntax racket/base))
  (provide (rename-out [mb #%module-begin])
           (except-out (all-from-out racket/base) #%module-begin))

  (define-syntax (mb stx)
    (syntax-case stx ()
      [(_ . b)
       (local-expand #`(#%module-begin . b) (syntax-local-context) null)])))

(module use-local-require-at-phase-1 'force-local-expand-of-body
  (require (for-syntax racket/base))

  (begin-for-syntax
    (local-require (only-in racket [+ ++]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try trivial nested `begin-for-syntax`,
;; avoiding anything else that might prepare phase 2 in advance

(module starts-phase-2-without-any-content racket
  (begin-for-syntax
    (begin-for-syntax)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure `eval-syntax` doesn't create a fallback context

(module exports-cons-with-context racket/base
  (provide cons-id)
  (define cons-id #'cons))
(require 'exports-cons-with-context racket/base)

(let ([mod (datum->syntax #f `(,#'module m racket/base
                               ;; If a fallback is installed, then
                               ;; the module context of `cons` applies:
                               ,cons-id))])
  (err/rt-test (eval-syntax mod)
               (lambda (exn) (regexp-match #rx"ambiguous" (exn-message exn)))))

;; `eval` should install a fallback for a non`-module` form:
(test (void) eval (datum->syntax #f `(begin (,#'module m racket/base
                                              ,cons-id))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module shadows-a-racket-base-binding-and-exports racket/base
  (provide (all-defined-out)) ; exports `path?`
  (struct path ()))

(module import-shadows-a-racket-base-binding racket/base
  (require 'shadows-a-racket-base-binding-and-exports)
  (provide (all-from-out racket/base)))

;; Fails because imported module doesn't provide `path?`:
(syntax-test #'(module m racket/base
                 (require (rename-in 'import-shadows-a-racket-base-binding
                                     [path? other-path?]))))

(module import-shadows-a-racket-base-binding-and-doesnt-confuse-struct-out racket/base
  (require 'shadows-a-racket-base-binding-and-exports)
  (provide (struct-out path)))

(module shadows-a-racket-base-binding-and-exports-all racket/base
  (provide (all-from-out racket/base)) ; does not export `path?`
  (struct path ()))

(syntax-test #'(module m racket/base
                 (require (rename-in 'shadows-a-racket-base-binding-and-exports-all
                                     [path? other-path?]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `syntax-local-lift-require` on an
;; spec that doesn't have the target environment's
;; context:

(module has-a-submodule-that-exports-x racket
  (module b racket/base
    (define x 1)
    (provide x))

  (define-syntax (lifted-require-of-x stx)
    (syntax-case stx ()
      [(_ mod)
       (let ([x (car (generate-temporaries '(x)))])
         (syntax-local-lift-require
          #`(rename mod #,x x)
          x))]))

  (provide lifted-require-of-x))

(require 'has-a-submodule-that-exports-x)

(test 1 values (lifted-require-of-x (submod 'has-a-submodule-that-exports-x b)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This test happens to trigger a combination
;; of lazy adds and reoves that exposed a bug
;; in caching lazy scope propagations

(eval
 (expand
  #'(module x racket/kernel
      (module ma racket/base
        (#%module-begin
         (#%require (for-syntax racket/kernel))
         (define-values (x) 1)
         (define-syntaxes (foo) (lambda (stx) (quote-syntax x)))
         (#%provide foo)))
      (module mb racket/kernel
        (#%require (submod ".." ma))
        (foo)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that shutting down a custodian
;; releases a lock as it should

(parameterize ([current-custodian (make-custodian)])
  (thread-wait
   (thread
    (lambda ()
      (parameterize ([current-namespace (make-base-namespace)])
        (eval '(module m racket/base
                (require (for-syntax racket/base))
                (begin-for-syntax
                  #;(log-error "nested")
                  ;; Using an environment variable to communicate across phases:
                  (when (getenv "PLT_ready_to_end")
                    #;(log-error "adios")
                    (custodian-shutdown-all (current-custodian))))))
        (eval '(module n racket/base
                (require (for-syntax racket/base))
                (begin-for-syntax
                  #;(log-error "outer")
                  (dynamic-require ''m 0)
                  (eval #f))))
        (putenv "PLT_ready_to_end" "yes")
        (dynamic-require ''n 0)
        #;(log-error "go")
        (eval #f))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `namespace-mapped-symbols` and modidx shifting

(let ()
  (define tmp (make-temporary-file "~a-module-test" 'directory))
  (parameterize ([current-directory tmp]
                 [current-load-relative-directory tmp])
    (make-directory* (car (use-compiled-file-paths)))
    (call-with-output-file*
     (build-path (car (use-compiled-file-paths)) "a_rkt.zo")
     (lambda (o) (write (compile '(module a racket/base
                                    (provide (all-defined-out))
                                    (define a 1)
                                    (define b 2)
                                    (define c 3)))
                        o)))
    (call-with-output-file*
     (build-path (car (use-compiled-file-paths)) "b_rkt.zo")
     (lambda (o) (write (compile '(module b racket/base
                                    (require "a.rkt"
                                             ;; Force saving of context, instead of
                                             ;; reconstruction:
                                             (only-in racket/base [car extra-car]))))
                        o))))
  (dynamic-require (build-path tmp "b.rkt") #f)
  (define ns (module->namespace (build-path tmp "b.rkt")))
  (test #t
        'mapped-symbols
        (and (for/and ([name '(a b c)])
               (member name (namespace-mapped-symbols ns)))
             #t))
  (delete-directory/files tmp))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module exports-x*-as-x racket/base
  (define x* 5)
  (provide (rename-out [x* x])))

(module exports-x**-as-x racket/base
  (require 'exports-x*-as-x)
  (define x* 5)
  (define-syntax-rule (x**) x*)
  (provide (rename-out [x x***])
           (rename-out [x** x])))

(require 'exports-x**-as-x)
(test 5 'five (x))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check 'module-body-context-simple? and 'module-body-...context properties

(define (check-module-body-context-properties #:rename? [with-kar? #f]
                                              #:intro [intro #f])
  (define scope-intro
    (if intro
        (let ([scope-intro (if (eq? intro 'space)
                               (make-interned-syntax-introducer 'racket-test-space)
                               (make-syntax-introducer))])
          (lambda (d) (scope-intro (datum->syntax #f d))))
        (lambda (d) d)))
  (define m (expand `(module m racket/base
                      ,@(if with-kar?
                            `((require (rename-in racket/base [car kar])))
                            null)
                      ,(if (eq? intro 'space)
                           '(require (for-space racket-test-space racket/promise))
                           (scope-intro '(require racket/promise)))
                      (require (for-meta 2 racket/promise))
                      (define inside 7))))

  (test (and (not with-kar?) (not intro))
        syntax-property m 'module-body-context-simple?)

  (define i (syntax-property m 'module-body-context))
  (define o (syntax-property m 'module-body-inside-context))

  (test #t syntax? i)
  (test #t syntax? o)

  (test car eval-syntax (datum->syntax i 'car))
  (test 'inside cadr (identifier-binding (datum->syntax i 'inside)))
  (test #f identifier-binding (datum->syntax o 'inside))
  (test (if with-kar? 'car #f)
        'kar-binding
        (let ([v (identifier-binding (datum->syntax i 'kar))])
          (and v (cadr v))))

  (test (and intro #t) not (identifier-binding (datum->syntax i 'force)))
  (test 'force cadr (identifier-binding (scope-intro (datum->syntax i 'force)))))

(check-module-body-context-properties)
(check-module-body-context-properties #:rename? #t)
(check-module-body-context-properties #:intro 'scope)
(check-module-body-context-properties #:intro 'space)
(check-module-body-context-properties #:rename? #t #:intro 'scope)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that nesting `module+` under multiple `begin-for-syntax`
;; layers works

(module module-with-nested-module+s racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (require (for-syntax racket/base))
    (module+ test 1)
    (begin-for-syntax
      (require (for-syntax racket/base))
      (module+ test1 1)
      (begin-for-syntax
        (module+ test2 1)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check phase shifting in `dynamic-require`

(module module-with-phase-2-definition-of-x racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (require (for-syntax racket/base))
    (begin-for-syntax
      (provide x)
      (define x 5))))

(module module-that-exports-phase-2-x-at-phase-0 racket/base
  (require (for-meta -2 'module-with-phase-2-definition-of-x))
  (provide (for-meta -2 (all-from-out 'module-with-phase-2-definition-of-x))))

(test 5 dynamic-require ''module-that-exports-phase-2-x-at-phase-0 'x)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Check that
;; `namespace-anchor->namespace` internally enables top-level mode for
;; binding handling:

;; Example from Alex Knauth:
(module module-that-uses-eval-to-define-a-macro-in-its-own-namespace racket/base
  (require (for-syntax racket/base))
  (define-namespace-anchor a)
  (define ns (namespace-anchor->namespace a))
  (eval '(define-syntax x (λ (stx) #'333)) ns)
  (define result (eval 'x ns))
  (provide result))

(test 333 dynamic-require ''module-that-uses-eval-to-define-a-macro-in-its-own-namespace 'result)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that multiple imports of a name are disallowed
;; when they're from the from the same module but different
;; phases of that module

(module defines-a-at-two-phase-levels racket/base
  (require (for-syntax racket/base))

  (provide a (for-syntax a))

  (define a 0)
  (begin-for-syntax
    (define a 1)))

(err/rt-test (eval #'(module b racket/base
                       (require 'defines-a-at-two-phase-levels
                                (for-syntax racket/base
                                            'defines-a-at-two-phase-levels))))
             (lambda (exn)
               (regexp-match? #rx" already" (exn-message exn))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check re-export of an identifier from `#%kernel`
;; through a rename transformer:

(module rexports-values-from-kernel racket/base
  (require (for-syntax racket/base))
  (provide f)
  (define-syntax f (make-rename-transformer #'values)))

(dynamic-require ''rexports-values-from-kernel 'f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that shifts associated with re-expansion are
;; properly tracked for `module->namespace`

(parameterize ([current-load-relative-directory
                (let-values ([(b n dir?)
                              (split-path
                               (collection-file-path "promise.rkt" "racket"))])
                  b)])
  (let* ([e (expand (namespace-syntax-introduce
                     (datum->syntax #f '(module m racket/base
                                         ;; A prefixed import forces saving of the
                                         ;; context as a syntax object in marshaled
                                         ;; form:
                                         (require (prefix-in p: "promise.rkt"))))))]
         [c (compile e)]
         [o (open-output-bytes)]
         [_ (write c o)]
         [r (parameterize ([read-accept-compiled #t])
              (read (open-input-bytes (get-output-bytes o))))])
    (parameterize ([current-namespace (make-base-namespace)])
      (eval r)
      (dynamic-require ''m #f)
      (parameterize ([current-namespace (module->namespace ''m)])
        (and (memq 'p:force (namespace-mapped-symbols))
             #t)))))

;; ----------------------------------------
;; Check that `syntax-source-module` is #f for a top-level evaluation
;; that starts outside of a module:

(define my-very-own-x 'x)

(define (make-module-that-has-a-complex-renaming v)
  `(module module-that-has-a-complex-renaming racket
    ;; this line is necessary, but you can require anything
    (require (rename-in racket/base [car prefix:car]))
    (module+ sub)

    (define my-very-own-x ,v)))

(eval (make-module-that-has-a-complex-renaming 10))
(parameterize ([current-module-declare-name
                (make-resolved-module-path 'module-that-has-a-complex-renaming2)])
  (eval (make-module-that-has-a-complex-renaming 11)))

(require 'module-that-has-a-complex-renaming)
(require 'module-that-has-a-complex-renaming2)
(require (submod 'module-that-has-a-complex-renaming sub))
(require (submod 'module-that-has-a-complex-renaming2 sub))

(parameterize ([current-namespace (module->namespace ''module-that-has-a-complex-renaming)])
  (test #f syntax-source-module (namespace-syntax-introduce #'my-very-own-x))
  (test 10 eval #'my-very-own-x))

(parameterize ([current-namespace (module->namespace ''module-that-has-a-complex-renaming2)])
  (test #f syntax-source-module (namespace-syntax-introduce #'my-very-own-x))
  (test 11 eval #'my-very-own-x))

(parameterize ([current-namespace (module->namespace '(submod 'module-that-has-a-complex-renaming sub))])
  (test #f syntax-source-module (namespace-syntax-introduce #'my-very-own-x))
  (test 10 eval 'my-very-own-x))

(parameterize ([current-namespace (module->namespace '(submod 'module-that-has-a-complex-renaming2 sub))])
  (test #f syntax-source-module (namespace-syntax-introduce #'my-very-own-x))
  (test 11 eval 'my-very-own-x))

(module provide-the-x-identifier racket/base
  (define x-id #'my-very-own-x)
  (provide x-id))

(parameterize ([current-namespace (module->namespace ''module-that-has-a-complex-renaming)])
  (test 'provide-the-x-identifier
        resolved-module-path-name
        (module-path-index-resolve (syntax-source-module
                                    (namespace-syntax-introduce
                                     (dynamic-require ''provide-the-x-identifier 'x-id))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `all-defined` exports at only the right phase

(module module-that-exports-at-phase-0-only racket/kernel
  (#%require (for-syntax racket/kernel))
  (#%provide (all-defined))
  (define-values (x) 1)
  (begin-for-syntax
    (define-values (x) 2)))

(module module-that-imports-at-multiple-phases racket/kernel
  (#%require 'module-that-exports-at-phase-0-only
             ;; Causes a collsion if the module exports too much
             (for-syntax 'module-that-exports-at-phase-0-only)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a top-level binding doesn't interefere
;; with reference

(define very-confused-x 1)

(module m-that-defines-very-confused-x racket
  ;; this line is necessary, but you can require anything
  ;;(require (only-in racket/base))

  (define very-confused-x 10))

(require 'm-that-defines-very-confused-x)

(test 10
      'very-confused-x
      (parameterize ([current-namespace (module->namespace ''m-that-defines-very-confused-x)])
        ;; Note: #'very-confused-x will have top-level context
        ;; as well as the module context
        (eval #'very-confused-x)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that re-expansion of a simple (in the sense of `require`
;; information kept for `module->namspace`) module body is ok

(module m racket/base
  (module mylang racket/base
    (require (for-syntax racket/base))
    (provide (rename-out [-#%module-begin #%module-begin]))
    (define-syntax (-#%module-begin stx)
      (syntax-case stx ()
        [(_ lng . rest)
         (with-syntax ([#%module-begin (datum->syntax #'lng '#%module-begin)])
           #`(#%plain-module-begin
              (require lng)
              (continue #%module-begin . rest)))]))
    (define-syntax (continue stx)
      (syntax-case stx ()
        [(_ lang-module-begin . rest)
         (let ([body-stx (local-expand
                          #'(lang-module-begin . rest)
                          'module-begin
                          (list))])
           (syntax-case body-stx (#%plain-module-begin)
             [(#%plain-module-begin . mod-body)
              #`(begin . mod-body)]))])))

  (module foo (submod ".." mylang) racket/base
          (module a-submod racket/base
            (define x 1)
            (provide x))
          (require 'a-submod)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that `variable-reference->namespace` works
;; with phase shifts

(module evaluate-using-module-namespace racket/base
  (provide go)

  (define x 'x)

  (define (go)
    (define ns (variable-reference->namespace (#%variable-reference)))
    (list (namespace-base-phase ns)
          (eval '(list x) ns))))

(test '(0 (x)) (dynamic-require ''evaluate-using-module-namespace 'go))

(module evaluate-using-module-namespace-at-phase-1 racket/base
  (require (for-syntax 'evaluate-using-module-namespace
                       racket/base))
  (provide went)
  (define-syntax (m stx)
    #`(quote #,(go)))
  (define (went) (m)))

(test '(1 (x)) (dynamic-require ''evaluate-using-module-namespace-at-phase-1 'went))

(module evaluate-using-module-namespace/saved-context racket/base
  (provide go)

  (define x 'x/sc)

  ;; Macro-introduced definition triggers saving the module's bindings
  (define-syntax-rule (force-save-context) (define x 1))
  (force-save-context)

  (define (go)
    (define ns (variable-reference->namespace (#%variable-reference)))
    (list (namespace-base-phase ns)
          (eval '(list x) ns))))

(test '(0 (x/sc)) (dynamic-require ''evaluate-using-module-namespace/saved-context 'go))

(module evaluate-using-module-namespace-at-phase-1/saved-context racket/base
  (require (for-syntax 'evaluate-using-module-namespace/saved-context
                       racket/base))
  (provide went)
  (define-syntax (m stx)
    #`(quote #,(go)))
  (define (went) (m)))

(test '(1 (x/sc)) (dynamic-require ''evaluate-using-module-namespace-at-phase-1/saved-context 'went))


(module defines-a-variable-x-in-its-body racket/base
  (define x 'defined))

(module uses-defines-a-variable-x-in-its-body-at-phase-1 racket/base
  (require (for-syntax racket/base))
  (provide out)

  (define-syntax (m stx)
    (dynamic-require ''defines-a-variable-x-in-its-body #f)
    #`(quote #,(eval 'x (module->namespace ''defines-a-variable-x-in-its-body))))

  (define out (m)))

(test 'defined dynamic-require ''uses-defines-a-variable-x-in-its-body-at-phase-1 'out)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure a hash-table constant is allowed as a compile-time RHS:

(let ()
  (define o (open-output-bytes))
  (write (compile '(module m racket/base
                    (require (for-syntax racket/base))
                    (define-syntax something #hash((1 . 2)))))
         o)
  (parameterize ([read-accept-compiled #t])
    (read (open-input-bytes (get-output-bytes o)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that when `variable-reference->namespace`
;; reconstitutes variable mappings, that it uses the
;; run-time module path index of the module instead
;; of it's compile-time module path index

(parameterize ([current-module-declare-name
                (make-resolved-module-path 'uses-own-namespace-for-eval-from-submodule)])
  (eval
   '(module m racket/base
     (require (for-syntax racket/base))

     (define (x) 1)

     (define-syntax-rule (def) (define y 0))
     (def)

     (void
      (variable-reference->namespace (#%variable-reference)))

     (define (get-x)
       (eval '(x) (variable-reference->namespace (#%variable-reference))))

     (module* main #f
       (provide v)
       (define v
         (parameterize ([current-namespace (variable-reference->namespace (#%variable-reference))])
           (eval '(get-x))))))))

(test 1 dynamic-require '(submod 'uses-own-namespace-for-eval-from-submodule main) 'v)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure a module that isn't in a file
;; correctly introduces and references compile-time
;; top-level definitions

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(module m racket

           (require syntax/parse/define)

           (define-simple-macro (f m:id)
             (begin
               (define-for-syntax x "prop value")
               (define-syntax (m stx) x #'(void))))))
  (eval '(dynamic-require ''m #f))
  (let ([ns (module->namespace ''m)])
    (eval '(f m) ns)
    (eval '(m) ns)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure a module can exports syntax bound to a rename transformer
;; to an unbound identifier

(let ([decl
       '(module provides-rename-transformer-to-nowhere '#%kernel
          (#%require (for-syntax '#%kernel))
          (#%provide x)
          (define-syntaxes (x) (make-rename-transformer (quote-syntax y))))])
  (define o (open-output-bytes))
  (write (compile decl) o)
  (eval (parameterize ([read-accept-compiled #t])
          (read (open-input-bytes (get-output-bytes o))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure `variable-reference->namespace` at phase 1
;; doesn't interfere with re-expansion when trigged
;; by a submodule
;;
;; This test is by William Bowman, Michael Ballantyne, and
;; Leif Andersen.

(let ([m '(module namespace-mismatch racket/base
            (#%plain-module-begin

             (#%require (for-syntax racket/base))

             (begin-for-syntax
               (let ([ns (variable-reference->namespace (#%variable-reference))])
                 ;; The top level at phase 1 ...
                 (eval #'(define-syntax-rule (m) (begin (define x 2) x)) ns)
                 ;; The expander will have to find the right macro-introduced `x`:
                 (eval #'(m) ns))
               (#%plain-lambda () foo))

             (begin-for-syntax
               (define-values (foo) #f))

             (module* f #f
               (#%plain-module-begin))))])
  (expand (expand m)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that prefixing a submodule require doesn't
;; run into trouble related to the expand-time submodule
;; instance not being registered in the bulk-binding
;; provides table

(module check-prefixed-bulk-provides-from-submodules racket/base
  (module a racket/base
    (provide a1 a2 a3)
    (define a1 'a1)
    (define a2 'a2)
    (define a3 'a3))

  (require (prefix-in a: 'a))

  (define another 'x))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow a reference to a never-defined variable in a `local-expand`
;; or `syntax-local-bind-syntaxes` on the grounds that the result is
;; not necessarily in the module's expansion. But keep track of
;; missing variables encountered during
;; `syntax-local-expand-expression`, since the opqaue result can be
;; included without further inspection.

(module im-ok-and-your-ok-local-expand racket/base
  (require (for-syntax racket/base)
           (for-meta 2 racket/base))
  (begin-for-syntax
    (define-syntax (m stx)
      (local-expand #'(lambda () nonesuch) 'expression '())
      #''ok)
    (m)))

(module im-ok-and-your-ok-syntax-local-bind-syntaxes racket/base
  (require (for-syntax racket/base))
  (define-syntax (m stx)
    (syntax-local-bind-syntaxes (list #'x)
                                #'(lambda () nonesuch)
                                (syntax-local-make-definition-context))
     #''ok)
  (m))

(syntax-test #'(module im-ok-and-your-ok-local-expand racket/base
                 (require (for-syntax racket/base)
                          (for-meta 2 racket/base))
                 (begin-for-syntax
                   (define-syntax (m stx)
                     (syntax-local-expand-expression #'(lambda () nonesuch))
                     #''ok)
                   (m))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that shadowing in phase 1 doesn't
;; prevent `all-from-out` from providing the same
;; binding unshadowed at phase 0

(module check-shadowing-in-other-phase-d racket/base
  (provide b)
  (define b 'd))

(module check-shadowing-in-other-phase-c racket/base
  (require (for-syntax racket/base))
  (provide b (all-from-out racket/base)
           (for-syntax b))
  (define b 'c)
  (define-for-syntax b 'c1))

(module check-shadowing-in-other-phase-b 'check-shadowing-in-other-phase-c
  (require (for-syntax 'check-shadowing-in-other-phase-d))
  (provide (all-from-out 'check-shadowing-in-other-phase-c)
           (for-syntax (all-from-out 'check-shadowing-in-other-phase-d))))

(module check-shadowing-in-other-phase-a racket/base
  (require 'check-shadowing-in-other-phase-b)
  b)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that cross-module inlinign doesn't
;; lose track of shifts for module path indices

(module likely-inlines-across-two-submodules racket/base
  (provide result)

  (module a racket/base
    (provide get-x)
    (define x 5)
    (set! x x)
    (define (get-x) x))

  (module b racket/base
    (require (submod ".." a))
    (provide get-x2)
    (define (get-x2) (get-x)))

  (require 'b)
  (define result (get-x2)))

(test 5 dynamic-require ''likely-inlines-across-two-submodules 'result)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that the lift context is different for multiple
;; `#%module-begin` expansions. That's important, for example, to make
;; sure that a lift in the first pass record by the contract system
;; isn't assumed to be from a lexically earlier expression within a
;; second pass.

(module module-begin-and-unique-context-check racket/base
  (require (for-syntax racket/base))

  (provide (except-out (all-from-out racket/base)
                       #%module-begin)
           (rename-out [module-begin #%module-begin])
           check-unique-context)

  (define-for-syntax prev-key #f)

  (define-syntax (module-begin stx)
    (syntax-case stx ()
      [(_ form ...)
       (with-syntax ([(pmb new-form ...) (local-expand #'(#%plain-module-begin form ...) 'module-begin null)])
         #'(#%plain-module-begin new-form ... (check-unique-context)))]))

  (define-syntax (check-unique-context stx)
    (define key (syntax-local-lift-context))
    (when (eq? key prev-key)
      (raise-syntax-error #f "context didn't change"))
    (set! prev-key key)
    #'(void)))

(module use-module-begin-and-unique-context-check 'module-begin-and-unique-context-check
  (#%expression (check-unique-context)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a namespace can modify a module's mutable variables

(module uses-a-namespace-to-mutate-x racket/base
  (provide done)
  (define x 8)
  (define (inc!) (set! x (add1 x)))
  (eval '(set! x 0)
        (variable-reference->namespace (#%variable-reference)))
  (define done x))

(test 0 dynamic-require ''uses-a-namespace-to-mutate-x 'done)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that syntax properties are propagated from a
;; `module` form to an implicit `#%module-begin`

(module module-begin-to-export-foo-property racket/base
  (require (for-syntax racket/base))
  (provide (rename-out [mb #%module-begin]))

  (define-syntax (mb stx)
    (with-syntax ([FOO-PROP (syntax-property stx 'foo)])
      #'(#%module-begin
         (provide prop)
         (define prop 'FOO-PROP)))))

(eval (syntax-property
       (datum->syntax
        #f
        '(module export-foo-property-as-bar 'module-begin-to-export-foo-property))
       'foo "bar"))

(test "bar" dynamic-require ''export-foo-property-as-bar 'prop)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module export-of-force-has-three-different-nominals racket/base
  (require racket
           racket/promise
           racket/private/promise)
  (provide force result)

  (define-values (result other)
    (module->exports (variable-reference->resolved-module-path
                      (#%variable-reference)))))

(module export-of-force-has-three-different-nominals-no-bulk racket/base
  (require (only-in racket force)
           (only-in racket/promise force)
           (only-in racket/private/promise force))
  (provide force result)

  (define-values (result other)
    (module->exports (variable-reference->resolved-module-path
                      (#%variable-reference)))))

(for ([modname (list ''export-of-force-has-three-different-nominals
                     ''export-of-force-has-three-different-nominals-no-bulk)])
  (let ([l (dynamic-require modname 'result)])
    (define (same-mod? a b) (equal? (module-path-index-resolve a)
                                    (module-path-index-resolve b)))
    (define b (cadr (assoc 'force (cdr (assoc 0 l)))))
    (test 3 length b)
    (test #f same-mod? (car b) (cadr b))
    (test #f same-mod? (cadr b) (caddr b))
    (test #f same-mod? (car b) (caddr b))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module m1-expansion-defines-and-provides-m2 racket/base
  (require (for-syntax racket/base))
  (provide m1)
  (define-syntax (m1 stx)
    #'(begin
        (provide m2)
        (define m2 42))))

(module defines-and-provides-m2 racket/base
  (provide add1)
  (require 'm1-expansion-defines-and-provides-m2)
  (m1))


(let-values ([(vals stxes) (module->exports ''defines-and-provides-m2 'defined-names)])
  (test '(3 3) map length (cdr (assq 0 vals)))
  (test (string->unreadable-symbol "m2.1") 'module->exports
        (for/first ([v (in-list (cdr (assq 0 vals)))]
                    #:when (eq? (car v) 'm2))
          (caddr v)))
  (test #t 'module->exports (and (memq 'add1 (map car (cdr (assq 0 vals)))) #t))
  (test null values stxes))

(let-values ([(vals stxes) (module->exports ''defines-and-provides-m2)])
  (test '(2 2) map length (cdr (assq 0 vals))))
  
(err/rt-test (module->exports ''no-such-module-defined 'not-a-valid-verbosity)
             exn:fail:contract?
             #rx"not-a-valid-verbosity")

(let-values ([(vals stxes) (module-compiled-exports (compile '(module m racket/kernel
                                                                (define-values (x) 1)
                                                                (#%provide x))))])
  (test null values stxes)
  (test '(2) map length (cdr (assq 0 vals))))

(let-values ([(vals stxes) (module-compiled-exports (compile '(module m racket/kernel
                                                                (define-values (x) 1)
                                                                (#%provide x)))
                                                    'defined-names)])
  (test null values stxes)
  (test '(3) map length (cdr (assq 0 vals))))

(err/rt-test (module-compiled-exports 'no #f))
(err/rt-test (module-compiled-exports (compile '(module m racket/kernel)) 'not-a-valid-verbosity)
             #rx"not-a-valid-verbosity")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([check
       (lambda (later rx)
         (err/rt-test (expand `(module m racket/base
                                 (require (for-syntax racket/base))
                                 (begin-for-syntax
                                   (define (foo) (bar)))
                                 ,later))
                      (lambda (exn)
                        (regexp-match? rx (exn-message exn)))))])
  (check '(void) "unbound identifier")
  (check '(begin-for-syntax (struct bar ())) #rx"later defined as syntax")
  (check '(require (for-syntax (only-in racket/base [car bar]))) #rx"later bound differently"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that dynamic-require with a symbol argument
;; does not make the required module available for visits

(module shouldntvisit racket/base
  (define x 5)
  (provide x)
  (require (for-syntax racket/base))
  (define-for-syntax v #f)
  (define-syntax (m stx)
    (set! v #t)
    #'(void))
  (m)
  (begin-for-syntax
    (when (not v)
      (error 'shouldntvisit "visited"))))

(module visitor racket/base
  (require (for-syntax racket/base))
  (begin-for-syntax
    (dynamic-require ''shouldntvisit 'x))
  (begin-for-syntax))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `local-require` with a phase shift

(module local-require-with-phase-shift racket/base
  (let ()
    (local-require (for-template racket/base))
    (void)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `local-require` works with spaces and does not have an
;; unnecessary space shift

(parameterize ([current-namespace (make-base-namespace)])
  (eval `(module n racket/base
           (define abcdef 1)
           (provide abcdef)
           (require (for-syntax racket/base))
           (define-syntax (define-at-soup stx)
             (syntax-case stx ()
               [(_ id) #`(define #,((make-interned-syntax-introducer 'soup) #'id) 'ok)]))
           (define-at-soup kettle)
           (provide (for-space soup kettle))))

  (define stx
    (expand `(module m racket/base
               (require (for-syntax racket/base))
               (provide result)
               (begin-for-syntax (local-require 'n) abcdef)
               (define-syntax (at-soup stx)
                 (syntax-case stx ()
                   [(_ id) ((make-interned-syntax-introducer 'soup) #'id)]))
               (define result
                 (let ()
                   (local-require 'n)
                   (at-soup kettle))))))

  (eval stx)
  (test 'ok dynamic-require ''m 'result)

  (let loop ([stx stx])
    (cond
      [(and (identifier? stx) (equal? 'abcdef (syntax-e stx)))
       (define binding (identifier-binding stx))
       (when binding
         (unless (memq (list-ref binding 5) '(0 1))
           (error 'local-require-test "shift for ~a: ~a" stx (list-ref binding 5))))]
      [(pair? stx)
       (loop (car stx))
       (loop (cdr stx))]
      [(syntax? stx)
       (loop (syntax-e stx))])))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `make-interned-syntax-introducer`

(let ([ns-code '(module ns racket/base
                  (require (for-syntax racket/base))
                  (provide (for-syntax ns-introduce) begin-for-ns)
                  (begin-for-syntax
                    (define ns-introducer (make-interned-syntax-introducer 'ns))
                    (define (ns-introduce stx) (ns-introducer stx 'add)))
                  (define-syntax (begin-for-ns stx)
                    (syntax-case stx ()
                      [(_ form ...) (ns-introduce #'(begin form ...))])))]
      [m-code '(module m racket/base
                 (require (for-syntax racket/base) 'ns)
                 (provide get-ns-value)
                 (define-syntax (get-ns-value stx)
                   (syntax-case stx ()
                     [(_ x)
                      (identifier? #'x)
                      #`(quote #,(syntax-local-value (ns-introduce #'x)))])))]
      [p-code '(module p racket/base
                 (require (for-syntax racket/base) 'ns 'm)
                 (provide foo)
                 (begin-for-ns
                   (define-syntax Foo 'ns-val))
                 (define-syntax-rule (foo)
                   (get-ns-value Foo)))]
      [u-code '(module u racket/base
                 (require 'p)
                 (provide v)
                 (define v (foo)))])
  (parameterize ([current-namespace (make-base-namespace)])
    (eval ns-code)
    (eval m-code)
    (eval p-code)
    (eval u-code)
    (test 'ns-val dynamic-require ''u 'v))
  (parameterize ([current-namespace (make-base-namespace)])
    (let ([compile/eval (λ (code) (let ([s (open-output-bytes)])
                                    (write (compile code) s)
                                    (eval (parameterize ([read-accept-compiled #t])
                                            (read (open-input-bytes (get-output-bytes s)))))))])
      (compile/eval ns-code)
      (compile/eval m-code)
      (compile/eval p-code)
      (compile/eval u-code)
      (test 'ns-val dynamic-require ''u 'v))))

(err/rt-test (make-interned-syntax-introducer 5))
(err/rt-test (make-interned-syntax-introducer (gensym)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check that bulk bindings based on interned scopes are preserved,
;; even if the scope is not otherwise referenced

(let ([m '(module provides-x-with-tester-scope-bindings racket/base
            (require (for-space tester (prefix-in t: racket/base)))
            (provide x)
            (define x #'x))])
  (define o (open-output-bytes))
  (write (compile m) o)
  (eval (parameterize ([read-accept-compiled #t])
          (read (open-input-bytes (get-output-bytes o))))))

(test #t pair?
      (identifier-binding ((make-interned-syntax-introducer 'tester)
                           (datum->syntax (dynamic-require ''provides-x-with-tester-scope-bindings 'x) 't:cons))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Another example to check that re-expansion generates definition
;; names consistent with the previoud expansion.

(parameterize ([current-namespace (make-base-namespace)])
  (define modbeg-trampoline
    '(module modbeg-trampoline racket/base
       (require (for-syntax racket/base
                            syntax/strip-context))

       (provide (rename-out [module-begin #%module-begin]))

       (define-syntax (module-begin stx)
         (syntax-case stx ()
           [(_ lang . body)
            #'(#%plain-module-begin (module-begin-trampoline lang . body))]))

       (define-syntax (module-begin-trampoline stx)
         (syntax-case stx ()
           [(_ lang . body)
            (with-syntax ([[modbeg . [body* ...]] (syntax-local-introduce
                                                   (syntax-local-lift-require
                                                    (strip-context #'lang)
                                                    (datum->syntax #f (cons '#%module-begin
                                                                            (strip-context #'body)))))])
              (with-syntax ([body** (if (= (length (syntax->list #'(body* ...))) 1)
                                        (error "oops")
                                        #'(modbeg . [body* ...]))])
                (with-syntax ([(modbeg* form ...) (local-expand #'body** 'module-begin #f)])
                  (with-syntax ([body*** (local-expand #'(modbeg* form ...) 'module-begin (list #'module*))])
                    (with-syntax ([(modbeg**:#%plain-module-begin form* ...) #'body***])
                      (syntax-track-origin #`(begin form* ...) #'body*** #'modbeg**))))))]))))

  (define m-use
    '(module m-use 'modbeg-trampoline racket/base
       (require racket/contract/base)
       (provide (contract-out [f (-> number? number?)]))
       (define (f x) (+ x 42))
       (module* main racket
         (require (submod ".."))
         (f 10))))

  (eval modbeg-trampoline)
  (eval (expand m-use))

  (dynamic-require '(submod 'm-use main) #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that syntax-local-lift-require works in `#%module-begin` expansion

(module adds-racket-promise-but-without-reachable-bindings racket/base
  (require (for-syntax racket/base))
  
  (provide (except-out (all-from-out racket/base)
                       #%module-begin)
           (rename-out [module-begin #%module-begin]))
  
  (define-syntax (module-begin stx)
    (syntax-case stx ()
      [(_ . body)
       (syntax-local-lift-require 'racket/promise #'body)
       #'(#%module-begin . body)])))

(test '(module m 'adds-racket-promise-but-without-reachable-bindings
         (#%module-begin
          (#%require racket/promise)
          (module configure-runtime '#%kernel
            (#%module-begin
             (#%require racket/runtime-config)
             (#%app configure '#f)))))
      syntax->datum
      (expand '(module m 'adds-racket-promise-but-without-reachable-bindings)))

(err/rt-test/once (expand '(module m 'adds-racket-promise-but-without-reachable-bindings
                             force))
                  exn:fail:syntax?)

(module adds-racket-promise-with-reachable-bindings racket/base
  (require (for-syntax racket/base))
  
  (provide (except-out (all-from-out racket/base)
                       #%module-begin)
           (rename-out [module-begin #%module-begin]))
  
  (define-syntax (module-begin stx)
    (syntax-case stx ()
      [(_ . body)
       (with-syntax ([body (syntax-local-lift-require 'racket/promise #'body)])
         #'(#%module-begin . body))])))

(test '(module m 'adds-racket-promise-with-reachable-bindings
         (#%module-begin
          (#%require racket/promise)
          (module configure-runtime '#%kernel
            (#%module-begin
             (#%require racket/runtime-config)
             (#%app configure '#f)))
          (#%app call-with-values (lambda () force) print-values)))
      syntax->datum
      (expand '(module m 'adds-racket-promise-with-reachable-bindings
                 force)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that a module can be attached without a recorded namespace syntax context

(eval
 (let ([o (open-output-bytes)])
   (write (compile
           '(module please-attach-me-successfully racket/kernel
              (#%declare #:empty-namespace)))
          o)
   (parameterize ([read-accept-compiled #t])
     (read (open-input-bytes (get-output-bytes o))))))

(namespace-attach-module-declaration (current-namespace) ''please-attach-me-successfully (make-base-namespace))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `#:unsafe` is allowed

(module unsafe-module-so-call-provided-carefully racket/base
  (#%declare #:unsafe)
  (provide unsafe-first)
  (define (unsafe-first l) (car l)))

(require 'unsafe-module-so-call-provided-carefully)
(test 3 unsafe-first '(3 4 5))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check `#:realm`

(module module-with-body-in-elsewhere-realm racket/base
  (#%declare #:realm elsewhere)
  (provide f1 f2 f3 f4)
  (define (f1 x) x)
  (define f2 (lambda (x) x))
  (define f3 (case-lambda
               [(x) x]
               [(x y) y]))
  (define f4 (lambda (x)
               (lambda (y) x))))

(test 'elsewhere procedure-realm (dynamic-require ''module-with-body-in-elsewhere-realm 'f1))
(test 'elsewhere procedure-realm (dynamic-require ''module-with-body-in-elsewhere-realm 'f2))
(test 'elsewhere procedure-realm (dynamic-require ''module-with-body-in-elsewhere-realm 'f3))
(test 'elsewhere procedure-realm ((dynamic-require ''module-with-body-in-elsewhere-realm 'f4) 1))

(test 'elsewhere module->realm ''module-with-body-in-elsewhere-realm)
(test 'elsewhere values (module-compiled-realm (compile '(module m racket/base
                                                           (#%declare #:realm elsewhere)))))

(parameterize ([current-compile-realm 'somewhere])
  (eval '(module module-with-body-in-parameterized-realm racket/base
           (provide f1)
           (define (f1 x) x)))
  (test 'somewhere procedure-realm (eval '(lambda (x) x)))
  (test 'somewhere procedure-realm (eval '(begin
                                            (define procedure-that-is-somewhere (lambda (x) x))
                                            procedure-that-is-somewhere)))
  (eval '(module module-with-body-still-in-elsewhere-realm racket/base
           (#%declare #:realm elsewhere)
           (provide f1)
           (define (f1 x) x))))

(test 'somewhere procedure-realm (dynamic-require ''module-with-body-in-parameterized-realm 'f1))
(test 'elsewhere procedure-realm (dynamic-require ''module-with-body-still-in-elsewhere-realm 'f1))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that a module with an attached instance
;; cannot be redeclared in the target namespace

(module module-to-attach-elsewhere racket/base)
(dynamic-require ''module-to-attach-elsewhere #f)
(eval '(module module-to-attach-elsewhere racket/base)) ; to to redeclare here
(let ([ns (make-base-namespace)])
  (namespace-attach-module (current-namespace) ''module-to-attach-elsewhere ns)
  (err/rt-test (eval '(module module-to-attach-elsewhere racket/base)))
  (parameterize ([current-namespace ns])
    (err/rt-test (eval '(module module-to-attach-elsewhere racket/base)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `local-expand` doesn't make module available in a way
;; that allows the module to import itself

(err/rt-test
 (eval
  '(module use-submodule-tries-to-import-itself racket/base
     (module mb racket/base
       (require (for-syntax racket/base))
       (provide (except-out (all-from-out racket/base)
                            #%module-begin)
                (rename-out [module-begin #%module-begin]))
       (define-syntax (module-begin stx)
         (syntax-case stx ()
           [(_ a b)
            (begin
              (local-expand #'(#%module-begin a) 'module-begin null)
              #'(#%module-begin b))])))

     (module use (submod ".." mb)
       (module* m racket/base)
       (require (submod "." m "..")))))
 exn:fail?
 #rx"cycle detected")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If `local-expand` creates a macro binding, and if
;; the definition is discarded with a non-macro definition
;; is introduced, then make sure the non-macro definition
;; is referenced.

(module discards-module-begin-macro-definition racket/base

  (module mb racket/base
    (require (for-syntax racket/base))
    (provide (except-out (all-from-out racket/base)
                         #%module-begin)
             (rename-out [module-begin #%module-begin]))
    (define-syntax (module-begin stx)
      (syntax-case stx ()
        [(_ a b)
         (begin
           (local-expand #'(#%module-begin a) 'module-begin null)
           #'(#%module-begin b))])))

  (module use (submod ".." mb)
    (begin
      (require (for-syntax racket/base))
      (define-syntax (m stx) #''not-ok))
    (begin
      (define (m) 'ok)
      (define result (m))
      (provide result)))

  (require 'use)
  (provide result))

(test 'ok dynamic-require ''discards-module-begin-macro-definition 'result)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure a compile-time `eval` (as opposed to `eval-syntax`)
;; doesn't add the wrong phase's bindings via
;; `namespace-syntax-introduce`. The wrong phase's bindings in this
;; example could make `#%app` ambiguous.

(module uses-eval-at-compile-time racket/base
  (require (for-syntax racket/base)
           (for-meta 2 racket/base))

  (define-syntax (ct-eval stx)
    (syntax-case stx ()
      [(_ e) #`'#,(eval #'e)]))

  (ct-eval (+ 1 2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure "self" module path indices are not mixed up
;; across namespaces

(module mixes-top-level-namespaces racket/base
  (define ns (variable-reference->namespace (#%variable-reference)))
  (define x 'orig)

  (namespace-require 'racket/base)

  (eval '(define x 'new))
  (define result
    (eval
     `(list x ,(parameterize ([current-namespace ns])
                 (expand #'x)))))

  (provide result))

(test '(new orig) dynamic-require ''mixes-top-level-namespaces 'result)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that re-expansion of a `(module _name #f ....)`
;; submodule doesn't lose track of the base scope of the
;; submodule --- which would, for example, cause a definition
;; of `x` in the original submodule to be mapped to an
;; inaccessible `x.1`.

(parameterize ([current-namespace (make-base-namespace)])
  (eval-syntax (expand '(module m racket/base
                          (require syntax/location)
                          (module* check #f
                            (define x 42)))))
  (dynamic-require '(submod 'm check) #f)
  (eval 'x (module->namespace '(submod 'm check))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to provoke race conditions in the expander related
;; to instanitiating compile-time instances on demand

(test #t
      'no-races-found
      (for/and ([i (in-range 100)])
        (let ([ok? #t]
              [work 0])
          (for-each
           sync
           (parameterize ([current-namespace (make-base-namespace)])
             (for/list ([i 5])
               (thread
                (lambda ()
                  ;; "make work" to try to trigger a thread swap during `expand`
                  (for ([w (in-range (random 1000))])
                    (set! work (add1 work)))
                  (with-handlers ([exn? (lambda (exn)
                                          (set! ok? #f)
                                          (raise exn))])
                    ;; This `expand` will force compile-time instances of
                    ;; various modules used by `racket/base`
                    (expand `(lambda (x) x))))))))
          ok?)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that an import that is already shadowed by a definition
;; does not prevent importing other things

;; We want enough bindings here to trigger bulk mode
(module provides-a-through-m racket/base
  (define a 1)
  (define b 2)
  (define c 3)
  (define d 4)
  (define e 5)
  (define f 6)
  (define g 7)
  (define h 8)
  (define i 9)
  (define j 10)
  (define k 11)
  (define l 12)
  (define m 13)
  (provide a b c d e f g h i j k l m))

(module shaodws-c-and-imports-the-rest racket/base
  (define c -3)
  (require (except-in 'provides-a-through-m a b d e))
  (define result f)
  (provide result))

(test 6 dynamic-require ''shaodws-c-and-imports-the-rest 'result)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure #%module-begin respects the stop list when module* is present

(module module-begin-stop-list racket/base
  (require (for-syntax racket/base))
  (define-syntax (stop stx)
    (raise-syntax-error #f "don't expand me!" stx))
  (begin-for-syntax
    (local-expand #'(#%plain-module-begin (#%expression (stop)))
                  'module-begin
                  (list #'module* #'stop))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check state of a module-instance namespace when initialization
;; is interrupted by an error

(module fails-after-f-and-before-g racket/base
  (provide f g)
  (define (f x) (error "boom"))
  (f 42)
  (define g (if (zero? (random 1)) 'ok 'oops)))

(err/rt-test/once (dynamic-require ''fails-after-f-and-before-g #f)
                  (lambda (x) (and (exn:fail? x)
                                   (regexp-match? #rx"boom" (exn-message x)))))
(test #t procedure? (eval 'f (module->namespace ''fails-after-f-and-before-g)))

(module uses-fails-after-f-and-before-g racket/base
  (require 'fails-after-f-and-before-g)
  g)

(err/rt-test/once (dynamic-require ''uses-fails-after-f-and-before-g #f)
                  (lambda (x) (and (exn:fail? x)
                                   (regexp-match? #rx"uninitialized" (exn-message x)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module assigns-to-self-variable-through-namespace racket/base
  (require (for-syntax racket/base))
  (define x 1)
  (set! x 2)

  (define-syntax z
    (lambda (stx)
      (syntax-case stx ()
        [(_ b)
         #'(set! x b)])))

  (define-syntax y
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx ()
         [(_ a b)
          #'(set! x b)]))))

  (define ns (variable-reference->namespace
              (#%variable-reference)))

  (eval `(set! x 3) ns)
  (eval `(z 4) ns)
  (eval `(set! y 45) ns))

(dynamic-require ''assigns-to-self-variable-through-namespace #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check machine-independent compilation and
;; machine-dependent recompilation

(let ()
  (define m (parameterize ([current-compile-target-machine #f])
              (compile
               ;; The intent of this module is to exercise cross-module
               ;; inlining when moving from machine-independent to
               ;; machine-dependent. The `x` should be inlined from a submodule
               ;; and `map` should be inlined --- but we don't actually
               ;; check, currently.
               `(module should-inline-when-fully-compiled racket/base
                  (module sub racket/base
                    (define x 1)
                    (provide x))
                  (require 'sub)
                  (define y #'y)
                  (define (f g)
                    (map (lambda (y) x) g))))))

  (define (check-vm bstr vm)
    (define vm-bstr (string->bytes/utf-8 (symbol->string vm)))
    (define expect (bytes-append #"#~"
                                 (bytes (string-length (version)))
                                 (string->bytes/utf-8 (version))
                                 (bytes (bytes-length vm-bstr))
                                 vm-bstr))
    (test #t equal? expect (subbytes bstr 0 (min (bytes-length bstr) (bytes-length expect)))))

  (define o (open-output-bytes))
  (write m o)
  (check-vm (get-output-bytes o) 'linklet)
  
  (define m2
    (parameterize ([read-accept-compiled #t])
      (read (open-input-bytes (get-output-bytes o)))))
  
  (define re-m (compiled-expression-recompile m))
  (define re-m2 (compiled-expression-recompile m2))

  (define re-o (open-output-bytes))
  (write re-m re-o)
  (check-vm (get-output-bytes re-o) (system-type 'vm))

  (define re-o2 (open-output-bytes))
  (write re-m2 re-o2)
  (check-vm (get-output-bytes re-o2) (system-type 'vm))

  ;; Check top-level compilation:
  (define tl-o (open-output-bytes))
  (parameterize ([current-compile-target-machine #f])
    (write (compile '(begin (display "hi") (newline))) tl-o))
  (check-vm (get-output-bytes tl-o) 'linklet))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure `(define-values (id ...) (values rhs ...))` is not
;; split if one of the `id`s is referenced early

(module uses-a-in-define-values-before-a-is-defined racket/base
  (define-values (a b)
    (values 'a a)))

(err/rt-test/once (dynamic-require ''uses-a-in-define-values-before-a-is-defined #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that inlining doesn't duplicated a quoted list

(let ([e (compile
          '(module check-with-inlining-duplicates-by-using-submodules racket/base
             (provide check)

             (module m racket/base
               (provide f)
               (define (f x) '(1 2)))

             (module n racket/base
               (require (submod ".." m))
               (provide v)
               (define v (f 0)))

             (require 'm 'n)

             (define check (eq? (f 0) v))))])
  (define-values (i o) (make-pipe))
  (write e o)
  (close-output-port o)
  (eval (parameterize ([read-accept-compiled #t])
          (read i)))
  (test #t dynamic-require ''check-with-inlining-duplicates-by-using-submodules 'check))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check copy propagation across module boundaries

(module defines-also-something-as-alias-for-something racket/base

  (provide also-something)

  (define something (list 1 2 3))
  (define also-something something))


(module uses-also-something-via-local-alias racket/base
  (require 'defines-also-something-as-alias-for-something)

  (provide v)

  (define copy also-something)
  (define v (list copy)))

(test '((1 2 3)) dynamic-require ''uses-also-something-via-local-alias 'v)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check resolution of module paths in a namespace produced by `module->namespace`

(err/rt-test (eval '(require (submod "." inner)))
             (lambda (exn) (and (exn:fail? exn)
                                (regexp-match? #rx"no base path for relative submodule path" (exn-message exn)))))

(module defines-a-submodule-named-inner racket/base
  (module inner racket/base
    (provide i)
    (define i 'yes)))

(dynamic-require ''defines-a-submodule-named-inner #f)
(parameterize ([current-namespace (module->namespace ''defines-a-submodule-named-inner)])
  (eval '(require (submod "." inner)))
  (test 'yes eval 'i))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check reporting of require conflicts includes "who" provided the binding first

(module require-conflict-is-sourced-a racket/base
  (provide x)
  (define x 'a))

(module require-conflict-is-sourced-b racket/base
  (provide x)
  (define x 'b))

(err/rt-test
 (eval
  '(module m racket/base
     (require 'require-conflict-is-sourced-a
              'require-conflict-is-sourced-b)))
 (lambda (exn)
   (regexp-match? #rx"already required\n  at: x\n  in: \\(quote require-conflict-is-sourced-b\\)\n  also provided by: \\(quote require-conflict-is-sourced-a\\)" (exn-message exn))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the error is reasonable if `current-compile`
;; is used directly and the result abused:

(parameterize ([current-compile
                (let ([orig-compile (current-compile)])
                  (lambda (stx im?)
                    (orig-compile stx #t)))]) ; #t argument sets up the abuse
  (define c (compile '(module m racket/base)))
  (err/rt-test (write c (open-output-bytes))
               exn:fail:contract?
               #rx"write: linklet is not serializable"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that re-exports at higher phases correctly track whether
;; the export is a variable or syntax

(for ([meta '(1 2)])
  (define name (string->symbol (format "submodule-reexports-macro-at-meta-~a" meta)))
  (eval `(module ,name racket/base
           (module foo racket/base
             (require (for-syntax racket/base))
             (provide x)
             (define-syntax x 5))
           (module bar racket/base
             (require (for-meta ,meta (submod ".." foo)))
             (provide (for-meta ,meta x)))))

  (namespace-require `(submod ',name bar))

  (let-values ([(vals stxes) (module->exports `(submod ',name bar))])
    (test 0 length vals)
    (test 1 length stxes)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that a `local-expand`-triggered lazy instantiation does not
;; re-enter an instantiation that is already in progress

(module uses-local-expand-at-phase-1-instantiation racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base)))
  (provide (for-syntax true))
  (struct Π- (X))
  (begin-for-syntax
    (define TY/internal+ (local-expand #'Π- 'expression null))
    (define true (lambda (x) #t))))

(module imports-uses-local-expand-at-phase-1-instantiation racket/base
  (require (for-syntax racket/base)
           'uses-local-expand-at-phase-1-instantiation)
  (provide #%module-begin)
  (define-for-syntax predicate true))

(module lang-is-imports-uses-local-expand 'imports-uses-local-expand-at-phase-1-instantiation)

(let ()
  ;; important that both of these are in the same top-level evaluation:
  (test (void) namespace-require ''lang-is-imports-uses-local-expand)
  (test #t namespace? (module->namespace ''lang-is-imports-uses-local-expand)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Similar to previous, but with a `for-template` in the mix

(module uses-local-expand-at-phase-1-instantiation-again racket/base
  (require (for-syntax racket/base
                       (for-syntax racket/base)))
  (provide (for-syntax true))
  (struct Π- (X))
  (begin-for-syntax
    (define TY/internal+ (local-expand #'Π- 'expression null))
    (define true (lambda (x) #t))))

(module imports-uses-local-expand-at-phase-1-instantiation-again racket/base
  (require (for-syntax racket/base)
           'uses-local-expand-at-phase-1-instantiation-again)
  (provide #%module-begin)
  (define-for-syntax predicate true))

(module local-expand-test2-reflect racket/base
  (require (for-template 'uses-local-expand-at-phase-1-instantiation-again))
  (define x true))

(module local-expand-test2 racket/base
  (provide #%module-begin)
  (require 'imports-uses-local-expand-at-phase-1-instantiation-again
           (for-syntax 'local-expand-test2-reflect)))

(module lang-is-local-expand-test2 'local-expand-test2)

(let ()
  ;; important that both of these are in the same top-level evaluation:
  (test (void) namespace-require ''lang-is-local-expand-test2)
  (test #t namespace? (module->namespace ''lang-is-local-expand-test2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check order of checking for redefinition of a constant

(let ([e '(module defines-a-spider-struct-type racket/base
            (struct spider (legs)))])
  (eval e)
  (namespace-require ''defines-a-spider-struct-type)
  (err/rt-test (eval e)
               exn:fail:contract:variable?
               #rx"struct:spider")
  (parameterize ([current-namespace (module->namespace ''defines-a-spider-struct-type)])
    (err/rt-test (eval '(struct spider (legs)))
                 exn:fail:contract:variable?
                 #rx"struct:spider")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure error message is right for wrong number of
;; values

(module returns-obviously-wrong-number-of-values-to-definition racket/base
  (define-values (x y z) (values 1 2 3 4)))

(err/rt-test/once (dynamic-require ''returns-obviously-wrong-number-of-values-to-definition #f)
                  exn:fail:contract:arity?
                  #rx"define-values: result arity mismatch")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure source-name tracking works across multiple definitions

(module tries-to-use-first-defined-function-too-early racket/base
  (define-syntax-rule (go)
    (begin
      (f 1)
      (define (f x) x)
      (define (g y) y)))
  (go))

(err/rt-test/once (dynamic-require ''tries-to-use-first-defined-function-too-early #f)
                  exn:fail:contract:variable?
                  #rx"^f:")

(module tries-to-use-second-defined-function-too-early racket/base
  (define-syntax-rule (go)
    (begin
      (f 1)
      (define (g y) y)
      (define (f x) x)))
  (go))

(err/rt-test/once (dynamic-require ''tries-to-use-second-defined-function-too-early #f)
                  exn:fail:contract:variable?
                  #rx"^f:")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check on gensyms that span phases in marshaled code

(let ([e '(module has-a-gensym-that-spans-phases racket/base
            (require (for-syntax racket/base))

            (define-syntax (def stx)
              (syntax-case stx ()
                [(_ id s-id)
                 (let ([sym (gensym)])
                   #`(begin
                       (provide id s-id)
                       (define id '#,sym)
                       (define-syntax (s-id stx)
                         #'(quote #,sym))))]))
            
            (def the-gensym expand-to-the-gensym))])
  (define o (open-output-bytes))
  (write (compile e) o)
  (eval (parameterize ([read-accept-compiled #t])
          (read-syntax 'expr (open-input-bytes (get-output-bytes o))))))
(require 'has-a-gensym-that-spans-phases)
(test #t eq? the-gensym (expand-to-the-gensym))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure cross-moodule inlining doesn't copy an uninterned symbol
;; across a module boundary

(module exports-a-quoted-uninterned-symbol racket/base
  (require (for-syntax racket/base))
  (define-syntax (provide-sym stx)
    (let ([sym (datum->syntax #f (string->uninterned-symbol "sym"))])
      #`(begin
          (define sym '#,sym)
          (define (get-sym) '#,sym)
          (provide sym
                   get-sym))))
  (provide-sym))

(let ([o (open-output-bytes)])
  (write (compile `(module imports-a-quoted-uninterned-symbol racket/base
                     (require 'exports-a-quoted-uninterned-symbol)
                     (define (get-sym1) sym)
                     (define (get-sym2) (get-sym))
                     (provide get-sym1
                              get-sym2)))
         o)
  (eval (parameterize ([read-accept-compiled #t])
          (read (open-input-bytes (get-output-bytes o)))))
  (test (dynamic-require ''exports-a-quoted-uninterned-symbol 'sym)
        (dynamic-require ''imports-a-quoted-uninterned-symbol 'get-sym1))
  (test (dynamic-require ''exports-a-quoted-uninterned-symbol 'sym)
        (dynamic-require ''imports-a-quoted-uninterned-symbol 'get-sym2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `(dynamic-require .... (void))` visits
;; available modules as it should

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(module defines-a-for-syntax racket/base
           (require (for-syntax racket/base))
           (provide (for-syntax a))
           (define-for-syntax a (make-parameter 'not-done))))
  (eval '(module uses-a-module-1 racket/base
           (require 'defines-a-for-syntax
                    (for-syntax racket/base))
           (begin-for-syntax (a 'done))))
  (eval '(module uses-a-module-2 racket/base
           (require 'defines-a-for-syntax
                    (for-syntax racket/base))
           (provide m)
           (define-syntax m
             (let ([val (a)])
               (lambda (stx)
                 #`'#,val)))))
                   
  ;; makes `uses-a-module-1` available:
  (eval '(require 'uses-a-module-1))
  ;; needs available module visited for `val` to be 'done:
  (dynamic-require ''uses-a-module-2 (void))
  ;; check `val` via `m`:
  (namespace-require ''uses-a-module-2)
  (test 'done eval '(m)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure a too-early function use is not inlined away

(module uses-a-function-too-early racket/base
  (define f
    (let ([v (g)])
      (lambda ()
        v)))
  (define (g)
    0))

(err/rt-test/once (dynamic-require ''uses-a-function-too-early #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module regression-test-for-cross-module-inline-quoted-identifier  racket/base
  (module a racket/base
    (provide FOUNDING)
    (define-values (FOUNDING) 'FOUNDING))
  
  (module b racket/base
    (require (submod ".." a))
    (define (f x_2) (eq? x_2 'FOUNDING)) ; quoted `FOUNDING` should not get replaced
    (if (f FOUNDING) FOUNDING (raise-user-error 'die)))

  (require (submod "." b)))

(parameterize ([current-output-port (open-output-bytes)])
  (dynamic-require ''regression-test-for-cross-module-inline-quoted-identifier #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([m '(module cross-inline-function-with-strange-srcloc racket/base
            (require (for-syntax racket/base))
            (define-syntax (m stx)
              (struct opaque ())
              #`(begin
                  (provide f)
                  (define f
                    #,(datum->syntax #'here
                                     '(lambda (x) x)
                                     (vector (opaque) 1 2 3 4)))))
            (m))])
  ;; Make sure this doesn't error:
  (write (compile m) (open-output-bytes)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module regression-test-for-set!-in-dead-compile-time-code '#%kernel
  (#%require (for-syntax '#%kernel))
  
  (begin-for-syntax
    (let-values ([(fresh)
                  (let-values ([(weird-next) 0])
                    (lambda ()
                      (set! weird-next (add1 weird-next))))])
      10)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module regression-test-for-delayed-set!-after-direct-use racket/base
  (define align
    (lambda ()
      'ok))
  
  (define strings-are-numbers
    (let-values ([(real-align) align])
      (lambda ()
        (set! align 7)
        real-align)))
  
  (void (strings-are-numbers)))

(dynamic-require ''regression-test-for-delayed-set!-after-direct-use #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module regression-test-for-loop-detection racket/base
  (provide go)
  
  (define (f pick)
    (let would-be-loop ([v #f])
      (if (pick)
          (let not-a-loop ()
            (if (pick)
                (let also-would-be-loop ()
                  (if (pick)
                      (if (pick)
                          (also-would-be-loop)
                          (would-be-loop #t))
                      null))
                (if (pick)
                    (list (not-a-loop))
                    null)))
          (would-be-loop v))))

  (define (go)
    (f (let ([l '(#t #t #t #f #t #f #f)])
         (lambda ()
           (begin0
             (car l)
             (set! l (cdr l))))))))

(test '() (dynamic-require ''regression-test-for-loop-detection 'go))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define (syntax-size stx)
    (syntax-case stx ()
      [(a . b) (+ (syntax-size #'a)
                  (syntax-size #'b))]
      [_ 1]))
  (test #t 'simple-rename-in-size-is-linear-to-renamed-ids
        (equal?
         (syntax-size
          (expand #'(module a racket/base
                      (require (rename-in racket/base
                                          [call/cc callcc])))))
         (syntax-size
          (expand #'(module a racket/base
                      (require (rename-in racket
                                          [call/cc callcc])))))))
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module regression-test-to-make-sure-property-procedure-mutation-is-seen racket/base
  (struct foo2 (f g) #:transparent
    #:property prop:equal+hash
    (list (λ (a b recur) #f)
          (λ (a recur) 0)
          (λ (a recur) (set! here? #t) 0)))
  
  (define here? #f)
  (void (equal-secondary-hash-code (foo2 0 "ggg"))))

(dynamic-require ''regression-test-to-make-sure-property-procedure-mutation-is-seen #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check an interaction of recompilation and cross-module-inlining

;; Make sure that a constant is inlined across a module boundary by
;; recompilation
(parameterize ([current-module-name-resolver
                (let ([orig (current-module-name-resolver)])
                  (case-lambda
                    [(mp ns) (orig mp ns)]
                    [(mp wrt stx load?)
                     (cond
                       [(equal? mp "module-out-of-thin-air.rkt")
                        (when load?
                          (unless (module-declared? ''module-out-of-thin-air)
                            (eval `(module module-out-of-thin-air racket/base
                                     (define five 5)
                                     (provide five)))))
                        (make-resolved-module-path 'module-out-of-thin-air)]
                       [else
                        (orig mp wrt stx load?)])]))])
  (define mi-compiled
    (parameterize ([current-namespace (make-base-namespace)]
                   [current-compile-target-machine #f])
      (compile '(module uses-module-out-of-thin-air racket/base
                  (require "module-out-of-thin-air.rkt")
                  (define also-five five)
                  (provide also-five)))))
  ;; We expect that `mi-compiled` does not have 5 inlined.
  ;; If it does, that's not actually a problem, but it means that
  ;; the rest of the test doesn't check what it means to check, so
  ;; count it as a problem
  (parameterize ([current-namespace (make-base-namespace)])
    (eval '(module module-out-of-thin-air racket/base
             (define five 6)
             (provide five)))
    (eval mi-compiled)
    (test 6 dynamic-require ''uses-module-out-of-thin-air 'also-five))
  (define recompiled
    (parameterize ([current-namespace (make-base-namespace)])
      (compiled-expression-recompile mi-compiled)))
  (parameterize ([current-namespace (make-base-namespace)])
    (eval '(module module-out-of-thin-air racket/base
             (define five 6)
             (provide five)))
    (eval recompiled)
    ;; Ir recompilation did not inline the 5, then the result
    ;; will be 6 instead of 5:
    (test 5 dynamic-require ''uses-module-out-of-thin-air 'also-five)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check relative-path encoding and decoding for procedure source locations

(let ([m (compile `(module m racket/base
                     (provide f)
                     (define f ,(datum->syntax #f
                                               `(lambda (thunk) (list (thunk)))
                                               (list (build-path (current-directory) "the-file.rkt")
                                                     2
                                                     3
                                                     4
                                                     5)))))])
  (define o (open-output-bytes))
  (parameterize ([current-write-relative-directory (current-directory)])
    (write m o))
  (define (get)
    (parameterize ([read-accept-compiled #t])
      (read (open-input-bytes (get-output-bytes o)))))
  (define (check-name m p)
    (parameterize ([current-namespace (make-base-namespace)])
      (eval m)
      (define f (dynamic-require ''m 'f))
      (define e (with-handlers ([exn:fail? values])
                  (f (lambda () (car 0)))))
      (define ctx (continuation-mark-set->context (exn-continuation-marks e)))
      (test
       #t
       `(has ,p)
       (for/or ([pr (in-list ctx)])
         (and (cdr pr)
              (equal? p (srcloc-source (cdr pr))))))))
  (let ([m1 (parameterize ([current-load-relative-directory #f])
              (get))])
    (check-name m1 (build-path (current-directory) "the-file.rkt"))
    (void))
  (let ([m1 (parameterize ([current-load-relative-directory (find-system-path 'temp-dir)])
              (get))])
    (check-name m1 (build-path (find-system-path 'temp-dir) "the-file.rkt"))
    (void)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure a top-level `begin` can be instantiated from
;; machine-independent form

(let ([o (open-output-bytes)])
  (parameterize ([current-compile-target-machine #f])
    (write (compile '(begin (random) 10)) o))

  (test 10 'top-begin/compile-any
        (eval (parameterize ([read-accept-compiled #t])
                (read (open-input-bytes (get-output-bytes o)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([o (open-output-bytes)])
  (parameterize ([current-output-port o])
    (define m
      '(module continuations-at-phase-1 racket/base
         (require (for-syntax racket/base))
         
         (begin-for-syntax
           (define k (call/cc (lambda (k) k)))
           (set! k k)
           (k 5)
           (printf "~s\n" k)
           (define more #f)
           (let ([me (call/cc (lambda (k) (lambda (v) (k v))))])
             (set! more me)
             (printf "~s\n" me))
           (more 88))))
    (eval m))
  (test "5\n#<procedure>\n88\n" get-output-string o))

(let ([o (open-output-bytes)])
  (parameterize ([current-output-port o])
    (namespace-require ''continuations-at-phase-1))
  (test "" get-output-string o)
  (parameterize ([current-output-port o])
    (eval 'car))
  (test "5\n#<procedure>\n88\n" get-output-string o))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check that expanded `require` doens't keep use-site scopes

(parameterize ([current-namespace (make-base-namespace)])
  (eval
   `(module p2 racket/base
      (provide cons)
      (define cons 'p)))

  (eval
    (expand
     `(module p racket/base
        (require (for-syntax racket/base))
        (define-syntax (bounce stx)
          (syntax-case stx ()
            [(_ mod ...)
             (with-syntax ([(mod ...) ((make-syntax-introducer) #'(mod ...))])
               #'(begin (begin (require mod)
                               (provide (all-from-out mod)))
                        ...))]))
        (bounce 'p2)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check that write and reading a module preserves flonum `eq?`

(let ([m '(module defines-a-and-b-infinity racket/base
            (provide a b)
            (define a +inf.0)
            (define b +inf.0))])
  (define o (open-output-bytes))
  (write (compile m) o)
  (parameterize ([read-accept-compiled #t])
    (eval (read (open-input-bytes (get-output-bytes o)))))
  (test #t eq?
        (dynamic-require ''defines-a-and-b-infinity 'a)
        (dynamic-require ''defines-a-and-b-infinity 'b)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression test aimed at a compiler bug: the target of known-copy
;; information from an imported module was confused with an import
;; from another module where the target and import happen to have the
;; same name

(module check-no-crash-on-misapplication racket/base
  (provide check)

  (module one racket/base
    (provide thing)
    (define f (random))
    (define thing f))

  (module two racket/base
    (provide f)
    (define (f x) (let loop () (loop))))

  (require 'one
           'two)

  (define (check)
    (when (thing #f)
      (f 10))))

(err/rt-test ((dynamic-require ''check-no-crash-on-misapplication 'check)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test 'cwv-ok
      (dynamic-require ''#%kernel 'call-with-values)
      (lambda () 'cwv-ok)
      (chaperone-procedure (lambda (v) v) (lambda (v) v)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regression test aimed at instantiation via shifting up and back down

(let ()
  (define ns (make-base-namespace))
  (define ns2 (make-base-namespace))

  (define d
    (parameterize ([current-namespace ns])
      (eval '(module zo racket/base
               (require (for-template racket/base))))
      (define d
        (compile '(module d racket/base
                    (require 'zo
                             racket/phase+space)
                    phase+space)))
      (eval d)
      (dynamic-require ''d #f)
      d))

  (parameterize ([current-namespace ns2])
    (namespace-attach-module ns ''zo)
    (eval d)
    (dynamic-require ''d #f)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make sure that definitions with interned scopes are accessible
;; via `module->namespace`, even if no syntax object is in the module

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(module ex racket/base
           (require (for-syntax racket/base))
           (define-syntax (def stx)
             (syntax-case stx ()
               [(_ id)
                #`(define #,((make-interned-syntax-introducer 'racket/example) #'id) 5)]))
           (define-syntax (ref stx)
             (syntax-case stx ()
               [(_ id)
                ((make-interned-syntax-introducer 'racket/example) #'id)]))
           (provide def ref)))
  (eval '(module m racket/base
           (require 'ex)
           (def x)))
  (namespace-require ''m)
  (eval '(ref x) (module->namespace ''m)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
