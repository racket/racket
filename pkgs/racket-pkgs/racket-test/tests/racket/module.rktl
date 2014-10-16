
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
(syntax-test #'(module m racket/base (#%require (rename n))))
(syntax-test #'(module m racket/base (#%require (rename n . n))))
(syntax-test #'(module m racket/base (#%require (rename n n))))
(syntax-test #'(module m racket/base (#%require (rename n n . m))))
(syntax-test #'(module m racket/base (#%require (rename n 1 m))))
(syntax-test #'(module m racket/base (#%require (rename n n 1))))
(syntax-test #'(module m racket/base (#%require (rename n n not-there))))
(syntax-test #'(module m racket/base (#%require (rename n n m extra))))

(syntax-test #'(module m racket/base (#%require racket/base) (define car 5)))
(syntax-test #'(module m racket/base (define x 6) (define x 5)))
(syntax-test #'(module m racket/base (define x 10) (define-syntax x 10)))
(syntax-test #'(module m racket/base (define-syntax x 10) (define x 10)))

;; Cyclic re-def of n:
(syntax-test #'(module n 'n 10))

;; It's now ok to shadow the initial import:
(module _shadow_ racket/base
  (define car 5)
  (provide car))

(test 5 dynamic-require ''_shadow_ 'car)

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
    (let ([finished '(f b e  a d b  d b d b c)])
      (test finished values l)
      (namespace-attach-module n ''f)
      (test finished values l)
      (parameterize ([current-namespace (make-empty-namespace)])
	(namespace-attach-module n ''f)
	(test finished values l)
        (namespace-require 'racket/base)
	(eval `(require 'a))
	(eval `(require 'f))
	(test (list* 'd 'b finished) values l)))))

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
            (compile (syntax-property #'(module m scheme/base)
                                      'module-language
                                      val)))])
  (test #f 'info (module-compiled-language-info (mk 10)))
  (test '#(scheme x "whatever") 'info (module-compiled-language-info (mk '#(scheme x "whatever"))))
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (eval mk ns)
      (eval (mk '#(scheme x "whatever")))
      (test '#(scheme x "whatever") module->language-info ''m)
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
      (test #f eval 'same? ns)
      (let ([n-ns (eval '(module->namespace ''n) ns)])
        (test 5 eval '(lambda (x) x) n-ns)))))

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
      ;; Add a hash, so that loading this module in two contexts tries to
      ;; use the same loaded bytecode and same JIT-generated code:
      (bytes-copy! s (+ 4 vlen)
                   (subbytes
                    (bytes-append (string->bytes/utf-8 (format "~s" (bytes-length s)))
                                  (make-bytes 20 0))
                    0
                    20))
      s))

  (define a-s (compile-m (a-expr #f) '()))
  (define am-s (compile-m (a-expr #t) '()))
  (define b-s (compile-m b-expr (list a-s)))

  (define temp-dir (find-system-path 'temp-dir))
  (define dir (build-path temp-dir "compiled"))
  (define dir-existed? (directory-exists? dir))
  (unless dir-existed? (make-directory dir))

  (define (go a-s)
    (parameterize ([current-namespace (make-base-namespace)]
                   [read-accept-compiled #t])
      (eval (read (open-input-bytes a-s)))
      (with-output-to-file (build-path dir "check-gen_rkt.zo")
        #:exists 'truncate
        (lambda () (write-bytes b-s)))
      ((dynamic-require (build-path temp-dir "check-gen.rkt") 'b) 10)))
  ;; Triger JIT generation with constant function as `a':
  (go a-s)
  ;; Check that we don't crash when trying to use a different `a':
  (err/rt-test (go am-s) exn:fail?)
  ;; Cleanup
  (delete-file (build-path dir "check-gen_rkt.zo"))
  (unless dir-existed? (delete-directory dir)))

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
;; check that `require' inside `beging-for-syntax' sets up the right phase dependency

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
    (eval '(require 'n)))
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
  (eval #'(foo 10)))

(err/rt-test (dynamic-require ''avail-y #f)
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
                         "exp\nexp\nrun\nexp\nexp\n"))])
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
;; Check that skipping definitions (but continuing
;; with the rest of a module body) is disallowed.

(module disallowed-definition-avoider racket/base

  (define fail
    ((call-with-continuation-prompt
      (lambda ()
        (call/cc values)))))
  
  (error "no"))

(err/rt-test (dynamic-require ''disallowed-definition-avoider #f)
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that module caching doesn't cause submodules
;; to be loaded/declared too early

(define (install-module-hashes! s start len c)
  (define vlen (bytes-ref s (+ start 2)))
  (define mode (integer->char (bytes-ref s (+ start 3 vlen))))
  (case mode
    [(#\T)
     (define h (make-bytes 20 (+ 42 c)))
     (bytes-copy! s (+ start 4 vlen) h)]
    [(#\D)
     (define (read-num rel-pos)
       (define pos (+ start rel-pos))
       (integer-bytes->integer s #t #f pos (+ pos 4)))
     (define count (read-num (+ 4 vlen)))
     (for/fold ([pos (+ 8 vlen)]) ([i (in-range count)])
       (define pos-pos (+ pos 4 (read-num pos)))
       (define mod-start (read-num pos-pos))
       (define mod-len (read-num (+ pos-pos 4)))
       (install-module-hashes! s (+ start mod-start) mod-len i)
       (+ pos-pos 16))
     (void)]
    [else (error "unknown")]))

(let ()
  (define dir (find-system-path 'temp-dir))
  (define tmx (build-path dir "tmx.rkt"))
  (define e (compile '(module tmx racket/base
                        (module s racket/base
                          (provide x)
                          (define x 1)))))
  (make-directory* (build-path dir "compiled"))
  (define zo-path (build-path dir "compiled" "tmx_rkt.zo"))

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
  (delete-file zo-path))

;; Check that module-code caching works
(let ()
  (define dir (find-system-path 'temp-dir))
  (define tmx (build-path dir "tmx2.rkt"))
  (define e (compile '(module tmx2 racket/kernel
                        (#%provide x)
                        (define-values (x) 1))))
  (make-directory* (build-path dir "compiled"))
  (define zo-path (build-path dir "compiled" "tmx2_rkt.zo"))

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
        (parameterize ([current-namespace (make-base-namespace)])
          (dynamic-require tmx 'x)))
  (delete-file zo-path))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
