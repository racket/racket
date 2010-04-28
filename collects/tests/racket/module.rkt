
(load-relative "loadtest.rkt")

(Section 'module)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module n mzscheme 
  (define n 'n) 
  (define-struct s (field1 field2))
  (provide n
	   (struct s (field1 field2))
	   (rename n m)))

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
(syntax-test #'(module 5 mzscheme))

(syntax-test #'(module m 5))

(syntax-test #'(module m mzscheme . 1))

(syntax-test #'(#%module-begin))
(syntax-test #'(+ (#%module-begin) 2))

(syntax-test #'(module n+ mzscheme (#%module-begin (#%module-begin (define n+ 'n+) (provide n+)))))
(syntax-test #'(module n+ mzscheme (define n+ 'n+) (#%module-begin (provide n+))))
(syntax-test #'(module n+ mzscheme (define n+ 'n+) (#%module-begin) (provide n+)))
(syntax-test #'(module n+ mzscheme (#%module-begin) (define n+ 'n+) (provide n+)))
(module n+ mzscheme (#%module-begin (define n+ 'n+) (provide n+)))

(syntax-test #'(provide))
(syntax-test #'(provide . x))
(syntax-test #'(provide y . x))
(syntax-test #'(module m mzscheme (define x 10) (provide . x)))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide y . x)))
(syntax-test #'(module m mzscheme (define x 10) (provide 1)))
(syntax-test #'(module m mzscheme (define x 10) (provide "bad")))
(syntax-test #'(module m mzscheme (define x 10) (provide not-here)))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide x (rename y x))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide x z)))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide x y (rename x y))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (rename))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (rename x))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (rename x y z))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (rename not-here x))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (rename x 1))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (rename 1 x))))
(syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct))))
(syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct . x))))
(syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct x))))
(syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct x (y) z))))
(syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct x (y) . z))))
(syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct 1 ()))))
(syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct x (1)))))
(syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct x (y . 1)))))
;; (syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct x (y y)))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from . mzscheme))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from 1))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from xxxx))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from mzscheme x))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from-except))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from-except . mzscheme))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from-except 1))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from-except mzscheme + . -))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from-except mzscheme 1))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from-except xxxx +))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from-except mzscheme no))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-from-except mzscheme + no))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-defined x))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-defined . x))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-defined 1))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-defined-except . x))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-defined-except 1))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-defined-except x 1))))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide (all-defined-except no))))

(syntax-test #'(require . x))
(syntax-test #'(require m . x))
(syntax-test #'(module m mzscheme (require n . x)))
(syntax-test #'(module m mzscheme (require (prefix))))
(syntax-test #'(module m mzscheme (require (prefix n))))
(syntax-test #'(module m mzscheme (require (prefix . pre:))))
(syntax-test #'(module m mzscheme (require (prefix pre: . n))))
(syntax-test #'(module m mzscheme (require (prefix 1 n))))
(syntax-test #'(module m mzscheme (require (prefix pre: n more))))
(syntax-test #'(module m mzscheme (require (prefix pre: n . more))))
(syntax-test #'(module m mzscheme (require (all-except))))
(syntax-test #'(module m mzscheme (require (all-except . n))))
(syntax-test #'(module m mzscheme (require (all-except n 1))))
(syntax-test #'(module m mzscheme (require (all-except n . n))))
(syntax-test #'(module m mzscheme (require (rename))))
(syntax-test #'(module m mzscheme (require (rename . n))))
(syntax-test #'(module m mzscheme (require (rename n))))
(syntax-test #'(module m mzscheme (require (rename n . n))))
(syntax-test #'(module m mzscheme (require (rename n n))))
(syntax-test #'(module m mzscheme (require (rename n n . m))))
(syntax-test #'(module m mzscheme (require (rename n 1 m))))
(syntax-test #'(module m mzscheme (require (rename n n 1))))
(syntax-test #'(module m mzscheme (require (rename n n not-there))))
(syntax-test #'(module m mzscheme (require (rename n n m extra))))

(syntax-test #'(module m mzscheme (require mzscheme) (define car 5)))
(syntax-test #'(module m mzscheme (define x 6) (define x 5)))
(syntax-test #'(module m mzscheme (define x 10) (define-syntax x 10)))
(syntax-test #'(module m mzscheme (define-syntax x 10) (define x 10)))

;; Cyclic re-def of n:
(syntax-test #'(module n n 10))

;; It's now ok to shadow the initial import:
(module _shadow_ mzscheme
  (define car 5)
  (provide car))

(test 5 dynamic-require ''_shadow_ 'car)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check namespace-attach-module:

(let* ([n (make-empty-namespace)]
       [l null]
       [here (lambda (v)
	       (set! l (cons v l)))])
  (namespace-attach-module (current-namespace) 'scheme/base n)
  (namespace-attach-module (current-namespace) 'mzscheme n)
  (parameterize ([current-namespace n])
    (namespace-require 'mzscheme)
    (eval `(module a mzscheme
	     (define a 1)
	     (,here 'a)
	     (provide a)))
    (test null values l)
    (eval `(module b mzscheme
	     (require-for-template 'a)
	     (define b 1)
	     (,here 'b)
	     (provide b)))
    (test null values l)
    (eval `(module c mzscheme
	     (require-for-template 'b)
	     (define c 1)
	     (,here 'c)
	     (provide c)))
    (test null values l)
    (eval `(module d mzscheme
	     (require-for-syntax 'c)
	     (define d 1)
	     (,here 'd)
	     (provide d)))
    (test '(c) values l)
    (eval `(module e mzscheme
	     (require-for-syntax 'd)
	     (define e 1)
	     (,here 'e)
	     (provide e)))
    (test '(d b c) values l)
    (eval `(module f mzscheme
	     (,here 'f)
	     (require 'b 'e)))
    (test '(d b d b c) values l)
    (eval `(require 'f))
    (let ([finished '(f b e  a d b  d b d b c)])
      (test finished values l)
      (namespace-attach-module n ''f)
      (test finished values l)
      (parameterize ([current-namespace (make-empty-namespace)])
	(namespace-attach-module n ''f)
	(test finished values l)
        (namespace-require 'scheme/base)
	(eval `(require 'a))
	(eval `(require 'f))
	(test (list* 'd 'b finished) values l)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check redundant import and re-provide

(module m_cr mzscheme
  (provide x_cr y_cr z_cr w_cr)
  (define x_cr 12)
  (define y_cr 14)
  (define z_cr 16)
  (define w_cr 18))

(syntax-test #'(module n_cr mzscheme
		 (require 'm_cr)
		 (provide (all-from-except 'm_cr no-such-var))))
(syntax-test #'(module n_cr mzscheme
		 (require 'm_cr)
		 (provide (all-from-except 'm_cr cons))))

(module n_cr mzscheme
  (require 'm_cr)
  (provide (all-from-except 'm_cr x_cr)))

(module p_cr mzscheme
  (require 'n_cr 'm_cr)
  (provide (all-from 'm_cr)))

(require 'p_cr)
(test 14 values y_cr)

(module p2_cr mzscheme
  (require 'm_cr 'n_cr)
  (provide (all-from 'm_cr)))

(require 'p2_cr)
(test 16 values z_cr)

(module p3_cr mzscheme
  (require 'm_cr 'n_cr)
  (provide (all-from 'n_cr)))

(require 'p3_cr)
(test 18 values w_cr)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test `require' scoping


(module fake-prefix-in scheme
  (require scheme/require-syntax)
  (define-require-syntax (pseudo-+ stx)
    (syntax-case stx ()
      [(_ id)
       #'(only-in scheme [+ id])]))
  (provide pseudo-+))

(require 'fake-prefix-in
         (pseudo-+ ++))
(test 12 values (++ 7 5))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test proper bindings for `#%module-begin'

(define expand-test-use-toplevel? #t)

(test (void) eval
      '(begin
	 (module mod_beg2 mzscheme
		 (provide (all-from-except mzscheme #%module-begin))
		 (provide (rename mb #%module-begin))
		 (define-syntax (mb stx)
		   (syntax-case stx ()
		     [(_ . forms)
		      #`(#%plain-module-begin 
			 #,(datum->syntax-object stx '(require-for-syntax mzscheme))
			 . forms)])))
	 (module m 'mod_beg2
		 3)))


(test (void) eval
      '(begin
	 (module mod_beg2 mzscheme
		 (provide (all-from-except mzscheme #%module-begin))
		 (provide (rename mb #%module-begin))
		 (define-syntax (mb stx)
		   (syntax-case stx ()
		     [(_ . forms)
		      #`(#%plain-module-begin 
			 #,(datum->syntax-object stx '(require-for-syntax mzscheme))
			 . forms)])))
	 (module m 'mod_beg2
		 3 4)))

(test (void) eval
      '(begin
	 (module mod_beg2 mzscheme
		 (provide (all-from-except mzscheme #%module-begin))
		 (provide (rename mb #%module-begin))
		 (define-syntax (mb stx)
		   (syntax-case stx ()
		     [(mb . forms)
		      #`(#%plain-module-begin 
			 #,(datum->syntax-object #'mb '(require-for-syntax mzscheme))
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
      (write `(module ,(string->symbol (path->string tmp1)) mzscheme (require (file ,(path->string f2)))))))
  (with-output-to-file f2
    #:exists 'truncate/replace
    (lambda ()
      (write `(module ,(string->symbol (path->string tmp2)) mzscheme (require (file ,(path->string f1)))))))
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
          ((current-module-name-resolver) (current-module-declare-name))))
      (test '#(tests/racket/lang/getinfo get-info closure-data)
            module->language-info 'tests/racket/langm))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check shadowing of initial imports:

(let ([m-code '(module m scheme/base (define-syntax-rule (lambda . _) 5) (provide lambda))]
      [n-code '(module n scheme/base 
                 (require 'm) 
                 (define five (lambda (x) x)) 
                 (define five-stx #'lambda)
                 (provide five five-stx))]
      [p-code '(module p scheme/base
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

(report-errs)
