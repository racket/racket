
(load-relative "loadtest.ss")

(Section 'module)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module n mzscheme 
  (define n 'n) 
  (define-struct s (field1 field2))
  (provide n
	   (struct s (field1 field2))
	   (rename n m)))

(require n)
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
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide x x)))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide x z)))
(syntax-test #'(module m mzscheme (define x 10) (define y 11) (provide x y x)))
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
(syntax-test #'(module m mzscheme (define-struct x (y)) (provide (struct x (y y)))))
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

(syntax-test #'(module m mzscheme (define car 5)))
(syntax-test #'(module m mzscheme (define x 6) (define x 5)))
(syntax-test #'(module m mzscheme (define x 10) (define-syntax x 10)))
(syntax-test #'(module m mzscheme (define-syntax x 10) (define x 10)))

;; Cyclic re-def of n:
(syntax-test #'(module n n 10))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check namespace-attach-module:

(let* ([n (make-namespace)]
       [l null]
       [here (lambda (v)
	       (set! l (cons v l)))])
  (parameterize ([current-namespace n])
    (eval `(module a mzscheme
	     (define a 1)
	     (,here 'a)
	     (provide a)))
    (test null values l)
    (eval `(module b mzscheme
	     (require-for-template a)
	     (define b 1)
	     (,here 'b)
	     (provide b)))
    (test null values l)
    (eval `(module c mzscheme
	     (require-for-template b)
	     (define c 1)
	     (,here 'c)
	     (provide c)))
    (test null values l)
    (eval `(module d mzscheme
	     (require-for-syntax c)
	     (define d 1)
	     (,here 'd)
	     (provide d)))
    (test '(c) values l)
    (eval `(module e mzscheme
	     (require-for-syntax d)
	     (define e 1)
	     (,here 'e)
	     (provide e)))
    (test '(d c b c) values l)
    (eval `(module f mzscheme
	     (,here 'f)
	     (require b e)))
    (test '(d c b d c b c) values l)
    (eval `(require f))
    (let ([finished '(f b e  d c b a  d c b d c b c)])
      (test finished values l)
      (let ([n2 (make-namespace)])
	(namespace-attach-module n 'f)
	(test finished values l)
	(eval `(require a))
	(eval `(require f))
	(test finished values l)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check redundant import and re-provide

(module m_cr mzscheme
  (provide x_cr y_cr z_cr w_cr)
  (define x_cr 12)
  (define y_cr 14)
  (define z_cr 16)
  (define w_cr 18))

(syntax-test #'(module n_cr mzscheme
		 (require m_cr)
		 (provide (all-from-except m_cr no-such-var))))
(syntax-test #'(module n_cr mzscheme
		 (require m_cr)
		 (provide (all-from-except m_cr cons))))

(module n_cr mzscheme
  (require m_cr)
  (provide (all-from-except m_cr x_cr)))

(module p_cr mzscheme
  (require n_cr m_cr)
  (provide (all-from m_cr)))

(require p_cr)
(test 14 values y_cr)

(module p2_cr mzscheme
  (require m_cr n_cr)
  (provide (all-from m_cr)))

(require p2_cr)
(test 16 values z_cr)

(module p3_cr mzscheme
  (require m_cr n_cr)
  (provide (all-from n_cr)))

(require p3_cr)
(test 18 values w_cr)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test proper bindings for `#%module-begin'

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
	 (module m mod_beg2
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
	 (module m mod_beg2
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
	 (module m mod_beg2
		 3)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([f1 "tmp1.ss"]
      [f2 "tmp2.ss"]
      [exn:fail-cycle? (lambda (exn)
                         (and (exn:fail? exn)
                              (regexp-match? #rx"cycle" (exn-message exn))))])
  (with-output-to-file f1
    (lambda ()
      (write `(module tmp1 mzscheme (require ,f2))))
    'truncate/replace)
  (with-output-to-file f2
    (lambda ()
      (write `(module tmp2 mzscheme (require ,f1))))
    'truncate/replace)
  (err/rt-test (dynamic-require (build-path (current-directory) f1) #f) exn:fail-cycle?)
  (err/rt-test (dynamic-require (build-path (current-directory) f2) #f) exn:fail-cycle?)
  (delete-file f1)
  (delete-file f2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
