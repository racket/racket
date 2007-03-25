
(load-relative "loadtest.ss")

(Section 'stx)

(test #t syntax? (datum->syntax-object #f 'hello #f))

(test #f syntax-line (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test #f syntax-column (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test 19 syntax-position (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test 'aha syntax-source (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test #f syntax-span (datum->syntax-object #f 10 '(aha #f #f 19 #f)))
(test 88 syntax-span (datum->syntax-object #f 10 '(aha #f #f 19 88)))

(test 7 syntax-line (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test 88 syntax-column (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test 999 syntax-position (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test 'aha syntax-source (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test #f syntax-span (datum->syntax-object #f 10 '(aha 7 88 999 #f)))
(test 22 syntax-span (datum->syntax-object #f 10 '(aha 7 88 999 22)))
(test 0 syntax-span (datum->syntax-object #f 10 '(aha 1 1 1 0)))
(test 0 syntax-column (datum->syntax-object #f 10 '(aha 1 0 1 0)))

(err/rt-test (datum->syntax-object #f 10 10))
(err/rt-test (datum->syntax-object #f 10 '(10)))
(err/rt-test (datum->syntax-object #f 10 '(10 11)))
(err/rt-test (datum->syntax-object #f 10 '(10 11 12)))
(err/rt-test (datum->syntax-object #f 10 '(10 11 12 13)))
(err/rt-test (datum->syntax-object #f 10 '(10 11 12 13 14 15)))
(err/rt-test (datum->syntax-object #f 10 '(a 11.0 12 13 14)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 12 -13 14)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 12 -13 14)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 12 13 -1)))
(err/rt-test (datum->syntax-object #f 10 '(a 0 12 13 0)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 -1 13 0)))
(err/rt-test (datum->syntax-object #f 10 '(a 11 12 0 0)))

(syntax-test #'quote-syntax)
(syntax-test #'(quote-syntax))
(syntax-test #'(quote-syntax . 7))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some syntax-case patterns
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test 17 'syntax-case (syntax-case '(1 1 1) () [(1 ...) 17]))

(define-syntax sd (syntax-rules () [(_ v) (syntax-object->datum (syntax v))]))

(test '(3 1 2) 'syntax-case (syntax-case '(1 2 3) () [(a ... b) (sd (b a ...))]))
(test 5 'syntax-case (syntax-case '(1 2 3) () [(a ... b . c) (sd (b a ... c))][_else 5]))
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
      (syntax-object->datum (syntax-case '(_ 1 (x y z) ((3 3 3) (4 4 4) (5 5 5))) () 
			      [(_ x (a ...) ((b ...) ...)) #'(((a b) ...) ...)])))

(test '(((x y z 3) (x y z 3) (x y z 3))
	((x y z 4) (x y z 4) (x y z 4))
	((x y z 5) (x y z 5) (x y z 5)))
      'ellipses
      (syntax-object->datum (syntax-case '(_ 1 (x y z) ((3 3 3) (4 4 4) (5 5 5))) () 
			      [(_ x (a ...) ((b ...) ...)) #'(((a ... b) ...) ...)])))


(test '((1 z) (2 w) (x z) (y w))
      'ellipses
      (syntax-object->datum (syntax-case '(((1 2) (x y)) (z w)) () 
			      [(((a ...) ...) (b ...)) #'((a b) ... ...)])))

(test '(#(1) #(2 3))
      'ellipses+vector
      (syntax-object->datum 
       (syntax-case '((1) (2 3)) () [((a ...) ...) #'(#(a ...) ...)])))

(test '(1 2 3 6 8 9 0 1 2 3)
      syntax-object->datum 
      (syntax-case '(((1) (2 3)) ((6)) ((8 9 0) (1 2 3))) () [(((a ...) ...) ...) #'(a ... ... ...)]))
(test '((1 2 3) (6) (8 9 0 1 2 3))
      syntax-object->datum 
      (syntax-case '(((1) (2 3)) ((6)) ((8 9 0) (1 2 3))) () [(((a ...) ...) ...) #'((a ... ...) ...)]))
(test '((1) (2 3) (6) (8 9 0) (1 2 3))
      syntax-object->datum 
      (syntax-case '(((1) (2 3)) ((6)) ((8 9 0) (1 2 3))) () [(((a ...) ...) ...) #'((a ...) ... ...)]))

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

(test 10 syntax-property (datum->syntax-object #f 0 #f s) 'testing)

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

(define s (syntax-property (with-syntax ([five (syntax-property (quote-syntax 5)
								'testing
								12)])
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
  (test '(aha yep) ssort (syntax-property-symbol-keys (syntax-property 
						       (syntax-property 
							(syntax-property #'a 'aha 1) 
							'yep 2)
						       'aha 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test module-identifier=? on different phases via syntax-case*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module mta mzscheme
  (define mtax 10)
  (provide mtax))

(module mtb mzscheme
  (define mtby 10)
  (provide mtby))

(module mt1 mzscheme
  (require (prefix a: mta))
  (require-for-syntax (prefix b: mtb))
  (require (prefix mz: mzscheme))

  (define-syntax ck
    (lambda (stx)
      (syntax-case stx ()
	[(_ id et?)
	 (with-syntax ([cmp (if (syntax-e (syntax et?))
				(syntax module-transformer-identifier=?)
				(syntax module-identifier=?))])
	   (syntax
	    (lambda (x)
	      (syntax-case* x (id) cmp
	        [(_ id) #t]
		[else #f]))))])))

  (define has-lam? (ck lambda #f))
  (define has-mz:lam? (ck mz:lambda #f))
  (define has-mtax? (ck a:mtax #f))
  (define has-mtby? (ck b:mtby #f))

  (define has-et-lam? (ck lambda #t))
  (define has-et-mz:lam? (ck mz:lambda #t))
  (define has-et-mtax? (ck a:mtax #t))
  (define has-et-mtby? (ck b:mtby #t))

  (provide has-lam? has-mz:lam? has-mtax? has-mtby?
	   has-et-lam? has-et-mz:lam? has-et-mtax? has-et-mtby?))

(require mt1)
(require-for-syntax mtb)

(test #t has-lam? #'(any lambda))
(test #f has-lam? #'(any lambada))

(test #t has-et-lam? #'(any lambda))
(test #f has-et-lam? #'(any lambada))

;; mz: prefix is there in normal environment:
(test #t has-mz:lam? #'(any lambda))
(test #f has-et-mz:lam? #'(any lambda))
(test #f has-mz:lam? #'(any mz:lambda))
(test #t has-et-mz:lam? #'(any mz:lambda))

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

(module mt2 #%kernel
  (require-for-syntax #%kernel)
  (require mt1)
  (require mta)

  ;; For #':
  (define-syntaxes (syntax)
    (lambda (stx)
      (datum->syntax-object
       stx
       (cons
	(quote-syntax quote-syntax)
	(cdr (syntax-e stx)))
       stx)))

  (define-values (run-mt2-test)
    (lambda (test)
      
      (test #t has-lam? #'(any lambda))
      (test #f has-lam? #'(any lambada))

      (test #t has-et-lam? #'(any lambda))
      (test #f has-et-lam? #'(any lambada))

      ;; mz: prefix is there in normal environment:
      (test #t has-mz:lam? #'(any lambda))
      (test #f has-et-mz:lam? #'(any lambda))
      (test #f has-mz:lam? #'(any mz:lambda))
      (test #t has-et-mz:lam? #'(any mz:lambda))

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

  (provide run-mt2-test))

(require mt2)
(run-mt2-test test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test '(1 2 3) syntax-object->datum (syntax (1 2 3)))
(test '(1 ... 2 3) syntax-object->datum (syntax (... (1 ... 2 3))))

(syntax-test #'(syntax (a (... ...))))
(syntax-test #'(syntax (... ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; identifier-binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test '(#%kernel lambda mzscheme lambda #f)  identifier-binding #'lambda)
(test '(#%more-scheme delay mzscheme delay #f)  identifier-binding #'delay)
(test '(#%kernel #%module-begin mzscheme #%plain-module-begin #f)  identifier-binding #'#%plain-module-begin)
(require (rename mzscheme #%pmb #%plain-module-begin))
(test '(#%kernel #%module-begin mzscheme #%plain-module-begin #f)  identifier-binding #'#%pmb)

(let ([b (identifier-binding (syntax-case (expand #'(module m mzscheme
						      (require (rename (lib "htdp-intermediate.ss" "lang") bcons cons))
						      bcons)) ()
			       [(mod m mz (#%mod-beg for-syntax req cons))
				(let ([s (syntax cons)])
				  (test 'bcons syntax-e s)
				  s)]))])
  (let-values ([(real real-base) (module-path-index-split (car b))]
	       [(nominal nominal-base) (module-path-index-split (caddr b))])
    (test '"teachprims.ss" values real)
    (test 'beginner-cons cadr b)
    (test '(lib "htdp-intermediate.ss" "lang") values nominal)
    (test 'cons cadddr b)))

(let ([b (identifier-binding (syntax-case (expand #'(module m (lib "htdp-intermediate.ss" "lang")
						      cons)) ()
			       [(mod m beg (#%mod-beg cons))
				(let ([s (syntax cons)])
				  (test 'cons syntax-e s)
				  s)]))])
  (let-values ([(real real-base) (module-path-index-split (car b))]
	       [(nominal nominal-base) (module-path-index-split (caddr b))])
    (test '"teachprims.ss" values real)
    (test 'beginner-cons cadr b)
    (test '(lib "htdp-intermediate.ss" "lang") values nominal)
    (test 'cons cadddr b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval versus eval-syntax, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless building-flat-tests?

  (test eval eval 'eval)
  (test eval eval eval)
  (test eval eval #'eval)
  (test eval eval (datum->syntax-object #f 'eval))

  (err/rt-test (eval-syntax 'eval))
  (err/rt-test (eval-syntax eval))
  (test eval eval-syntax #'eval)
  (test #t 
	'eval-syntax
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  (eval-syntax (datum->syntax-object #f 'eval))))

  (test eval (current-eval) 'eval)
  (test eval (current-eval) eval)
  (test eval (current-eval) #'eval)
  (test #t 
	'current-eval-syntax
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  ((current-eval) (datum->syntax-object #f 'eval))))

  (test eval 'compile (eval (compile 'eval)))
  (test eval 'compile (eval (compile eval)))
  (test eval 'compile (eval (compile #'eval)))
  (test eval 'compile (eval (compile (datum->syntax-object #f 'eval))))

  (err/rt-test (compile-syntax 'eval))
  (err/rt-test (compile-syntax eval))
  (test eval 'compile (eval (compile-syntax #'eval)))
  (test #t 
	'compile-syntax
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  (compile-syntax (datum->syntax-object #f 'eval))))

  (test eval 'expand (eval (expand 'eval)))
  (test eval 'expand (eval (expand eval)))
  (test eval 'expand (eval (expand #'eval)))
  (test eval 'expand (eval (expand (datum->syntax-object #f 'eval))))

  (err/rt-test (expand-syntax 'eval))
  (err/rt-test (expand-syntax eval))
  (test eval 'expand (eval (expand-syntax #'eval)))
  (test #t 
	'expand-syntax
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  (expand-syntax (datum->syntax-object #f 'eval))))

  (test eval 'expand-once (eval (expand-once 'eval)))
  (test eval 'expand-once (eval (expand-once eval)))
  (test eval 'expand-once (eval (expand-once #'eval)))
  (test eval 'expand-once (eval (expand-once (datum->syntax-object #f 'eval))))

  (err/rt-test (expand-syntax-once 'eval))
  (err/rt-test (expand-syntax-once eval))
  (test eval 'expand-once (eval (expand-syntax-once #'eval)))
  (test #t 
	'expand-syntax-once
	(with-handlers ([exn:fail:syntax? (lambda (x) #t)])
	  (expand-syntax-once (datum->syntax-object #f 'eval))))

  (test eval 'expand-to-top-form (eval (expand-to-top-form 'eval)))
  (test eval 'expand-to-top-form (eval (expand-to-top-form eval)))
  (test eval 'expand-to-top-form (eval (expand-to-top-form #'eval)))
  (test eval 'expand-to-top-form (eval (expand-to-top-form (datum->syntax-object #f 'eval))))

  (err/rt-test (expand-syntax-to-top-form 'eval))
  (err/rt-test (expand-syntax-to-top-form eval))
  (test eval 'expand-to-top-form (eval (expand-syntax-to-top-form #'eval)))
  (test #t syntax? (expand-syntax-to-top-form (datum->syntax-object #f 'eval))))

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
	(syntax-case stx (lambda case-lambda begin begin0
				 set! with-continuation-mark
				 if #%app module #%plain-module-begin
				 define-values)
	  [(lambda formals expr ...)
	   (ormap loop (syntax->list #'(expr ...)))]
	  [(case-lambda [formals expr ...] ...)
	   (ormap (lambda (l)
		    (ormap loop (syntax->list l)))
		  (syntax->list #'((expr ...) ...)))]
	  [(let ([(id ...) rhs] ...) expr ...)
	   (or (module-identifier=? #'let #'let-values)
	       (module-identifier=? #'let #'letrec-values))
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
	  [(#%app expr ...)
	   (ormap loop (syntax->list #'(expr ...)))]
	  [(module name init body)
	   (loop #'body)]
	  [(#%plain-module-begin expr ...)
	   (ormap loop (syntax->list #'(expr ...)))]
	  [(define-values (id ...) expr)
	   (loop #'expr)]
	  [_else #f]))))

(test #t has-stx-property? (expand #'(let ([x 1]) 2)) 'let-values 'let 'origin)

;; The define-struct macro expands to begin,
(test #t has-stx-property? (expand #'(define-struct x (a))) 'begin 'define-struct 'origin)
(test #t has-stx-property? (expand #'(module m mzscheme (define-struct x (a)))) 'define-values 'define-struct 'origin)
(test #t has-stx-property? (expand #'(module m mzscheme (define-struct x (a)))) 'define-syntaxes 'define-struct 'origin)

;; The s macro also expands to begin:
(test #t has-stx-property? (expand #'(module m mzscheme 
				 (define-syntax (s stx)
				   #'(begin
				       (+ 1 10)
				       14))
				 s))
      '#%app 's 'origin)
(test #t has-stx-property? (expand #'(module m mzscheme 
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
(test #t has-stx-property? (expand #'(let () (define-syntax x 1) (define y 12) 10)) 'letrec-values 'x 'disappeared-binding)
(test #t has-stx-property? (expand #'(let () (define-struct s (x)) 10)) 'letrec-values 's 'disappeared-binding)
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
                (printf "~s\n" (syntax-object->datum #'beg))
                (let-values ([(bg e)
                              (syntax-case #'beg (#%app #%top list)
                                [(bg () (#%app (#%top . list) e))
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

(module ++p mzscheme 
  (define ++c 12)
  (define-syntax (++goo stx) #'++c)
  (provide ++goo))
(module ++q mzscheme 
  (require-for-syntax ++p)
  (define ++d 11) 
  (define-syntax (++o stx) #'++d)
  (define-syntax (++s stx)
    (syntax-case stx ()
      [(_ id) #'(define-syntax (id stx) 
		  (datum->syntax-object #'here (++goo)))]))
  (define-syntax (++t stx) (syntax-case stx () [(_ id) #'(define-values (id) ++d)]))
  (define-syntax (++t2 stx) #'(begin ++d))
  (define-syntax (++t3 stx) (syntax-property #'(begin0 ++d) 'certify-mode 'transparent))
  (define-syntax (++t4 stx) (syntax-case stx () [(_ id) #'(define id ++d)]))
  (define-syntax (++v stx) #'(begin0 ++d))
  (define-syntax (++v2 stx) #'(++d))
  (define-syntax (++v3 stx) (syntax-property #'(begin ++d) 'certify-mode 'opaque))
  (define-syntax ++ds 17)
  (define-syntax (++check-val stx)
    (syntax-case stx ()
      [(_ id) (datum->syntax-object #'here (add1 (syntax-local-value #'id)))]))
  (define-syntax (++o2 stx) #'(++check-val ++ds))
  (define-syntax (++apply-to-ds stx) 
    (syntax-case stx ()
      [(_ id) #'(id ++ds)]))
  (define-syntax (++apply-to-d stx) 
    (syntax-case stx ()
      [(_ id) #'(id ++d)]))
  (provide ++o ++o2 ++s ++t ++t2 ++t3 ++t4 ++v ++v2 ++v3
	   ++apply-to-d ++apply-to-ds))

(require ++q)
(++s ++ack)
(test 12 values ++ack)
(test 11 values ++v)
(test 11 values ++o)
(test 18 values ++o2)
(test 13 values (let () (++t id) 13))

(let-syntax ([goo (lambda (stx)
		    (syntax-case stx ()
		      [(_ id) (datum->syntax-object #'here (sub1 (syntax-local-value #'id)))]))])
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

(let ([expr (expand-syntax #'++v)])
  (test expr syntax-recertify expr expr (current-inspector) #f)
    (let ([new (syntax-recertify #'no-marks expr (current-inspector) #f)])
      (test #t syntax? new)
      (test 'no-marks syntax-e new))
    (test #t syntax? (syntax-recertify (syntax-case expr ()
					 [(beg id) #'beg])
				       expr (current-inspector) #f))
    ;; we'd prefer this to fail, but it's defined to succeed:
    (test #t syntax? (syntax-recertify (syntax-case expr ()
					 [(beg id) #'id])
				       expr (current-inspector) #f))
    (test #t syntax? (syntax-recertify (datum->syntax-object expr (syntax-e expr))
				       expr (current-inspector) #f))
    ;; we'd prefer this to fail, but it's defined to succeed:
    (test #t syntax? (syntax-recertify (syntax-case expr ()
					 [(beg id) #'(ack id)])
				       expr (current-inspector) #f)))

(let ([expr (expand-syntax #'(++apply-to-d ack))])
  (test '(#%app (#%top . ack) ++d) syntax-object->datum expr)
  (let ([try (lambda (cvt? other)
	       (syntax-recertify (datum->syntax-object 
				  expr
				  (cons (car (syntax-e expr))
					((if cvt?
					     (lambda (x) (datum->syntax-object
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
      (test '(#%app other! ++d) syntax-object->datum new))
    ;; we'd prefer this to fail, but it's defined to succeed:
    (test #t syntax? (try #t (syntax-case expr ()
			       [(ap _ d) #'d])))))

    
;; ----------------------------------------

(module ++m mzscheme 
  (define ++x 10) 
  (define-syntax (++xm stx) #'100)
  (provide (protect ++x ++xm)))
(module ++n mzscheme 
  (require ++m) 
  (define ++y ++x)
  (define-syntax (++y-macro stx) #'++x)
  (define-syntax (++y-macro2 stx) (datum->syntax-object stx '++x))
  (define-syntax (++u-macro stx) #'++u)
  (define-syntax ++u2 (make-rename-transformer #'++u))
  (define ++u 8) ; unexported
  (provide ++y ++y-macro ++y-macro2 ++u-macro ++u2))
(require ++n)

(test 10 values ++y)
(test 10 values ++y-macro)
(test 8 values ++u-macro)
(test 8 values ++u2)

(require ++m)

(test 10 values ++x)
(test 100 values ++xm)
(test 10 values ++y-macro2)

(let ()
  (define n (current-namespace))
  (define n2 (make-namespace))
  (define i (make-inspector))

  (parameterize ([current-namespace n2])
    (namespace-attach-module n '++n))

  (parameterize ([current-code-inspector i]
		 [current-namespace n2])
    (teval '(require ++n))

    (test 10 teval '++y)
    (test 10 teval '++y-macro)
    (test 8 teval '++u-macro)
    (test 8 teval '++u2)

    (err/rt-test (teval '++y-macro2) exn:fail:contract:variable?)
    (err/rt-test (teval '++x) exn:fail:contract:variable?)
    (err/rt-test (teval '++xm) exn:fail:contract:variable?)

    (teval '(require ++m))
    (err/rt-test (teval '++x) exn:fail:syntax?)
    (err/rt-test (teval '++xm) exn:fail:syntax?)
    (err/rt-test (teval '++y-macro2) exn:fail:syntax?)
    
    (teval '(module zrt mzscheme 
	      (require ++n)
	      (define (vy) ++y)
	      (define (vy2) ++y-macro)
	      (define (vu) ++u-macro)
	      (define (vu2) ++u2)
	      (provide vy vy2 vu vu2)))
    (teval '(module zct mzscheme 
	      (require-for-syntax ++n)
	      (define-syntax (wy stx) (datum->syntax-object #'here ++y))
	      (let-syntax ([goo ++y-macro]) 10)
	      (define-syntax (wy2 stx) (datum->syntax-object #'here ++y-macro))
	      (define-syntax (wu stx) (datum->syntax-object #'here ++u-macro))
	      (provide wy wy2 wu)))

    (teval '(require zct))

    (test 10 teval 'wy)
    (test 10 teval 'wy2)
    (test 8 teval 'wu)

    (teval '(require zrt))

    (test 10 teval '(vy))
    (test 10 teval '(vy2))
    (test 8 teval '(vu))
    (test 8 teval '(vu2)))
  
  (let ([old-insp (current-code-inspector)])
    (parameterize ([current-code-inspector i]
		   [current-namespace n2])
      (namespace-unprotect-module old-insp '++m)))

  (parameterize ([current-code-inspector i]
		 [current-namespace n2])
    (test 10 teval '++y-macro)
    (test 10 teval '++y-macro2)))


(module ++/n mzscheme
  (provide ++/get-foo)
  (define-syntax foo #'10)
  (define-syntax (++/get-foo stx)
    (syntax-local-value #'foo)))
(require ++/n)
(test 10 values ++/get-foo)

(module ++//n mzscheme
  (provide ++//def)
  (define-syntax foo #'17)
  (define-syntax ++//def
    (syntax-rules ()
      [(_ get-foo)
       (define-syntax (get-foo stx)
	 (syntax-local-value #'foo))])))
(require ++//n)
(++//def ++//get-foo)
(test 17 values ++//get-foo)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lifting expressions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (@@foo stx)
  (syntax-case stx ()
    [(_ n)
     (if (zero? (syntax-e #'n))
	 #'0
	 (with-syntax ([m (sub1 (syntax-e #'n))])
	   (syntax-local-lift-expression #'(add1 (@@foo m)))))]))

(define lifted-output #f) 

(define-syntax (@@goo stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([id (syntax-local-lift-expression #'(set! lifted-output "lifted!"))])
       #'(list lifted-output id))]))

(test 2 '@@foo (@@foo 2))
(test 2 eval (expand-once #'(@@foo 2)))
(test 2 eval (expand #'(@@foo 2)))
(test 2 eval (expand-to-top-form #'(@@foo 2)))
(test (list "lifted!" (void)) '@@goo (@@goo))
(set! lifted-output #f)
(test (list "lifted!" (void)) eval (expand-once #'(@@goo)))
(test (list "lifted!" (void)) eval (expand #'(@@goo)))
(test (list "lifted!" (void)) eval (expand-to-top-form #'(@@goo)))

(module @@n mzscheme
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

(require-for-syntax @@n)

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

(module @@p mzscheme
  (require-for-syntax @@n)
  (provide @@goo)
  (define-syntax (@@goo stx) #`#,(@@foo 10)))

(require @@p)
(test 10 '@@goo (@@goo))

(module @@m mzscheme
  (define-syntax (@@foo stx)
    (syntax-case stx ()
      [(_ n)
       (if (zero? (syntax-e #'n))
	   #'0
	   (with-syntax ([m (sub1 (syntax-e #'n))])
	     (syntax-local-lift-expression #'(add1 (@@foo m)))))]))
  (define @@local #f)
  (define (set-local v)
    (set! @@local v))
  (set-local (@@foo 2))
  (provide @@local))

(require @@m)
(test 2 '@@local @@local)

(define-syntax (@@local-top stx)
  (syntax-case stx ()
    [(_ expr)
     (local-expand/capture-lifts #'expr
				 (list (gensym))
				 (list #'begin #'#%top))]))

(test 1 'let-foo (let ([x 5]) (@@foo 1)))
(test 1 eval (expand #'(let ([x 5]) (@@foo 1))))
(test 1 'local-foo (let ([x 5]) (@@local-top (@@foo 1))))
(test 1 eval (expand #'(let ([x 5]) (@@local-top (@@foo 1)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check interaction of macro-introduced/lifted names and
;;  module->namespace

(let ([go-once
       (lambda (eval)
	 (parameterize ([current-namespace (make-namespace)])
	   (eval '(module mm mzscheme
		    (define-syntax (define$ stx)
		      (syntax-case stx ()
			[(_ id val)
			 (with-syntax ([x (datum->syntax-object #f 'x)])
			   #'(begin
			       (define x val)
			       (define-syntax (id stx) #'x)))]))
		    (define$ a 1)
		    (define$ b 2)
		    (printf "~a ~a~n" a b)))
	   (eval '(require mm))
	   (eval '(current-namespace (module->namespace 'mm)))

	   (eval '(define$ c 7))
	   (test '(1 2 7) eval '(list a b c))
	   (eval '(define$ d 8))
	   (test '(1 2 7 8) eval '(list a b c d)))

	 (parameterize ([current-namespace (make-namespace)])
	   (eval '(module mm mzscheme
		    (define-syntax (define$ stx)
		      (syntax-case stx ()
			[(_ id val)
			 (with-syntax ([x (syntax-local-lift-expression #'val)])
			   #'(define-syntax (id stx) #'x))]))
		    (define$ a 1)
		    (define$ b 2)
		    (printf "~a ~a~n" a b)))
	   (eval '(require mm))
	   (eval '(current-namespace (module->namespace 'mm)))

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

(module @!$m mzscheme
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
(require @!$m)
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

(let ([load-ok? #t])
  (parameterize ([current-namespace (make-namespace)]
		 [current-module-name-resolver
		  (case-lambda 
		   [(a) (void)]
		   [(name _ __) 'huh?]
		   [(name _ __ load?)
		    (unless load-ok?
		      (test #f 'load-ok load?))
		    'a])])
    (let ([a-code '(module a mzscheme
		     (provide x y)
		     (define x 1)
		     (define y #'x))])
      (eval a-code)
      (let ([b-code (let ([p (open-output-bytes)])
		      (write (compile
			      '(module b mzscheme
				 (require "a")
				 (provide f)
				 (define (f) #'x)))
			     p)
		      (lambda ()
			(parameterize ([read-accept-compiled #t])
			  (read (open-input-bytes (get-output-bytes p))))))]
	    [x-id (parameterize ([current-namespace (make-namespace)])
		    (eval a-code)
		    (eval '(require a))
		    (eval '#'x))])
	(eval (b-code))
	(eval '(require b))
	(set! load-ok? #f)
	(test #f eval '(module-identifier=? (f) #'x))
	(test #t eval `(module-identifier=? (f) (quote-syntax ,x-id)))
	(eval '(require a))
	(test #t eval '(module-identifier=? (f) #'x))
	(test #t eval `(module-identifier=? (f) (quote-syntax ,x-id)))
	(parameterize ([current-namespace (make-namespace)])
	  (eval '(module a mzscheme
		   (provide y)
		   (define y 3)))
	  (set! load-ok? #t)
	  (eval (b-code))
	  (eval '(require b))
	  (set! load-ok? #f)
	  (test #t eval '(module-identifier=? (f) #'x))
	  (test #f eval `(module-identifier=? (f) (quote-syntax ,x-id))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  certification example from the manual

(module @-m mzscheme
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

(module @-n mzscheme
  (require @-m)
  (def-go go)
  (go 10)) ; access to unchecked-go is allowed

(require @-n)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propagating inactive certificates through a transparent macro-expansion result:

(module @!m mzscheme
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

(module @!n mzscheme
  (require @!m)
  (define-x def-y)
  (def-y))

;; If we get here, then macro expansion didn't fail.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
