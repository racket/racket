
(load-relative "loadtest.rktl")

;; Hide keywords from scheme/unit.rkt:
(define import #f)
(define export #f)
(define link #f)

(require mzlib/unit200)
(require mzlib/unitsig200)
(require mzlib/include)
(require racket/undefined)

(Section 'unit/sig)

(syntax-test #'(define-signature))
(syntax-test #'(define-signature))
(syntax-test #'(define-signature 8))
(syntax-test #'(define-signature . x))
(syntax-test #'(define-signature x))
(syntax-test #'(define-signature 8))
(syntax-test #'(define-signature x (8)))
(syntax-test #'(define-signature x (a . 8)))
(syntax-test #'(define-signature x (a . y)))
(syntax-test #'(define-signature x (y y)))
(syntax-test #'(define-signature x ((y))))
(syntax-test #'(define-signature x ((struct))))
(syntax-test #'(define-signature x ((struct y))))
(syntax-test #'(define-signature x ((struct . y))))
(syntax-test #'(define-signature x ((struct y . x))))
(syntax-test #'(define-signature x ((struct y x))))
(syntax-test #'(define-signature x ((struct y (x)) . x)))
(syntax-test #'(define-signature x ((unit))))
(syntax-test #'(define-signature x ((unit y))))
(syntax-test #'(define-signature x ((unit . y))))
(syntax-test #'(define-signature x ((unit y : a))))
(define-signature a ())
(syntax-test #'(define-signature x ((unit y a))))
(syntax-test #'(define-signature x ((unit y . a))))
(syntax-test #'(define-signature x ((unit y : . a))))
(syntax-test #'(define-signature x ((unit y a) . x)))
(syntax-test #'(define-signature x (y (unit y a))))

(syntax-test #'(unit/sig))
(syntax-test #'(unit/sig 8))
(syntax-test #'(unit/sig b))
(define-signature b (x y))
(syntax-test #'(unit/sig (a)))
(syntax-test #'(unit/sig a (impLort)))
(syntax-test #'(unit/sig a (impLort) 5))
(syntax-test #'(unit/sig a import 5))
(syntax-test #'(unit/sig a (import . x) . 5))
(syntax-test #'(unit/sig a (import (x) 8) 5))
(syntax-test #'(unit/sig a (import (x) . i) 5))
(syntax-test #'(unit/sig a (import (i : a) . b) 5))
(syntax-test #'(unit/sig b (import (i : a)) 5))
(syntax-test #'(unit/sig a (import (i : a x)) 5))
(syntax-test #'(unit/sig a (import (i : a) x) 5))
(syntax-test #'(unit/sig b (import (i : a)) (define x 7)))
(syntax-test #'(unit/sig b (import (i : a)) (define x 7) (define i:y 6)))
(syntax-test #'(unit/sig blah (import) (define x 7)))

(syntax-test #'(unit/sig () (import) (begin 1 . 2)))
(syntax-test #'(unit/sig () (import) (begin (define x 5)) (define x 5)))

(define b@ (unit/sig b (import) (define x 9) (define y 9)))
(define b2@ (unit/sig b (import (i : a)) (define x 9) (define y 9)))
(define b3@ (unit/sig b (import (i : ())) (define x 9) (define y 9)))
(define b3u@ (unit/sig b (import ()) (define x 9) (define y 9)))
(define b3u2@ (unit/sig b (import a) (define x 9) (define y 9)))
(define-signature >b ((unit b@ : b)))
(define b3u3@ (unit/sig b (import (i : >b)) (define x 9) (define y 9)))

(define >b@ (compound-unit/sig (import) (link [b@ : b (b@)]) (export (unit b@))))

(syntax-test #'(compound-unit/sig))
(syntax-test #'(compound-unit/sig 8))
(syntax-test #'(compound-unit/sig b))
(syntax-test #'(compound-unit/sig (import) (link) (export (var (U x)))))
(syntax-test #'(compound-unit/sig (import a) (link) (export)))
(syntax-test #'(compound-unit/sig (import 5) (link) (export)))
(syntax-test #'(compound-unit/sig (import . i) (link) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link ()) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@)) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ b)) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b)) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b ())) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@ 5))) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@ . i))) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@ (i . a)))) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@ (i a a)))) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@ c@))) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@ (c@ a)))) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export . b@)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export b@)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit c@))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@ : c))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@ (b@)))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@ : (b@)))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (open))))
(error-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@ (i : a)))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (5 (i : a)))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : b)) (link (b@ : b (b3@ (i : b)))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : b)) (link (b@ : b (b3u@ (i : b)))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : b)) (link (b@ : b (b3u2@ (i : b)))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : >b)) (link (b@ : b (b3@ (i : >b)))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : ((open a) x))) (link (b@ : b (b3@ (i : ((open a) x))))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : ((unit b@ : ((open b) w))))) (link (b@ : b (b3u3@ i))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : a)) (link (b@ : (w) (b@))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : ())) (link (b@ : b (b3u3@ i))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : ((unit b@ : ())))) (link (b@ : b (b3u3@ i))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : (b@))) (link (b@ : b (b3u3@ i))) (export)) exn:fail:unit?)
(error-test #'(compound-unit/sig (import (i : ((unit b@ : (x (unit y : ())))))) (link (b@ : b (b3u3@ i))) (export)) exn:fail:unit?)
(syntax-test #'(compound-unit/sig (import) (link [b@ : b (0 5)]) (export)))
(syntax-test #'(compound-unit/sig (import) (link [b@ : b (0 ())]) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : a (5 (i : b)))) (export)))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var b@))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@)))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ x y)))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (5 x)))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ 5)))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var ((b@ w) 5)))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var ((b@ 7) 5)))))
(syntax-test #'(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ x . a)))))

; Self-import is now allowed
; (syntax-test #'(compound-unit/sig (import) (link (A : () (0 A))) (export))) ; self-import
; (syntax-test #'(compound-unit/sig (import) (link (A : (x) (0 A))) (export))) ; self-import
(test (list undefined 5)
      'self-import
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [U : (a) ((unit/sig (a) (import (a)) (rename (b a)) (define x a) (define b 5) (list x a))
			U)])
	(export))))

(define-signature not-defined^ (not-defined))
(error-test #'(invoke-unit/sig (unit/sig () (import not-defined^) 10) not-defined^) exn:fail:contract:variable?)

(test #t unit/sig? (unit/sig a (import)))
(test #t unit/sig? (unit/sig b (import) (define x 1) (define y 2)))
(test #t unit/sig? (unit/sig a (import (i : b)) i:x))
(test 5 (lambda (f) (invoke-unit/sig f ())) (unit/sig a (import ()) 5))
(test #t unit/sig? (unit/sig (x) (import) (begin (define x 5))))
(test #t unit/sig? (unit/sig (x) (import) (define a 14) (begin (define x 5) (define y 10)) (define z 12)))
(test #t unit/sig? (compound-unit/sig (import) (link) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b2@ (i : a)))) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b2@ ((i) : a)))) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b2@ ((i) : ())))) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ x)))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ x) w))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var ((b@) x) w))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit (b@)))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@ b@))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (open b@))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (open (b@ : b)))))

(test #t unit/sig? (compound-unit/sig (import)
		      (link [compound-unit : () ((unit/sig () (import) 10))])
		      (export)))
(test #t unit/sig? (compound-unit/sig 
		    (import)
		    (link [export : () ((unit/sig () (import) 10))])
		    (export)))

; Empty begin is OK in a unit context:
(test #t unit/sig? (unit/sig () (import) (begin)))
(test #t unit/sig? (unit/sig () (import) (begin (begin))))

; Include:

(define i1@
  (unit/sig 
   ()
   (import)
   
   (include "uinc.rktl")))

(test 9 'include (invoke-unit/sig i1@))

;; Nested includes, macros that expand to `(include ...)'
(define i1.5@
  (unit/sig 
   ()
   (import)
   
   (+ 3 4)
   (include "uinc3.rktl")))

(test 9 'include (invoke-unit/sig i1.5@))

(define i2@
  (unit/sig 
   ()
   (import)
   
   (include "uinc.rktl")
   (include "uinc2.rktl")
   (include "uinc.rktl")
   (+ x 2)))

(test 10 'include (invoke-unit/sig i2@))

;; Include with begin:

(test 5 'include-begin
      (invoke-unit/sig
       (unit/sig ()
	 (import)
	 (define x 5)
	 (include "binc.rktl")
	 y)))

; Simple:

(define-signature m1^
  (x y a? set-a-b!))

(define m1@
  (unit/sig 
   m1^
   (import)
   
   (define-struct a (b c) #:mutable)

   (define x 7)
   (define z 8)
   (define y (lambda () (* z x)))

   (list x y z)))

(test #t apply (lambda (x y z) (and (= x 7) (= z 8) (procedure? y) (= 0 (procedure-arity y))))
      (invoke-unit/sig m1@))

(test #t apply
      (lambda (x y-val a? set-a-b!)
	(and (= x 7) (= y-val 56)
	     (= 1 (procedure-arity a?))
	     (= 2 (procedure-arity set-a-b!))))
      (invoke-unit/sig
       (compound-unit/sig 
	(import)
	(link [M@ : m1^ (m1@)]
	      [N@ : () ((unit/sig
			 ()
			 (import (i@ : m1^))
			 (list i@:x (i@:y) i@:a? i@:set-a-b!))
			M@)])
	(export (open M@)))))

; More:

(define-signature m2-1-lite^
  (xx struct:a v y))

(define-signature m2-1^
  (a?
   (open m2-1-lite^)))

(define-signature m2-2^
  (x? make-x x-z both))

(define m2-1@
  (unit/sig 
   m2-1^
   (import)

   (define xx 5)
   (define-struct a (b c) #:inspector (make-inspector) #:mutable)
   (define v (make-a 5 6))
   (define (y v) (a? v))))

(define m2-2@
  (unit/sig
   m2-2^
   (import m2-1^)

   (define-syntax a (list #'struct:a #f #f (list #f) (list #f) #f))

   (define-struct (x a) (y z) #:mutable)
   (define both (lambda (v)
		  (and (a? v) (x? v))))))

(define-signature m2-3^
  (simple))

(let-signature m2-3^
  ((unit one@ : m2-1-lite^)
   (unit two@ : m2-2^)
   a?-again)

  (define m2-3@
    (compound-unit/sig
     (import)
     (link [O@ : m2-1^ (m2-1@)]
	   [T@ : m2-2^ (m2-2@ O@)])
     (export (unit (O@ : m2-1-lite^) one@)
	     (unit T@ two@)
	     (var (O@ a?) a?-again))))

  (let ([p (open-output-string)]
	[filter (lambda (v)
		  (if (procedure? v)
		      `(proc: ,(object-name v))
		      v))])
    (invoke-unit/sig
     (compound-unit/sig
      (import)
      (link [M@ : m2-3^ (m2-3@)]
	    [N@ : () ((unit/sig
		       ()
		       (import (i : m2-3^))
		       (display (map
				 filter
				 (list i:one@:xx i:one@:v i:one@:struct:a i:one@:y 
				       i:two@:make-x i:two@:x? i:two@:x-z i:two@:both
				       i:a?-again))
				p)
		       (let ([v2 (i:two@:make-x 1 2 3 4)])
			 (display (map
				   filter
				   (list i:one@:xx (struct-type? i:one@:struct:a)
					 i:one@:v (i:one@:y i:one@:v) (i:one@:y i:one@:xx)
					 v2
					 (i:one@:y v2)
					 (i:two@:x? v2)
					 (i:two@:both i:one@:v)
					 (i:two@:both v2)))
				  p)))
		      M@)])
      (export)))
    (test (string-append "(5 #(struct:a 5 6) #<struct-type:a> (proc: y)"
			 " (proc: x) (proc: x?)"
			 " (proc: x-z) (proc: both) (proc: a?))"
			 "(5 #t #(struct:a 5 6) #t #f #(struct:x 1 2 ...) #t #t #f #t)")
	  get-output-string p)))

(test 5 'let-sig
      (invoke-unit/sig
       (unit/sig
	m2-3^
	(import)
	(define simple 5)
	simple)))

(define-signature big^
  (a b c))
(define-signature little^
  (a b c))

(test 11
      'link-restrict 
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [a@ : big^ ((unit/sig big^ (import) (define a 5) (define b 6) (define c 7)))]
	      [b@ : () ((unit/sig () (import (i : little^)) (+ i:a i:b)) 
			(a@ : little^))])
	(export))))

(define-signature just-a^
  (a))
(define-signature >just-a^
  ((unit s@ : just-a^)))

; Test a path for linking: root is a constiuent
(test 12
      'link-path
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [a@ : >just-a^ ((compound-unit/sig
			       (import)
			       (link [i@ : just-a^ ((unit/sig 
						     just-a^ 
						     (import) 
						     (define a 5)))])
			       (export (unit i@ s@))))]
	      [r@ : () ((unit/sig 
			 () 
			 (import (i : just-a^))
			 (+ i:a 7))
			(a@ s@))])
	(export))))

; Test a path for linking: root is a constituent, interface is restricted
(test 12
      'import-path
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [a@ : >just-a^ ((compound-unit/sig
			       (import)
			       (link [i@ : just-a^ ((unit/sig 
						     just-a^ 
						     (import) 
						     (define a 5)))])
			       (export (unit i@ s@))))]
	      [u@ : () ((compound-unit/sig
			 (import (a@ : >just-a^))
			 (link [r@ : () ((unit/sig 
					  () 
					  (import (i : just-a^))
					  (+ i:a 7))
					 (a@ s@))])
			 (export))
			a@)])
	(export))))

; Test a path for linking: 
(test 85
      'import-path
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [zodiac : (z)
		      ((unit/sig (z)
			 (import (i))
			 
			 (define z 1))
		       ((export* interface) : (i)))]
	      [export* : ((unit interface : (i extra)))
		       ((compound-unit/sig
			 (import)
			 (link [interface : (i extra)
					  ((unit/sig (i extra)
					     (import)
					     (define i 1)
					     (define extra 2)
					     85))])
			 (export (unit interface))))])
	(export))))

; Signature ordering

(define o1 (unit/sig (num sym) (import) (define num 5) (define sym 'a)))
(define o2 (unit/sig () (import (sym num)) (list sym (+ num))))

(test (list 'a 5)
      'order
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [one : (num sym) (o1)]
	      [two : () (o2 one)])
	(export))))

; unit->unit/sig, etc.

(define-signature s1
  (a b c))
(define-signature s2
  (+))

(define us1 
  (unit
   (import +)
   (export a b c)
   
   (define a 1)
   (define b 2)
   (define c 3)
   (+ a b c)))
   
(test 6 'u->s (invoke-unit us1 +))
(test 6 'u->s (invoke-unit/sig (unit->unit/sig us1 (s2) s1) s2))

; Exporting a name twice:

(syntax-test
 #'(compound-unit/sig
    (import)
    (link [A : (a) ((unit/sig (a) (import) (define a 1)))])
    (export (var (A a)) (open A))))

(syntax-test
 #'(compound-unit/sig
    (import)
    (link [A : (a) ((unit/sig (a) (import) (define a 1)))]
	  [B : (b) ((unit/sig (b) (import) (define b 2)))])
    (export (unit A x) (unit B x))))

(syntax-test
 #'(compound-unit/sig
    (import)
    (link [A : (a) ((unit/sig (a) (import) (define a 1)))]
	  [B : (b) ((unit/sig (b) (import) (define b 2)))])
    (export (unit A) (unit B A))))

; Can shadow syntax/macros in unit
(test #t unit/sig? (unit/sig ()
		     (import) 
		     (define define 10)))
(test #t unit/sig? (unit/sig ()
		     (import) 
		     (define lambda 11)))

; Shadowing ok if it's in the export list:
(test #t unit/sig? (unit/sig (define-values)
		    (import) 
		    (define define-values 12)))
(test #t unit/sig? (unit/sig (lambda)
		    (import) 
		    (define lambda 13)))
(test #t unit/sig? (unit/sig (l)
		    (import) 
		    (rename (lambda l))
		    (define lambda 14)))

(test 9 'rename (let ()
		  (define-signature s (b))
		  (define-values/invoke-unit/sig s
		    (unit/sig s
		      (import)
		      (rename (a b))
		      (define a 9)))
		  b))
(test 10 'rename (let ()
		   (define-signature s (b))
		   (define-values/invoke-unit/sig s
		     (unit/sig s
		       (import)
		       (rename (a b))
		       (define a 10)
		       (define b 12)))
		   b))

; These are ok, too:
(test #t unit/sig? (unit/sig ()
		    (import (define))
		    (define define 15)))
(test #t unit/sig? (let ([define-values 5])
		     (unit/sig ()
		      (import)
		      (define define-values 16))))

; Not ok if defining an imported name, but error should be about
; redefining an imported name. (This behavior is not actually tested.)
(syntax-test #'(unit/sig ()
		 (import (define-values))
		 (define define-values 17)))

(test #t unit/sig? (unit/sig ()
		     (import (define-values))
		     (let () (define define-values 10) define-values)))

;; Invoke-unit linking in let-bound variables
(define x 'not-the-right-x)
(test '(the-x 10) 'invoke/sig 
      (let ([x 'the-x])
	(invoke-unit/sig
	 (unit/sig () (import (x))
		   (list x 10))
	 (x))))

;; Check -setters, -selectors, and -

(let ([foo-bar 10]
      [set-foo-bar! 100]
      [foo@ (unit/sig ((struct foo (bar)))
	      (import)
	      (define-struct foo (bar) #:mutable))])
  (define-syntax (go stx)
    (syntax-case stx ()
      [(_ lookup omit ...)
       (syntax
	(invoke-unit/sig
	 (compound-unit/sig
	  (import)
	  (link [FOO : ((struct foo (bar))) (foo@)]
		[CLIENT : () ((unit/sig ()
				(import ((struct foo (bar) omit ...)))
				(if (eq? 'foo-bar 'lookup)
				    foo-bar
				    set-foo-bar!))
			      (FOO : ((struct foo (bar) omit ...))))])
	  (export))))]))

  (test #t struct-accessor-procedure? (go foo-bar))
  (test #t struct-mutator-procedure? (go set-foo-bar!))
  (test 10 'no-sel (go foo-bar -selectors))
  (test #t struct-mutator-procedure? (go set-foo-bar! -selectors))
  (test #t struct-accessor-procedure? (go foo-bar -setters))
  (test 100 'no-set (go set-foo-bar! -setters))
  (test 10 'no-sel (go foo-bar (- foo-bar)))
  (test 100 'no-set (go set-foo-bar! (- set-foo-bar!)))
  (test #t struct-accessor-procedure? (go foo-bar (- set-foo-bar!)))
  (test #t struct-mutator-procedure? (go set-foo-bar! (- foo-bar)))
  (test #t struct-accessor-procedure? (go foo-bar (- make-foo)))
  (test #t struct-mutator-procedure? (go set-foo-bar! (- make-foo))))

;; Definitions and namespace:
(test 12
      'def-val
      (let ()
	(define-values/invoke-unit/sig (foo)
	  (unit/sig (foo)
	    (import) 
	    (define foo 12)))
	foo))

(test 120
      'namespace
      (parameterize ([current-namespace (make-base-namespace)])
	(namespace-variable-bind/invoke-unit/sig
	 (foo)
	 (unit/sig (foo)
	   (import) 
	   (define foo 120)))
	(eval 'foo)))

;; -- Macro interaction ----------------------------------------

(define-syntax let-values/invoke-unit/sig
  (syntax-rules ()
    [(_ (sig unit) exp ...)
     (let ()
       (define-values/invoke-unit/sig sig unit)
       (let () exp ...))]))

(define-signature b (y z))
(define-signature a (x (open b)))
(define-signature c (x (unit i : b)))

(define u@ (unit/sig a
	     (import)
	     (define x 1)
	     (define y 2)
	     (define z 3)))

(test '(1 2 3) 'macro-unitsig
      (let-values/invoke-unit/sig ((x y z) u@) (list x y z)))
(test '(1 2 3) 'macro-unitsig
      (let-values/invoke-unit/sig ((x (open b)) u@) (list x y z)))

(define-syntax goo
  (syntax-rules ()
    [(_ id body)
     (let-values/invoke-unit/sig ((x id) u@) body)]))

(test '(0 2 0) 'macro-unitsig
      (let ([x 0][y 0][z 0])
	(goo y (list x y z))))

(test '(0 2 3) 'macro-unitsig
      (let ([x 0][y 0][z 0])
	(goo (open b) (list x y z))))

(define-syntax goow
  (syntax-rules ()
    [(_ sid body)
     (let-values/invoke-unit/sig ((x (open sid)) u@) body)]))

(test '(0 2 3) 'macro-unitsig
      (let ([x 0][y 0][z 0])
	(goow b (list x y z))))

(define t@ (compound-unit/sig
	    (import)
	    (link [u1 : a (u@)]
		  [u2 : b (u@)])
	    (export (open u1) (unit u2 i))))

(test '(1 2 3) 'macro-unitsig
      (let-values/invoke-unit/sig (c t@) (list x i:y i:z)))

(define-syntax moo
  (syntax-rules ()
    [(_ id body)
     (let-values/invoke-unit/sig ((x id) t@) body)]))

(test '(0 2 3) 'macro-unitsig
      (let ([x 0][i:y 0][i:z 0])
	(moo (unit i : b) (list x i:y i:z))))

(define-syntax moow
  (syntax-rules ()
    [(_ id body)
     (let-values/invoke-unit/sig ((x (unit i : id)) t@) body)]))

(test '(0 2 3) 'macro-unitsig
      (let ([x 0][i:y 0][i:z 0])
	(moow b (list x i:y i:z))))

(test '(0 2 3) 'macro-unitsig
      (let ([x 0][i:y 0][i:z 0])
	(moow (y z) (list x i:y i:z))))

(test '(0 0 3) 'macro-unitsig
      (let ([x 0][i:y 0][i:z 0])
	(moow (z) (list x i:y i:z))))

(define-syntax moot
  (syntax-rules ()
    [(_ id body)
     (let-values/invoke-unit/sig ((id (unit i : b)) t@) body)]))

(test '(1 0 0) 'macro-unitsig
      (let ([x 0][i:y 0][i:z 0])
	(moot x (list x i:y i:z))))

;; --------------------------------------------------

(let ()
  (define-signature s^ (x)) 
  (test (void)
	verify-linkage-signature-match 'where 
	(list 'tag) 
	(list (unit/sig s^ (import) (define x 1)))
	(list (signature->symbols s^))
	(list (list))))

;; ----------------------------------------
;; In a submodule

(module unit-in-a-submodule racket/base
  (require racket/unit)

  (define-signature foo^ (f))
  
  (module+ main
    (define-unit foo@
      (import)
      (export foo^)
      
      (define f (lambda (x) x)))
    (define-values/invoke-unit/infer foo@)

    (define out (f 50))
    (provide out)))

(test 50 dynamic-require '(submod 'unit-in-a-submodule main) 'out)

;; --------------------------------------------------

(report-errs)

