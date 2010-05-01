
(load-relative "loadtest.rktl")

(Section 'unit)
(require mzlib/unit200)

;; Hide keywords from scheme/unit.rkt:
(define import #f)
(define export #f)
(define link #f)

(syntax-test #'(unit))
(syntax-test #'(unit (import)))
(syntax-test #'(unit (impLort)))
(syntax-test #'(unit (impLort) (export) 5))
(syntax-test #'(unit (import) (expLort) 5))
(syntax-test #'(unit import (export) 5))
(syntax-test #'(unit (import) export 5))
(syntax-test #'(unit (import) (export) . 5))
(syntax-test #'(unit (import 8) (export) 5))
(syntax-test #'(unit (import . i) (export) 5))
(syntax-test #'(unit (import (i)) (export) 5))
(syntax-test #'(unit (import i 8) (export) 5))
(syntax-test #'(unit (import i . b) (export) 5))
(syntax-test #'(unit (import i (b)) (export) 5))
(syntax-test #'(unit (import i) (export 7) 5))
(syntax-test #'(unit (import i) (export . a) (define a 6)))
(syntax-test #'(unit (import i) (export a . b) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export (a x) . b) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export (a 8) b) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export b (a 8)) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export (a . x) b) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export b (a . x)) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export (a x y) b) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export (a x . y) b) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export b (a x . y)) (define a 5) (define b 6)))

(syntax-test #'(unit (import i) (export) (begin 1 . 2)))
(syntax-test #'(unit (import i) (export b a) (begin (define a 5) (define b 6) . x)))
(syntax-test #'(unit (import i) (export b a) (begin (define a 5) (define b 6)) (define b 6)))

(syntax-test #'(unit (import) (export) (define-values (3) 5)))

(syntax-test #'(unit (import a) (export (a x) b) (define a 5) (define b 6)))
(syntax-test #'(unit (import a) (export (a x) (a y)) (define a 5) (define b 6)))
(syntax-test #'(unit (import i a) (export (a x) b) (define a 5) (define b 6)))
(syntax-test #'(unit (import b) (export (a x) b) (define a 5) (define b 6)))
(syntax-test #'(unit (import i j i) (export (a x) b) (define a 5) (define b 6)))
(syntax-test #'(unit (import i j j) (export (a x) b) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export a a) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export (a x) (b x)) (define a 5) (define b 6)))
(syntax-test #'(unit (import i) (export (a x) b) (define a 5) (define a 6) (define b 6)))
(syntax-test #'(unit (import make-i) (export (a x) b) (define a 5) (define-struct i ()) (define b 6)))
(syntax-test #'(unit (import i) (export (make-a x) b) (define make-a 5) (define-struct a ()) (define b 6)))
(syntax-test #'(unit (import i) (export (a x) b) (define a 5) (define r 6) (define r 7) (define b 6)))

(syntax-test #'(unit (import i) (export b (a x)) 5))
(syntax-test #'(unit (import i) (export (a x) b) (define x 5) (define b 6)))
(syntax-test #'(unit (import i) (export (a x) b) (set! a 5) (define b 6)))

(syntax-test #'(unit (import i) (export) (set! i 5)))

; Empty exports are syntactically ok::
(error-test #'(compound-unit (import) (link (A (0))) (export (A))) exn:fail:unit?)
(error-test #'(compound-unit (import) (link (A (0 (B))) (B (0))) (export)) exn:fail:unit?)
(error-test #'(compound-unit (import) (link (A (0)) (B (0))) (export (A x) (B))) exn:fail:unit?)

; Self-import is now allowed
; (syntax-test #'(compound-unit (import) (link (A (0 (A)))) (export))) 
; (syntax-test #'(compound-unit (import) (link (A (0 (A x)))) (export)))
(test (list (letrec ([x x]) x) 5)
      'self-import
      (invoke-unit 
       (compound-unit
	(import)
	(link [U ((unit (import a) (export b) (define x a) (define b 5) (list x a))
		  (U b))])
	(export))))

(error-test #'(invoke-unit (unit (import not-defined) (export) 10) not-defined) exn:fail:contract:variable?)

(test #t unit? (unit (import) (export)))
(test #t unit? (unit (import) (export) 5))
(test #t unit? (unit (import i) (export (a x)) (define a 8) (define x 5)))
(test 5 (lambda (f) (invoke-unit f)) (unit (import) (export) 5))

(test #t unit? (unit (import i) (export b a) (begin (define a 5) (define b 6))))
(test #t unit? (unit (import i) (export b a) 'a (begin (define a 5) (define b 6)) 'b))
(test #t unit? (unit (import i) (export b a) (begin (define a 5)) (define b 6)))
(test #t unit? (unit (import i) (export b a) (define a 5) (begin (define b 6))))
(test #t unit? (unit (import i) (export b a) (define a 5) (begin (define y 7) (define b 6)) (+ y b a)))

(test 3 'embedded-deeply ((invoke-unit (unit (import) (export) (lambda () (define x 3) x)))))
(test 1 'embedded-deeply-struct ((invoke-unit (unit (import) (export) (lambda () 
									(define-struct a ())
									make-a
									1)))))

; Empty begin is OK in a unit context:
(test #t unit? (unit (import i) (export) (begin)))
(test #t unit? (unit (import i) (export) (begin (begin))))

(syntax-test #'(compound-unit))
(syntax-test #'(compound-unit . x))
(syntax-test #'(compound-unit (import)))
(syntax-test #'(compound-unit (import) . x))
(syntax-test #'(compound-unit (import) (link)))
(syntax-test #'(compound-unit (import) (link) . x))
(syntax-test #'(compound-unit import (link) (export)))
(syntax-test #'(compound-unit (import) link (export)))
(syntax-test #'(compound-unit (import) (link) export))
(syntax-test #'(compound-unit ((import)) (link) (export)))
(syntax-test #'(compound-unit (import) ((link)) (export)))
(syntax-test #'(compound-unit (import) (link) ((export))))
(syntax-test #'(compound-unit (import . a) (link) (export)))
(syntax-test #'(compound-unit (import b . a) (link) (export)))
(syntax-test #'(compound-unit (import 1) (link) (export)))
(syntax-test #'(compound-unit (import (a)) (link) (export)))
(syntax-test #'(compound-unit (import (a . b)) (link) (export)))
(syntax-test #'(compound-unit (import (a (b))) (link) (export)))
(syntax-test #'(compound-unit (import ((a) b)) (link) (export)))
(syntax-test #'(compound-unit (import) (link . a) (export)))
(syntax-test #'(compound-unit (import) (link a) (export)))
(syntax-test #'(compound-unit (import) (link (a)) (export)))
(syntax-test #'(compound-unit (import) (link (a (b)) . c) (export)))
(syntax-test #'(compound-unit (import) (link (a (b) . c)) (export)))
(syntax-test #'(compound-unit (import) (link (a (b . c)) (c (d))) (export)))
(syntax-test #'(compound-unit (import) (link (a (b c . e)) (c (d)) (e (f))) (export)))
(syntax-test #'(compound-unit (import) (link (a (b 1))) (export)))
(syntax-test #'(compound-unit (import) (link (a (b))) (export . a)))
(syntax-test #'(compound-unit (import) (link (a (b))) (export a)))
(syntax-test #'(compound-unit (import) (link (a (b))) (export (a w) . a)))
(syntax-test #'(compound-unit (import) (link (a (b))) (export (a 1))))
(syntax-test #'(compound-unit (import) (link (a (b))) (export (a (x)))))
(syntax-test #'(compound-unit (import) (link (a (b))) (export (1 w))))

(test #t unit? (compound-unit (import) (link) (export)))

; Simple:

(define m1
  (unit
   (import)
   (export x y a? set-a-b!)
   
   (define-struct a (b c) #:mutable)

   (define x 7)
   (define z 8)
   (define y (lambda () (* z x)))

   (list x y z)))

(test #t apply (lambda (x y z) (and (= x 7) (= z 8) (procedure? y) (= 0 (procedure-arity y))))
      (invoke-unit m1))

(test #t apply
      (lambda (x y-val a? set-a-b!)
	(and (= x 7) (= y-val 56)
	     (= 1 (procedure-arity a?))
	     (= 2 (procedure-arity set-a-b!))))
      (invoke-unit
       (compound-unit 
	(import)
	(link [M (m1)]
	      [N ((unit 
		   (import x y a? set-a-b!)
		   (export)
		   (list x (y) a? set-a-b!))
		  (M x y a? set-a-b!))])
	(export))))

; Structures:


(define m2-1
  (unit 
   (import)
   (export x struct:a a? v y)

   (define x 5)
   (define-struct a (b c) #:mutable)
   (define v (make-a 5 6))
   (define (y v) (a? v))))

(define m2-2
  (unit
   (import struct:a a?)
   (export x? make-x x-z both)
   
   (define-syntax a (list #'struct:a #f #'a? (list #f) (list #f) #f))

   (define-struct (x a) (y z) #:mutable)
   (define both (lambda (v)
		  (and (a? v) (x? v))))))

(define m2-3
  (compound-unit
   (import)
   (link [O (m2-1)][T (m2-2 (O struct:a) (O a?))])
   (export [O x struct:a v y]
	   [T x? make-x x-z both])))


(let ([p (open-output-string)])
  (invoke-unit
   (compound-unit
    (import)
    (link [M (m2-3)]
	  [N ((unit
	       (import x v struct:a y x? make-x x-z both)
	       (export)
	       (define (filter v)
		 (if (procedure? v)
		     `(proc: ,(object-name v))
		     v))
	       (display 
		(map filter (list x v struct:a y make-x x? x-z both))
		p)
	       (let ([v2 (make-x 1 2 3 4)])
		 (display (map filter
			       (list x (struct-type? struct:a)
				     v (y v) (y x)
				     v2
				     (y v2)
				     (x? v2)
				     (both v)
				     (both v2)))
			  p)))
	      (M x v struct:a y x? make-x x-z both))])
    (export)))

  (test (string-append "(5 #<a> #<struct-type:a> (proc: y)"
		       " (proc: x) (proc: x?)"
		       " (proc: x-z) (proc: both))"
		       "(5 #t #<a> #t #f #<x> #t #t #f #t)")
	get-output-string p))

; Compound with circularity

(define make-z
  (lambda (x-val)
    (unit
     (import z)
     (export (x z) y)
     
     (define x x-val)
     (define y (lambda () (- z x))))))
    
(define z1 (make-z 8))
(define z2 (make-z 7))

; Dynamic linking

(let ([u
       (unit
	(import x)
	(export)
	
	(+ x 8))])

  (test 10 'dynamic (invoke-unit
		     (unit
		      (import)
		      (export w)
		      
		      (define w 2)

		      (invoke-unit u w)))))

; Misc

(test 12 'nested-units
      (invoke-unit
       (compound-unit
	(import)
	(link (a@ ((unit (import) (export s@:a) (define s@:a 5))))
	      (u@ ((compound-unit
		    (import a@:s@:a)
		    (link (r@ ((unit (import a) (export) (+ a 7)) a@:s@:a)))
		    (export))
		   (a@ s@:a))))
	(export))))

; Import linking via invoke-unit

(test '(5 7 (7 7)) 'invoke-unit-linking
      (let ([u (unit (import x) (export) x)]
	    [v (unit (import x) (export) (lambda () x))]
	    [x 5])
	(list (invoke-unit u x)
	      (begin 
		(set! x 7)
		(invoke-unit u x))
	      (let ([f (invoke-unit v x)])
		(list
		 (f)
		 (begin
		   (set! x 2)
		   (f)))))))


; Multiple values
(test '(1 2 3) 
      call-with-values
      (lambda () (invoke-unit (unit (import) (export) (values 1 2 3))))
      list)

; Units within units:

(define u (unit
	   (import)
	   (export)
	   (define y 10)
	   (define x 5)
	   (unit
	    (import)
	    (export)
	    x)))
(test #t unit? u)
(define u2 (invoke-unit u))
(test #t unit? u2)
(test 5 'invoke-unit-in-unit (invoke-unit u2))


; Units and objects combined:

(require scheme/class)

(define u@
  (unit (import x) (export)  
	(class* object% ()
		(public y)
		(define y (lambda () x))
		(super-make-object))))
(define v (invoke-unit u@ car))
(test #t class? v)
(define w (make-object v))
(test car 'ivar (send w y))

(define c%
  (class* object% ()
    (init x)
    (define -x x)
    (public z)
    (define (z) (unit (import) (export) -x))
    (super-make-object)))
(define u (send (make-object c% car) z))
(test #t unit? u)
(test car 'invoke (invoke-unit u))

(define c%
  (class* object% () 
	  (init x) 
	  (define -x x)
	  (public y z)
	  (define (y) -x)
	  (define (z) (unit (import) (export) (y)))
	  (super-make-object)))
(define u (make-object c% 3))
(define u2 (send u z))
(test #t unit? u2)
(test 3 'invoke (invoke-unit u2))

(test (letrec ([x y][y 0]) x) 'invoke 
      (invoke-unit (unit (import) (export) (define x y) (define y 7) x)))

; Can shadow syntax/macros in unit
(test #t unit? (unit 
		 (import) 
		 (export)
		 (define define 10)))
(test #t unit? (unit 
		 (import) 
		 (export)
		 (define lambda 10)))

; Shadowing also ok if it's in the export list:
(test #t unit? (unit 
		(import) 
		(export define-values)
		(define define-values 10)))
(test #t unit? (unit 
		(import) 
		(export lambda)
		(define lambda 10)))
(test #t unit? (unit 
		(import) 
		(export [lambda l])
		(define lambda 10)))

; These are ok, too:
(test #t unit? (unit
		(import define)
		(export)
		(define define 10)))
(test #t unit? (let ([define-values 5])
		 (unit
		  (import)
		  (export)
		  (define define-values 10))))
(test 10 'invoke-w/shadowed 
      (let ([define-values 5])
	(invoke-unit
	 (unit
	   (import)
	   (export define-values)
	   (define define-values 10)
	   define-values))))
	
; Check set! of shadowed variable:
(test #t unit? (unit 
		 (import x)
		 (export)
		 (let ([x 10])
		   (set! x 5))))
(test #t unit? (unit 
		 (import x)
		 (export)
		 (class object%
		   (field
		     [x 10])
		   (set! x 5))))
(syntax-test #'(let ([x 10])
		(unit 
		  (import x)
		  (export)
		  (set! x 5))))

; Especially for zodiac:
(test '(b c 10 b a (c a b) (c b a) (c . c) (a) #t
	  (nested-b a b c) (a 2 b) (10 b c) (cl-unit-a 12 c))
      'invoke-w/shadowed-a-lot
      (let ([a 'bad-a]
	    [b 'bad-b]
	    [c 'bad-c]
	    [struct:d 'bad-d]
	    [i 'bad-i])
	(invoke-unit
	 (unit
	   (import)
	   (export b)
	   (define a 'a)
	   (define b 'b)
	   (begin
	     (define c 'c)
	     (define-struct d (w)))
	   (define x '...)

	   (define-struct (e d) ())
	   (set! x (cons c c))

	   (define i (interface ()))

	   (list
	    (if (eq? a 'a)
		b
		c)
	    (if (eq? a 'bad-a)
		b
		c)
	    (d-w (make-e 10))
	    (begin a b)
	    (begin0 a b)
	    (let ([ab (list a b)])
	      (cons c ab))
	    (letrec ([mk-ba (lambda ()
			      (list b a))])
	      (cons c (mk-ba)))
	    x
	    (with-continuation-mark 
	     b a 
	     (continuation-mark-set->list (current-continuation-marks) b))
	    (interface? (interface (i)))
	    (invoke-unit
	     (unit
	       (import w a)
	       (export)
	       (define b 'nested-b)
	       (list b w a c))
	     a b)
	    (invoke-unit
	      (compound-unit
	       (import a)
	       (link [u ((unit (import c) (export (xa a) (b xb))
			       (define xa 1)
			       (define b 2)
			       (list a b c))
			 a)])
	       (export))
	      b)
	    (send
	     (make-object
	      (class object%
		(field [a 10])
		(public tester)
		(define tester
		  (lambda () (list a b c)))
		(super-make-object)))
	     tester)
	    (send
	     (make-object
	      (class object%
		(field
		 [a 10]
		 [b 12])
		(public tester)
		(define tester
		   (lambda () 
		     (invoke-unit
		      (unit 
			(import)
			(export)
			(define a 'cl-unit-a)
			(list a b c)))))
		(super-make-object)))
	     tester))))))

; Not ok if defining an imported name, but error should be about
; redefining an imported name. (This behavior is not actually tested.)
(syntax-test #'(unit 
	       (import define-values) 
	       (export)
	       (define define-values 10)))

(test #t unit? (unit 
		(import define-values) 
		(export)
		(let () (define define-values 10) define-values)))

;; Invoke-unit linking in let-bound variables
(test '(the-x 10) 'invoke 
      (let ([x 'the-x])
	(invoke-unit
	 (unit (import w) (export)
	       (list w 10))
	 x)))

;; ----------------------------------------
;; Macro definitions in units

(test 5 'stx-def
      (invoke-unit
       (unit 
	 (import)
	 (export)
	 
	 (define-syntax (five stx) #'5)
	 five)))

(test 5 'stx-def
      (let ()
	(define-values/invoke-unit (x)
	  (unit 
	    (import)
	    (export x)
	 
	    (define-syntax (five stx) #'5)
	    (define x five)))
	x))

(test 50 'stx-def
      (let ()
	(define-values/invoke-unit (x)
	  (unit 
	    (import)
	    (export x)
	 
	    (define-syntax (five stx) #'fifty)
	    (define fifty 50)
	    (define x five)))
	x))

(test 60 'stx-def
      (let ()
	(define-values/invoke-unit (x)
	  (unit 
	    (import)
	    (export x sixty)
	 
	    (define-syntax (five stx) #'sixty)
	    (define sixty 60)
	    (define x five)))
	x))

(test 70 'stx-def
      (let ([y 70])
	(define-values/invoke-unit (x)
	  (unit 
	    (import seventy)
	    (export x)
	 
	    (define-syntax (five stx) #'seventy)
	    (define x five))
	  #f y)
	x))

(test 55 'namespace
      (parameterize ([current-namespace (make-base-namespace)])
	(namespace-variable-bind/invoke-unit
	 (x)
	 (unit 
	   (import)
	   (export x)
	   
	   (define-syntax (five stx) #'55)
	   (define x five)))
	(eval 'x)))


(report-errs)
