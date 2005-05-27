
; Test MzScheme's object system

(load-relative "loadtest.ss")

(require (lib "class.ss"))

(SECTION 'OBJECT)

(define (stx-test e)
  (syntax-test (datum->syntax-object #f e #f)))
(define (err-test e exn)
  (error-test (datum->syntax-object #f e #f) exn))

(define (test-class* cl* renames)
  (stx-test  `(,cl*))
  (stx-test  `(,cl* ,@renames . x))
  (stx-test  `(,cl* ,@renames 0))
  (stx-test  `(,cl* ,@renames object% . x))
  (stx-test  `(,cl* ,@renames object% 0))
  (stx-test  `(,cl* ,@renames object% x))
  (stx-test  `(,cl* ,@renames object% ()))
  (stx-test  `(,cl* ,@renames object% () (0) x))
  (stx-test  `(,cl* ,@renames object% () 0))
  (stx-test  `(,cl* ,@renames object% () . x))
  (stx-test  `(,cl* ,@renames object% () () . x))
  (stx-test  `(,cl* ,@renames object% () () x))
  (stx-test  `(,cl* ,@renames object% () () public))
  (stx-test  `(,cl* ,@renames object% () () (x)))
  (stx-test  `(,cl* ,@renames object% () (x) ()))

  (let ()
    (define (try-dotted cl)
      (stx-test  `(,cl* ,@renames object% () () (,cl . x))))
    
    (map try-dotted '(public override private inherit rename
			     inherit-from rename-from
			     sequence)))
  
  (let ()
    (define (try-defn-kind cl)
      (stx-test  `(,cl* ,@renames object% () () (,cl 8)))
      (stx-test  `(,cl* ,@renames object% () () (,cl [8 9])))
      (stx-test  `(,cl* ,@renames object% () () (,cl [(x) 9])))
      (stx-test  `(,cl* ,@renames object% () () (,cl [(x y x) 9])))
      (stx-test  `(,cl* ,@renames object% () () (,cl [x . 1])))
      (stx-test  `(,cl* ,@renames object% () () (,cl [x 1 . 3])))
      (stx-test  `(,cl* ,@renames object% () () (,cl [x 1 3]))))
    
    (try-defn-kind 'public)
    (try-defn-kind 'override)
    (try-defn-kind 'private))

  (let ()
    (define (try-defn-rename-kind cl)
      (stx-test  `(,cl* ,@renames object% () () (,cl [((x) y) 9])))
      (stx-test  `(,cl* ,@renames object% () () (,cl [(x (y)) 9])))
      (stx-test  `(,cl* ,@renames object% () () (,cl [(x . y) 9])))
      (stx-test  `(,cl* ,@renames object% () () (,cl [(x 1) 9])))
      (stx-test  `(,cl* ,@renames object% () () (,cl [(1 x) 9]))))
    (try-defn-rename-kind 'public)
    (try-defn-rename-kind 'override))

  (let ()
    (define (try-ref-kind cl)
      (stx-test  `(,cl* ,@renames object% () () (,cl 8)))
      (stx-test  `(,cl* ,@renames object% () () (,cl x 8)))
      (stx-test  `(,cl* ,@renames object% () () (,cl (x . y))))
      (stx-test  `(,cl* ,@renames object% () () (,cl (x y z)))))
    
    (map try-ref-kind '(inherit rename share)))
  (err-test `(,cl* ,@renames object% () () (inherit x)) exn:object?)
  (err-test `(,cl* ,@renames object% () () (inherit (x y))) exn:object?)
  (err-test `(,cl* ,@renames object% () () (override [x void])) exn:object?)
  (err-test `(,cl* ,@renames object% () () (override [(x y) void])) exn:object?)
  (stx-test  `(,cl* ,@renames object% () () (inherit (x y z))))
  (stx-test  `(,cl* ,@renames object% () () (inherit (x 5))))
  (stx-test  `(,cl* ,@renames object% () () (inherit (x))))
  (stx-test  `(,cl* ,@renames object% () () (rename x)))
  (stx-test  `(,cl* ,@renames object% () () (rename (x))))
  (stx-test  `(,cl* ,@renames object% () () (rename ((x) y))))
  (stx-test  `(,cl* ,@renames object% () () (rename ((x y) y))))
  (stx-test  `(,cl* ,@renames object% () () (rename ((1) y))))

  (stx-test  `(,cl* ,@renames object% () () (inherit x) (sequence (set! x 5))))
  (stx-test  `(,cl* ,@renames object% () () (rename [x y]) (sequence (set! x 5))))

  (stx-test  `(,cl* ,@renames object% () () (sequence 1 . 2)))
  
  (stx-test  `(,cl* ,@renames object% () () (public [x 7] [x 9])))
  (stx-test  `(,cl* ,@renames object% () (x) (public [x 7])))
  (stx-test  `(,cl* ,@renames object% () (x) (public [(x w) 7])))
  (stx-test  `(,cl* ,@renames object% () () (public [(x y) 7] [(z y) 9])))
  (stx-test  `(,cl* ,@renames object% () () (public [(x y) 7] [(x z) 9])))

  (stx-test  `(,cl* ,@renames object% a ()))
  (stx-test  `(,cl* ,@renames object% (1 . a) ())))

(test-class* 'class* ())
(test-class* 'class*/names '((this super)))

(stx-test #'(class*/names 8 object% () () ()))
(stx-test #'(class*/names () object% () ()))
(stx-test #'(class*/names (8) object% () ()))
(stx-test #'(class*/names (this . 8) object% () ()))
(stx-test #'(class*/names (this 8) object% () ()))
(stx-test #'(class*/names (this super-init . 8) object% () ()))
(stx-test #'(class*/names (this super-init 8) object% () ()))

(test #t class? (class* object% () ()))
(test #t class? (class* object% () ()))
(test #t class? (class* object% () x))
(test #t class? (class* object% () () (public)))
(test #t class? (class* object% () () (public sequence)))
(test #t class? (class* object% () (x) (public [(y x) 9])))
(test #t class? (class*/names (this super-init) object% () () (public)))

(define c (class object% () (public x)))
(err/rt-test (class c () (public x)) exn:object?)
(err/rt-test (class c () (public ([y x] 5))) exn:object?)
(err/rt-test (class c () (override ([x y] 5))) exn:object?)

(stx-test #'(interface))
(stx-test #'(interface . x))
(stx-test #'(interface 8))
(stx-test #'(interface () 8))
(stx-test #'(interface () x . y))
(stx-test #'(interface () x 8))
(stx-test #'(interface () x x))
(err/rt-test (interface (8) x) exn:object?)

(err/rt-test (interface ((class->interface (class object% ()))
			 (class->interface (class object% ()))))
	    exn:object?)

(err/rt-test (interface ((interface () x)) x) exn:object?)
(err/rt-test (interface ((interface ((interface () x)) y)) x) exn:object?)
(test #t interface? (let ([i (interface () x)]
			  [j (interface () x)])
		      (interface (i j) y)))
(err/rt-test (let ([i (interface () x)]
		   [j (interface () x)])
	       (interface (i j) x))
	    exn:object?)
(err/rt-test (interface ((class->interface (class object% () (public w)))) w)
	    exn:object?)

(test #t interface? (interface ()))
(test #t interface? (interface () x))
(test #f interface? (class* object% () ()))

(define i0.1 (interface () x y))
(define i0.2 (interface () y c d))
(define i1 (interface (i0.1 i0.2) e))
(define ix (interface () x y))

(test #t interface-extension? i1 i0.1)
(test #t interface-extension? i1 i0.2)
(test #f interface-extension? i0.1 i1)
(test #f interface-extension? i0.2 i1)
(test #f interface-extension? i0.2 i0.1)
(test #f interface-extension? i0.1 i0.2)

(err/rt-test (let [(bad (class* object% (i0.1) ()))] bad) exn:object?)
(test #t class? (class* object% (i0.1) () (public x y)))
(err/rt-test (let ([cl (class* object% (i0.1 i0.2) () (public x y c))]) cl) exn:object?)
(err/rt-test (class* object% (i1) () (public x y c)) exn:object?)
(test #t class? (class* object% (i0.1 i0.1) () (public x y c d)))
(err/rt-test (class* object% (i1) () (public x y c d)) exn:object?)
(test #t class? (class* object% (i1) () (public x y c d e)))

; No initialization:
(define no-init-c% (class* object% () ()))
(err/rt-test (make-object no-init-c%) exn:object?)

(define c1 
  (let ((v 10))
    (class* object% (i1) (in [in-2 'banana] . in-rest)
	   (public (x 1) (y 2))
	   (private (a in) (b3 3))
	   (public (b1 2) (b2 2) (e 0))
	   (public (c 3) (d 7)
		   (f-1-a (lambda () a))
		   (f-1-b1 (lambda () b1))
		   (f-1-b2 (lambda () b2))
		   (f-1-c (lambda () c))
		   (f-1-v (lambda () v))
		   (f-1-x (lambda () x))
		   (f-1-top-a (lambda () (ivar this a)))
		   (f-1-other-e (lambda (o) (ivar o e)))
		   (f-1-set-b2 (lambda (v) (set! b2 v) b2))
		   (f-1-in-2 (lambda () in-2))
		   (f-1-in-rest (lambda () in-rest)))
	   (sequence
	     (set! e in)
	     (super-init)))))
  
(test #t implementation? c1 i0.1)
(test #t implementation? c1 i0.2)
(test #t implementation? c1 (class->interface c1))
(test #t implementation? c1 i1)
(test #f implementation? c1 ix)

(test #t implementation? object% (class->interface object%))
(test #t implementation? c1 (class->interface c1))
(test #t implementation? (class c1 ()) (class->interface c1))
(let ([i (interface ((class->interface c1)))])
  (test #f implementation? c1 i)
  (test #t implementation? (class* c1 (i) ()) i))

(define o1 (make-object c1 0 'apple "first" "last"))

(define c2 
  (let ((v 20))
    (class c1 ()
	   (inherit b2 (sup-set-b2 f-1-set-b2))
	   (rename (also-e e)
		   (also-b2 b2))
	   (override (b1 5) (c 6))
	   (public (a 4)
		   (f-2-a (lambda () a))
		   (f-2-b1 (lambda () b1))
		   (f-2-b2 (lambda () b2))
		   (f-2-also-b2 (lambda () also-b2))
		   (f-2-c (lambda () c))
		   ((i-f-2-v f-2-v) (lambda () v))
		   (f-2-v-copy (lambda () (i-f-2-v)))
		   (f-2-set-b2 (lambda (v) (sup-set-b2 v))))
	   (private (y 3))
	   (sequence
	     (super-init 1)))))

(test #t implementation? c2 i0.1)
(test #t implementation? c2 i0.2)
(test #t implementation? c2 i1)
(test #f implementation? c2 ix)
(test #t implementation? c2 (class->interface c2))
(test #t implementation? c2 (class->interface c1))
(test #f implementation? c1 (class->interface c2))

(test #t interface-extension? (class->interface c2) (class->interface object%))
(test #t interface-extension? (class->interface c2) (class->interface c1))
(test #t interface-extension? (class->interface c2) (class->interface c2))
(test #f interface-extension? (class->interface c1) (class->interface c2))
(test #t interface-extension? (class->interface c2) i0.1)
(test #f interface-extension? i0.1 (class->interface c2))

(define o2 (make-object c2))

(define c2.1
  (class*/names (this c2-init) c2 () ()
	  (sequence
	    (c2-init))))

(define o2.1 (make-object c2.1))

(test #t interface? (interface ((class->interface c2)
				(class->interface c2.1))))

(define c3
  (class* object% () ()
	  (public (x 6) (z 7) (b2 8)
		  (f-3-b2 (lambda () b2)))
	  (sequence (super-init))))

(define o3 (make-object c3))

(define c6
  (class object% (x-x)
    (public
     [(i-a x-a) (lambda () 'x-a)]
     [(x-a i-a) (lambda () 'i-a)]
     [(i-x x-x) (lambda () 'x-x)]
     [x-a-copy (lambda () (i-a))]
     [i-a-copy (lambda () (x-a))])
    (sequence (super-init))))

(define o6 (make-object c6 'bad))

(define c7
  (class*/names (self super-init) object% () ()
    (public
     [get-self (lambda () self)])
    (sequence (super-init))))

(define o7 (make-object c7))

(define display-test 
  (lambda (p v)
    (printf "Should be ~s: ~s ~a~n"
	    p v (if (equal? p v)
		    ""
		    "ERROR"))))

(define ivar? exn:object?)

(test #t is-a? o1 c1)
(test #t is-a? o1 i1)
(test #t is-a? o1 (class->interface c1))
(test #f is-a? o1 (interface ((class->interface c1))))
(test #t is-a? o2 c1)
(test #t is-a? o2 i1)
(test #f is-a? o1 c2)
(test #f is-a? o1 (class->interface c2))
(test #t is-a? o2 c2)
(test #t is-a? o2.1 c1)
(test #f is-a? o1 c3)
(test #f is-a? o2 c3)
(test #f is-a? o1 ix)
(test #f is-a? o2 ix)
(test #f is-a? o3 i1)
(test #f is-a? i1 i1)
(test #t subclass? c2 c1)
(test #t subclass? c2.1 c1)
(test #f subclass? c1 c2)
(test #f subclass? c1 c3)
(test #f subclass? i1 c3)
(test #t ivar-in-interface? 'f-1-a (class->interface c1))
(test #t ivar-in-interface? 'f-1-a (class->interface c2))
(test #f ivar-in-interface? 'f-2-a (class->interface c1))
(test #t ivar-in-interface? 'f-2-a (class->interface c2))
(test #t ivar-in-interface? 'x i0.1)
(test #t ivar-in-interface? 'x i1)
(test #f ivar-in-interface? 'x i0.2)
(test #f ivar-in-interface? 'c i0.1)
(test #t ivar-in-interface? 'c i0.2)
(test #t ivar-in-interface? 'c i1)
(test #f ivar-in-interface? 'zzz i1)
(test #t ivar-in-interface? 'f-1-a (class->interface c2))
(test #t ivar-in-interface? 'f-1-a (interface ((class->interface c2)) one-more-method))
(test #f ivar-in-interface? 'f-2-a (class->interface c1))

(err/rt-test (is-a? o1 o1))
(err/rt-test (subclass? o1 o1))
(err/rt-test (subclass? o1 i1))
(err/rt-test (implementation? o1 o1))
(err/rt-test (implementation? o1 c1))
(err/rt-test (ivar-in-interface? 0 i1))
(err/rt-test (ivar-in-interface? 'a o1))
(err/rt-test (ivar-in-interface? 'a c1))
(err/rt-test (ivar-in-interface? 'a o1))

(define (test/list l1 l2)
  (test #t 'ivar-list (and (= (length l1)
			      (length l2))
			   (andmap (lambda (i) (member i l2))
				   l1)
			   #t)))

(test/list '(hi there)
	   (interface->ivar-names 
	    (interface () hi there)))
(test/list '(hi too mee there)
	   (interface->ivar-names 
	    (interface ((interface () hi there)) mee too)))
(test/list '(hi too mee z y there) 
	   (interface->ivar-names 
	    (interface ((interface ((class->interface 
				     (class object% () 
				       (public y z)
				       (private nono)))) 
			  hi there)) 
	      mee too)))


(test 0 class-initialization-arity object%)
(test #t arity-at-least? (class-initialization-arity c1))
(test 1 arity-at-least-value (class-initialization-arity c1))
(test 0 class-initialization-arity c2)

(test '(1 2) class-initialization-arity (class object% (a [b 2])))

(arity-test object? 1 1)
(arity-test class? 1 1)
(arity-test interface? 1 1)
(arity-test is-a? 2 2)
(arity-test subclass? 2 2)
(arity-test interface-extension? 2 2)
(arity-test ivar-in-interface? 2 2)
(arity-test class-initialization-arity 1 1)

(arity-test ivar/proc 2 2)
(arity-test make-generic/proc 2 2)

(err/rt-test (ivar o1 a) ivar?)
(test 4 ivar/proc o2 'a)

(define (ivar-tests -ivar xtra-ok?)
  (stx-test  `(,-ivar))
  (stx-test  `(,-ivar 7))
  (stx-test  `(,-ivar 7 8))
  (stx-test  `(,-ivar 7 (x)))
  (stx-test  `(,-ivar 7 8 9))
  (unless xtra-ok?
    (stx-test  `(,-ivar 7 x 9))))
(ivar-tests 'ivar #f)
(ivar-tests 'send #t)
(ivar-tests 'make-generic #f)

(test 0 'send (send o1 f-1-a))
(test 1 'send (send o2 f-1-a))
(test 4 'send (send o2 f-2-a))

(test 'apple 'send (send o1 f-1-in-2))
(test 'banana 'send (send o2 f-1-in-2))
(test '("first" "last") 'send (send o1 f-1-in-rest))
(test '() 'send (send o2 f-1-in-rest))

(err/rt-test (send o1 f-1-top-a) ivar?)
(test 4 'send (send o2 f-1-top-a))

(test 5 ivar/proc o2 'b1)

(test 2 'send (send o1 f-1-b1))
(test 2 'send (send o1 f-1-b2))
(test 5 'send (send o2 f-1-b1))
(test 2 'send (send o2 f-1-b2))
(test 5 'send (send o2 f-2-b1))
(test 2 'send (send o2 f-2-b2))
(test 2 'send (send o2 f-2-also-b2))

(test 3 ivar/proc o1 'c)
(test 6 ivar/proc o2 'c)

(test 3 'send (send o1 f-1-c))
(test 6 'send (send o2 f-1-c))
(test 6 'send (send o2 f-2-c))

(test 7 ivar/proc o1 'd)
(test 7 ivar/proc o2 'd)

(test 10 'send (send o1 f-1-v))
(test 10 'send (send o2 f-1-v))
(test 20 'send (send o2 f-2-v))
(test 20 'send (send o2 f-2-v-copy))

(err/rt-test (ivar o2 i-f-2-v) ivar?)

(test 0 'send (send o1 f-1-other-e o1))
(test 1 'send (send o1 f-1-other-e o2))

(test 2 ivar/proc o2 'y)

(test 3 'send (send o2 f-2-set-b2 3))
(test 3 'send (send o2 f-2-also-b2))

(test 'i-a 'send (send o6 i-a))
(test 'x-a 'send (send o6 x-a))
(test 'i-a 'send (send o6 i-a-copy))
(test 'x-a 'send (send o6 x-a-copy))
(test 'x-x 'send (send o6 x-x))

(test #t eq? o7 (send o7 get-self))

(define g1 (make-generic c1 x))
(test 1 g1 o1)
(test 1 g1 o2)
(arity-test g1 1 1)

(err/rt-test (make-generic c1 www)  exn:object?)

(define g2 (make-generic c2 x))
(test 1 g2 o2)

(define g0 (make-generic i0.1 x))
(test 1 g0 o1)
(test 1 g0 o2)
(arity-test g0 1 1)
(test 'hi g0 (make-object (class* object% (i0.1) () 
				  (public [x 'hi][y 'bye])
				  (sequence (super-init)))))

(err/rt-test (make-generic i0.1 www) exn:object?)

(err/rt-test (g2 o1) exn:object?)
(err/rt-test (g0 o3) exn:object?)

(err/rt-test (class* 7 () ()) exn:object?)
(err/rt-test (class* null () ()) exn:object?)
(err/rt-test (let ([c (class* 7 () ())]) c) exn:object?)
(err/rt-test (class* object% (i1 7) ()) exn:object?)
(err/rt-test (let ([c (class* object% (i1 7) ())]) c) exn:object?)
(err/rt-test (interface (8) x) exn:object?)
(err/rt-test (let ([i (interface (8) x)]) i) exn:object?)
(err/rt-test (interface (i1 8) x) exn:object?)
(err/rt-test (make-generic c2 not-there) exn:object?)

(err/rt-test (make-object (class* c1 () ())) exn:object?)
(err/rt-test (make-object (let ([c (class* c1 () ())]) c)) exn:object?)

(err/rt-test (make-object 
	      (class* c2 () () (sequence (super-init) (super-init)))) 
	    exn:object?)
(err/rt-test (make-object 
	      (let ([c (class* c2 () () (sequence (super-init) (super-init)))]) c))
	    exn:object?)

(err/rt-test (make-object (class object% (x))) exn:application:arity?)
(err/rt-test (make-object (let ([c (class object% (x))]) c)) exn:application:arity?)
	      

(define c100
  (let loop ([n 99][c (class c1 args (public [z -1]) (sequence (apply super-init args)))])
    (if (zero? n)
	c
	(loop (sub1 n) (class c args 
			      (override (z n))
			      (sequence
				(apply super-init args)))))))

(define o100 (make-object c100 100))
(test 100 'send (send o100 f-1-a))
(test 1 'ivar (ivar o100 z))

(test 5 'init (let ([g-x 8]) (make-object (class* object% () ([x (set! g-x 5)]) (sequence (super-init)))) g-x))
(test 8 'init (let ([g-x 8]) (make-object (class* object% () ([x (set! g-x 5)]) (sequence (super-init))) 0) g-x))

(test (letrec ([x x]) x) 'init (send (make-object 
				      (class* object% () ([x y] [y x]) 
					      (public (f (lambda () x)))
					      (sequence (super-init))))
				     f))

(define inh-test-expr
 (lambda (super derive-pre? rename? override? override-pre?)
   (let* ([order
	   (lambda (pre? a b)
	     (if pre?
		 (list a b)
		 (list b a)))]
	  [base-class
	   `(class ,(if super
			super
			'(class object% (n) 
				(public [name (lambda () n)])
				(sequence (super-init))))
		   ()
		   ,(if (not rename?)
			'(inherit name)
			'(rename [super-name name]))
		   ,@(order
		      derive-pre?
		      `(public [w ,(if rename? 'super-name 'name)])
		      '(sequence (super-init 'tester))))])
     `(ivar
       (make-object
	,(if override?
	     `(class ,base-class ()
		     ,@(order
			override-pre?
			'(sequence (super-init))
			'(override [name (lambda () 'o-tester)])))
	     base-class))
       w))))

(define (do-override-tests super)
  (define (eval-test v e)
    (teval `(test ,v (quote, e)
		  (let ([v ,e]) 
		    (if (procedure? v)
			(v)
			v)))))

  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #f  #f #f))
  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #f  #t #t))
  (eval-test '(letrec ([x x]) x) (inh-test-expr super #f #f  #t #t))

  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #t  #f #f))
  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #t  #t #f))
  (eval-test '(letrec ([x x]) x) (inh-test-expr super #t #t  #t #t))
  
  (eval-test ''tester (inh-test-expr super #f #f  #f #f))
  (eval-test ''o-tester (inh-test-expr super #t #f  #t #f))
  (eval-test ''o-tester (inh-test-expr super #f #f  #t #f))

  (eval-test ''tester (inh-test-expr super #f #t  #f #f))  
  (eval-test ''tester (inh-test-expr super #f #t  #t #t))
  (eval-test ''tester (inh-test-expr super #f #t  #t #f)))

(do-override-tests #f)

'(when (defined? 'primclass%)
  (err/rt-test (make-object primclass%) exn:application:arity?)
  (err/rt-test (make-object primsubclass%) exn:application:arity?)

  (let ()
    (define o (make-object primclass% 'tester))
    (arity-test (ivar o name) 0 0)
    (test 'tester (ivar o name))
    (test "primclass%" (ivar o class-name))
    
    (let ()
      (define o2 (make-object primsubclass% 'tester))
      (arity-test (ivar o2 name) 0 0)
      (arity-test (ivar o2 detail) 0 0)
      (test 'tester (ivar o2 name))
      (test #f (ivar o2 detail))
      (test "primsubclass%" (ivar o2 class-name))
      
      (do-override-tests 'primclass%)
      (do-override-tests 'primsubclass%)

      (let ()
	(define name-g (make-generic primclass% name))
	(define class-name-g (make-generic primclass% class-name))
	
	(define sub-name-g (make-generic primsubclass% name))
	(define sub-class-name-g (make-generic primsubclass% class-name))
	(define sub-detail-g (make-generic primsubclass% detail))
	
	(test 'tester (name-g o))
	(test "primclass%" (class-name-g o))
	
	(test 'tester (name-g o2))
	(test "primsubclass%" (class-name-g o2))
	(test 'tester (sub-name-g o2))
	(test "primsubclass%" (sub-class-name-g o2))
	(test #f (sub-detail-g o2))

	(let ()
	  (define c%
	    (class primsubclass% ()
	      (inherit name detail class-name)
	      (sequence (super-init 'example))
	      (public
		[n name]
		[d detail]
		[c class-name])))

	  (define o3 (make-object c%))
	  (test 'example (ivar o3 n))
	  (test #f (ivar o3 d))
	  (test "primsubclass%" (ivar o3 c))
	  (test 'example (ivar o3 name))
	  (test #f (ivar o3 detail))
	  (test "primsubclass%" (ivar o3 class-name))

	  (test 'example (name-g o3))
	  (test "primsubclass%" (class-name-g o3))
	  (test 'example (sub-name-g o3))
	  (test "primsubclass%" (sub-class-name-g o3))
	  (test #f (sub-detail-g o3)))))))


; Test for override/rename order
(define bsc (class object% ()
		   (public [x (lambda () 10)])
		   (sequence (super-init))))
(define orc (class bsc ()
	      (public [y (lambda () (super-x))])
	      (override [x (lambda () 20)])
	      (rename [super-x x])
	      (sequence (super-init))))
(test 10 (ivar (make-object orc) y))

(report-errs)

