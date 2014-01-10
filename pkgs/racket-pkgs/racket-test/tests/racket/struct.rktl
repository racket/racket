
(load-relative "loadtest.rktl")

(Section 'struct)

(let-values ([(prop:p p? p-ref) (make-struct-type-property 'prop (lambda (x y) (add1 x)))]
	     [(prop:p2 p2? p2-ref) (make-struct-type-property 'prop2)]
	     [(insp1) (make-inspector)]
	     [(insp2) (make-inspector)])
  (arity-test make-struct-type-property 1 4)
  (test 3 primitive-result-arity make-struct-type-property)
  (arity-test p? 1 1)
  (arity-test p-ref 1 2)
  (arity-test struct-type-property? 1 1)
  (test #t struct-type-property? prop:p)
  (test #f struct-type-property? 5)
  (let-values ([(type make pred sel set) (make-struct-type 'a #f 2 1 'un (list (cons prop:p 87)) (make-inspector insp1))]
	       [(typex makex predx selx setx) (make-struct-type 'ax #f 0 5 #f null (make-inspector insp2))])
    (arity-test make-struct-type 4 11)
    (test 5 primitive-result-arity make-struct-type)
    (test #t struct-type? type)
    (test #t procedure? make)
    (arity-test make 2 2)
    (arity-test makex 0 0)
    (arity-test sel 2 2)
    (arity-test set 3 3)
    (test #t struct-mutator-procedure? set)
    (test #t struct-accessor-procedure? sel)
    (test #f struct-mutator-procedure? sel)
    (test #f struct-accessor-procedure? set)
    (test #t p? type)
    (test #f p2? type)
    (test #f p? 5)
    (test 88 p-ref type)
    (err/rt-test (p-ref 5))
    (err/rt-test (p2-ref type))
    (let ([sel1 (make-struct-field-accessor sel 0)]
	  [set1 (make-struct-field-mutator set 0)]
	  [sel2 (make-struct-field-accessor sel 2)]
	  [set2 (make-struct-field-mutator set 2)])
      (err/rt-test (make-struct-field-accessor set 0))
      (err/rt-test (make-struct-field-mutator sel 0))
      (err/rt-test (make-struct-field-accessor sel -1))
      (err/rt-test (make-struct-field-mutator set -1))
      (err/rt-test (make-struct-field-accessor sel 0.0))
      (err/rt-test (make-struct-field-mutator set 0.0))
      (arity-test sel1 1 1)
      (arity-test set1 2 2)
      (arity-test sel2 1 1)
      (arity-test set2 2 2)
      (test #t struct-mutator-procedure? set2)
      (test #t struct-accessor-procedure? sel2)
      (test #t struct-mutator-procedure? set1)
      (test #t struct-accessor-procedure? sel1)
      (test #f struct-mutator-procedure? sel2)
      (test #f struct-accessor-procedure? set2)
      (test #f struct-mutator-procedure? sel1)
      (test #f struct-accessor-procedure? set1)
      (err/rt-test (make-struct-field-accessor sel 3) exn:application:mismatch?)
      (let ([an-a (make 'one 'two)]
	    [an-ax (makex)])
        (test #f procedure-struct-type? type)
	(test #f procedure? an-a)
	(test #f procedure? an-ax)

	(test 'one sel an-a 0)
	(test 'two sel an-a 1)
	(test 'un sel an-a 2)
	(test 'one sel1 an-a)
	(test 'un sel2 an-a)

	(err/rt-test (sel an-a -1))
	(err/rt-test (sel an-a 3) exn:application:mismatch?)
	(err/rt-test (set an-a -1 'v))
	(err/rt-test (set an-a 3 'v) exn:application:mismatch?)

	(test #t p? an-a)
	(test 88 p-ref an-a)
	(err/rt-test (p2-ref an-a))

	(test #f selx an-ax 0)
	(test #f selx an-ax 1)
	(test #f selx an-ax 2)
	(test #f selx an-ax 3)
	(test #f selx an-ax 4)

	(test (void) set an-a 0 'yi)
	(test 'yi sel an-a 0)
	(test (void) set an-a 2 'san)
	(test 'san sel2 an-a)

	(err/rt-test (sel 5 0))
	(err/rt-test (set 5 0 10))
	(err/rt-test (sel an-ax 0))
	(err/rt-test (set an-ax 0 10))
	(err/rt-test (sel1 5))
	(err/rt-test (set1 5 10))

	(let-values ([(prop:p3 p3? p3-ref) (make-struct-type-property 'prop3)]
		     [(prop:p4 p4? p4-ref) (make-struct-type-property 'prop4)])
	  (let-values ([(btype bmake bpred bsel bset) (make-struct-type 'b type 0 3 'unun (list (cons prop:p3 33) (cons prop:p4 44)) (make-inspector insp2))]
		       [(btypex bmakex bpredx bselx bsetx) (make-struct-type 'bx typex 1 5 'nope (list (cons prop:p3 330)) (make-inspector insp1))]
		       [(ctype cmake cpred csel cset) (make-struct-type 'c type 3 2 #f null (make-inspector insp1))])
	    (arity-test bmake 2 2)
	    (arity-test bmakex 1 1)

	    ;; Override ok:
	    (make-struct-type 'bb type 0 0 #f (list (cons prop:p 12)))
	    (make-struct-type 'bb btype 0 0 #f (list (cons prop:p3 12)))

	    (err/rt-test (make-struct-type 'bb type 0 0 #f (list (cons prop:p 12) (cons prop:p 13))) exn:application:mismatch?)
	    (err/rt-test (make-struct-type 'bb btype 0 0 #f (list (cons prop:p3 12) (cons prop:p3 13))) exn:application:mismatch?)
	    (err/rt-test (make-struct-type 'bb #f 0 0 #f (list (cons prop:p 12) (cons prop:p2 12) (cons prop:p 13))) exn:application:mismatch?)
	    (err/rt-test (make-struct-type 'bb type 0 0 #f (list (cons (let-values ([(p p? p-v)
										     (make-struct-type-property 'p (lambda (v s)
														     ;; this guard will fail!
														     (/ 1 v)))])
									 p)
								       0))) exn:fail:contract:divide-by-zero?)
	    
	    (test #t p3? btype)
	    (test #t p3? btypex)
	    (test #f p3? type)
	    (test #t p4? btype)
	    (test #f p4? btypex)

	    (test 88 p-ref btype)
	    (test 33 p3-ref btype)
	    (test 44 p4-ref btype)
	    (err/rt-test (p2-ref btype))
	    
	    (test 330 p3-ref btypex)
	    (err/rt-test (p2-ref btypex))
	    (err/rt-test (p4-ref btypex))
	    
	    (let ([a-b (bmake 'bone 'btwo)]
		  [a-bx (bmakex 'byi)])
	      (test 'bone sel a-b 0)
	      (test 'btwo sel a-b 1)
	      (test 'bone sel1 a-b)
	      (test 'un sel2 a-b)

	      (test 'unun bsel a-b 0)
	      (test 'unun bsel a-b 1)
	      (test 'unun bsel a-b 2)
	      (test (void) bset a-b 1 'did)
	      (test 'unun bsel a-b 0)
	      (test 'did bsel a-b 1)
	      (test 'unun bsel a-b 2)

	      (err/rt-test (sel a-b 3) exn:application:mismatch?)
	      (err/rt-test (set a-b 3 'v) exn:application:mismatch?)
	      (err/rt-test (bsel a-b 3) exn:application:mismatch?)

	      (err/rt-test (bsel an-a 0))
	      (err/rt-test (bset an-a 0 10))

	      (arity-test struct->vector 1 2)

	      (parameterize ([current-inspector insp1])
		(test #(struct:a yi two san) struct->vector an-a)
		(test #(struct:b bone btwo un ...) struct->vector a-b)
		(test #(struct:ax ...) struct->vector an-ax)
		(test #(struct:bx ... byi nope nope nope nope nope) struct->vector a-bx)

		(test-values (list type #f) (lambda () (struct-info an-a)))
		(test-values (list type #t) (lambda () (struct-info a-b)))

		(test-values (list #f #t) (lambda () (struct-info an-ax)))
		(test-values (list btypex #f) (lambda () (struct-info a-bx)))
		
		(err/rt-test (struct-type-info typex) exn:application:mismatch?)
		(err/rt-test (struct-type-info btype) exn:application:mismatch?)

		(let-values ([(name isize asize get put const super skipped?) (struct-type-info type)])
		  (test 'bone get a-b 0)
		  (put a-b 0 'ok)
		  (test 'ok get a-b 0)
		  (test (list 'a 2 1 null #f #f) list name isize asize const super skipped?))
		(let-values ([(name isize asize get put const super skipped?) (struct-type-info btypex)])
		  (test 'byi get a-bx 0)
		  (put a-bx 0 'yep)
		  (test 'yep get a-bx 0)
		  (test (list 'bx 1 5 null #f #t) list name isize asize const super skipped?))
                (let-values ([(name isize asize get put const super skipped?) (struct-type-info ctype)])
		  (test (list 'c 3 2 null type #f) list name isize asize const super skipped?))

		'...))))))))

(err/rt-test (make-struct-type-property 1))
(err/rt-test (make-struct-type-property 's (lambda () 10)))
(err/rt-test (make-struct-type-property 's (lambda (x) 10)))

(let ([t-insp (make-inspector)])
  (define (try-proc-structs proc proc2 bad-arity name re 1-2-value t-insp)
    (letrec-values ([(type make pred sel set) (make-struct-type 'p #f 1 2 (lambda (x) (pred x)) null t-insp proc)]
		    ;; Derived from proc
		    [(type2 make2 pred2 sel2 set2) (make-struct-type 'q type 1 2 (lambda (x) (pred2 x)) null t-insp #f)]
		    ;; Derived, adds proc
		    [(type3 make3 pred3 sel3 set3) (make-struct-type 'r struct:arity-at-least 1 1 (lambda (x) (pred3 x)) 
								     null t-insp proc)])
      (test #t procedure-struct-type? type)
      (let* ([bad1 (make 17)]
	     [bad2 (make2 18 -18)]
	     [bad3 (make3 700 19)]
	     [bad11 (make bad1)])
	(test #t pred bad1)
	(test #t pred2 bad2)
	(test #t pred3 bad3)
	(test #t pred bad11)

	(test #t procedure? bad1)
	(test #t procedure? bad2)
	(test #t procedure? bad3)
	(test #t procedure? bad11)

	(test bad-arity procedure-arity bad1)
	(test bad-arity procedure-arity bad2)
	(test bad-arity procedure-arity bad3)
	(test bad-arity procedure-arity bad11)

	(test 'p object-name bad1)
	(test 'q object-name bad2)
	(test 'r object-name bad3)
	(test 'p object-name bad11)

	(when (equal? 2 bad-arity)
	  (test 1-2-value bad1 1 2)
	  (test 1-2-value bad2 1 2)
	  (test 1-2-value bad3 1 2)
	  (test 1-2-value bad11 1 2))

	(err/rt-test (bad1) exn:application:arity?)
	(err/rt-test (bad2) exn:application:arity?)
	(err/rt-test (bad3) exn:application:arity?)
	(err/rt-test (bad11) exn:application:arity?)
	(err/rt-test (bad1 1) exn:application:arity?)
	(err/rt-test (bad2 1) exn:application:arity?)
	(err/rt-test (bad3 1) exn:application:arity?)
	(err/rt-test (bad11 1) exn:application:arity?)

	(test '("p") regexp-match "^p"
	      (with-handlers ([exn:fail? exn-message])
		(bad1)))
	(test '("q") regexp-match "^q"
	      (with-handlers ([exn:fail? exn-message])
		(bad2)))
	(test '("r") regexp-match "^r"
	      (with-handlers ([exn:fail? exn-message])
		(bad3)))
	(test '("p") regexp-match "^p"
	      (with-handlers ([exn:fail? exn-message])
		(bad11))))

      (let* ([cons1 (make cons)]
	     [cons2 (make2 cons -18)]
	     [cons3 (make3 700 cons)]
	     [cons11 (make cons1)])
	(test #t pred cons1)
	(test #t pred2 cons2)
	(test #t pred3 cons3)
	(test #t pred cons11)

	(test #t procedure? cons1)
	(test #t procedure? cons2)
	(test #t procedure? cons3)
	(test #t procedure? cons11)

	(test 2 procedure-arity cons1)
	(test 2 procedure-arity cons2)
	(test 2 procedure-arity cons3)
	(test 2 procedure-arity cons11)

	(test (name 'p) object-name cons1)
	(test (name 'q) object-name cons2)
	(test (name 'r) object-name cons3)
	(test (name 'p) object-name cons11)

	(arity-test cons1 2 2)
	(arity-test cons2 2 2)
	(arity-test cons3 2 2)
	(arity-test cons11 2 2)

	(test 1-2-value cons1 1 2)
	(test 1-2-value cons2 1 2)
	(test 1-2-value cons3 1 2)
	(test 1-2-value cons11 1 2)

	(test #f not (regexp-match (re "^p")
				   (with-handlers ([exn:fail? exn-message])
				     (cons1))))
	(test #f not (regexp-match (re "^q")
				   (with-handlers ([exn:fail? exn-message])
				     (cons2))))
	(test #f not (regexp-match (re "^r")
				   (with-handlers ([exn:fail? exn-message])
				     (cons3))))
	(test #f not (regexp-match (re "^p")
				   (with-handlers ([exn:fail? exn-message])
				     (cons11)))))
      
      'done))

  (try-proc-structs 0 0 null (lambda (x) 'cons) (lambda (x) "^cons") '(1 . 2) (current-inspector))
  (try-proc-structs 0 0 null (lambda (x) 'cons) (lambda (x) "^cons") '(1 . 2) t-insp)
  (try-proc-structs (lambda (s a b) 
		      (when (and (struct? s) (not (arity-at-least? s)))
			(error "should be opaque"))
		      (cons b a)) 
		    (lambda (s a b) 
		      (when (struct? s)
			(error "should be opaque"))
		      (cons b a))
		    2 values values '(2 . 1) (current-inspector))
  (try-proc-structs (lambda (s a b) 
		      (unless (struct? s) (error "should be transparent"))
		      (unless ((vector-ref (struct->vector s) 3) s) (error "should be instance"))
		      (cons b a))
		    (lambda (s a b) 
		      (unless (struct? s) (error "should be transparent"))
		      (unless ((vector-ref (struct->vector s) 3) s) (error "should be instance"))
		      (unless ((vector-ref (struct->vector s) 5) s) (error "should be instance"))
		      (cons b a))
		    2 values values '(2 . 1) t-insp))

;; Override super-struct procedure spec:
(let-values ([(s:s make-s s? s-ref s-set!)
              (make-struct-type 'a #f 1 1 #f null (current-inspector) 0)])
  (let-values ([(s:b make-b b? b-ref s-set!)
                (make-struct-type 'b s:s 1 1 #f null (current-inspector) 0)])
    (test 11 (make-b 1 add1) 10)))

(let-values ([(type make pred sel set) (make-struct-type 'p #f 1 0 #f null (current-inspector) (lambda () 5))])
  (let ([useless (make 7)])
    (test #t pred useless)
    (test #t procedure? useless)
    (test null procedure-arity useless)
    (test 'p object-name useless)
    (err/rt-test (useless) exn:application:arity?)
    (err/rt-test (useless 1) exn:application:arity?)
    (err/rt-test (useless 1 2) exn:application:arity?)))

(let-values ([(type make pred sel set) (make-struct-type 'p #f 1 0 #f null (current-inspector)
							 (case-lambda 
							  [(x) 7]
							  [() 5]
							  [(x y z) 8]))]) 
  (let ([useless (make 7)])
    (test #t pred useless)
    (test #t procedure? useless)
    (test '(0 2) procedure-arity useless)
    (test 'p object-name useless)
    (test 7 useless)
    (err/rt-test (useless 1) exn:application:arity?)
    (test 8 useless 1 2)
    (err/rt-test (useless 1 2 3) exn:application:arity?)))

;; Test constructor guard
(let ()
  (define got null)
  (define mode 'normal)
  (define-values (s:o make-o o? o-ref o-set!)
    (make-struct-type 'o #f 1 0 'odefault
		      null
		      (make-inspector) #f
		      null (lambda (o n) 
			     (+ o 1))))
  (define-values (s:a make-a a? a-ref a-set!)
    (make-struct-type 'a #f 2 1 'adefault
		      null
		      (make-inspector) #f
		      null (lambda (a b n) 
			     (set! got (cons (list a b n) got))
			     (case mode
			       [(normal) (values 1 2)]
			       [(reject) (/ 1 0)]
			       [(bad) 'one]))))
  (define-values (s:b make-b b? b-ref b-set!)
    (make-struct-type 'b s:a 1 2 'bdefault
		      null
		      (make-inspector) #f
		      null (lambda (a b c n) 
			     (set! got (cons (list a b c n) got))
			     (case mode
			       [(normal) (values 10 20 30)]
			       [(reject) ((let/ec k k))]))))
  (let ([o1 (make-o 10)])
    (test 11 o-ref o1 0))
  (let ([a1 (make-a 'x 'y)])
    (test 1 a-ref a1 0)
    (test 2 a-ref a1 1)
    (test 'adefault a-ref a1 2)
    (test '((x y a)) values got))
  (let ([b1 (make-b 'x 'y 'z)])
    (test 1 a-ref b1 0)
    (test 2 a-ref b1 1)
    (test 'adefault a-ref b1 2)
    (test 30 b-ref b1 0)
    (test 'bdefault b-ref b1 1)
    (test '((10 20 b) (x y z b) (x y a)) values got))
  (set! mode 'reject)
  (err/rt-test (make-a 'x 'y) exn:fail:contract:divide-by-zero?)
  (err/rt-test (make-b 'x 'y 'z) exn:fail:contract:continuation?)
  (set! mode 'bad)
  (err/rt-test (make-a 'x 'y) exn:application:arity?)
  (err/rt-test (make-b 'x 'y 'z) exn:application:arity?))
(err/rt-test (make-struct-type 'a #f 2 1 'adefault null (make-inspector) #f null 
			       ;; Not a proc:
			       10))
(err/rt-test (make-struct-type 'a #f 2 1 'adefault null (make-inspector) #f null 
			       ;; Wrong arg count:
			       (lambda (x y) (void)))
	     exn:application:mismatch?)
(err/rt-test (make-struct-type 'a #f 2 0 'adefault null (make-inspector) #f null 
			       ;; Wrong arg count, again:
			       (lambda (x y) (void)))
	     exn:application:mismatch?)


(define-struct a (b c) #:mutable)
(define-struct aa ())
(define ai (make-a 1 2))
(define aai (make-aa))
(test #t struct-type? struct:a)
(test #f struct-type? 5)
(test #t procedure? a?)
(test #t a? ai)
(test #f a? 1)
(test #f aa? ai)
(test #f struct? ai)
(test 1 a-b ai)
(test 2 a-c ai)
(define ai2 (make-a 1 2))
(set-a-b! ai2 3)
(set-a-c! ai2 4)
(test 1 a-b ai)
(test 2 a-c ai)
(test 3 a-b ai2)
(test 4 a-c ai2)
(define-struct a (b c) #:mutable)
(test #f a? ai)
(arity-test make-a 2 2)
(err/rt-test (make-aa 1) exn:application:arity?)
(arity-test a? 1 1)
(arity-test a-b 1 1)
(arity-test a-c 1 1)
(arity-test set-a-b! 2 2)
(arity-test set-a-c! 2 2)
(err/rt-test (a-b 5))
(err/rt-test (a-b ai))
(err/rt-test (set-a-b! ai 5))
(err/rt-test (set-a-c! ai 5))

(arity-test struct-type? 1 1)

(define (gen-struct-syntax-test formname suffix)
  (syntax-test (datum->syntax #f `(,formname 1 (x) ,@suffix) #f))
  (syntax-test (datum->syntax #f `(,formname a (1) ,@suffix) #f))
  (syntax-test (datum->syntax #f `(,formname a (x 1) ,@suffix) #f))
  (syntax-test (datum->syntax #f `(,formname a (x . y) ,@suffix) #f))
  (syntax-test (datum->syntax #f `(,formname (a) (x) ,@suffix) #f))
  (syntax-test (datum->syntax #f `(,formname (a . y) (x) ,@suffix) #f))
  (syntax-test (datum->syntax #f `(,formname (a 2) (x) ,@suffix) #f))
  (syntax-test (datum->syntax #f `(,formname (a 2 3) (x) ,@suffix) #f)))
(define (struct-syntax-test formname)
  (syntax-test (datum->syntax #f `(,formname) #f))
  (syntax-test (datum->syntax #f `(,formname . a) #f))
  (syntax-test (datum->syntax #f `(,formname a . x) #f))
  (syntax-test (datum->syntax #f `(,formname (a 9) (x)) #f))
  (syntax-test (datum->syntax #f `(,formname a x) #f))
  (gen-struct-syntax-test formname '()))

(struct-syntax-test 'define-struct)

(syntax-test #'(define-struct a (b c) #:transparent #:inspector #f))
(syntax-test #'(define-struct a (b c) #:transparent #:prefab))
(syntax-test #'(define-struct a (b c) #:prefab #:guard 10))
(syntax-test #'(define-struct a (b c) #:prefab #:property 1 10))
(syntax-test #'(define-struct a (b c) #:guard 10 #:prefab))
(syntax-test #'(define-struct a (b c) #:property 1 10 #:prefab))

(define-struct base0 ())
(define-struct base1 (a))
(define-struct base2 (l r))
(define-struct base3 (x y z))

(define-struct (one00 base0) ())
(define-struct (one01 base1) ())
(define-struct (one02 base2) ())
(define-struct (one03 base3) ())

(define-struct (one10 base0) (a))
(define-struct (one11 base1) (a))
(define-struct (one12 base2) (a))
(define-struct (one13 base3) (a))

(define-struct (one20 base0) (l r))
(define-struct (one21 base1) (l r))
(define-struct (one22 base2) (l r))
(define-struct (one23 base3) (l r))

(define-struct (one30 base0) (x y z))
(define-struct (one31 base1) (x y z))
(define-struct (one32 base2) (x y z))
(define-struct (one33 base3) (x y z))

(define-struct (two100 one00) (a))
(define-struct (two101 one01) (a))
(define-struct (two102 one02) (a))
(define-struct (two103 one03) (a))
(define-struct (two110 one10) (a))
(define-struct (two111 one11) (a))
(define-struct (two112 one12) (a))
(define-struct (two113 one13) (a))
(define-struct (two120 one20) (a))
(define-struct (two121 one21) (a))
(define-struct (two122 one22) (a))
(define-struct (two123 one23) (a))
(define-struct (two130 one30) (a))
(define-struct (two131 one31) (a))
(define-struct (two132 one32) (a))
(define-struct (two133 one33) (a))

(define x00 (make-one00))

(define x01 (make-one01 1))

(define x10 (make-one10 1))
(define x11 (make-one11 1 2))
(define x12 (make-one12 1 2 3))
(define x13 (make-one13 1 2 3 4))

(define x31 (make-one31 1 2 3 4))

(define x33 (make-one33 1 2 3 4 5 6))

(define x132 (make-two132 1 2 3 4 5 6))

(define (ones v)
  (cond
   [(one00? v) 'one00]
   [(one01? v) 'one01]
   [(one02? v) 'one02]
   [(one03? v) 'one03]
   
   [(one10? v) 'one10]
   [(one11? v) 'one11]
   [(one12? v) 'one12]
   [(one13? v) 'one13]
   
   [(one20? v) 'one20]
   [(one21? v) 'one21]
   [(one22? v) 'one22]
   [(one23? v) 'one23]
   
   [(one30? v) 'one30]
   [(one31? v) 'one31]
   [(one32? v) 'one32]
   [(one33? v) 'one33]))

(define (multi v)
  (cond
   [(two130? v) 'two130]
   [(two131? v) 'two131]
   [(two132? v) 'two132]
   [(two133? v) 'two133]
   
   [(one10? v) 'one10]
   [(one11? v) 'one11]
   [(one12? v) 'one12]
   [(one13? v) 'one13]
   
   [(one20? v) 'one20]
   [(one21? v) 'one21]
   [(one22? v) 'one22]
   [(one23? v) 'one23]
   
   [(base0? v) 'base0]
   [(base1? v) 'base1]
   [(base2? v) 'base2]
   [(base3? v) 'base3]))

(define (dummy v)
  'ok)

(define (go f v n)
  (time
   (let loop ([n n])
     (unless (zero? n)
	     (f v)
	     (loop (sub1 n))))))

(define check
  (lambda (l)
    (cond
     [(null? l) #f]
     [else
      (test (caddr l) (car l) (cadr l))
      (check (cdddr l))])))

(define ones-test
  (list x00 'one00
	x10 'one10
	x11 'one11
	x12 'one12
	x13 'one13
	x33 'one33))
	
(define multi-test
  (list x00 'base0
	x10 'one10
	x11 'one11
	x12 'one12
	x13 'one13
	x33 'base3
	x132 'two132))

(letrec ([bundle
	  (lambda (l f)
	    (if (null? l)
		null
		(list* f (car l) (cadr l)
		       (bundle (cddr l) f))))])
  (check (append
	  (bundle ones-test ones)
	  (bundle multi-test multi)
	  (list base1-a x11 1
		one11-a x11 2
		one10-a x10 1
		
		base1-a x31 1
		one31-z x31 4
		
		base2-l x132 1
		two132-a x132 6
		one32-y x132 4))))

;; ------------------------------------------------------------
;; Property accessor errors

(let-values ([(prop:p p? p-ref) (make-struct-type-property 'prop1 #f '() #t)])
  (test 42 p-ref 5 42)
  (test 17 p-ref 5 (lambda () (* 1 17)))
  (err/rt-test (p-ref 5) exn:fail:contract?))

;; ------------------------------------------------------------
;; Property type supers

(require (only-in mzscheme [prop:procedure mz:prop:procedure])) ; more primitive - no keywords

(let ([try
       (lambda (base prop:procedure)
         (err/rt-test (make-struct-type '? base 1 0 #f (list (cons prop:procedure 0) 
                                                             (cons prop:procedure 1))
                                        #f #f '(0)))
         ;; Ok to re-set to same value:
         (test #t list? (call-with-values
                            (lambda () (make-struct-type '? base 1 0 #f (list (cons prop:procedure 0) 
                                                                              (cons prop:procedure 0))
                                                         #f #f '(0)))
                          list))
         (err/rt-test (make-struct-type '? base 1 0 #f (list (cons prop:procedure 0)) #f 1))
         (test #t list? (call-with-values
                            (lambda () (make-struct-type '? base 1 0 #f (list (cons prop:procedure 0)) #f 0))
                          list))

         (let-values ([(prop:s s? s-get)
                       (make-struct-type-property 's #f (list (cons prop:procedure (lambda (v) (add1 v)))))])
           (define-struct a (x y) #:super base #:property prop:s 0)
           (test 0 s-get struct:a)
           (test #t procedure-struct-type? struct:a)
           (test 5 (make-a 1 (lambda (v) (+ 2 v))) 3)

           (err/rt-test (make-struct-type-property 't #f 10))
           (err/rt-test (make-struct-type-property 't #f (list (cons prop:s 10))))

           ;; Allow multiple inheritances of a property at this stage, because we can't in general
           ;;  tell whether the results will be eq?
           (test #t list?
                 (call-with-values
                     (lambda ()
                       (make-struct-type-property 't #f (list (cons prop:s void) (cons prop:s values))))
                   list))
           (test #t list?
                 (call-with-values
                     (lambda ()
                       (make-struct-type-property 't #f (list (cons prop:s void) (cons prop:procedure values))))
                   list))

           (let-values ([(prop:t t? t-get)
                         (make-struct-type-property 't #f (list (cons prop:s (lambda (v) (add1 v)))))]
                        [(prop:u u? u-get)
                         (make-struct-type-property 'u)])
             (define-struct b (x y z) #:super base #:property prop:u '? #:property prop:t 0)
             (test 8 (make-b 1 2 (lambda (v) (- v 4))) 12)
             (test 0 t-get struct:b)
             (test 1 s-get struct:b)
             (test '? u-get struct:b)

             (let-values ([(prop:w w? w-get)
                           (make-struct-type-property 'w (lambda (v s) (sub1 v)) (list (cons prop:u values)))]
                          [(prop:z z? z-get)
                           (make-struct-type-property 'z #f (list (cons prop:u values)))])
               (define-struct c () #:super base #:property prop:w 10)
               (test 9 w-get struct:c)
               (test 9 u-get struct:c) ; i.e., after guard

               (err/rt-test (make-struct-type '? base 0 0 #f (list (cons prop:w 3) (cons prop:z 3))))
               (err/rt-test (make-struct-type '? base 3 0 #f (list (cons prop:s 0) (cons prop:t 0)) #f #f '(0 1 2)))
               (err/rt-test (make-struct-type '? base 3 0 #f (list (cons prop:s 0) (cons prop:procedure 0)) #f #f '(0 1 2)))
               ))))])

  (try #f mz:prop:procedure)
  (try #f prop:procedure)
  (let ([props (map (lambda (n)
                      (let-values ([(prop ? -get) (make-struct-type-property n)])
                        prop))
                    '(a b c d e f g h j i))])
    (let-values ([(s: make-s s? s-ref s-set!)
                  (make-struct-type 'base #f 0 0 #f (map (lambda (p) (cons p 5)) props))])
      (try s: mz:prop:procedure)
      (try s: prop:procedure))))

(let ()
  (define-struct a (x y) #:property prop:procedure (lambda (s v #:kw [kw #f]) (list (a-x s) v kw)))
  (test '(1 3 #f) (make-a 1 2) 3)
  (test '(1 3 8) 'kw ((make-a 1 2) 3 #:kw 8))
  (test-values '(() (#:kw)) (lambda () (procedure-keywords (make-a 1 2)))))

;; ------------------------------------------------------------
;; Check that struct definition sequences work:

(let ()
  (define-struct a (x y))
  (define-struct (b a) (z))
  (define-struct (c b) (w))

  (test 1 a-x (make-a 1 2))
  (test 10 a-x (make-b 10 20 30))
  (test 100 a-x (make-c 100 200 300 400)))

;; ------------------------------------------------------------
;; Prefab

(let ([v1 #s(v one)]
      [v2 #s(v one two)]
      [v2-prime #s((v 2) one two)]
      [vw3 #s((v w 2) one two three)]
      [vw3-prime #s((v 1 w 2) one two three)])
  (test #f equal? v1 v2)
  (test #t equal? v2 v2-prime)
  (test #t equal? vw3 vw3-prime)
  (let ()
    (define-struct v (a) #:prefab)
    (test #t v? v1)
    (test #f v? v2)
    (test #f v? vw3)
    (test 'one v-a v1))
  (let ()
    (define-struct v (a b) #:prefab)
    (test #f v? v1)
    (test #t v? v2)
    (test #f v? vw3)
    (test 'one v-a v2)
    (test 'two v-b v2))
  (let ()
    (define-struct w (a b) #:prefab)
    (define-struct (v w) (c) #:prefab)
    (test #f v? v1)
    (test #f v? v2)
    (test #t v? vw3)
    (test #t w? vw3)
    (test 'one w-a vw3)
    (test 'two w-b vw3)
    (test 'three v-c vw3)))

(err/rt-test (make-struct-type 'bad struct:date 2 0 #f null 'prefab))

;; ------------------------------------------------------------
;; Misc. built-in structures

(test #f srcloc? 10)
(test #t srcloc? (make-srcloc 'ok 1 2 3 4))
(test 'ok srcloc-source (make-srcloc 'ok 1 2 3 4))
(test 1 srcloc-line (make-srcloc 'ok 1 2 3 4))
(test #f srcloc-line (make-srcloc 'ok #f 2 3 4))
(test 2 srcloc-column (make-srcloc 'ok 1 2 3 4))
(test 0 srcloc-column (make-srcloc 'ok 1 0 3 4))
(test #f srcloc-column (make-srcloc 'ok 1 #f 3 4))
(test 3 srcloc-position (make-srcloc 'ok 1 2 3 4))
(test #f srcloc-position (make-srcloc 'ok 1 2 #f 4))
(test 4 srcloc-span (make-srcloc 'ok 1 2 3 4))
(test 0 srcloc-span (make-srcloc 'ok 1 2 3 0))
(test #f srcloc-span (make-srcloc 'ok 1 2 3 #f))

(err/rt-test (make-srcloc 'ok 'no 2 3 4))
(err/rt-test (make-srcloc 'ok 0 2 3 4))
(err/rt-test (make-srcloc 'ok 1 'no 3 4))
(err/rt-test (make-srcloc 'ok 1 -1 3 4))
(err/rt-test (make-srcloc 'ok 1 2 'no 4))
(err/rt-test (make-srcloc 'ok 1 2 0 4))
(err/rt-test (make-srcloc 'ok 1 2 3 'no))
(err/rt-test (make-srcloc 'ok 1 2 3 -1))

;; ------------------------------------------------------------
;; Custom write

(define (custom-write-check normal?)
  (define (tuple-print tuple port write?)
    (when write? (write-string "<" port))
    (let ([l (tuple-ref tuple 0)])
      (unless (null? l)
	((if write? write display) (car l) port)
	(for-each (lambda (e)
		    (write-string ", " port)
		    (if normal?
			;; Test normal recusrive write:
			((if write? write display) e port)
			;; Test writing recursively to a string:
			(let ([p (open-output-string)])
			  (port-write-handler p (port-write-handler port))
			  (port-display-handler p (port-display-handler port))
			  (port-print-handler p (port-print-handler port))
			  ((if write? write display) e p)
			  (write-string (get-output-string p) port))))
		  (cdr l))))
    (when write? (write-string ">" port)))

  (define-values (s:tuple make-tuple tuple? tuple-ref tuple-set!)
    (make-struct-type 'tuple #f 1 0 #f
		      (list (cons prop:custom-write tuple-print))))
  
  (define (with-output-string thunk)
    (let ([p (open-output-string)])
      (parameterize ([current-output-port p])
	(thunk))
      (get-output-string p)))

  (test "<1, 2, \"a\">" with-output-string 
	(lambda ()
	  (tuple-print (make-tuple '(1 2 "a")) (current-output-port) #t)))

  (test "1, 2, a" with-output-string 
	(lambda ()
	  (display (make-tuple '(1 2 "a")))))
  (test "#0=<#&#0#, 2, \"a\">" with-output-string 
	(lambda ()
	  (let ([t (make-tuple (list (box 1) 2 "a"))])
	    (set-box! (car (tuple-ref t 0)) t)
	    (write t))))
  (test "ack: here: <10, 2, \"a\">" with-output-string 
	(lambda ()
	  (with-handlers ([exn:fail? (lambda (exn)
				       (printf "~a" (exn-message exn)))])
	    (error 'ack "here: ~e" (make-tuple (list 10 2 "a"))))))
  
  (test "ack: here: <100000..." with-output-string 
	(lambda ()
	  (parameterize ([error-print-width 10])  
	    (with-handlers ([exn:fail? (lambda (exn)
					 (printf "~a" (exn-message exn)))])
	      (error 'ack "here: ~e" (make-tuple (list 10000000000000000000000000000000000 2 "a"))))))))

(custom-write-check #t)
(custom-write-check #f)

;; ----------------------------------------

(let ()
  (define-struct t1 (a b) #:transparent)
  (define-struct t2 (c d) #:transparent #:mutable)
  (define-struct o (x y z)
    #:property prop:equal+hash (list
                                (lambda (a b equal?)
                                  (and (equal? (o-x a) (o-x b))
                                       (equal? (o-z a) (o-z b))))
                                (lambda (a hash)
                                  (+ (hash (o-x a)) (* 9 (hash (o-z a)))))
                                (lambda (a hash)
                                  (+ (hash (o-x a)) (hash (o-z a)))))
    #:mutable)

  (test #f equal? (make-t1 0 1) (make-t2 0 1))
  (test #t equal? (make-t1 0 1) (make-t1 0 1))
  (test #t equal? (make-t2 0 1) (make-t2 0 1))
  (test #t equal? 
        (shared ([t (make-t2 0 t)]) t) 
        (shared ([t (make-t2 0 t)]) t))
  (test #f equal?
        (shared ([t (make-t2 0 t)]) t) 
        (shared ([t (make-t2 1 t)]) t))
  (test #t = 
        (equal-hash-code (make-t1 0 1))
        (equal-hash-code (make-t1 0 1)))
  (test #t =
        (equal-hash-code (shared ([t (make-t2 0 t)]) t))
        (equal-hash-code (shared ([t (make-t2 0 t)]) t)))
  (test #t = 
        (equal-secondary-hash-code (make-t1 0 1))
        (equal-secondary-hash-code (make-t1 0 1)))
  (test #t =
        (equal-secondary-hash-code (shared ([t (make-t2 0 t)]) t))
        (equal-secondary-hash-code (shared ([t (make-t2 0 t)]) t)))
  
  (test #t equal? (make-o 1 2 3) (make-o 1 20 3))
  (test #f equal? (make-o 10 2 3) (make-o 1 2 3))
  (test #f equal? (make-o 1 2 3) (make-o 1 2 30))
  (test #t equal? 
        (shared ([t (make-o t 0 t)]) t) 
        (shared ([t (make-o t 0 t)]) t))
  (test #t equal?
        (shared ([t (make-o t 0 t)]) t) 
        (shared ([t (make-o t 1 t)]) t))
  (test #f equal?
        (shared ([t (make-o t 0 0)]) t) 
        (shared ([t (make-o t 0 1)]) t))

  (test #t = 
        (equal-hash-code (make-o 1 2 3))
        (equal-hash-code (make-o 1 20 3)))
  (test #t =
        (equal-hash-code (shared ([t (make-o t 0 t)]) t))
        (equal-hash-code (shared ([t (make-o t 0 t)]) t)))
  (test #t =
        (equal-hash-code (shared ([t (make-o t 1 t)]) t))
        (equal-hash-code (shared ([t (make-o t 1 t)]) t)))
  (test #t =
        (equal-secondary-hash-code (shared ([t (make-o t 0 t)]) t))
        (equal-secondary-hash-code (shared ([t (make-o t 0 t)]) t)))
  (test #t =
        (equal-secondary-hash-code (shared ([t (make-o t 1 t)]) t))
        (equal-secondary-hash-code (shared ([t (make-o t 1 t)]) t)))

  (void))

;; ----------------------------------------

(let ()
  (define-struct foo (a [b #:mutable]) #:transparent)
  (define-struct (bar foo) (f g)
    #:transparent
    #:property
    prop:procedure
    (struct-field-index f))
  (test '(1) (make-bar 1 2 list 4) 1)
  (test '(foo 2 0 (0)) call-with-values 
        (lambda () (struct-type-info struct:foo))
        (lambda (name cnt auto-cnt acc mut imm super skipped?)
          (list name cnt auto-cnt imm)))
  (test '(bar 2 0 (0 1)) call-with-values 
        (lambda () (struct-type-info struct:bar))
        (lambda (name cnt auto-cnt acc mut imm super skipped?)
          (list name cnt auto-cnt imm))))

(let ()
  (define-struct foo (a [b #:mutable] [z #:auto]) #:transparent)
  (define-struct (bar foo) (f g)
    #:transparent
    #:property
    prop:procedure
    (struct-field-index f))
  (test '#&1 (make-bar 1 2 box 4) 1)
  (test '(foo 2 1 (0)) call-with-values 
        (lambda () (struct-type-info struct:foo))
        (lambda (name cnt auto-cnt acc mut imm super skipped?)
          (list name cnt auto-cnt imm)))
  (test '(bar 2 0 (0 1)) call-with-values 
        (lambda () (struct-type-info struct:bar))
        (lambda (name cnt auto-cnt acc mut imm super skipped?)
          (list name cnt auto-cnt imm))))

(let ()
  (define-struct foo (a [b #:mutable] [z #:auto]) #:transparent)
  (define (try v)
    (define-struct (bar foo) ([f #:mutable] g [q #:auto])
      #:property
      prop:procedure
      v)
    10)
  (err/rt-test (try 0))
  (err/rt-test (try 2))
  (err/rt-test (try -1))
  (err/rt-test (try 'x))
  (test 10 try 1))

;; ----------------------------------------

(require (for-syntax scheme/struct-info))

(let ()
  (define-struct a (x y))
  (define-syntax foo (make-struct-info
                      (lambda ()
                        (list #'struct:a #'make-a #'a?
                              (list #'a-y #'a-x)
                              (list #f #f)
                              #f))))
  (define-syntax foo2 (let ()
                        (define-struct si (pred)
                          #:property 
                          prop:struct-info
                          (lambda (v)
                            (list #'struct:a #'make-a (si-pred v)
                                  (list #'a-y #'a-x)
                                  (list #f #f)
                                  #f)))
                        (make-si #'a?)))
  (test (list 1 2) 'match (match (make-a 1 2)
                            [(struct foo (x y)) (list x y)]))
  (test (list 1 2) 'match (match (make-a 1 2)
                            [(struct foo2 (x y)) (list x y)])))
                              

;; ----------------------------------------

(let ()
  (struct s (a b))
  (struct t s (c))
  (struct u t (d))
  (test 11
        'struct-copy1
        (t-c (struct-copy t (t 1 2 3) [c 11])))
  (test 11
        'struct-copy2
        (s-a (struct-copy t (t 1 2 3) [a #:parent s 11])))
  (test 11
        'struct-copy2
        (s-a (struct-copy u (u 1 2 3 4) [a #:parent s 11])))
  
  (syntax-test #'(struct-copy t (t 1 2 3) [a #:parent p 11])))

(let ()
  (struct s (a b) #:transparent)
  (struct t s (c) #:transparent)
  (struct u t (d) #:transparent)
  (test (t 1 2 11)
        'struct-copy1
        (struct-copy t (t 1 2 3) [c 11]))
  (test (t 11 2 3)
        'struct-copy2
        (struct-copy t (t 1 2 3) [a #:parent s 11]))
  (test (s 11 2)
        'struct-copy2
        (struct-copy s (t 1 2 3) [a 11]))
  (test (u 11 2 3 4)
        'struct-copy2
        (struct-copy u (u 1 2 3 4) [a #:parent s 11]))
  
  (syntax-test #'(struct-copy t (t 1 2 3) [a #:parent p 11])))

(let ()
  (struct s (a b) #:prefab)
  (struct t s (c) #:prefab)
  (struct u t (d) #:prefab)
  (test (t 1 2 11)
        'struct-copy1
        (struct-copy t (t 1 2 3) [c 11]))
  (test (t 11 2 3)
        'struct-copy2
        (struct-copy t (t 1 2 3) [a #:parent s 11]))
  (test (s 11 2)
        'struct-copy2
        (struct-copy s (t 1 2 3) [a 11]))
  (test (u 11 2 3 4)
        'struct-copy2
        (struct-copy u (u 1 2 3 4) [a #:parent s 11]))
  
  (syntax-test #'(struct-copy t (t 1 2 3) [a #:parent p 11])))

(test #t prefab-key? 'apple)
(test #f prefab-key? '#(apple))
(test #t prefab-key? '(apple 4))

;; ----------------------------------------
;; We can make a bogus mutator, but we can't apply it:

(let ()
  ;; Test based on code from dmarshall:
  (define-values (struct:thing make-thing thing? thing-ref thing-set!)
    (make-struct-type
     'thing #f 1 0  
     #f               ; auto val
     (list)           ; property list
     #f               ; inspector
     #f               ; proc-spec
     (list 0)))       ; immutables
  
  (define thing.id  (make-struct-field-accessor thing-ref 0))
  (define thing.id! (make-struct-field-mutator thing-set! 0))

  (test #t struct-mutator-procedure? thing.id!)
  (err/rt-test (thing.id!  'new-val))
  
  (let ([f #f])
    ;; defeat inlining to ensure that thunk is JITted:
    (set! f (lambda () (thing.id! (make-thing 1) 'new-val)))
    (err/rt-test (f))))

;; ----------------------------------------
;; Check interaction of `struct-type-info` and GC:

(struct-type-info struct:arity-at-least)
(collect-garbage)
(let-values ([(name init-cnt auto-cnt acc mut immut super skipped?)
              (struct-type-info struct:arity-at-least)])
  (test #t procedure? acc)
  (test #t procedure? mut))

;; ----------------------------------------

(report-errs)
