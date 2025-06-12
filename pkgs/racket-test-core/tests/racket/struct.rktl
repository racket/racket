
(load-relative "loadtest.rktl")

(Section 'struct)

(let-values ([(prop:p p? p-ref) (make-struct-type-property 'prop (lambda (x y) (add1 x)))]
	     [(prop:p2 p2? p2-ref) (make-struct-type-property 'prop2)]
	     [(insp1) (make-inspector)]
	     [(insp2) (make-inspector)])
  (test 'prop-accessor object-name p-ref)
  (test 'racket procedure-realm p-ref)
  (arity-test make-struct-type-property 1 7)
  (arity-test struct-type-property-accessor-procedure? 1 1)
  (arity-test struct-type-property-predicate-procedure? 1 2)
  (test 3 primitive-result-arity make-struct-type-property)
  (arity-test p? 1 1)
  (arity-test p-ref 1 2)
  (arity-test struct-type-property? 1 1)
  (test #t struct-type-property? prop:p)
  (test #f struct-type-property? 5)
  (test #t struct-type-property-accessor-procedure? p-ref)
  (test #t struct-type-property-accessor-procedure? p2-ref)
  (test #f struct-type-property-predicate-procedure? p-ref)
  (test #f struct-type-property-predicate-procedure? p? prop:p2)
  (test #t struct-type-property-predicate-procedure? p?)
  (test #t struct-type-property-predicate-procedure? p? #f)
  (test #t struct-type-property-predicate-procedure? p? prop:p)
  (err/rt-test (struct-type-property-predicate-procedure? p? 'oops))
  (err/rt-test (struct-type-property-predicate-procedure? 7 'oops))
  (err/rt-test (struct-type-property-predicate-procedure? 7 0))
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
      (test 'make-a object-name (struct-type-make-constructor type))
      (test 'a-field2 object-name sel2)
      (test 'racket procedure-realm sel2)
      (test 'set-a-field2! object-name set2)
      (test 'racket procedure-realm set2)
      (let ([sel2x (make-struct-field-accessor sel 2 'x)]
            [set2x (make-struct-field-mutator set 2 'x)])
        (test 'a-x object-name sel2x)
        (test 'racket procedure-realm sel2x)
        (test 'set-a-x! object-name set2x)
        (test 'racket procedure-realm set2x))
      (let ([new-ctor (struct-type-make-constructor type 'some-other-name)])
        (test 'some-other-name object-name new-ctor)
        (test #t struct-constructor-procedure? new-ctor))
      (let ([new-pred (struct-type-make-predicate type)])
        (test #t struct-predicate-procedure? new-pred)
        (test #f struct-constructor-procedure? new-pred))
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

;; test using the transformer binding incorrectly
(syntax-test #'(let ()
                 (define-struct a ())
                 (a . b)))

(syntax-test #'(define-struct a (b c) #:transparent #:inspector #f))
(syntax-test #'(define-struct a (b c) #:transparent #:prefab))
(syntax-test #'(define-struct a (b c) #:prefab #:guard 10))
(syntax-test #'(define-struct a (b c) #:prefab #:property 1 10))
(syntax-test #'(define-struct a (b c) #:guard 10 #:prefab))
(syntax-test #'(define-struct a (b c) #:property 1 10 #:prefab))
(syntax-test #'(define-struct a (b c) #:sealed #:prefab))
(syntax-test #'(define-struct a (b c) #:prefab #:sealed))
(syntax-test #'(define-struct a (b c) #:prefab #:authentic))
(syntax-test #'(define-struct a (b c) #:authentic #:prefab))

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
;; Inspectors

(test #t inspector? (make-inspector))
(test #t inspector? (current-inspector))
(test #f inspector? (list (make-inspector)))

(test #f inspector-superior? (current-inspector) (current-inspector))
(test #t inspector-superior? (current-inspector) (make-inspector))
(test #f inspector-superior? (make-inspector) (make-inspector))
(test #t inspector-superior? (current-inspector) (make-inspector (make-inspector (make-inspector))))

(test #t inspector? (make-sibling-inspector))
(test #f inspector-superior? (current-inspector) (make-sibling-inspector))
(test #f inspector-superior? (make-sibling-inspector) (current-inspector))
(test #t inspector-superior? (current-inspector) (make-sibling-inspector (make-inspector)))

;; ------------------------------------------------------------
;; Some built-in structure procedures

(test #t struct-predicate-procedure? exn?)
(test #t struct-predicate-procedure? exn:fail?)
(test #t struct-predicate-procedure? exn:fail:contract?)
(test #t struct-predicate-procedure? srcloc?)
(test #t struct-predicate-procedure? date?)

(test #t struct-accessor-procedure? exn-message)
(test #t struct-accessor-procedure? srcloc-line)
(test #t struct-accessor-procedure? date-month)

;; ------------------------------------------------------------
;; Property accessor errors

(let-values ([(prop:p p? p-ref) (make-struct-type-property 'prop1 #f '() #t)])
  (test 42 p-ref 5 42)
  (test 17 p-ref 5 (lambda () (* 1 17)))
  (err/rt-test (p-ref 5) exn:fail:contract?))

;; ------------------------------------------------------------
;; Property type supers

(require (only-in '#%kernel [prop:procedure mz:prop:procedure])) ; more primitive - no keywords

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
;; struct->vector on non `struct?`

(test #f struct? void)
(test #f struct? (procedure-rename void 'still-void))
(test #f struct? (procedure-reduce-arity void 1))
(test #f struct? (cons 1 2))
(test #f struct? (box 1))
(test #f struct? (vector 1 2 3))

(test '#(struct:procedure ...) struct->vector void)
(test '#(struct:procedure ...) struct->vector (procedure-rename void 'still-void))
(test '#(struct:procedure ...) struct->vector (procedure-reduce-arity void 1))
(test '#(struct:pair ...) struct->vector (cons 1 2))
(test '#(struct:box ...) struct->vector (box 1))
(test '#(struct:vector ...) struct->vector (vector 1 2 3))

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

(test 'v prefab-struct-key #s(v one))
(test '(v w 2) prefab-struct-key #s((v w 2) one two three))
(test #f prefab-struct-key "apple")
(test #f prefab-struct-key 10)

(let ()
  (define-struct t (a b) #:prefab)
  (define-struct t2 (a b))
  (define-struct (t3 t) (c) #:prefab)
  (test '(t . 2) prefab-struct-type-key+field-count struct:t)
  (test #f prefab-struct-type-key+field-count struct:t2)
  (test '((t3 t 2) . 3) prefab-struct-type-key+field-count struct:t3))

;; ------------------------------------------------------------
;; Sealed

(err/rt-test (let ()
               (struct x () #:sealed)
               (struct y x ())
               (y))
             exn:fail:contract?
             "make-struct-type: cannot make a subtype of a sealed type")

(err/rt-test (let ()
               (struct x () #:sealed)
               (struct y x () #:sealed)
               (y))
             exn:fail:contract?
             "make-struct-type: cannot make a subtype of a sealed type")

(err/rt-test (let ()
               (define-values (prop:s s? s-ref)
                 (make-struct-type-property 's #f (list (cons prop:sealed (lambda (x) #t)))))
               (struct x () #:property prop:s #t)
               (struct y x ())
               (y))
             exn:fail:contract?
             "make-struct-type: cannot make a subtype of a sealed type")

(test '(#f #t) cdr (let ()
                     (struct x ())
                     (struct y x () #:sealed)
                     (list (y)
                           (struct-type-sealed? struct:x)
                           (struct-type-sealed? struct:y))))

(err/rt-test (let ()
               (struct x ())
               (struct y x () #:sealed)
               (struct z y ())
               (y))
             exn:fail:contract?
             "make-struct-type: cannot make a subtype of a sealed type")

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

  (test "#<struct-type:tuple>" values (format "~s" s:tuple)) ; shouldn't trigger custom write
  
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
  (test #t equal-always? (make-t1 0 1) (make-t1 0 1))
  (test #t equal? (make-t2 0 1) (make-t2 0 1))
  (test #f equal-always? (make-t2 0 1) (make-t2 0 1))
  (test #f chaperone-of? (make-t2 0 1) (make-t2 0 1))
  (test #t impersonator-of? (make-t2 0 1) (make-t2 0 1))
  (let ([t (make-t2 0 1)])
    (test #t equal-always? t t))
  (test #t equal? 
        (shared ([t (make-t2 0 t)]) t) 
        (shared ([t (make-t2 0 t)]) t))
  (test #f equal?
        (shared ([t (make-t2 0 t)]) t) 
        (shared ([t (make-t2 1 t)]) t))
  (test #t = 
        (equal-hash-code (make-t1 0 1))
        (equal-hash-code (make-t1 0 1)))
  (let ([t (make-t1 0 1)])
    (test #t = 
          (equal-always-hash-code t)
          (equal-always-hash-code t)))
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
  (test #f equal-always? (make-o 1 2 3) (make-o 1 20 3))
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
  (let ([t (make-o 1 2 3)])
    (test #t =
          (equal-always-hash-code t)
          (equal-always-hash-code t)))
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

(let ([was-always? #f])
  (test 'new-protocol car '(new-protocol))
  ;; new `prop:equal+hash` that more fully supports `equal-always?`
  (define-struct o (x y z)
    #:property prop:equal+hash (list
                                (lambda (a b equal? now?)
                                  (set! was-always? (not now?))
                                  (and (equal? (o-x a) (o-x b))
                                       (equal? (o-z a) (o-z b))))
                                (lambda (a hash now?)
                                  (set! was-always? (not now?))
                                  (+ (hash (o-x a)) (* 9 (hash (o-z a))))))
    #:mutable)

  (test #t equal? (make-o 1 2 3) (make-o 1 20 3))
  (test #f equal? (make-o 1 2 3) (make-o 1 20 30))
  (test #f values was-always?)
  (test #t equal-always? (make-o 1 2 3) (make-o 1 20 3))
  (test #f equal-always? (make-o 1 2 3) (make-o 1 20 30))
  (test #t values was-always?)
  (test #t impersonator-of? (make-o 1 2 3) (make-o 1 20 3))
  (test #f impersonator-of? (make-o 1 2 3) (make-o 1 20 30))
  (test #f values was-always?)
  (test #t chaperone-of? (make-o 1 2 3) (make-o 1 20 3))
  (test #f chaperone-of? (make-o 1 2 3) (make-o 1 20 30))
  (test #t values was-always?)

  (test #t equal?
        (shared ([t (make-o t 0 0)]) t) 
        (shared ([t (make-o t 0 0)]) t))
  (test #f equal?
        (shared ([t (make-o t 0 0)]) t) 
        (shared ([t (make-o t 0 1)]) t))
  (test #t equal-always?
        (shared ([t (make-o t 0 0)]) t) 
        (shared ([t (make-o t 0 0)]) t))
  (test #f equal-always?
        (shared ([t (make-o t 0 0)]) t) 
        (shared ([t (make-o t 0 1)]) t))

  (test #t = (equal-hash-code (make-o 1 2 3)) (equal-hash-code (make-o 1 20 3)))
  (test #f values was-always?)
  (test #t = (equal-always-hash-code (make-o 1 2 3)) (equal-always-hash-code (make-o 1 20 3)))
  (test #t values was-always?)
  (test #t =
        (equal-hash-code (shared ([t (make-o 0 0 t)]) t))
        (equal-hash-code (shared ([t (make-o 0 0 t)]) t)))
  (test #t =
        (equal-always-hash-code (shared ([t (make-o 0 0 t)]) t))
        (equal-always-hash-code (shared ([t (make-o 0 0 t)]) t)))
  
  (test #t = 
        (equal-secondary-hash-code (make-o 0 1 3))
        (equal-secondary-hash-code (make-o 0 10 3)))
  (test #t =
        (equal-secondary-hash-code (shared ([t (make-o 0 1 t)]) t))
        (equal-secondary-hash-code (shared ([t (make-o 0 2 t)]) t)))

  (void))

;; ----------------------------------------

(let ([got-here? #f])
  (struct foo2 (f g) #:transparent
    #:property prop:equal+hash
    (list (λ (a b recur) #f)
          (λ (a recur) 0)
          (λ (a recur) (set! got-here? #t) 0)))
  (define (check-secondary-used v)
    (set! got-here? #f)
    (equal-secondary-hash-code v)
    got-here?)
  (test #t check-secondary-used (foo2 0 "ggg"))
  ;; Although nothing promises that we'll hash an element within a
  ;; list, vector, etc., the current implementation is meant to
  ;; do so in at least these cases:
  (test #t check-secondary-used (list (foo2 0 "ggg")))
  (test #t check-secondary-used (cons 6 (foo2 0 "ggg")))
  (test #t check-secondary-used (vector (foo2 0 "ggg")))
  (test #t check-secondary-used (box (foo2 0 "ggg")))
  (test #t check-secondary-used (hash 'ok (foo2 0 "ggg"))))

;; ----------------------------------------

(let ([got-here? #f])
  (struct foo (f g)
    #:property prop:equal+hash
    (list (λ (a b recur) #f)
          (λ (a recur) 'wrong)
          (λ (a recur) 'wrong)))
  (err/rt-test (equal-hash-code (foo 1 2))
               exn:fail:contract?
               #rx"hash procedure returned a value other than an exact integer")
  (err/rt-test (equal-secondary-hash-code (foo 1 2))
               exn:fail:contract?
               #rx"hash procedure returned a value other than an exact integer"))

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

(let ()
  (define-values (prop:a a? a-ref) (make-struct-type-property 'a))
  (define-values (prop:b b? b-ref) (make-struct-type-property 'b))
  (define-values (prop:c c? c-ref) (make-struct-type-property 'c))
  (struct s (x y)
    #:properties (list (cons prop:a "abc") (cons prop:b "xyz"))
    #:property prop:procedure (lambda (self arg) arg)
    #:properties (list (cons prop:c 'here)))

  (test "abc" a-ref (s 1 2))
  (test "xyz" b-ref (s 1 2))
  (test 'here c-ref (s 1 2))
  (test 123 (s 1 2) 123))

;; ----------------------------------------

(require (for-syntax racket/struct-info))

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

(module test-struct-rename racket/base
  (provide (rename-out [point point2d]))
  (struct point (x y) #:transparent))

(let ()
  (local-require 'test-struct-rename)
  (test (point2d 3 2) 'struct-copy1 (struct-copy point2d (point2d 1 2) [x 3])))

(module test-struct-parent racket/base
  (provide a)
  (struct a (b-c) #:transparent))

(let ()
  (local-require 'test-struct-parent)
  (struct a-b a (c) #:transparent)

  (test (a-b 10 2) 'struct-copy1 (struct-copy a-b (a-b 1 2) [b-c #:parent a 10]))
  (test (a-b 1 10) 'struct-copy2 (struct-copy a-b (a-b 1 2) [c 10])))

(let ()
  (local-require 'test-struct-parent)
  (struct a-b a (d) #:transparent)
  (syntax-test #'(struct-copy a-b (a-b 1 2) [c 10])))

(module test-struct-copy-no-struct-field-info racket/base
  (provide bar)
  (require (for-syntax racket/struct-info
                       racket/base))
  (define (bar-car x) (car x))
  (define (bar-cdr x) (cdr x))
  (define (bar? x) (pair? x))

  (struct foo ())

  (define-syntax bar
    (make-struct-info
     (λ () (list #f
                 #'cons
                 #'bar?
                 (list #'bar-cdr #'bar-car)
                 (list #f #f)
                 #'foo)))))

(let ()
  (local-require 'test-struct-copy-no-struct-field-info)
  (test (cons 3 2) 'struct-copy1 (struct-copy bar (cons 1 2) [car 3])))

(test #t prefab-key? 'apple)
(test #f prefab-key? '#(apple))
(test #t prefab-key? '(apple 4))
(test #t prefab-key? '(foo #(0)))
(test #f prefab-key? '(foo 0 #(0)))
(err/rt-test (make-prefab-struct '(foo #(0))) (lambda (exn)
                                                (regexp-match? #rx"mismatch" (exn-message exn))))

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
  (err/rt-test (thing.id! (make-thing 1) 'new-val))
  
  (let ([f #f])
    ;; defeat inlining to ensure that thunk is JITted:
    (set! f (lambda () (thing.id! (make-thing 1) 'new-val)))
    (err/rt-test (f))))

;; ----------------------------------------
;; Test `prop:object-name`:

(let ()
  (struct x1 (v) #:property prop:object-name 0)
  (struct x2 (v) #:property prop:object-name
          (lambda (s) 'name))
  (struct x3 (v) #:property prop:object-name
          (lambda (s) (x3-v s)))
  (test 'x object-name (x1 'x))
  (test 'name object-name (x2 'x))
  (test "x" object-name (x3 "x"))
  (err/rt-test (let () (struct x1 (v) #:property prop:object-name 1) 0))
  (err/rt-test (let () (struct x0 (w)) (struct x1 x0 () #:property prop:object-name 0) 0))
  (err/rt-test (let () (struct x1 (v) #:property prop:object-name (lambda () 0)) 0))
  (err/rt-test (let () (struct x1 (v) #:property prop:object-name (lambda (a b) 0)) 0))
  (err/rt-test (let () (struct x1 (v) #:mutable #:property prop:object-name 0) 0)))


;; ----------------------------------------
;; Check interaction of `struct-type-info` and GC:

(struct-type-info struct:arity-at-least)
(collect-garbage)
(let-values ([(name init-cnt auto-cnt acc mut immut super skipped?)
              (struct-type-info struct:arity-at-least)])
  (test #t procedure? acc)
  (test #t procedure? mut))

;; ----------------------------------------
;; Check that prefab struct size limit is enforced:

(err/rt-test (make-prefab-struct '(foo 2999999999999999) 1))
(err/rt-test (make-prefab-struct '(foo 5 bar 2999999999999999) 1))

;; ----------------------------------------
;; Check that prefab mutable-field spec makes sense for size:

(err/rt-test (make-prefab-struct '(foo 5 (1 #f) #(1) bar 2 #(99999)) 1 2 3 4 5 6 7 8))
(test #t struct? (make-prefab-struct '(foo 5 (1 #f) #(1) bar 2 #()) 1 2 3 4 5 6 7 8))
(test #t struct? (make-prefab-struct '(foo 5 (1 #f) #(1) bar 0 #()) 1 2 3 4 5 6))

;; ----------------------------------------
;; Check `#:name` and `#:extra-name`:

(let ()
  (struct ghost (color name) #:extra-name GHOST)
  (test 'blinky ghost-name (ghost 'red 'blinky))
  (struct running-ghost GHOST (edible?))
  (test 'blinky ghost-name (running-ghost 'red 'blinky #f)))

(let ()
  (struct ghost (color name) #:name GHOST)
  (struct running-ghost GHOST (edible?))
  (test 'blinky ghost-name (running-ghost 'red 'blinky #f)))

(syntax-test #'(struct ghost (color name) #:name gHoSt #:extra-name GHOST))

(struct ghost (color name) #:name GHOST)
(test #t procedure? ghost)
(test #t ghost? (ghost 'red 'blinky))
(test 'blinky ghost-name (struct-copy GHOST (ghost 'red 'blinky)))
(syntax-test #'GHOST)

(syntax-test #'(struct ghost (color name) #:extra-name GHOST #:omit-define-syntaxes)
             "cannot be combined")

;; ----------------------------------------
;; Check `#:authentic`:

(let ()
  (struct posn (x y) #:authentic)
  (test 1 posn-x (posn 1 2))
  (test #t struct-type-authentic? struct:posn)
  (err/rt-test (chaperone-struct (posn 1 2) posn-x (lambda (p x) x)))

  ;; Subtype must be consistent:
  (err/rt-test (let ()
                 (struct posn3D posn (z))
                 'ok)))

(let ()
  (struct posn (x y))
  (test #f struct-type-authentic? struct:posn)

  ;; Subtype must be consistent:
  (err/rt-test (let ()
                 (struct posn3D posn (z)
                   #:authentic)
                 'ok)))

;; ----------------------------------------
;; Check that constructing a prefab type via its key before via
;; `make-struct-type` gets the mutability of auto fields right.

(let ([s (string->symbol (format "s~a" (current-milliseconds)))])
  (define v (read (open-input-string (format "#s((~a (2 #f)) 1 2)" s))))
  (define-values (struct: make- ? -ref -set!)
    (make-struct-type s #f 0 2 #f null 'prefab))
  (-set! v 0 'ok)
  (test 'ok -ref v 0))

(let ([s (string->symbol (format "s~a" (current-milliseconds)))])
  (define v (read (open-input-string (format "#s((~a (2 #f)) 'x 'y 'z 1 2)" s))))
  (define-values (struct: make- ? -ref -set!)
    (make-struct-type s #f 3 2 #f null 'prefab #f '(0 1 2)))
  (-set! v 3 'ok)
  (test 'ok -ref v 3))

;; ----------------------------------------
;; Check that prefab auto fields count as mutable

(let ()
  (struct flag ([x #:auto #:mutable]) #:prefab)
  (define f (flag))
  (set-flag-x! f 'ok)
  (test 'ok flag-x f)

  (err/rt-test (set-flag-x! 'no 'way) exn:fail:contract? #rx"^set-flag-x!:")

  (define f2 (read (open-input-string "#s((flag (1 #f)) #f)")))
  (test #f flag-x f2)
  (set-flag-x! f2 'ok)
  (test 'ok flag-x f2)

  (struct flag-3d flag (y [z #:auto #:mutable]) #:prefab)
  (define f3 (flag-3d 'y))
  (set-flag-x! f3 'three)
  (test 'three flag-x f3)
  (set-flag-3d-z! f3 'zee)
  (test 'zee flag-3d-z f3))

;; ----------------------------------------
;; Make sure that a JIT-inlined predicate doesn't
;; fail improperly on chaperones and struct types

(let ()
  (define-values (prop:a a? a-ref) (make-struct-type-property 'a))
  (define (mk-prop)
    (define-values (prop:b b? b-ref) (make-struct-type-property 'b))
    prop:b)

  (struct posn (x y)
    #:property prop:a 'yes)
  (struct posn2 posn ()
    #:property (mk-prop) 0
    #:property (mk-prop) 1
    #:property (mk-prop) 2
    #:property (mk-prop) 3
    #:property (mk-prop) 4
    #:property (mk-prop) 5)

  (define (f p)
    (and (a? p)
         (a-ref p 'no)))
  (define (g p get-no)
    (a-ref p get-no))
  (set! f f)
  (set! g g)
  
  (test 'yes f (posn 1 2))
  (test 'yes f (posn2 1 2))
  (test 'yes f (chaperone-struct (posn 1 2) posn-x (lambda (p x) x)))
  (test 'yes f struct:posn)
  (test #f f struct:arity-at-least)
  (test #f f 5)
  (test 'nope g 5 (lambda () 'nope))
  (test 'nope g struct:arity-at-least (lambda () 'nope)))

;; ----------------------------------------
;; Make sure an indirect struct constructor reports
;; the right arity when there are more than 6 fields

(let ()
  (struct b (case-sensitive 
             printing-style 
             fraction-style
             show-sharing
             insert-newlines
             annotations))
  (struct a (x y) #:super struct:b)
  (test 8 procedure-arity a))

;; ----------------------------------------
;; Make sure all checking and good error messages are in place for
;; position-based accessors and mutators:

(let ()
  (define-values (struct:s make-s s? s-ref s-set!)
    (make-struct-type 's #f 3 0 #f null (current-inspector) #f '(0 1 2)))

  (define s (make-s 1 2 3))

  (test 1 s-ref s 0)
  (test 2 s-ref s 1)
  (test 3 s-ref s 2)

  (err/rt-test (s-ref 's 0) exn:fail:contract? #rx"^s-ref:.*  expected: s[?]")
  (err/rt-test (s-ref s -1) exn:fail:contract? #rx"^s-ref:.*  expected: exact-nonnegative-integer[?]")
  (err/rt-test (s-ref s 'no) exn:fail:contract? #rx"^s-ref:.*  expected: exact-nonnegative-integer[?]")
  (err/rt-test (s-ref s 3) exn:fail:contract? #rx"s-ref: index too large")
  (err/rt-test (s-ref s (expt 2 100)) exn:fail:contract? #rx"s-ref: index too large")

  (err/rt-test (s-set! 's 0 'v) exn:fail:contract? #rx"^s-set!:.*  expected: s[?]")
  (err/rt-test (s-set! s -1 'v) exn:fail:contract? #rx"^s-set!:.*  expected: exact-nonnegative-integer[?]")
  (err/rt-test (s-set! s 'no 'v) exn:fail:contract? #rx"^s-set!:.*  expected: exact-nonnegative-integer[?]")
  (err/rt-test (s-set! s 3 'v) exn:fail:contract? #rx"s-set!: index too large")
  (err/rt-test (s-set! s (expt 2 100) 'v) exn:fail:contract? #rx"s-set!: index too large")
  (err/rt-test (s-set! s 0 'v) exn:fail:contract? #rx"s-set!: cannot modify value of immutable field")
  (err/rt-test (s-set! s 1 'v) exn:fail:contract? #rx"s-set!: cannot modify value of immutable field")
  (err/rt-test (s-set! s 2 'v) exn:fail:contract? #rx"s-set!: cannot modify value of immutable field"))

(let ()
  (define-values (struct:s make-s s? s-ref s-set!)
    (make-struct-type 's #f 3 0 #f null (current-inspector) #f '()))

  (define s (make-s 1 2 3))

  (test (void) s-set! s 0 10)
  (test (void) s-set! s 1 20)
  (test (void) s-set! s 2 30)
  (test 10 s-ref s 0)
  (test 20 s-ref s 1)
  (test 30 s-ref s 2)

  (err/rt-test (s-set! 's 0 'v) exn:fail:contract? #rx"^s-set!:.*  expected: s[?]")
  (err/rt-test (s-set! s -1 'v) exn:fail:contract? #rx"^s-set!:.*  expected: exact-nonnegative-integer[?]")
  (err/rt-test (s-set! s 'no 'v) exn:fail:contract? #rx"^s-set!:.*  expected: exact-nonnegative-integer[?]")
  (err/rt-test (s-set! s 3 'v) exn:fail:contract? #rx"s-set!: index too large")
  (err/rt-test (s-set! s (expt 2 100) 'v) exn:fail:contract? #rx"s-set!: index too large"))

;; ----------------------------------------
;; Make sure that non-typical `make-struct-type` patterns are
;; not transformed incorrectly by the compiler

(test '(1 2) 'not-acc/ref
      (let-values ([(struct:s make-s s? a b)
                    (let-values ([(struct:s make s? -ref -set!) (make-struct-type 's #f 3 0 #f)])
                      (values struct:s
                              make
                              s?
                              1
                              2))])
        (list a b)))

(define-syntax (try-failing-extra stx)
  (syntax-case stx ()
    [(_ expr rx)
     (with-syntax ([expr (syntax-local-introduce #'expr)])
       #'(err/rt-test (let-values ([(struct:s make-s s? bad)
                                    (let-values ([(struct:s make s? -ref -set!) (make-struct-type 's #f 3 0 #f)])
                                      (values struct:s
                                              make
                                              s?
                                              expr))])
                        bad-ref)
                      exn:fail:contract?
                      rx))]))

(try-failing-extra (make-struct-field-accessor -ref 3 'name)
                   #rx"index too large")
(try-failing-extra (make-struct-field-mutator -set! 3 'name)
                   #rx"index too large")

(try-failing-extra (make-struct-field-accessor -ref -1 'name)
                   #rx"make-struct-field-accessor: contract violation")
(try-failing-extra (make-struct-field-mutator -set! -1 'name)
                   #rx"make-struct-field-mutator: contract violation")

(try-failing-extra (make-struct-field-accessor -set! 0 'name)
                   #rx"make-struct-field-accessor: contract violation")
(try-failing-extra (make-struct-field-mutator -ref 0 'name)
                   #rx"make-struct-field-mutator: contract violation")

;; ----------------------------------------

(test #t struct-type-property-accessor-procedure? custom-write-accessor)
(test #t struct-type-property-accessor-procedure? custom-print-quotable-accessor)

;; ----------------------------------------

(let ()
  (define-values (s cns pred ref set) (make-struct-type 'thing #f 1 1 #f))
  (test 'make-thing object-name cns)
  (test 'thing? object-name pred)
  (test 'thing-ref object-name ref)
  (test 'thing-set! object-name set))

;; ----------------------------------------

(let ()
  (struct foo (x))
  (struct bar foo (y z))
  (define-syntax (get-bar-field-names stx)
    #`'#,(struct-field-info-list (syntax-local-value #'bar)))
  (define (get-bar-field-names*) (get-bar-field-names))
  (test '(z y) get-bar-field-names*))

;; ----------------------------------------

(let ()
  (struct bar (a [b #:mutable] [c #:mutable] [d #:auto] [e #:auto] [f #:auto #:mutable]))
  (define-syntax (get-bar-auto-field-accessors+mutators stx)
    #`'#,(struct-auto-info-lists (syntax-local-value #'bar)))
  (define (get-bar-auto-field-accessors+mutators*) (get-bar-auto-field-accessors+mutators))
  (test '((bar-d bar-e bar-f) (set-bar-f!)) get-bar-auto-field-accessors+mutators*))

;; ----------------------------------------

(let ()
  (struct exn:foo exn () #:constructor-name make-exn:foo)
  (test "foo" exn-message (make-exn:foo "foo" (current-continuation-marks))))

(let ()
  (struct foo (x) #:constructor-name Foo #:name Foo)
  (test #t foo? (Foo 1))
  (test 1 foo-x (Foo 1))
  (test #t foo? (let-syntax ([mk (lambda (stx)
                                   #`(#,(cadr (extract-struct-info (syntax-local-value #'Foo))) 1))])
                  (mk))))

;; ----------------------------------------

(err/rt-test
 (let ()
   (struct x ())
   (define unknown struct:x)
   (set! unknown unknown)

   (define-values (struct:y y y? y-z)
     (let-values ([(struct:_1 make-_2 ?_3 -ref_4 -set!_5)
                   (let-values ()
                     (let-values ()
                       (make-struct-type 'y unknown 1 0 #f
                                         (list)
                                         'prefab ; (current-inspector)
                                         #f '() #f 'y)))])
       (values
        struct:_1
        make-_2
        ?_3
        (make-struct-field-accessor -ref_4 0 'z))))

   'done)
 exn:fail:contract?
 "generative supertype disallowed for non-generative structure type")

(err/rt-test
 (let ()
   ;; Should be arity error (as opposed to a crash)
   (define-values (struct:y y y? y-z)
     (let-values ([(struct:_1 make-_2 ?_3 -ref_4 -set!_5)
                   (let-values ()
                     (let-values ()
                       (make-struct-type 'y #f 1 0 #f
                                         (list)
                                         (current-inspector)
                                         #f '() #f 'y 'extra)))])
       (values
        struct:_1
        make-_2
        ?_3
        (make-struct-field-accessor -ref_4 0 'z))))
   5))

;; ----------------------------------------
;; names and realms

(let ()
  (define-values (struct:cat make-cat cat? cat-ref cat-set!)
    (make-struct-type 'cat #f 2 2 'auto))
  (define c1 (make-cat 1 2))
  (define cat-paw1 (make-struct-field-accessor cat-ref 0 'cat-paw1 "gato?" 'elsewhere))
  (define set-cat-paw1! (make-struct-field-mutator cat-set! 0 'set-cat-paw1! "gato!?" 'elsewhere!))
  (define cat-paw4 (make-struct-field-accessor cat-ref 3 'cat-paw4 "gato?" 'elsewhere))
  (define set-cat-paw4! (make-struct-field-mutator cat-set! 3 'set-cat-paw4! "gato!?" 'elsewhere!))
  (test 'cat-paw1 object-name cat-paw1)
  (test 'elsewhere procedure-realm cat-paw1)
  (test 'set-cat-paw1! object-name set-cat-paw1!)
  (test 'elsewhere! procedure-realm set-cat-paw1!)
  (test 'cat-paw4 object-name cat-paw4)
  (test 'elsewhere procedure-realm cat-paw4)
  (test 'set-cat-paw4! object-name set-cat-paw4!)
  (test 'elsewhere! procedure-realm set-cat-paw4!)
  (err/rt-test (cat-paw1 "apple") exn:fail:contract? #rx"cat-paw1: .*gato[?]")
  (err/rt-test (set-cat-paw1! "apple" 0) exn:fail:contract? #rx"set-cat-paw1!: .*gato![?]")
  (err/rt-test (cat-paw4 "apple") exn:fail:contract? #rx"cat-paw4: .*gato[?]")
  (err/rt-test (set-cat-paw4! "apple" 0) exn:fail:contract? #rx"set-cat-paw4!: .*gato![?]")
  (let ()
    (define-values (struct:xcat make-xcat xcat? xcat-ref xcat-set!)
      (make-struct-type 'cat #f 0 0))
    (err/rt-test (cat-paw1 (make-xcat)) exn:fail:contract? #rx"cat-paw1: .*gato[?]")
    (err/rt-test (set-cat-paw1! (make-xcat) 0) exn:fail:contract? #rx"set-cat-paw1!: .*gato![?]"))
  (define (adjuster mode)
    (case mode
      [(name) (lambda (name realm)
                (cond
                  [(and (eq? name 'cat-paw1) (eq? realm 'elsewhere))
                   (values 'kitty-paw-one 'here)]
                  [(and (eq? name 'set-cat-paw1!) (eq? realm 'elsewhere!))
                   (values 'set-kitty-paw-one! 'here)]
                  [else (values name realm)]))]
      [(contract) (lambda (ctc realm)
                    (cond
                      [(and (equal? ctc "gato?") (eq? realm 'elsewhere))
                       (values "is-kitty?" 'here)]
                      [(and (equal? ctc "gato!?") (eq? realm 'elsewhere!))
                       (values "is-kitty!?" 'here)]
                      [else (values ctc realm)]))]
      [else #f]))
  (err/rt-test (with-continuation-mark
                error-message-adjuster-key adjuster
                (cat-paw1 "apple"))
               exn:fail:contract?
               #rx"kitty-paw-one: .*is-kitty[?]")
  (err/rt-test (with-continuation-mark
                error-message-adjuster-key adjuster
                (set-cat-paw1! "apple" 0))
               exn:fail:contract?
               #rx"set-kitty-paw-one!: .*is-kitty![?]")
  (let ()
    (define-values (struct:xcat make-xcat xcat? xcat-ref xcat-set!)
      (make-struct-type 'cat #f 0 0))
    (err/rt-test (with-continuation-mark
                  error-message-adjuster-key adjuster
                  (cat-paw1 (make-xcat)))
                 exn:fail:contract?
                 #rx"kitty-paw-one: .*is-kitty[?]")
    (err/rt-test (with-continuation-mark
                  error-message-adjuster-key adjuster
                  (set-cat-paw1! (make-xcat) 0))
                 exn:fail:contract?
                 #rx"set-kitty-paw-one!: .*is-kitty![?]"))
  (void))

(let ()
  (define-values (prop:animal animal? animal-ref)
    (make-struct-type-property 'animal
                               #f
                               null
                               #t
                               'animal-get
                               "is-animal?"
                               'elsewhere))
  (test 'animal-get object-name animal-ref)
  (test 'elsewhere procedure-realm animal-ref)
  (err/rt-test (animal-ref 10) exn:fail:contract?
               #rx"animal-get: .*is-animal[?]"))

(let ()
  (struct a (b) #:authentic #:mutable)
  (err/rt-test (a-b 1) exn:fail:contract? #rx"^a-b:")
  (err/rt-test (set-a-b! 1 #f) exn:fail:contract? #rx"^set-a-b!:"))

;; ----------------------------------------
;; make sure `prop:object-name` works with applicables

(let ()
  (struct x ()
    #:property prop:object-name
    (let ()
      (struct p ()
        #:property prop:procedure (lambda (self v) 'x))
      (p)))
  (test 'x object-name (x)))

;; ----------------------------------------
;; make sure `prop:object-name` is not used to try to get a name
;; for the structure type itself

(let ([asked? #f])
  (struct x ()
    #:property prop:object-name (lambda (self) (set! asked? #t) "NAME"))

  (test #f values asked?)
  (test 'x object-name struct:x)
  (test #f values asked?)
  (test "#<struct-type:x>" format "~a" struct:x)
  (test #f values asked?)

  (test "NAME" object-name (x))
  (test #t values asked?))

;; ----------------------------------------
;; Check that a property guard always gets a super structure type

(let ()
  (define-values (prop has-prop? prop-ref)
    (make-struct-type-property
     'prop
     (lambda (v info)
       (test (if (eq? (car info) 'a)
                 #f
                 struct:a)
             values
             (list-ref info 6))
       v)))
  (struct a (x) #:property prop 1)
  (struct b a (y) #:property prop 2)
  (void))

;; ----------------------------------------
;; Regression test related to constant-folding optimization

(let ([c (compile '(module m racket/base
                     (struct? #s(a 1))))])
  (define o (open-output-bytes))
  (write c o)
  (test #t 'compiled (compiled-module-expression? (parameterize ([read-accept-compiled #t])
                                                    (read (open-input-bytes (get-output-bytes o)))))))

(let ([c (compile (prefab-key->struct-type 'something 10))])
  (define o (open-output-bytes))
  (write c o)
  (test (prefab-key->struct-type 'something 10)
        values
        (eval (parameterize ([read-accept-compiled #t])
                (read (open-input-bytes (get-output-bytes o)))))))

;; ----------------------------------------

(report-errs)
