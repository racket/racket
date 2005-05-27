
(define ones-case
  (make-struct-case
   (list
    one00?
    one01?
    one02?
    one03?
    
    one10?
    one11?
    one12?
    one13?
    
    one20?
    one21?
    one22?
    one23?
    
    one30?
    one31?
    one32?
    one33?)
   
   (list
    (lambda (x) 'one00)
    (lambda (x) 'one01)
    (lambda (x) 'one02)
    (lambda (x) 'one03)
    
    (lambda (x) 'one10)
    (lambda (x) 'one11)
    (lambda (x) 'one12)
    (lambda (x) 'one13)
    
    (lambda (x) 'one20)
    (lambda (x) 'one21)
    (lambda (x) 'one22)
    (lambda (x) 'one23)
    
    (lambda (x) 'one30)
    (lambda (x) 'one31)
    (lambda (x) 'one32)
    (lambda (x) 'one33))))

(define multi-case
  (make-struct-case
   (list
    two130?
    two131?
    two132?
    two133?
    
    one10?
    one11?
    one12?
    one13?
    
    one20?
    one21?
    one22?
    one23?
    
    base0?
    base1?
    base2?
    base3?)

   (list
    (lambda (x) 'two130)
    (lambda (x) 'two131)
    (lambda (x) 'two132)
    (lambda (x) 'two133)
    
    (lambda (x) 'one10)
    (lambda (x) 'one11)
    (lambda (x) 'one12)
    (lambda (x) 'one13)
    
    (lambda (x) 'one20)
    (lambda (x) 'one21)
    (lambda (x) 'one22)
    (lambda (x) 'one23)
    
    (lambda (x) 'base0)
    (lambda (x) 'base1)
    (lambda (x) 'base2)
    (lambda (x) 'base3))
   
   (lambda (x) x)))

(letrec ([bundle
	  (lambda (l f)
	    (if (null? l)
		null
		(list* f (car l) (cadr l)
		       (bundle (cddr l) f))))])
  (check (append
	  (bundle ones-test ones-case)
	  (bundle multi-test multi-case)
	  (list base1-a x11 1
		one11-a x11 2
		one10-a x10 1
		
		base1-a x31 1
		one31-z x31 4
		
		base2-l x132 1
		two132-a x132 6
		one32-y x132 4))))

(test #t arity-at-least? (multi-case (arity void)))

(arity-test multi-case 1 1)

(error-test `(,ones-case 6) type?)
(error-test `(,multi-case 6) type?)

(error-test `(,ones-case (arity void)) exn:else?)

(test (void) (make-struct-case null null void) x00)
(test #t procedure? (make-struct-case null null))

(error-test `((make-struct-case null null) x00) exn:else?)

(error-test `(make-struct-case (list 8) (list void)))
(error-test `(make-struct-case (list exn:misc? 8) (list void void)))
(error-test `(make-struct-case (list exn:misc? 8 exn?) (list void void void)))
(error-test `(make-struct-case exn? (list void)))
(error-test `(make-struct-case (list* exn:misc? exn?) (list void)))

(error-test `(make-struct-case (list exn?) (list 8)))
(error-test `(make-struct-case (list exn?) (list (lambda () 8))))
(error-test `(make-struct-case (list exn:misc? exn?) 
			       (list void string-set!)))
(error-test `(make-struct-case (list exn:misc? exn:syntax? exn?) 
			       (list void void string-set!)))
(error-test `(make-struct-case (list exn?) void))
(error-test `(make-struct-case (list exn?) (list* void void)))

(error-test `(make-struct-case (list exn:misc?) (list void void)) 
	    exn:application:list-sizes?)
(error-test `(make-struct-case (list exn:misc? exn?) (list void)) 
	    exn:application:list-sizes?)

(arity-test make-struct-case 2 3)

(test 0 (struct-case-lambda x (else 0)) (arity void))
(test (arity void) (struct-case-lambda x (else)) (arity void))
(test (arity void) (struct-case-lambda x (arity-at-least?)) (arity void))
(test 0 (struct-case-lambda x (arity-at-least? 0) (else 1)) (arity void))

(define s (struct-case-lambda x
	    [exn? 'exn]
	    [arity-at-least? x]
	    [else (cons x 5)]))

(test 'exn s (make-exn 1 2))
(test (arity void) s (arity void))
(test (cons x00 5) s x00)

(arity-test s 1 1)

(error-test '(s 9))
(error-test '(struct-case-lambda) syntaxe?)
(error-test '(struct-case-lambda 5) syntaxe?)
(error-test '(struct-case-lambda x . 5) syntaxe?)
(error-test '(struct-case-lambda x ()) syntaxe?)
(error-test '(struct-case-lambda x else) syntaxe?)
(error-test '(struct-case-lambda x (else 9) (exn? 8)) syntaxe?))

(define time-branch
  (lambda (proc list)
    (time
     (let loop ([n 1000])
       (unless (zero? n)
	       (let loop ([l list])
		 (unless (null? l)
			 (proc (car l))
			 (loop (cddr l))))
	       (loop (sub1 n)))))))

