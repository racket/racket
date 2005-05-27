;;; `test.scm' Test correctness of MzScheme implementations.
;;; Copyright (C) 1991, 1992, 1993, 1994 Aubrey Jaffer.
;;; Modified for MzScheme by Matthew

;;; MODIFIED for MzScheme - Matthew 8/95
;;;  Added a few more tests, like append!, reverse!, etc.
;;;  Added testing for inexact numbers
;;;  Added a lot of error testing
;;; modified for rational and complex numbers - Matthew 12/95
;;; modified to test exceptions and more of MzScheme - Matthew 4/96
;;; split into multiple files - Matthew 4/96
;;; extended, extended, extended

;;; This includes examples from
;;; William Clinger and Jonathan Rees, editors.
;;; Revised^4 Report on the Algorithmic Language Scheme
;;; and the IEEE specification.

; The format of the next line is important: file.ss relies on it
(define cur-section '())(define errs '())

#|

The test form has these two shapes:

  (test <expected> <procdure> <argument1> <argument2> ...)
  
  (test <expected> <symbolic-name> <expression>)

In the first case, it applies the result of <procedure> 
to the results of <argument1> etc and compares that (with equal?)
to the result of the <expected>

In the second case, it evaluates the <expression> and compares
the results of that (with equal?) to the value of the
<expected>. In this case, <symbolic-name> must evaluate to
something that isn't a procedure. That name is used in the
transcript.
  
|#



(define teval eval)

(namespace-variable-value 
 'building-flat-tests?
 #f
 (lambda ()
   (namespace-set-variable-value! 'building-flat-tests? #f)))
(namespace-variable-value 
 'in-drscheme?
 #f
 (lambda ()
   (namespace-set-variable-value! 'in-drscheme? #f)))

(define SECTION (lambda args
		  (let ([ep (current-error-port)])
		    (display "SECTION" ep) (write args ep) (newline ep)
		    (set! cur-section args) #t)))
(define record-error (lambda (e) (set! errs (cons (list cur-section e) errs))))

(print-struct #t)

(define number-of-tests 0)
(define number-of-error-tests 0)
(define number-of-exn-tests 0)

(define test
  (lambda (expect fun . args)
    (set! number-of-tests (add1 number-of-tests))
    (write (cons fun args))
    (display "  ==> ")
    (flush-output)
    ((lambda (res)
      (write res)
      (newline)
      (cond ((not (equal? expect res))
	     (record-error (list res expect (cons fun args)))
	     (display " BUT EXPECTED ")
	     (write expect)
	     (newline)
	     #f)
	    (else #t)))
     (if (procedure? fun) (apply fun args) (car args)))))


(define (nonneg-exact? x)
  (and (exact? x)
       (integer? x)
       (x . >= . 0)))

(define (pos-exact? x)
  (and (exact? x)
       (integer? x)
       (positive? x)))

(define exn-table
  (list (cons exn? (cons exn-message string?))
	(cons exn? (cons exn-continuation-marks continuation-mark-set?))
	(cons exn:fail:contract:variable? (cons exn:fail:contract:variable-id symbol?))
	(cons exn:fail:syntax? (cons exn:fail:syntax-exprs (lambda (x) (and (list? x) (andmap syntax? x)))))
	
	(cons exn:fail:read? (cons exn:fail:read-srclocs (lambda (x) (and (list? x) (andmap srcloc? x)))))))

(define exn:application:mismatch? exn:fail:contract?)
(define exn:application:type? exn:fail:contract?)
(define exn:application:arity? exn:fail:contract:arity?)

(define mz-test-syntax-errors-allowed? #t)

(define thunk-error-test
  (case-lambda 
   [(th expr) (thunk-error-test th expr exn:application:type?)]
   [(th expr exn?)
    (set! expr (syntax-object->datum expr))
    (set! number-of-error-tests (add1 number-of-error-tests))
    (write expr)
    (display "  =e=> ")
    (call/ec (lambda (escape)
	       (let* ([old-esc-handler (error-escape-handler)]
		      [old-handler (current-exception-handler)]
		      [orig-err-port (current-error-port)]
		      [test-handler
		       (lambda ()
			 (escape #t))]
		      [test-exn-handler
		       (lambda (e)
			 (when (and exn? (not (exn? e)))
			       (printf " WRONG EXN TYPE: ~s " e)
			       (record-error (list e 'exn-type expr)))
			 (when (and (exn:fail:syntax? e)
				    (not mz-test-syntax-errors-allowed?))
			       (printf " LATE SYNTAX EXN: ~s " e)
			       (record-error (list e 'exn-late expr)))

			 (for-each
			  (lambda (row)
			    (let ([pred? (car row)])
			      (when (pred? e)
				    (set! number-of-exn-tests 
					  (add1 number-of-exn-tests))
				    (let ([sel (cadr row)]
					  [pred? (cddr row)])
				      (unless (pred? (sel e))
					      (printf " WRONG EXN ELEM ~s: ~s " sel e)
					      (record-error (list e (cons 'exn-elem sel) expr)))))))
			  exn-table)
						 
			 (old-handler e))])
		 (dynamic-wind
		  (lambda () 
		    (current-error-port (current-output-port))
		    (current-exception-handler test-exn-handler)
		    (error-escape-handler test-handler))
		  (lambda ()
		    (let ([v (th)])
		      (write v)
		      (display " BUT EXPECTED ERROR")
		      (record-error (list v 'Error expr))
		      (newline)
		      #f))
		  (lambda () 
		    (current-error-port orig-err-port)
		    (current-exception-handler old-handler)
		    (error-escape-handler old-esc-handler))))))]))

(namespace-variable-value
 'error-test 
 #f
 (lambda ()
  (namespace-set-variable-value!
   'error-test
   (case-lambda 
    [(expr) (error-test expr exn:application:type?)]
    [(expr exn?)
     (thunk-error-test (lambda () (eval expr)) expr exn?)]))))

(require (rename mzscheme err:mz:lambda lambda)) ; so err/rt-test works with beginner.ss
(define-syntax err/rt-test
  (lambda (stx)
    (syntax-case stx ()
      [(_ e exn?)
       (syntax
	(thunk-error-test (err:mz:lambda () e) (quote-syntax e) exn?))]
      [(_ e)
       (syntax
	(err/rt-test e exn:application:type?))])))
  
(define no-extra-if-tests? #f)

(define (syntax-test expr)
  (error-test expr exn:fail:syntax?)
  (unless no-extra-if-tests?
    (error-test (datum->syntax-object expr `(if #f ,expr) expr) exn:fail:syntax?)))

(define arity-test 
  (case-lambda
   [(f min max except)
    (letrec ([aok?
	      (lambda (a)
		(cond
		 [(integer? a) (= a min max)]
		 [(arity-at-least? a) (and (negative? max)
					   (= (arity-at-least-value a) min))]
		 [(and (list? a) (andmap integer? a))
		  (and (= min (car a)) (= max
					  (let loop ([l a])
					    (if (null? (cdr l))
						(car l)
						(loop (cdr l))))))]
		 [(list? a)
		  ;; Just check that all are consistent for now.
		  ;; This should be improved.
		  (andmap
		   (lambda (a)
		     (if (number? a)
			 (<= min a (if (negative? max) a max))
			 (>= (arity-at-least-value a) min)))
		   a)]
		 [else #f]))]
	     [make-ok?
	      (lambda (v)
		(lambda (e)
		  (exn:application:arity? e)))]
	     [do-test
	      (lambda (f args check?)
		(set! number-of-error-tests (add1 number-of-error-tests))
		(printf "(apply ~s '~s)  =e=> " f args)
		(let/ec done
		  (let ([v (with-handlers ([void
					    (lambda (exn)
					      (if (check? exn)
						  (printf " ~a~n" (exn-message exn))
						  (let ([ok-type? (exn:application:arity? exn)])
						    (printf " WRONG EXN ~a: ~s~n" 
							    (if ok-type?
								"FIELD"
								"TYPE")
							    exn)
						    (record-error (list exn 
									(if ok-type?
									    'exn-field
									    'exn-type)
									(cons f args)))))
					      (done (void)))])
			     (apply f args))])
		    (printf "~s~n BUT EXPECTED ERROR~n" v)
		    (record-error (list v 'Error (cons f args))))))])
      (let loop ([n 0][l '()])
	(unless (>= n min)
	  (unless (memq n except)
	    (do-test f l (make-ok? n)))
	  (loop (add1 n) (cons 1 l))))
      (let loop ([n min])
	(unless (memq n except)
	  (test #t procedure-arity-includes? f n))
	(unless (>= n max)
	  (loop (add1 n))))
      (if (>= max 0)
	  (do-test f (let loop ([n 0][l '(1)])
		       (if (= n max)
			   l
			   (loop (add1 n) (cons 1 l))))
		   (make-ok? (add1 max)))
	  (test #t procedure-arity-includes? f (arithmetic-shift 1 100))))]
   [(f min max) (arity-test f min max null)]))

(define (test-values l thunk)
  (test l call-with-values thunk list))

(define (report-errs)
  (printf "~nPerformed ~a expression tests (~a good expressions, ~a bad expressions)~n"
	  (+ number-of-tests number-of-error-tests)
	  number-of-tests 
	  number-of-error-tests)
  (printf "and ~a exception field tests.~n~n" 
	  number-of-exn-tests)
  (if (null? errs) 
      (display "Passed all tests.")
      (begin
	(display "Errors were:")
	(newline)
	(display "(SECTION (got expected (call)))")
	(newline)
	(for-each (lambda (l) (write l) (newline))
		  errs)))
  (newline)
  (display "(Other messages report successful tests of error-handling behavior.)")
  (newline))

(define type? exn:application:type?)
(define arity? exn:application:arity?)
(define syntaxe? exn:fail:syntax?)

(define non-z void)

(define (find-depth go)
  ; Find depth that triggers a stack overflow (assuming no other
  ; threads are running and overflowing)
  (let ([v0 (make-vector 6)]
	[v1 (make-vector 6)])
    (let find-loop ([d 100])
      (vector-set-performance-stats! v0)
      (go d)
      (vector-set-performance-stats! v1)
      (if (> (vector-ref v1 5)
	     (vector-ref v0 5))
	  d
	  (find-loop (* 2 d))))))
