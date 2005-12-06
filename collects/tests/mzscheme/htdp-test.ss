
(define body-accum null)
(define-syntax (htdp-top stx)
  (syntax-case stx (quote)
    [(_ expr)
     #'(set! body-accum (append body-accum (list #'expr)))]))
(define (htdp-top-pop w)
  (set! body-accum (let loop ([body-accum body-accum])
		     (if (null? (cdr body-accum))
			 null
			 (cons (car body-accum) (loop (cdr body-accum)))))))

(define htdp-syntax-test
  (case-lambda
   [(stx) (htdp-syntax-test stx #rx".")]
   [(stx rx)
    (error-test #`(module m #,current-htdp-lang
		    #,@body-accum
		    #,stx)
		(lambda (x)
		  (and (exn:fail:syntax? x)
		       (regexp-match rx (exn-message x)))))]))

(require (rename mzscheme mz-let let))

(define-syntax (htdp-test stx)
  (syntax-case stx ()
    [(_ expect f . args)
     #'(begin
	 (do-htdp-test #'(test expect f . args) #f #f)
	 (htdp-try-direct-module f . args))]))

(define-syntax (htdp-try-direct-module stx)
  (syntax-case stx ()
    [(_ 'nm expr)
     ;; double-check that there's no error, at least,
     ;;  when using the real module-begin:
     #'(mz-let ([name (gensym)])
	       (eval
		#`(module #,name #,current-htdp-lang
		    #,@body-accum
		    expr))
	       (dynamic-require name #f))]
    [_ 
     (printf "~s\n" (syntax-object->datum stx))
     #'(void)]))

(define (htdp-string-to-pred exn?/rx)
  (if (string? exn?/rx)
      (lambda (x)
	(regexp-match exn?/rx (exn-message x)))
      exn?/rx))

(define-syntax (htdp-err/rt-test stx)
  (syntax-case stx ()
    [(_ expr)
     #'(do-htdp-test #'expr #f exn:application:type?)]
    [(_ expr exn?)
     #'(do-htdp-test #'expr #f (htdp-string-to-pred exn?))]))

(define (htdp-error-test stx)
  (do-htdp-test stx #t #f))

(module helper mzscheme
  (define-syntax (module-begin stx)
    (syntax-case stx ()
     [(_ the-test lang to-export . rest)
      #`(#%module-begin
	 (require (rename tester the-test test))
	 (require lang)
         #,@(if (syntax-object->datum (syntax to-export))
                (list (syntax (provide to-export)))
                '())
	 . rest)]))
  (provide (rename module-begin #%module-begin)))
       
(module tester mzscheme
  (define test (namespace-variable-value 'test))
  (provide test))

(define (do-htdp-test stx stx-err? exn?)
  (let ([name (gensym 'm)])
    ((if stx-err? syntax-test eval)
     #`(module #,name helper
	 test
	 (all-except #,current-htdp-lang #%module-begin)
         #f
	 #,@body-accum
	 #,stx))
    (unless stx-err?
      (if exn?
	  (err/rt-test (eval #`(require #,name)) exn?)
	  (eval #`(require #,name))))))

(define-syntax (htdp-eval stx)
  (syntax-case stx ()
    [(_ arg) (syntax (do-htdp-eval #'arg))]))

(define (do-htdp-eval stx)
  (let ([name (gensym 'm)])
    (eval
     #`(module #,name helper
	 test
	 (all-except #,current-htdp-lang #%module-begin)
         the-answer
	 #,@body-accum
	 (define the-answer #,stx)))
    (dynamic-require name 'the-answer)))
