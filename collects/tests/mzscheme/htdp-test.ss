
(define (htdp-syntax-test stx)
  (syntax-test #`(module m #,current-htdp-lang
		   #,stx)))

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

(define-syntax (htdp-test stx)
  (syntax-case stx ()
    [(_ expect f . args)
     #'(do-htdp-test #'(test expect f . args) #f #f)]))

(define-syntax (htdp-err/rt-test stx)
  (syntax-case stx ()
    [(_ expr)
     #'(do-htdp-test #'expr #f exn:application:type?)]
    [(_ expr exn?)
     #'(do-htdp-test #'expr #f exn?)]))

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
