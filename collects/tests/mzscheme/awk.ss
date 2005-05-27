
(load-relative "loadtest.ss")

(SECTION 'awk)

(require (lib "awk.ss"))

(define-syntax (test-awk stx)
  (syntax-case stx ()
    [(_ val body ...) 
     (with-syntax ([next (datum->syntax-object stx 'next)]
		   [result (datum->syntax-object stx 'result)])
       (syntax
	(let* ([p (open-input-string "Hello world.")]
	       [next (lambda () (let ([o (read p)])
				  (if (eof-object? o)
				      o
				      (symbol->string o))))]
	       [r null]
	       [result (lambda (v)
			 (set! r (append r (list v))))])
	  body ...
	  (test val 'awk r))))]))
			
		

(test-awk '(hi there)
	  (awk (next) (str) ()
	       ["ell" (result 'hi)]
	       ["x" (result 'yikes)]
	       ["orl" (result 'there)]))

(test-awk '("l" done)
	  (awk (next) (str) ()
	       [/ "el(l)" / (#f a) (result a)]
	       [(equal? str "world.") (result 'done)]))

(report-errs)
