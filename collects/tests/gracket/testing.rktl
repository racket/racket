
;; GRacket automatic testing basic functions and macros

(require mzlib/class)

(define SECTION #t)

(define errs null)
(define test-count 0)

(define (test expect name got)
  (set! test-count (add1 test-count))
  (unless (equal? expect got)
    (let ([s (format "~a: expected ~e; got ~e" name expect got)])
      (fprintf (current-error-port) "ERROR: ~a\n" s)
      (set! errs (cons s errs)))))

(define-syntax mismatch
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr)
       (syntax
	(test 'was-mismatch 'mismtach
	      (with-handlers ([exn:fail:contract?
			       (lambda (x)
                                 (printf "~a\n" (exn-message x))
				 'was-mismatch)]
			      [exn:fail? values])
		expr)))])))

(define-syntax st
  (lambda (stx)
    (syntax-case stx ()
      [(_ val obj method . args)
       (syntax
	(test val 'method (send obj method . args)))])))

(define-syntax stv
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (syntax (st (void) . args))])))

(define-syntax stvals
  (lambda (stx)
    (syntax-case stx ()
      [(_ vals obj method . args)
       (syntax
	(test vals 'method (call-with-values (lambda () (send obj method . args)) list)))])))

(define (report-errs)
  (flush-output)
  (sleep 1)
  (newline)
  (if (null? errs)
      (printf "Passed all ~a tests\n" test-count)
      (begin
	(fprintf (current-error-port) "~a Error(s) in ~a tests\n" (length errs) test-count)
	(for-each
	 (lambda (s)
	   (fprintf (current-error-port) "~a\n" s))
	 (reverse errs)))))

