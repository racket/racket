#lang racket/base
(require "test-suite-utils.rkt")

(module test racket/base)

(test 'exit/no-prompt
      (lambda (x)
	(and (eq? x 'passed)
	     (not (mred-running?))))
      (lambda ()
	(with-handlers ([eof-result? (lambda (x) 'passed)])
	  (send-sexp-to-mred '(begin (exit:exit) (sleep/yield 1)))
	  'failed)))

(define tmp-file (build-path (find-system-path 'temp-dir) "framework-exit-test-suite"))
;; need to test "on" callbacks
(test 'exit-callback-called
      (lambda (x)
	(begin0 (and (file-exists? tmp-file) (not (mred-running?)))
		(when (file-exists? tmp-file) (delete-file tmp-file))))

      (lambda ()
	(when (file-exists? tmp-file) (delete-file tmp-file))
	(with-handlers ([eof-result? (lambda (x) 'passed)])
	  (send-sexp-to-mred
	   `(begin
	      (exit:insert-can?-callback (lambda () (call-with-output-file (bytes->path ,(path->bytes tmp-file)) void) #t))
	      (begin (exit:exit) (sleep/yield 1)))))))

(test 'exit-callback-removed
      (lambda (x) (and (eq? x 'passed) (not (mred-running?))))
      (lambda ()
	(with-handlers ([eof-result? (lambda (x) 'passed)])
	  (send-sexp-to-mred
	   `(begin
	      ((exit:insert-can?-callback (lambda () (error 'called-exit-callback))))
	      (begin (exit:exit) (sleep/yield 1)))))))

(test 'exit-callback-stops-exit
      (lambda (x) (eq? x 'passed))
      (lambda ()
	(begin0
	 (send-sexp-to-mred
	  `(begin
	     (let ([rm-callback (exit:insert-can?-callback (lambda () #f))])
	       (exit:exit)
	       (rm-callback)
	       'passed)))
	 (with-handlers ([eof-result? (lambda (x) 'passed)])
	   (send-sexp-to-mred
	    `(exit:exit))))))
