
(load-relative "loadtest.ss")

(require (lib "restart.ss"))

(SECTION 'restart)

(test #t restart-mzscheme #("ignore-me") values #("-qmv") void)
(let ([test-in-out 
       (lambda (pre post in out)
	 (test (string-append "> " out "\n> ")
	       'result
	       (let ([s (open-output-string)])
		 (parameterize ([current-input-port (open-input-string in)]
				[current-output-port s])
		   (restart-mzscheme pre values post void)
		   (get-output-string s)))))])
  (test-in-out #("ignore-me") #("-qm") "(current-command-line-arguments)" "#0()")
  (test-in-out #("") #("-qm") "'Hello" "hello")
  (test-in-out #("-g") #("-qm") "'Hello" "Hello")
  (test-in-out #("") #("-qmg") "'Hello" "Hello"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
